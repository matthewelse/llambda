open Core
open Llvm
open Wrapllvm

let optype ctx (op : Cmm.operation) =
  match op with
  | Caddi
  | Csubi
  | Cmuli
  | Cmulhi
  | Cdivi
  | Cmodi
  | Cand
  | Cor
  | Cxor
  | Clsl
  | Clsr
  | Casr -> i64_type ctx
  | Cstore _ -> void_type ctx
  | Ccmpi _ -> i1_type ctx
  | Calloc -> pointer_type (i8_type ctx)
  | Capply machtype -> Declarations.type_of_machtype ctx machtype
  | _ ->
    Format.print_string ("Unknown operation type: " ^ Printcmm.operation Debuginfo.none op);
    Format.print_newline ();
    assert false
;;

let rec exprtype ctx env (expr : Cmm.expression) =
  match expr with
  | Clet (name, expr, body) | Clet_mut (name, _, expr, body) ->
    let name = Backend_var.With_provenance.name name in
    String.Table.add_exn env ~key:name ~data:(exprtype ctx env expr);
    let body_type = exprtype ctx env body in
    String.Table.remove env name;
    body_type
  | Cvar name -> String.Table.find_exn env (Backend_var.name name)
  | Cconst_int _ ->
    (* this is a little weird, since this can be used to construct boxed
    integers too :/ *)
    i64_type ctx
  | Cconst_natint _ -> i64_type ctx
  | Cconst_float _ -> double_type ctx
  | Cphantom_let (_, _, body) | Csequence (_, body) | Cifthenelse (_, _, body, _, _, _) ->
    exprtype ctx env body
  | Cop (op, _, _) -> optype ctx op
  | Cconst_pointer _ ->
    (* TODO melse: this is removed in 4.12 anyway *)
    pointer_type (i8_type ctx)
  | _ ->
    print_s [%message "error while trying to type expression"];
    Printcmm.expression Format.err_formatter expr;
    print_endline "";
    assert false
;;

let type_of_memory_chunk ctx (chunk : Cmm.memory_chunk) =
  match chunk with
  | Byte_unsigned -> i8_type ctx
  | Byte_signed -> i8_type ctx (* signed/unsigned addition? *)
  | Sixteen_unsigned -> i16_type ctx
  | Sixteen_signed -> i16_type ctx
  | Thirtytwo_signed -> i32_type ctx
  | Thirtytwo_unsigned -> i32_type ctx
  | Word_int -> i64_type ctx
  | Word_val -> pointer_type (i8_type ctx)
  | Single -> float_type ctx
  | Double | Double_u -> double_type ctx
;;

let to_integer_if_pointer ~ctx ~builder value =
  match classify_type (type_of value) with
  | Pointer -> build_ptrtoint value (i64_type ctx) "" builder
  | Integer -> value
  | _ -> failwith "idk"
;;

type t =
  { ctx : llcontext
  ; builder : llbuilder
  ; this_module : llmodule
  ; env :
      [ `Mutable of llvalue | `Immutable of llvalue | `Value of llvalue ] String.Table.t
  ; fundecl : llvalue
  ; catches : llbasicblock Int.Table.t
  }

let rec codegen_expr t (expr : Cmm.expression) =
  match expr with
  | Cconst_int (value, _) -> const_int (i64_type t.ctx) value
  | Cconst_natint (value, _) -> const_int (i64_type t.ctx) (Nativeint.to_int_exn value)
  | Cconst_float (value, _) -> const_float (double_type t.ctx) value
  | Cop (operation, args, debug_info) -> codegen_op t operation args debug_info
  | Cconst_pointer (value, _) ->
    let intval = const_int (i64_type t.ctx) value in
    const_inttoptr intval (pointer_type (i8_type t.ctx))
  | Cvar var ->
    let real_name = Backend_var.name var in
    let var = String.Table.find_exn t.env real_name in
    (match var with
    | `Immutable ptr | `Mutable ptr -> build_load ptr "" t.builder
    | `Value value -> value)
  | Csequence (before, after) ->
    let (_ : llvalue) = codegen_expr t before in
    codegen_expr t after
  | Clet (var, value, body) ->
    let value = codegen_expr t value in
    let ptr = build_alloca (type_of value) "" t.builder in
    let (_ : llvalue) = build_store value ptr t.builder in
    let name = Backend_var.With_provenance.name var in
    String.Table.add_exn t.env ~key:name ~data:(`Immutable ptr);
    let body = codegen_expr t body in
    String.Table.remove t.env name;
    body
  | Clet_mut (var, machtype, value, body) ->
    let ptr = build_alloca (Declarations.type_of_machtype t.ctx machtype) "" t.builder in
    let value = codegen_expr t value in
    let (_ : llvalue) = build_store value ptr t.builder in
    String.Table.add_exn
      t.env
      ~key:(Backend_var.With_provenance.name var)
      ~data:(`Mutable ptr);
    let body = codegen_expr t body in
    String.Table.remove t.env (Backend_var.With_provenance.name var);
    body
  | Cconst_symbol (name, _) ->
    (match lookup_global name t.this_module with
    | Some global -> global
    | None ->
      (match lookup_function name t.this_module with
      | None ->
        print_s
          [%message
            "unable to find symbol"
              (name : string)
              ~env:
                (t.env : [ `Immutable of _ | `Mutable of _ | `Value of _ ] String.Table.t)];
        Printcmm.expression Format.err_formatter expr;
        assert false
      | Some fn -> fn))
  | Cifthenelse (cond, _, then_, _, else_, _) ->
    let start_bb = insertion_block t.builder in
    let cond = codegen_expr t cond in
    let then_bb = append_block t.ctx "then" t.fundecl in
    position_at_end then_bb t.builder;
    let then_value = codegen_expr t then_ in
    (* in case the then block adds basic blocks *)
    let real_then_bb = insertion_block t.builder in
    let else_bb = append_block t.ctx "else" t.fundecl in
    position_at_end else_bb t.builder;
    let else_value = codegen_expr t else_ in
    (* in case the else block adds basic blocks *)
    let real_else_bb = insertion_block t.builder in
    let merge_bb = append_block t.ctx "merge" t.fundecl in
    position_at_end merge_bb t.builder;
    (* add a cond branch to the original bb *)
    position_at_end start_bb t.builder;
    let (_ : llvalue) = build_cond_br cond then_bb else_bb t.builder in
    (* branch from the ends of the conditional branches to the merge branch *)
    position_at_end real_then_bb t.builder;
    let incoming =
      match block_terminator real_then_bb with
      | None ->
        let (_ : llvalue) = build_br merge_bb t.builder in
        [ then_value, real_then_bb ]
      | Some _ -> []
    in
    position_at_end real_else_bb t.builder;
    let incoming =
      match block_terminator real_else_bb with
      | None ->
        let (_ : llvalue) = build_br merge_bb t.builder in
        (else_value, real_else_bb) :: incoming
      | Some _ -> incoming
    in
    (match incoming with
    | [] ->
      remove_block merge_bb;
      const_int (i64_type t.ctx) 1
    | _ ->
      position_at_end merge_bb t.builder;
      build_phi incoming "iftmp" t.builder)
  | Ccatch (_, [ (index, [], handler, _) ], body) ->
    let body_bb = insertion_block t.builder in
    let handler_bb = append_block t.ctx [%string "handler.%{index#Int}"] t.fundecl in
    let exit_bb = append_block t.ctx [%string "exit.%{index#Int}"] t.fundecl in
    Int.Table.add_exn t.catches ~key:index ~data:handler_bb;
    (* handler *)
    position_at_end handler_bb t.builder;
    let handler_value = codegen_expr t handler in
    let real_handler_bb = insertion_block t.builder in
    let incoming =
      match block_terminator real_handler_bb with
      | None ->
        let (_ : llvalue) = build_br exit_bb t.builder in
        [ handler_value, real_handler_bb ]
      | Some _ -> []
    in
    (* body *)
    position_at_end body_bb t.builder;
    let body_value = codegen_expr t body in
    let real_body_bb = insertion_block t.builder in
    Int.Table.remove t.catches index;
    let incoming =
      match block_terminator real_body_bb with
      | None ->
        let (_ : llvalue) = build_br exit_bb t.builder in
        (body_value, real_body_bb) :: incoming
      | Some _ -> incoming
    in
    (match incoming with
    | [] ->
      remove_block exit_bb;
      const_int (i64_type t.ctx) 1
    | _ ->
      position_at_end exit_bb t.builder;
      build_phi incoming [%string "phi.%{index#Int}"] t.builder)
  | Cexit (index, []) ->
    let (_ : llvalue) = build_br (Int.Table.find_exn t.catches index) t.builder in
    const_int (i64_type t.ctx) 1
  | Cassign (var, expr) ->
    let name = Backend_var.name var in
    (match String.Table.find t.env name with
    | None | Some (`Immutable _ | `Value _) ->
      raise_s [%message "Unknown mutable variable." (name : string)]
    | Some (`Mutable ptr) ->
      let value = codegen_expr t expr in
      let (_ : llvalue) = build_store value ptr t.builder in
      const_int (i64_type t.ctx) 1)
  | Ctuple [] -> const_int (i64_type t.ctx) 1
  | _ ->
    Printcmm.expression Format.err_formatter expr;
    assert false

and codegen_op t operation args debug_info =
  match operation, args with
  | Capply _, func :: args ->
    let func = codegen_expr t func in
    let args = List.map args ~f:(codegen_expr t) in
    build_call func (List.to_array args) "" t.builder
  | Capply _, [] -> raise_s [%message "capply with empty list"]
  | Caddv, [ pointer; offset ] ->
    let pointer = codegen_expr t pointer in
    let offset = codegen_expr t offset in
    build_gep pointer [| offset |] "" t.builder
  | Caddi, values ->
    List.map values ~f:(fun value ->
        codegen_expr t value |> to_integer_if_pointer ~ctx:t.ctx ~builder:t.builder)
    |> List.reduce_exn ~f:(fun l r -> build_add l r "" t.builder)
  | Cadda, [ pointer; offset ] ->
    let pointer = codegen_expr t pointer in
    let offset = codegen_expr t offset in
    string_of_llvalue pointer |> print_endline;
    string_of_llvalue offset |> print_endline;
    build_gep pointer [| offset |] "" t.builder
  | Cadda, _ -> failwith "adda has too many arguments."
  | Cor, [ left; right ] ->
    let left = codegen_expr t left in
    let right = codegen_expr t right in
    build_or left right "" t.builder
  | Clsr, [ left; right ] ->
    let left = codegen_expr t left in
    let right = codegen_expr t right in
    build_lshr left right "" t.builder
  | Casr, [ left; right ] ->
    let left = codegen_expr t left in
    let right = codegen_expr t right in
    build_ashr left right "" t.builder
  | Cstore _, [ value; dst ] ->
    let dst = codegen_expr t dst in
    let value = codegen_expr t value in
    build_store value dst t.builder
  | Cstore _, _ -> failwith "store has too many arguments."
  | Cload (memory_chunk, _), [ src ] ->
    let src = codegen_expr t src in
    let ptr_type = pointer_type (type_of_memory_chunk t.ctx memory_chunk) in
    let ptr = build_pointercast src ptr_type "" t.builder in
    build_load ptr "" t.builder
  | Cload _, _ -> failwith "load has too many arguments."
  | Ccmpi cmp, [ left; right ] ->
    let left = codegen_expr t left in
    let right = codegen_expr t right in
    let left_type = type_of left in
    let right_type = type_of right in
    let (cmp_type : TypeKind.t), left, right =
      match classify_type left_type, classify_type right_type with
      | Pointer, Pointer -> Pointer, left, right
      | Integer, Integer -> Integer, left, right
      | Pointer, Integer -> Pointer, left, build_inttoptr right left_type "" t.builder
      | Integer, Pointer -> Pointer, build_inttoptr left right_type "" t.builder, right
      | _ ->
        string_of_llvalue left |> print_endline;
        string_of_llvalue right |> print_endline;
        failwith "don't know how to compare these things"
    in
    (match cmp_type, cmp with
    | Integer, Cne -> build_icmp Ne left right "" t.builder
    | Pointer, Cne ->
      let diff = build_ptrdiff left right "" t.builder in
      build_icmp Ne diff (const_int (type_of diff) 0) "" t.builder
    | Integer, Ceq -> build_icmp Eq left right "" t.builder
    | Pointer, Ceq ->
      let diff = build_ptrdiff left right "" t.builder in
      build_icmp Eq diff (const_int (type_of diff) 0) "" t.builder
    | Integer, Cgt -> build_icmp Sgt left right "" t.builder
    | Pointer, Cgt ->
      let diff = build_ptrdiff left right "" t.builder in
      build_icmp Sgt diff (const_int (type_of diff) 0) "" t.builder
    | _ ->
      let cmp = Printcmm.integer_comparison cmp in
      string_of_llvalue left |> print_endline;
      string_of_llvalue right |> print_endline;
      failwith ("don't know how to build this comparison " ^ cmp))
  | Calloc, data ->
    let ptr_ptr (* : val pointer *) =
      build_alloca (pointer_type (i8_type t.ctx)) "" t.builder
    in
    let ptr =
      build_call
        (lookup_function "caml_alloc" t.this_module
        |> Option.value_exn ~message:"(BUG) caml_alloc not defined.")
        [| const_int (i64_type t.ctx) (List.length data); const_int (i32_type t.ctx) 0 |]
        ""
        t.builder
    in
    let (_ : llvalue) = build_store ptr ptr_ptr t.builder in
    let (_ : llvalue) =
      build_call
        (lookup_function "llvm.gcroot" t.this_module |> Option.value_exn)
        [| ptr_ptr; const_null (pointer_type (i8_type t.ctx)) |]
        ""
        t.builder
    in
    ptr
  | _ ->
    let operation = Printcmm.operation debug_info operation in
    raise_s [%message "I don't know how to compile this operator." operation]
;;

let funtype ctx (fundecl : Cmm.fundecl) =
  let return_type = exprtype ctx (String.Table.create ()) fundecl.fun_body in
  let args =
    List.map fundecl.fun_args ~f:(fun (_, machtype) ->
        Declarations.type_of_machtype ctx machtype)
  in
  function_type return_type (List.to_array args)
;;

let value_of_data_item ctx (data_item : Cmm.data_item) =
  match data_item with
  | Cdefine_symbol _ | Cglobal_symbol _ -> None
  | Cint8 value -> Some (const_int (i8_type ctx) value)
  | Cint16 value -> Some (const_int (i16_type ctx) value)
  | Cint32 value -> Some (const_of_int64 (i32_type ctx) (Nativeint.to_int64 value) true)
  | Cint value -> Some (const_of_int64 (i64_type ctx) (Nativeint.to_int64 value) true)
  | Csingle value -> Some (const_float (float_type ctx) value)
  | Cdouble value -> Some (const_float (double_type ctx) value)
  | Cstring str ->
    (* should this string have 00 01 02 03 at the end? *) Some (const_stringz ctx str)
  | Csymbol_address _ -> (* TODO? *) None
  | Cskip _ | Calign _ -> None
;;

let emit (cmm : Cmm.phrase list) =
  let ctx = Ir_context.global () in
  Ir_module.with_module ~ctx "melse" (fun this_module ->
      let builder = Ir_builder.create ctx in
      List.iter (Declarations.builtin_functions ctx) ~f:(fun (name, funtype) ->
          Ir_module.declare_function' this_module ~name ~funtype);
      (* Pre-define functions to avoid issues with function ordering. *)
      List.iter cmm ~f:(function
          | Cfunction cfundecl ->
            let funtype = funtype ctx cfundecl in
            Ir_module.declare_function' this_module ~name:cfundecl.fun_name ~funtype
          | Cdata _ -> ());
      (* Compile the functions and globals. *)
      List.iter cmm ~f:(function
          | Cfunction cfundecl ->
            let fundecl =
              Ir_module.lookup_function_exn this_module ~name:cfundecl.fun_name
            in
            set_gc (Some "ocaml") fundecl;
            (* set argument names *)
            let args =
              List.map2_exn
                (params fundecl |> Array.to_list)
                cfundecl.fun_args
                ~f:(fun arg (name, _) ->
                  let real_name = Backend_var.With_provenance.name name in
                  set_value_name real_name arg;
                  real_name, `Value arg)
            in
            let env = String.Table.of_alist_exn args in
            let catches = Int.Table.create () in
            String.Table.add_exn env ~key:cfundecl.fun_name ~data:(`Value fundecl);
            let entry = append_block ctx "entry" fundecl in
            position_at_end entry builder;
            (try
               let ret_val =
                 codegen_expr
                   { ctx; builder; env; this_module; fundecl; catches }
                   cfundecl.fun_body
               in
               build_ret ret_val builder |> (ignore : llvalue -> unit)
             with
            | exn ->
              delete_function fundecl;
              eprint_s
                [%message
                  "exception raised while compiling function"
                    ~name:(cfundecl.fun_name : string)
                    (exn : Exn.t)])
          | Cdata items ->
            (* This is kind of weird, but the way to think about this is one big
               anonymous global, with a few labels inside it. *)
            let glob_value =
              let struct_if_non_empty acc =
                match acc with
                | [] -> None
                | [ x ] -> Some x
                | xs -> const_struct ctx (Array.of_list xs) |> Some
              in
              let items = List.rev items in
              let glob_value, pointers =
                List.fold_map items ~init:[] ~f:(fun acc next ->
                    match next with
                    | Cdefine_symbol name ->
                      (* private *)
                      let value = struct_if_non_empty acc in
                      Option.to_list value, Option.map value ~f:(fun value -> name, value)
                    | Cglobal_symbol name ->
                      let value = struct_if_non_empty acc in
                      Option.to_list value, Option.map value ~f:(fun value -> name, value)
                    | value ->
                      (match value_of_data_item ctx value with
                      | None -> acc, None
                      | Some value -> value :: acc, None))
              in
              let pointers = List.filter_opt pointers in
              List.iter pointers ~f:(fun (name, value) ->
                  let (_ : llvalue) = define_global name value this_module in
                  ());
              struct_if_non_empty glob_value
            in
            (match glob_value with
            | None -> ()
            | Some glob_value ->
              let _anon_global = define_global "" glob_value this_module in
              ()
              (* Add labels to internal things *)));
      string_of_llmodule this_module |> print_endline)
;;
