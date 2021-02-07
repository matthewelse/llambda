open! Core
open! Import
module Backend_var = Ocaml_optcomp.Backend_var
module Debug_info = Ocaml_common.Debuginfo

let type_of_memory_chunk ctx (chunk : Cmm.memory_chunk) =
  match chunk with
  | Byte_unsigned | Byte_signed -> Cmm.typ_int, i8_type ctx
  | Sixteen_unsigned | Sixteen_signed -> Cmm.typ_int, i16_type ctx
  | Thirtytwo_unsigned | Thirtytwo_signed -> Cmm.typ_int, i32_type ctx
  | Word_int -> Cmm.typ_int, i64_type ctx
  | Word_val -> Cmm.typ_val, Declarations.value_type ctx
  | Single -> Cmm.typ_float, float_type ctx
  | Double | Double_u -> Cmm.typ_float, double_type ctx
;;

let type_of_expression ctx env expression =
  let unify (machtypel, lltypel) (machtyper, lltyper) =
    match machtypel, machtyper with
    | None, None -> machtypel, lltypel
    | Some Cmm.Int, Some Cmm.Int ->
      (* Let's assume that we sign-extend things or something. *)
      let larger_bitwidth =
        Int.max (integer_bitwidth lltypel) (integer_bitwidth lltyper)
      in
      machtypel, integer_type ctx larger_bitwidth
    | Some l, Some r ->
      let machtype = Some (Cmm.lub_component l r) in
      machtype, Declarations.type_of_machtype ctx machtype
    | _ -> assert false
  in
  let rec machtype_of_operation (op : Cmm.operation) env args =
    match op with
    | Capply typ -> typ, Declarations.type_of_machtype ctx typ
    | Cextcall (_, result, _, _) -> result, Declarations.type_of_machtype ctx result
    | Cload (memory_chunk, _) -> type_of_memory_chunk ctx memory_chunk
    | Calloc -> Cmm.typ_val, Declarations.type_of_machtype ctx Cmm.typ_val
    | Cstore _ -> None, Declarations.type_of_machtype ctx None
    | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor | Cxor | Clsl | Clsr
    | Casr ->
      List.map args ~f:(machtype_of_expression env)
      |> List.reduce_exn ~f:(fun (machtype1, lltype1) (machtype2, lltype2) ->
             match machtype1, machtype2 with
             | Some Cmm.Float, _ | _, Some Float | None, None -> assert false
             | Some Int, Some Int ->
               (* Let's assume that we sign-extend things or something. *)
               let larger_bitwidth =
                 Int.max (integer_bitwidth lltype1) (integer_bitwidth lltype2)
               in
               machtype1, integer_type ctx larger_bitwidth
             | Some l, Some r ->
               let machtype = Some (Cmm.lub_component l r) in
               machtype, Declarations.type_of_machtype ctx machtype
             | _ -> assert false)
    | Ccmpi _ -> Cmm.typ_int, i1_type ctx
    | Caddv -> Cmm.typ_val, Declarations.type_of_machtype ctx Cmm.typ_val
    | Cadda -> Cmm.typ_addr, Declarations.type_of_machtype ctx Cmm.typ_addr
    | Ccmpa _ ->
      (* TODO: this can be a boolean *)
      Cmm.typ_int, i1_type ctx
    | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf ->
      List.map args ~f:(machtype_of_expression env)
      |> List.reduce_exn ~f:(fun (machtype1, lltype1) (machtype2, lltype2) ->
             match machtype1, machtype2 with
             | Some Cmm.Float, _ | _, Some Float ->
               if phys_equal lltype1 lltype2
               then machtype1, lltype1
               else
                 raise_s
                   [%message
                     "Tried to compare two floats with different sizes. This seems \
                      sketchy."
                       (lltype1 : Ir_type.t)
                       (lltype2 : Ir_type.t)]
             | _ -> assert false)
    | Cfloatofint -> Cmm.typ_float, Declarations.type_of_machtype ctx Cmm.typ_float
    | Cintoffloat -> Cmm.typ_int, Declarations.type_of_machtype ctx Cmm.typ_int
    | Ccmpf _ -> Cmm.typ_int, i1_type ctx
    | Craise _ -> raise_s [%message "TODO: handle raising"]
    | Ccheckbound -> raise_s [%message "idk"]
  and machtype_of_expression env (expr : Cmm.expression) =
    match expr with
    | Clet (name, value, body) | Clet_mut (name, _, value, body) ->
      let name = Backend_var.With_provenance.name name in
      let type_value = machtype_of_expression env value in
      String.Table.add_exn env ~key:name ~data:type_value;
      let body_type = machtype_of_expression env body in
      String.Table.remove env name;
      body_type
    | Cvar name -> String.Table.find_exn env (Backend_var.name name)
    | Cconst_int _ -> Cmm.typ_int, Declarations.type_of_machtype ctx Cmm.typ_int
    | Cconst_natint _ -> Cmm.typ_int, Declarations.type_of_machtype ctx Cmm.typ_int
    | Cconst_float _ -> Cmm.typ_float, Declarations.type_of_machtype ctx Cmm.typ_float
    | Cphantom_let _ -> raise_s [%message "TODO: handle phantom let bindings."]
    | Cop (op, args, _) -> machtype_of_operation op env args
    | Cconst_pointer _ -> Cmm.typ_addr, Declarations.type_of_machtype ctx Cmm.typ_addr
    | Csequence (_, r) -> machtype_of_expression env r
    | Cifthenelse (_, _, then_, _, else_, _) ->
      unify (machtype_of_expression env then_) (machtype_of_expression env else_)
    | _ -> raise_s [%message "Trying to find type for unknown expression type."]
  in
  machtype_of_expression env expression |> snd
;;

let to_integer_if_pointer ~ctx ~builder value =
  match classify_type (type_of value) with
  | Pointer -> build_ptrtoint value (i64_type ctx) "" builder
  | Integer -> value
  | _ -> failwith "idk"
;;

let to_pointer_if_integer ~builder ~typ value =
  match classify_type (type_of value) with
  | Pointer -> build_pointercast value typ "" builder
  | Integer -> build_inttoptr value typ "" builder
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
  | Cop (operation, args, debug_info) -> codegen_operation t operation args debug_info
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
        eprint_s
          [%message
            "unable to find symbol"
              (name : string)
              ~env:
                (t.env : [ `Immutable of _ | `Mutable of _ | `Value of _ ] String.Table.t)];
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
  | _ -> assert false

and codegen_operation t operation args (_ : Debug_info.t) =
  match operation, args with
  | Capply return_type, func :: args ->
    let types =
      Option.map return_type ~f:(function
          | Val -> [%sexp Val]
          | Addr -> [%sexp Addr]
          | Int -> [%sexp Int]
          | Float -> [%sexp Float])
    in
    let func = codegen_expr t func in
    let func_type = type_of func |> element_type in
    let typs = param_types func_type |> Array.to_list in
    eprint_s
      [%message "function call" (types : Sexp.t option) (string_of_lltype func_type)];
    let args =
      List.map2_exn args typs ~f:(fun arg typ ->
          let arg = codegen_expr t arg in
          to_pointer_if_integer ~builder:t.builder ~typ arg)
    in
    let call = build_call func (List.to_array args) "" t.builder in
    set_instruction_call_conv Declarations.ghc_calling_convention call;
    call
  | Capply _, [] -> raise_s [%message "capply with empty list"]
  | Caddv, [ pointer; offset ] ->
    let pointer = codegen_expr t pointer in
    let offset = codegen_expr t offset in
    build_gep pointer [| offset |] "" t.builder
  | Caddi, [ left; right ] ->
    let left =
      codegen_expr t left |> to_integer_if_pointer ~ctx:t.ctx ~builder:t.builder
    in
    let right =
      codegen_expr t right |> to_integer_if_pointer ~ctx:t.ctx ~builder:t.builder
    in
    build_add left right "" t.builder
  | Csubi, [ left; right ] ->
    let left =
      codegen_expr t left |> to_integer_if_pointer ~ctx:t.ctx ~builder:t.builder
    in
    let right =
      codegen_expr t right |> to_integer_if_pointer ~ctx:t.ctx ~builder:t.builder
    in
    build_sub left right "" t.builder
  | Cadda, [ pointer; offset ] ->
    let pointer = codegen_expr t pointer in
    let offset = codegen_expr t offset in
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
  | Clsl, [ left; right ] ->
    let left = codegen_expr t left in
    let right = codegen_expr t right in
    build_shl left right "" t.builder
  | Casr, [ left; right ] ->
    let left = codegen_expr t left in
    let right = codegen_expr t right in
    build_ashr left right "" t.builder
  | Cstore (memory_chunk, _), [ value; dst ] ->
    let dst = codegen_expr t dst in
    let mem_type = Declarations.type_of_memory_chunk t.ctx memory_chunk in
    let ptr_type = pointer_type mem_type in
    let dst = build_pointercast dst ptr_type "" t.builder in
    let value = codegen_expr t value in
    let value = build_zext_or_bitcast value mem_type "" t.builder in
    build_store value dst t.builder
  | Cstore _, _ -> failwith "store has too many arguments."
  | Cload (memory_chunk, _), [ src ] ->
    let src = codegen_expr t src in
    let ptr_type = pointer_type (Declarations.type_of_memory_chunk t.ctx memory_chunk) in
    let ptr = build_pointercast src ptr_type "" t.builder in
    let raw = build_load ptr "" t.builder in
    (match memory_chunk with
    | Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed | Thirtytwo_unsigned
    | Thirtytwo_signed ->
      build_sext raw (i64_type t.ctx) "" t.builder
    | Word_int | Word_val -> raw
    | Single | Double | Double_u -> raw)
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
      string_of_llvalue left |> print_endline;
      string_of_llvalue right |> print_endline;
      failwith "don't know how to build this comparison.")
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
  | Cextcall (name, return_type, does_alloc, label), args ->
    eprint_s
      [%message
        "wtf does label mean" (name : string) (label : int option) (does_alloc : bool)];
    let arg_types = List.map args ~f:(fun _ -> Declarations.value_type t.ctx) in
    let function_type =
      function_type
        (Declarations.type_of_machtype t.ctx return_type)
        (Array.of_list arg_types)
    in
    let func = Ir_module.declare_function t.this_module ~name ~funtype:function_type in
    build_call
      func
      (Array.of_list
         (List.map2_exn args arg_types ~f:(fun arg typ ->
              codegen_expr t arg |> to_pointer_if_integer ~builder:t.builder ~typ)))
      ""
      t.builder
  | _ -> raise_s [%message "I don't know how to compile this operator."]
;;

let type_of_function ctx (fundecl : Cmm.fundecl) =
  let return_type = type_of_expression ctx (String.Table.create ()) fundecl.fun_body in
  let args =
    List.map fundecl.fun_args ~f:(fun (_, machtype) ->
        Declarations.type_of_machtype ctx machtype)
  in
  function_type return_type (List.to_array args)
;;

let value_of_data_item this_module ctx (data_item : Cmm.data_item) =
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
  | Csymbol_address name ->
    (match
       Option.first_some
         (lookup_global name this_module)
         (lookup_function name this_module)
     with
    | Some value -> Some value
    | None -> Some (declare_global (Declarations.void_pointer_type ctx) name this_module))
  | Cskip _ | Calign _ -> None
;;

let rec structify ~ctx ~this_module (items : Cmm.data_item list) =
  match items with
  | [] -> []
  | (Cdefine_symbol _ | Cglobal_symbol _) :: xs -> structify ~ctx ~this_module xs
  | value :: xs ->
    (match value_of_data_item this_module ctx value with
    | None -> structify ~this_module ~ctx xs
    | Some value -> value :: structify ~this_module ~ctx xs)
;;

let emit (cmm : Cmm.phrase list) =
  let ctx = Ir_context.global () in
  Ir_module.with_module ~ctx "melse" (fun this_module ->
      let builder = Ir_builder.create ctx in
      List.iter (Declarations.builtin_functions ctx) ~f:(fun (name, funtype) ->
          Ir_module.declare_function' this_module ~name ~funtype);
      (* Pre-define functions and globals to avoid issues with ordering. *)
      List.iter cmm ~f:(function
          | Cfunction cfundecl ->
            (* It seems kind of strange that we don't always return a value here... *)
            let function_type = type_of_function ctx cfundecl in
            eprint_s [%message "declaring function" (cfundecl.fun_name : string)];
            Ir_module.declare_function'
              this_module
              ~name:cfundecl.fun_name
              ~funtype:function_type
          | Cdata _ -> ());
      List.iter cmm ~f:(function
          | Cfunction _ -> ()
          | Cdata items ->
            let global_names =
              List.filter_map items ~f:(function
                  | Cglobal_symbol name -> Some name
                  | _ -> None)
              |> String.Set.of_list
            in
            let items =
              List.filter items ~f:(function
                  | Cdefine_symbol name -> not (String.Set.mem global_names name)
                  | _ -> true)
            in
            let rec pointers ~index ~pointer (items : Cmm.data_item list) =
              match items with
              | [] -> []
              | Cdefine_symbol name :: xs ->
                let ptr = build_struct_gep pointer index name builder in
                let ptr =
                  build_pointercast
                    ptr
                    (Declarations.type_of_machtype ctx (Some Addr))
                    ""
                    builder
                in
                (Linkage.External, name, ptr) :: pointers ~index ~pointer xs
              | Cglobal_symbol name :: xs ->
                let ptr = build_struct_gep pointer index name builder in
                let ptr =
                  build_pointercast
                    ptr
                    (Declarations.type_of_machtype ctx (Some Addr))
                    ""
                    builder
                in
                (External, name, ptr) :: pointers ~index ~pointer xs
              | value :: xs ->
                (match value_of_data_item this_module ctx value with
                | None -> pointers ~index ~pointer xs
                | Some _ -> pointers ~index:(index + 1) ~pointer xs)
            in
            let glob_value =
              match structify ~ctx ~this_module items with
              | [] -> None
              | elems -> Some (const_struct ctx (Array.of_list elems))
            in
            (match glob_value with
            | None -> ()
            | Some glob_value ->
              let anon_global = define_global "" glob_value this_module in
              set_linkage External anon_global;
              let pointers = pointers ~index:0 ~pointer:anon_global items in
              List.iter pointers ~f:(fun (linkage, name, route) ->
                  let glob = Ir_module.define_global this_module ~name route in
                  set_linkage linkage glob)));
      (* Compile the functions and globals. *)
      List.iter cmm ~f:(function
          | Cfunction cfundecl ->
            let fundecl =
              Ir_module.lookup_function_exn this_module ~name:cfundecl.fun_name
            in
            set_function_call_conv Declarations.ghc_calling_convention fundecl;
            (* set_gc (Some "ocaml") fundecl; *)
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
              raise_s
                [%message
                  "exception raised while compiling function"
                    ~name:(cfundecl.fun_name : string)
                    (exn : Exn.t)])
          | Cdata _ -> ());
      string_of_llmodule this_module |> print_endline)
;;
