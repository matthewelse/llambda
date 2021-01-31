open Core
open Llvm

let type_of_machtype ctx (machtype : Cmm.machtype) =
  match machtype with
  | [||] -> void_type ctx
  | [| component |] ->
    (match component with
    | Val | Addr | Int -> i64_type ctx
    | Float -> float_type ctx)
  | _ -> assert false
;;

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
  | _ -> assert false
;;

let rec exprtype ctx (expr : Cmm.expression) =
  match expr with
  | Cconst_int _ -> i64_type ctx
  | Cconst_natint _ -> i64_type ctx
  | Cconst_float _ -> double_type ctx
  | Clet (_, _, body)
  | Clet_mut (_, _, _, body)
  | Cphantom_let (_, _, body)
  | Csequence (_, body) -> exprtype ctx body
  | Cop (op, _, _) -> optype ctx op
  | Cconst_pointer _ ->
    (* TODO melse: this is removed in 4.12 anyway *)
    pointer_type (i64_type ctx)
  | _ -> assert false
;;

let rec codegen_expr ~ctx ~builder ~env ~this_module (expr : Cmm.expression) =
  match expr with
  | Cconst_int (value, _) -> const_int (i64_type ctx) value
  | Cconst_natint (value, _) -> const_int (i64_type ctx) (Nativeint.to_int_exn value)
  | Cconst_float (value, _) -> const_float (double_type ctx) value
  | Cop (Caddv, values, _) ->
    (* 2n + 1 + 2m + 1 = 2(n + m) + 2, so subtract one at the end... this is
    probably wrong for pointers though *)
    let exprs = List.map values ~f:(codegen_expr ~ctx ~builder ~env ~this_module) in
    let exprs = const_int (i64_type ctx) (-1) :: exprs in
    List.reduce_exn exprs ~f:(fun l r -> build_add l r "" builder)
  | Cop (Caddi, values, _) ->
    List.map values ~f:(codegen_expr ~ctx ~builder ~env ~this_module)
    |> List.reduce_exn ~f:(fun l r -> build_add l r "" builder)
  | Cop (Cstore _, [ value; dst ], _) ->
    let dst = codegen_expr ~ctx ~builder ~env ~this_module dst in
    let value = codegen_expr ~ctx ~builder ~env ~this_module value in
    build_store value dst builder
  | Cconst_pointer (value, _) ->
    let intval = const_int (i64_type ctx) value in
    const_inttoptr intval (pointer_type (i64_type ctx))
  | Cvar var ->
    let real_name = Backend_var.name var in
    String.Table.find_exn env real_name
  | Csequence (before, after) ->
    let (_ : llvalue) = codegen_expr ~ctx ~builder ~env ~this_module before in
    codegen_expr ~ctx ~builder ~env ~this_module after
  | Clet (var, value, body) ->
    let value = codegen_expr ~ctx ~builder ~env ~this_module value in
    String.Table.add_exn env ~key:(Backend_var.With_provenance.name var) ~data:value;
    codegen_expr ~ctx ~builder ~env ~this_module body
  | Cconst_symbol (name, _) -> lookup_global name this_module |> Option.value_exn
  | _ ->
    Printcmm.expression Format.std_formatter expr;
    assert false
;;

let funtype ctx (fundecl : Cmm.fundecl) =
  let return_type = exprtype ctx fundecl.fun_body in
  let args =
    List.map fundecl.fun_args ~f:(fun (_, machtype) -> type_of_machtype ctx machtype)
  in
  function_type return_type (List.to_array args)
;;

let type_of_data_item ctx (data_item : Cmm.data_item) =
  match data_item with
  | Cdefine_symbol _ | Cglobal_symbol _ -> None
  | Cint8 _ -> Some (i8_type ctx)
  | Cint16 _ -> Some (i16_type ctx)
  | Cint32 _ -> Some (i32_type ctx)
  | Cint _ -> Some (i64_type ctx)
  | Csingle _ -> Some (float_type ctx)
  | Cdouble _ -> Some (double_type ctx)
  | Cstring str -> Some (vector_type (i8_type ctx) (String.length str))
  | Csymbol_address _ -> (* TODO? *) None
  | Cskip _ | Calign _ -> None
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
  let ctx = global_context () in
  let this_module = create_module ctx "melse" in
  let builder = builder ctx in
  List.iter cmm ~f:(function
      | Cfunction cfundecl ->
        let funtype = funtype ctx cfundecl in
        let fundecl = declare_function cfundecl.fun_name funtype this_module in
        (* set argument names *)
        let args =
          List.map2_exn
            (params fundecl |> Array.to_list)
            cfundecl.fun_args
            ~f:(fun arg (name, _) ->
              let real_name = Backend_var.With_provenance.name name in
              set_value_name real_name arg;
              real_name, arg)
        in
        let env = String.Table.of_alist_exn args in
        let entry = append_block ctx "entry" fundecl in
        position_at_end entry builder;
        (match
           let ret_val = codegen_expr ~ctx ~builder ~env ~this_module cfundecl.fun_body in
           build_ret ret_val builder
         with
        | _ -> ()
        | exception exn ->
          delete_function fundecl;
          print_s
            [%message
              "exception raised while compiling function"
                ~name:(cfundecl.fun_name : string)
                (exn : Exn.t)])
      | Cdata items ->
        (* This is kind of weird, but the way to think about this is one big
        anonymous global, with a few labels inside it. *)
        let glob_value =
          List.filter_map items ~f:(value_of_data_item ctx)
          |> Array.of_list
          |> const_struct ctx
        in
        let anon_global = define_global "" glob_value this_module in
        (* Add labels to internal things *)
        let (_ : int) =
          List.fold items ~init:0 ~f:(fun acc next ->
              match next with
              | Cdefine_symbol name ->
                (* private *)
                let sym =
                  if acc = 0
                  then anon_global
                  else const_in_bounds_gep anon_global [| const_int (i64_type ctx) acc |]
                in
                let glob = define_global name sym this_module in
                set_linkage Private glob;
                acc
              | Cglobal_symbol name ->
                let sym =
                  if acc = 0
                  then anon_global
                  else const_in_bounds_gep anon_global [| const_int (i64_type ctx) acc |]
                in
                let (_ : llvalue) = define_global name sym this_module in
                acc
              | Cint8 _
              | Cint16 _
              | Cint32 _
              | Cint _
              | Csingle _
              | Cdouble _
              | Cstring _
              | Csymbol_address _
              | Cskip _
              | Calign _ -> acc + 1)
        in
        ());
  dump_module this_module
;;

let%expect_test "" =
  let source = {| let f x = 10 + x;; |} in
  let cmm = Trycmm.cmm_of_source source in
  [%expect
    {|
    (data)(data int 3063 "camlMelse__2": addr "camlMelse__f_80" int 3)(data
                                                                       int 1792
                                                                       global "camlMelse"
                                                                       "camlMelse":
                                                                       int 1)
    (data
     global "camlMelse__gc_roots"
     "camlMelse__gc_roots":
     addr "camlMelse"
     int 0)(function{:1,7-17} camlMelse__f_80 (x/82: val) (+ x/82 20))
    (function camlMelse__entry ()
     (let f/80 "camlMelse__2" (store val(root-init) "camlMelse" f/80)) 1a) |}];
  emit cmm;
  [%expect
    {|
    ; ModuleID = 'melse'
    source_filename = "melse"

    @0 = global {} zeroinitializer
    @1 = global { i64, i64 } { i64 3063, i64 3 }
    @camlMelse__2 = private global { i64, i64 }* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @1, i64 1)
    @2 = global { i64, i64 } { i64 1792, i64 1 }
    @camlMelse = global { i64, i64 }* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @2, i64 1)
    @camlMelse.1 = private global { i64, i64 }* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @2, i64 1)
    @3 = global { i64 } zeroinitializer
    @camlMelse__gc_roots = global { i64 }* @3
    @camlMelse__gc_roots.2 = private global { i64 }* @3

    define i64 @camlMelse__f_80(i64 %x) {
    entry:
      %0 = add i64 %x, 20
      ret i64 %0
    }

    define i64* @camlMelse__entry() {
    entry:
      store { i64, i64 }** @camlMelse, { i64, i64 }** @camlMelse__2
      ret i64* inttoptr (i64 1 to i64*)
    } |}]
;;
