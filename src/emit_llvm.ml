open Core
open Llvm

let type_of_machtype ctx (machtype : Cmm.machtype) =
  match machtype with
  | [||] -> void_type ctx
  | [| component |] ->
    (match component with
    | Val | Addr -> pointer_type (i64_type ctx)
    | Int -> i64_type ctx
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
  | Ccmpi _ -> i1_type ctx
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
  | Csequence (_, body)
  | Cifthenelse (_, _, body, _, _, _) -> exprtype ctx body
  | Cop (op, _, _) -> optype ctx op
  | Cconst_pointer _ ->
    (* TODO melse: this is removed in 4.12 anyway *)
    pointer_type (i64_type ctx)
  | _ ->
    print_s [%message "error while trying to type expression"];
    Printcmm.expression Format.std_formatter expr;
    print_endline "";
    assert false
;;

let rec codegen_expr ~ctx ~builder ~env ~this_module ~fundecl (expr : Cmm.expression) =
  match expr with
  | Cop (Capply _, func :: args, _) ->
    let func = codegen_expr ~ctx ~builder ~env ~this_module ~fundecl func in
    let args = List.map args ~f:(codegen_expr ~ctx ~builder ~env ~this_module ~fundecl) in
    build_call func (List.to_array args) "" builder
  | Cconst_int (value, _) -> const_int (i64_type ctx) value
  | Cconst_natint (value, _) -> const_int (i64_type ctx) (Nativeint.to_int_exn value)
  | Cconst_float (value, _) -> const_float (double_type ctx) value
  | Cop (Caddv, values, _) ->
    (* 2n + 1 + 2m + 1 = 2(n + m) + 2, so subtract one at the end... this is
    probably wrong for pointers though *)
    let exprs =
      List.map values ~f:(codegen_expr ~ctx ~builder ~env ~this_module ~fundecl)
    in
    let exprs = const_int (i64_type ctx) (-1) :: exprs in
    List.reduce_exn exprs ~f:(fun l r -> build_add l r "" builder)
  | Cop (Caddi, values, _) ->
    List.map values ~f:(codegen_expr ~ctx ~builder ~env ~this_module ~fundecl)
    |> List.reduce_exn ~f:(fun l r -> build_add l r "" builder)
  | Cop (Cadda, values, _) ->
    let sum =
      List.map values ~f:(codegen_expr ~ctx ~builder ~env ~this_module ~fundecl)
      |> List.reduce_exn ~f:(fun l r -> build_add l r "" builder)
    in
    build_inttoptr sum (pointer_type (i64_type ctx)) "" builder
  | Cop (Cstore _, [ value; dst ], _) ->
    let dst = codegen_expr ~ctx ~builder ~env ~this_module ~fundecl dst in
    let value = codegen_expr ~ctx ~builder ~env ~this_module ~fundecl value in
    build_store value dst builder
  | Cop (Cload _, [ src ], _) ->
    let src = codegen_expr ~ctx ~builder ~env ~this_module ~fundecl src in
    build_load src "" builder
  | Cop (Ccmpi Cne, [ left; right ], _) ->
    let left = codegen_expr ~ctx ~builder ~env ~this_module ~fundecl left in
    let right = codegen_expr ~ctx ~builder ~env ~this_module ~fundecl right in
    build_icmp Ne left right "" builder
  | Cconst_pointer (value, _) ->
    let intval = const_int (i64_type ctx) value in
    const_inttoptr intval (pointer_type (i64_type ctx))
  | Cvar var ->
    let real_name = Backend_var.name var in
    String.Table.find_exn env real_name
  | Csequence (before, after) ->
    let (_ : llvalue) = codegen_expr ~ctx ~builder ~env ~this_module ~fundecl before in
    codegen_expr ~ctx ~builder ~env ~this_module ~fundecl after
  | Clet (var, value, body) ->
    let value = codegen_expr ~ctx ~builder ~env ~this_module ~fundecl value in
    String.Table.add_exn env ~key:(Backend_var.With_provenance.name var) ~data:value;
    codegen_expr ~ctx ~builder ~env ~this_module ~fundecl body
  | Cconst_symbol (name, _) ->
    (match lookup_global name this_module with
    | Some global -> global
    | None ->
      (match String.Table.find env name with
      | Some value -> value
      | None ->
        print_s
          [%message "unable to find symbol" (name : string) (env : _ String.Table.t)];
        Printcmm.expression Format.std_formatter expr;
        assert false))
  | Cifthenelse (cond, _, then_, _, else_, _) ->
    let start_bb = insertion_block builder in
    let cond = codegen_expr ~ctx ~builder ~env ~this_module ~fundecl cond in
    let then_bb = append_block ctx "then" fundecl in
    position_at_end then_bb builder;
    let then_value = codegen_expr ~ctx ~builder ~env ~this_module ~fundecl then_ in
    (* in case the then block adds basic blocks *)
    let then_bb = insertion_block builder in
    let else_bb = append_block ctx "else" fundecl in
    position_at_end else_bb builder;
    let else_value = codegen_expr ~ctx ~builder ~env ~this_module ~fundecl else_ in
    (* in case the else block adds basic blocks *)
    let else_bb = insertion_block builder in
    let merge_bb = append_block ctx "merge" fundecl in
    position_at_end merge_bb builder;
    let incoming = [ then_value, then_bb; else_value, else_bb ] in
    let phi = build_phi incoming "iftmp" builder in
    (* add a cond branch to the original bb *)
    position_at_end start_bb builder;
    let (_ : llvalue) = build_cond_br cond then_bb else_bb builder in
    (* branch from the ends of the conditional branches to the merge branch *)
    position_at_end then_bb builder;
    let (_ : llvalue) = build_br merge_bb builder in
    position_at_end else_bb builder;
    let (_ : llvalue) = build_br merge_bb builder in
    position_at_end merge_bb builder;
    phi
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
        String.Table.add_exn env ~key:cfundecl.fun_name ~data:fundecl;
        let entry = append_block ctx "entry" fundecl in
        position_at_end entry builder;
        (match
           let ret_val =
             codegen_expr ~ctx ~builder ~env ~this_module ~fundecl cfundecl.fun_body
           in
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
