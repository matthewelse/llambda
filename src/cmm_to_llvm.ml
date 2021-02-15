(** Convert OCaml's internal C-- representation to LLVM. *)

open! Core
open! Import
module Backend_var = Ocaml_optcomp.Backend_var
include Cmm_to_llvm_intf

module With_context (Context : Context) = struct
  open Context

  let int_type = i64_type ctx
  let val_type = pointer_type (i8_type ctx)
  let float_type = double_type ctx
  let void_type = void_type ctx

  module Intrinsics = struct
    let stacksave =
      Llvm.declare_function "llvm.stacksave" (function_type val_type [||]) this_module
    ;;

    let stackrestore =
      Llvm.declare_function
        "llvm.stackrestore"
        (function_type void_type [| val_type |])
        this_module
    ;;

    let gcroot =
      Llvm.declare_function
        "llvm.gcroot"
        (function_type void_type [| pointer_type val_type; val_type |])
        this_module
    ;;
  end

  let llambda_raise_exn =
    Llvm.declare_function
      "llambda_raise_exn"
      (function_type void_type [| val_type |])
      this_module
  ;;

  let type_of_kind kind = Var.Kind.lltype_of_t ~ctx kind

  let const_int value =
    { value = `Register (Llvm.const_int int_type value); kind = Machtype Int }
  ;;

  let const_int64 ?(signed = true) value =
    { value = `Register (Llvm.const_of_int64 int_type value signed); kind = Machtype Int }
  ;;

  let const_pointer value =
    { value = `Register (Llvm.const_inttoptr (Llvm.const_int int_type value) val_type)
    ; kind = Machtype Val
    }
  ;;

  let const_float value =
    { value = `Register (Llvm.const_float float_type value); kind = Machtype Float }
  ;;

  let with_var_in_env ~name ~value ~f =
    let previous = String.Table.find env name in
    String.Table.set env ~key:name ~data:value;
    let result = f () in
    String.Table.remove env name;
    Option.iter previous ~f:(fun old_value ->
        String.Table.set env ~key:name ~data:old_value);
    result
  ;;

  let with_catch_in ~index ~target ~f =
    let previous = Int.Table.find catches index in
    Int.Table.set catches ~key:index ~data:target;
    let result = f () in
    Int.Table.remove catches index;
    Option.iter previous ~f:(fun old_value ->
        Int.Table.set catches ~key:index ~data:old_value);
    result
  ;;

  let promote_value_if_necessary ~(new_machtype : Var.Kind.t) t : t option =
    match new_machtype, t.kind with
    | Void, Void -> Some t
    | Machtype Int, Machtype Int
    | Machtype Val, Machtype Val
    | Machtype Addr, Machtype Addr
    | Machtype Float, Machtype Float ->
      Some t
    | Machtype (Addr | Val), Machtype Int ->
      (* print_s
        [%message
          "promoting"
            ~from:(t.kind : Cmm.machtype_component option)
            ~from_irtype:(Llvm.type_of t.value : Ir_type.t)
            ~to_:(new_machtype : Cmm.machtype_component option)
            ~to_irtype:(val_type : Ir_type.t)]; *)
      let new_type = val_type in
      Some
        { kind = new_machtype
        ; value =
            (match t.value with
            | `Register value ->
              `Register (Llvm.build_inttoptr value new_type "promote" builder)
            | `Stack value ->
              `Stack
                (Llvm.build_pointercast value (pointer_type new_type) "promote" builder))
        }
    | Machtype Addr, Machtype Val -> Some { t with kind = new_machtype }
    | Void, _
    | _, Void
    | Machtype Int, Machtype (Addr | Val | Float)
    | Machtype Val, Machtype (Addr | Float)
    | Machtype Addr, Machtype Float
    | Machtype Float, Machtype (Addr | Val | Int) ->
      None
    | Never_returns, _ | _, Never_returns -> Some t
  ;;

  let promote_value_if_necessary_exn ~new_machtype t =
    match promote_value_if_necessary ~new_machtype t with
    | Some t -> t
    | None ->
      raise_s [%message "Unable to promote value." (new_machtype : Var.Kind.t) (t : t)]
  ;;

  let cast_to_int_if_necessary_exn t =
    match t.kind with
    | Void | Machtype Float | Machtype Addr ->
      raise_s [%message "Cannot demomote addresses or floats to integer."]
    | Machtype Int -> t
    | Machtype Val ->
      let new_type = int_type in
      { value =
          (match t.value with
          | `Register value -> `Register (build_ptrtoint value new_type "" builder)
          | `Stack value ->
            `Stack (build_pointercast value (pointer_type new_type) "" builder))
      ; kind = Machtype Int
      }
    | Never_returns -> t
  ;;

  let llvm_value t =
    match t.value with
    | `Register value -> value
    | `Stack value -> build_load value "" builder
  ;;

  let compile_int_binop ~(operation : Cmm.operation) left right =
    let build_llvm_op =
      match operation with
      | Caddi -> build_add
      | Csubi -> build_sub
      | Cmuli -> build_mul
      | Cmulhi -> build_mul
      | Cdivi -> build_sdiv
      | Cmodi -> build_srem
      | Cand -> build_and
      | Cor -> build_or
      | Cxor -> build_xor
      | Clsl -> build_shl
      | Clsr -> build_lshr
      | Casr -> build_ashr
      | _ -> assert false
    in
    let left = cast_to_int_if_necessary_exn left |> llvm_value in
    let right = cast_to_int_if_necessary_exn right |> llvm_value in
    let value = build_llvm_op left right "binop" builder in
    { value = `Register value; kind = Machtype Int }
  ;;

  let compile_float_binop ~(operation : Cmm.operation) left right =
    let build_llvm_op =
      match operation with
      | Caddf -> build_fadd
      | Csubf -> build_fsub
      | Cmulf -> build_fmul
      | Cdivf -> build_fdiv
      | _ -> assert false
    in
    let left =
      promote_value_if_necessary_exn ~new_machtype:(Machtype Float) left |> llvm_value
    in
    let right =
      promote_value_if_necessary_exn ~new_machtype:(Machtype Float) right |> llvm_value
    in
    let value = build_llvm_op left right "binop" builder in
    { value = `Register value; kind = Machtype Float }
  ;;

  let build_fabs value name builder =
    let fabs_intrinsic =
      Llvm.declare_function
        "llvm.fabs.double"
        (Llvm.function_type float_type [| float_type |])
        this_module
    in
    build_call fabs_intrinsic [| value |] name builder
  ;;

  let compile_float_unop ~(operation : Cmm.operation) value =
    let build_llvm_op =
      match operation with Cabsf -> build_fabs | Cnegf -> build_fneg | _ -> assert false
    in
    let value =
      promote_value_if_necessary_exn ~new_machtype:(Machtype Float) value |> llvm_value
    in
    let value = build_llvm_op value "unop" builder in
    { value = `Register value; kind = Machtype Float }
  ;;

  let rec compile_expression (expr : Cmm.expression) : t =
    (* Core.eprint_s [%message "compiling expression" (expr : Cmm.expression)]; *)
    match expr with
    | Cconst_int (value, _) -> const_int value
    | Cconst_natint (value, _) -> const_int64 (Nativeint.to_int64 value)
    | Cconst_float (value, _) -> const_float value
    | Cconst_pointer (value, _) -> const_pointer value
    | Cconst_natpointer (value, _) -> const_pointer (Nativeint.to_int_exn value)
    | Cconst_symbol (name, _) ->
      (match lookup_symbol name with
      | Some (`Indirect { value = `Register global_ptr; kind }) ->
        let new_ptr_kind = pointer_type (type_of_kind kind) in
        let ptr = build_pointercast global_ptr new_ptr_kind "" builder in
        let real_ptr = build_load ptr "real_ptr" builder in
        { value = `Register real_ptr; kind = Machtype Val }
      | Some (`Direct t) -> t
      | Some (`Indirect { value = `Stack _; kind = _ }) ->
        (* TODO: unify this with direct/indirect *)
        raise_s [%message "BUG: global symbol on the stack"]
      | None -> raise_s [%message "Unknown global" (name : string)])
    | Cblockheader (value, _) ->
      (* I think this is right - chances are we'll write it down somewhere. *)
      const_int64 ~signed:false (Nativeint.to_int64 value)
    | Cvar name ->
      let name = Backend_var.name name in
      (match String.Table.find env name with
      | Some t -> t
      | None -> raise_s [%message "Unknown variable" (name : string)])
    | Clet (var, value, body) ->
      let var_name = Backend_var.With_provenance.name var in
      let var_value = compile_expression value in
      with_var_in_env ~name:var_name ~value:var_value ~f:(fun () ->
          compile_expression body)
    | Clet_mut (var, machtype, value, body) ->
      (* Maybe we should convert these to ssa? *)
      let var_name = Backend_var.With_provenance.name var in
      let var_value =
        compile_expression value
        |> promote_value_if_necessary_exn ~new_machtype:(Var.Kind.of_machtype machtype)
      in
      let var_ptr = Llvm.build_alloca (raw_type var_value) var_name builder in
      let previous_stack = build_call Intrinsics.stacksave [||] "" builder in
      let (_ : llvalue) = build_store (raw_value var_value) var_ptr builder in
      let result =
        with_var_in_env
          ~name:var_name
          ~value:{ value = `Stack var_ptr; kind = var_value.kind }
          ~f:(fun () -> compile_expression body)
      in
      let (_ : llvalue) =
        build_call Intrinsics.stackrestore [| previous_stack |] "" builder
      in
      result
    | Cphantom_let _ ->
      raise_s
        [%message
          "TODO: I don't know how to handle phantom lets yet." (expr : Cmm.expression)]
    | Cassign (var, new_value_expr) ->
      let var_name = Backend_var.name var in
      (match String.Table.find env var_name with
      | None ->
        raise_s
          [%message
            "Tried to assign to an unknown variable."
              (var_name : string)
              (env : t String.Table.t)]
      | Some { value = `Register _; kind = _ } ->
        raise_s
          [%message
            "Tried to assign to an immutable variable."
              (var_name : string)
              (env : t String.Table.t)]
      | Some { value = `Stack ptr; kind } ->
        let new_value =
          compile_expression new_value_expr
          |> promote_value_if_necessary_exn ~new_machtype:kind
          |> llvm_value
        in
        let (_ : llvalue) = build_store new_value ptr builder in
        const_int 1)
    | Ctuple [] -> const_int 1
    | Ctuple _ -> raise_s [%message "TODO: Handle larger tuples?"]
    | Cop (operation, args, debug_info) -> compile_operation operation args debug_info
    | Csequence (before, after) ->
      let (_ : t) = compile_expression before in
      compile_expression after
    | Cifthenelse (cond, _, then_, _, else_, _) ->
      let start_bb = insertion_block builder in
      let cond = compile_expression cond |> cast_to_int_if_necessary_exn
    |> llvm_value in
      let cond = build_trunc cond (i1_type ctx) "" builder in
      let then_bb = append_block ctx "then" this_function in
      position_at_end then_bb builder;
      let then_value = compile_expression then_ in
      (* in case the then block adds basic blocks *)
      let real_then_bb = insertion_block builder in
      let else_bb = append_block ctx "else" this_function in
      position_at_end else_bb builder;
      let else_value = compile_expression else_ in
      let target_kind =
        match then_value.kind, else_value.kind with
        | Machtype l, Machtype r -> Var.Kind.Machtype (Cmm.lub_component l r)
        | Void, Void -> Void
        | Never_returns, other | other, Never_returns -> other
        | Void, _ | _, Void ->
          raise_s
            [%message
              "If expression with mis-matching branches."
                (then_ : Cmm.expression)
                (else_ : Cmm.expression)]
      in
      let then_value =
        promote_value_if_necessary_exn ~new_machtype:target_kind then_value
      in
      let else_value =
        promote_value_if_necessary_exn ~new_machtype:target_kind else_value
      in
      (* in case the else block adds basic blocks *)
      let real_else_bb = insertion_block builder in
      let merge_bb = append_block ctx "merge" this_function in
      position_at_end merge_bb builder;
      (* add a cond branch to the original bb *)
      position_at_end start_bb builder;
      let (_ : llvalue) = build_cond_br cond then_bb else_bb builder in
      (* branch from the ends of the conditional branches to the merge branch *)
      position_at_end real_then_bb builder;
      let incoming =
        match block_terminator real_then_bb with
        | None ->
          let (_ : llvalue) = build_br merge_bb builder in
          [ then_value, real_then_bb ]
        | Some _ -> []
      in
      position_at_end real_else_bb builder;
      let incoming =
        match block_terminator real_else_bb with
        | None ->
          let (_ : llvalue) = build_br merge_bb builder in
          (else_value, real_else_bb) :: incoming
        | Some _ -> incoming
      in
      (match incoming with
      | [] ->
        remove_block merge_bb;
        const_int 1
      | _ ->
        let incoming = List.map incoming ~f:(fun (t, bb) -> llvm_value t, bb) in
        position_at_end merge_bb builder;
        let value = build_phi incoming "iftmp" builder in
        { value = `Register value; kind = target_kind })
    | Ccatch (_, [ (index, [], handler, _) ], body) ->
      let body_bb = insertion_block builder in
      let handler_bb = append_block ctx [%string "handler.%{index#Int}"] this_function in
      let exit_bb = append_block ctx [%string "exit.%{index#Int}"] this_function in
      let handler_value =
        with_catch_in ~index ~target:handler_bb ~f:(fun () ->
            position_at_end handler_bb builder;
            compile_expression handler)
      in
      let real_handler_bb = insertion_block builder in
      let incoming =
        match block_terminator real_handler_bb with
        | None ->
          let (_ : llvalue) = build_br exit_bb builder in
          [ handler_value, real_handler_bb ]
        | Some _ -> []
      in
      let body_value =
        with_catch_in ~index ~target:handler_bb ~f:(fun () ->
            position_at_end body_bb builder;
            compile_expression body)
      in
      let real_body_bb = insertion_block builder in
      let incoming =
        match block_terminator real_body_bb with
        | None ->
          let (_ : llvalue) = build_br exit_bb builder in
          (body_value, real_body_bb) :: incoming
        | Some _ -> incoming
      in
      (match incoming with
      | [] ->
        remove_block exit_bb;
        const_int 1
      | _ ->
        let incoming = List.map incoming ~f:(fun (t, bb) -> llvm_value t, bb) in
        position_at_end exit_bb builder;
        { value = `Register (build_phi incoming [%string "phi.%{index#Int}"] builder)
        ; kind = Machtype Val
        })
    | Ccatch _ ->
      raise_s [%message "TODO: complex catch statements" (expr : Cmm.expression)]
    | Cexit (index, []) ->
      let (_ : llvalue) = build_br (Int.Table.find_exn catches index) builder in
      const_int 1
    | Cexit _ -> raise_s [%message "TODO: complex exits" (expr : Cmm.expression)]
    | Cswitch (value, ints, expressions, _) ->
      let match_value =
        compile_expression value |> cast_to_int_if_necessary_exn |> llvm_value
      in
      let default =
        let bb = Llvm.append_block ctx "default" this_function in
        position_at_end bb builder;
        let (_ : llvalue) = build_unreachable builder in
        bb
      in
      let switch = build_switch match_value default (Array.length expressions) builder in
      let exit_bb = append_block ctx "exit" this_function in
      let results =
        Array.map2_exn ints expressions ~f:(fun case (then_, _) ->
            let bb = Llvm.append_block ctx [%string "case%{case#Int}"] this_function in
            position_at_end bb builder;
            let result = compile_expression then_ in
            add_case switch (const_int case |> llvm_value) bb;
            let real_bb = insertion_block builder in
            match block_terminator real_bb with
            | None ->
              let (_ : llvalue) = build_br exit_bb builder in
              Some (result, real_bb)
            | Some _ -> None)
        |> Array.to_list
        |> List.filter_opt
      in
      (match results with
      | [] ->
        remove_block exit_bb;
        const_int 1
      | results ->
        let kind =
          List.map results ~f:(fun (t, _) -> t.kind)
          |> List.reduce_exn ~f:(fun l r ->
                 match l, r with
                 | Machtype l, Machtype r -> Machtype (Cmm.lub_component l r)
                 | Void, Void -> Void
                 | Never_returns, other | other, Never_returns -> other
                 | Void, _ | _, Void ->
                   raise_s
                     [%message
                       "If expression with mis-matching branches."
                         (l : Var.Kind.t)
                         (r : Var.Kind.t)])
        in
        let incoming = List.map results ~f:(fun (t, builder) -> llvm_value t, builder) in
        { value = `Register (build_phi incoming "phi" builder); kind })
    | Ctrywith _ -> raise_s [%message "TODO: try/with" (expr : Cmm.expression)]

  and compile_operation operation args (_ : Ocaml_common.Debuginfo.t) =
    match operation, args with
    | Capply return_type, func :: args ->
      let func = compile_expression func in
      let args = List.map args ~f:compile_expression |> List.map ~f:llvm_value in
      let new_func_type =
        function_type
          (type_of_kind (Var.Kind.of_machtype return_type))
          (List.map args ~f:Llvm.type_of |> List.to_array)
        |> pointer_type
      in
      let func = build_pointercast (llvm_value func) new_func_type "func_cast" builder in
      let call = build_call func (List.to_array args) "" builder in
      set_instruction_call_conv Declarations.ocaml_calling_convention call;
      { value = `Register call; kind = Var.Kind.of_machtype return_type }
    | Capply _, _ -> assert false
    | Cextcall (function_name, return_type, does_alloc, _label), args ->
      let args = List.map args ~f:compile_expression |> List.map ~f:llvm_value in
      let return_kind = Var.Kind.of_machtype return_type in
      let return_type = type_of_kind (Var.Kind.of_machtype return_type) in
      let func =
        Llvm.declare_function
          function_name
          (function_type return_type (List.map args ~f:Llvm.type_of |> Array.of_list))
          this_module
      in
      if does_alloc
      then (
        let args = [ func ] @ args in
        let function_type =
          function_type return_type (List.map args ~f:Llvm.type_of |> Array.of_list)
        in
        let caml_c_call = Llvm.declare_function "caml_c_call" function_type this_module in
        let call = build_call caml_c_call (Array.of_list ([ func ] @ args)) "" builder in
        set_instruction_call_conv Declarations.ocaml_ext_calling_convention call;
        { value = `Register call; kind = return_kind })
      else (
        let call = build_call func (Array.of_list args) "" builder in
        set_instruction_call_conv Declarations.ocaml_calling_convention call;
        { value = `Register call; kind = return_kind })
    | Caddv, [ left; right ] ->
      let left =
        compile_expression left
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Val)
        |> llvm_value
      in
      let right =
        compile_expression right
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Val)
        |> llvm_value
      in
      let right = build_ptrtoint right int_type "" builder in
      { kind = Machtype Val; value = `Register (build_gep left [| right |] "" builder) }
    | Caddv, _ -> assert false
    | Cadda, [ left; right ] ->
      let left =
        compile_expression left
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Addr)
        |> llvm_value
      in
      let right =
        compile_expression right
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Addr)
        |> llvm_value
      in
      let right = build_ptrtoint right int_type "" builder in
      { kind = Machtype Addr; value = `Register (build_gep left [| right |] "" builder) }
    | Cadda, _ -> assert false
    | ( ( Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor | Cxor | Clsl | Clsr
        | Casr )
      , [ left; right ] ) ->
      (* We should be able to do these operations on Val too *)
      let left = compile_expression left in
      let right = compile_expression right in
      compile_int_binop ~operation left right
    | ( ( Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor | Cxor | Clsl | Clsr
        | Casr )
      , _ ) ->
      assert false
    | Ccmpi cmp, [ left; right ] ->
      let cmp : Llvm.Icmp.t =
        match cmp with
        | Ceq -> Eq
        | Cne -> Ne
        | Clt -> Slt
        | Cgt -> Sgt
        | Cle -> Sle
        | Cge -> Sge
      in
      let left = cast_to_int_if_necessary_exn (compile_expression left) |> llvm_value in
      let right = cast_to_int_if_necessary_exn (compile_expression right) |> llvm_value in
      { value =
          `Register
            (build_zext
               (build_icmp cmp left right "icmp" builder)
               int_type
               "zext"
               builder)
      ; kind = Machtype Int
      }
    | Ccmpi _, _ -> assert false
    | Ccmpf cmp, [ left; right ] ->
      let cmp : Llvm.Fcmp.t =
        match cmp with
        | CFeq -> Oeq
        | CFneq -> One
        | CFlt -> Olt
        | CFnlt -> Oge
        | CFgt -> Ogt
        | CFngt -> Ole
        | CFle -> Ole
        | CFnle -> Ogt
        | CFge -> Oge
        | CFnge -> Olt
      in
      let left =
        compile_expression left
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Float)
        |> llvm_value
      in
      let right =
        compile_expression right
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Float)
        |> llvm_value
      in
      { value =
          `Register
            (build_zext
               (build_fcmp cmp left right "fcmp" builder)
               int_type
               "zext"
               builder)
      ; kind = Machtype Int
      }
    | Ccmpf _, _ -> assert false
    | Ccmpa cmp, [ left; right ] ->
      let cmp : Llvm.Icmp.t =
        match cmp with
        | Ceq -> Eq
        | Cne -> Ne
        | Clt -> Slt
        | Cgt -> Sgt
        | Cle -> Sle
        | Cge -> Sge
      in
      let left =
        compile_expression left
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Addr)
        |> llvm_value
      in
      let right =
        compile_expression right
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Addr)
        |> llvm_value
      in
      let diff = build_ptrdiff left right "diff" builder in
      { value =
          `Register
            (build_zext
               (build_icmp cmp diff (const_int 0 |> llvm_value) "icmp" builder)
               int_type
               "zext"
               builder)
      ; kind = Machtype Int
      }
    | Ccmpa _, _ -> assert false
    | (Caddf | Csubf | Cmulf | Cdivf), [ left; right ] ->
      let left = compile_expression left in
      let right = compile_expression right in
      compile_float_binop ~operation left right
    | (Caddf | Csubf | Cmulf | Cdivf), _ -> assert false
    | (Cnegf | Cabsf), [ value ] ->
      let value = compile_expression value in
      compile_float_unop ~operation value
    | (Cnegf | Cabsf), _ -> assert false
    | Cintoffloat, [ value ] ->
      let value =
        compile_expression value
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Float)
        |> llvm_value
      in
      { value = `Register (build_bitcast value int_type "intoffloat" builder)
      ; kind = Machtype Int
      }
    | Cintoffloat, _ -> assert false
    | Cfloatofint, [ value ] ->
      let value =
        compile_expression value
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Int)
        |> llvm_value
      in
      { value = `Register (build_bitcast value float_type "floatofint" builder)
      ; kind = Machtype Float
      }
    | Cfloatofint, _ -> assert false
    | Cload (memory_chunk, _mutability), [ src ] ->
      let ptr =
        promote_value_if_necessary_exn
          ~new_machtype:(Machtype Addr)
          (compile_expression src)
        |> llvm_value
      in
      let (kind : Cmm.machtype_component), mem_lltype, should_extend =
        match memory_chunk with
        | Byte_unsigned -> Int, i8_type ctx, `Zero
        | Byte_signed -> Int, i8_type ctx, `Signed
        | Sixteen_unsigned -> Int, i16_type ctx, `Zero
        | Sixteen_signed -> Int, i16_type ctx, `Signed
        | Thirtytwo_unsigned -> Int, i32_type ctx, `Zero
        | Thirtytwo_signed -> Int, i32_type ctx, `Signed
        | Word_int -> Int, int_type, `Don't
        | Word_val -> Val, val_type, `Don't
        | Single -> Float, Llvm.float_type ctx, `Float
        | Double | Double_u -> Float, float_type, `Don't
      in
      let ptr = build_pointercast ptr (pointer_type mem_lltype) "load" builder in
      let value = build_load ptr "" builder in
      let value =
        match should_extend with
        | `Don't -> value
        | `Zero -> build_zext value mem_lltype "zext" builder
        | `Signed -> build_sext value mem_lltype "sext" builder
        | `Float -> build_fpext value mem_lltype "fpext" builder
      in
      { value = `Register value; kind = Machtype kind }
    | Cload _, _ -> assert false
    | Cstore (memory_chunk, _), [ dst; value ] ->
      let (kind : Cmm.machtype_component), mem_lltype, should_truncate =
        match memory_chunk with
        | Byte_unsigned -> Int, i8_type ctx, `Trunc
        | Byte_signed -> Int, i8_type ctx, `Trunc
        | Sixteen_unsigned -> Int, i16_type ctx, `Trunc
        | Sixteen_signed -> Int, i16_type ctx, `Trunc
        | Thirtytwo_unsigned -> Int, i32_type ctx, `Trunc
        | Thirtytwo_signed -> Int, i32_type ctx, `Trunc
        | Word_int -> Int, int_type, `Don't
        | Word_val -> Val, val_type, `Don't
        | Single -> Float, Llvm.float_type ctx, `Float
        | Double | Double_u -> Float, float_type, `Don't
      in
      let value =
        compile_expression value
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype kind)
        |> llvm_value
      in
      let write_value =
        match should_truncate with
        | `Don't -> value
        | `Trunc -> build_trunc value mem_lltype "trunc" builder
        | `Float -> build_fptrunc value mem_lltype "fptrunc" builder
      in
      let ptr =
        compile_expression dst
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Addr)
        |> llvm_value
      in
      let ptr = build_pointercast ptr (pointer_type mem_lltype) "" builder in
      (* eprint_s [%message "building store" (ptr : llvalue) (write_value : llvalue)]; *)
      let (_ : llvalue) = build_store write_value ptr builder in
      const_int 1
    | Cstore _, _ -> assert false
    | Calloc, tag :: data ->
      let caml_alloc =
        Llvm.declare_function
          "caml_alloc"
          (function_type
             val_type
             [| (* block size words *) int_type
              ; (*tag *) int_type (* actually a char *)
             |])
          this_module
      in
      let tag_value =
        compile_expression tag
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Int)
        |> llvm_value
      in
      let length = const_int (List.length data) |> llvm_value in
      (* eprint_s
        [%message "building call" (caml_alloc : llvalue) (length : t) (tag_value : t)]; *)
      (* LLVM is sad if we try to put the allocated pointer on the stack

        TODO melse: we probably need to introduce a new kind, which is a pointer
        to an ocaml block, which we de-reference when we need it. We
        additionally need to consider that pointer on the stack volatile (across
        calls into the OCaml runtime), since the garbage collector could change
        it to point somewhere else during a minor GC. 

        There are some more challenges with using LLVM's gcroot intrinsic - all
        of the calls to @llvm.gcroot have to be in the entry basic block,
        otherwise we break things. *)
      (* Build an alloca in the entry bb for this function *)
      let insertion_block = insertion_block builder in
      let entry_bb = entry_block this_function in
      (match block_terminator entry_bb with
      | None -> position_at_end entry_bb builder
      | Some terminator -> position_before terminator builder);
      let ptr_ptr = build_alloca (pointer_type (i8_type ctx)) "alloc_ptr" builder in
      let ptr = build_call caml_alloc [| length; tag_value |] "alloc" builder in
      let (_ : llvalue) = build_store ptr ptr_ptr builder in
      let (_ : llvalue) =
        build_call Intrinsics.gcroot [| ptr_ptr; const_pointer_null val_type |] "" builder
      in
      position_at_end insertion_block builder;
      let ptr = build_call caml_alloc [| length; tag_value |] "alloc" builder in
      let (_ : llvalue) = build_store ptr ptr_ptr builder in
      List.iteri data ~f:(fun i elem ->
          let elem_ptr =
            build_in_bounds_gep ptr [| const_int (i * 8) |> llvm_value |] "gep" builder
          in
          let value = compile_expression elem |> llvm_value in
          let elem_ptr =
            build_pointercast elem_ptr (pointer_type (type_of value)) "" builder
          in
          let (_ : llvalue) = build_store value elem_ptr builder in
          ());
      { kind = Machtype Val; value = `Stack ptr_ptr }
    | Calloc, _ -> assert false
    | Ccheckbound, _ -> raise_s [%message "TODO check bounds"]
    | Craise _, [ exn ] ->
      let exn_val =
        compile_expression exn
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Val)
        |> llvm_value
      in
      let call = build_call llambda_raise_exn [| exn_val |] "" builder in
      set_instruction_call_conv Declarations.ocaml_calling_convention call;
      { kind = Never_returns; value = `Register (build_unreachable builder) }
      (* assert false *)
    | Craise _, _ -> assert false
  ;;
end
