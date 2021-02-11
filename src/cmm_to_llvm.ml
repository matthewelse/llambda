(** Convert OCaml's internal C-- representation to LLVM. *)

open! Core
open! Import
module Backend_var = Ocaml_optcomp.Backend_var

type t =
  { value : Ir_value.t
  ; kind : Cmm.machtype_component option
  }
[@@deriving sexp_of]

module type Context = sig
  val ctx : Ir_context.t
  val builder : Ir_builder.t
  val this_module : Ir_module.t
  val this_function : Ir_value.t
  val env : Var.t String.Table.t
  val catches : Ir_basic_block.t Int.Table.t

  (** Lookup a global symbol. *)
  val lookup_symbol : string -> [ `Direct of t | `Indirect of t ] option
end

let machtype_option_of_array = function
  | [||] -> None
  | [| x |] -> Some x
  | machtype ->
    raise_s
      [%message
        "Found a machtype with more than one element."
          (machtype : Cmm.machtype_component array)]
;;

module With_context (Context : Context) = struct
  open Context

  let int_type = i64_type ctx
  let val_type = pointer_type (i8_type ctx)
  let float_type = double_type ctx

  let llvm_gcroot =
    Llvm.declare_function
      "llvm.gcroot"
      (function_type (void_type ctx) [| pointer_type val_type; val_type |])
      this_module
  ;;

  let type_of (kind : Cmm.machtype_component option) =
    match kind with
    | None -> void_type ctx
    | Some Int -> int_type
    | Some Val | Some Addr -> val_type
    | Some Float -> float_type
  ;;

  let const_int value = { value = Llvm.const_int int_type value; kind = Some Int }

  let const_pointer value =
    { value = Llvm.const_inttoptr (Llvm.const_int int_type value) val_type
    ; kind = Some Val
    }
  ;;

  let const_float value = { value = Llvm.const_float float_type value; kind = Some Int }

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

  let promote_value_if_necessary ~(new_machtype : Cmm.machtype_component option) t =
    match new_machtype, t.kind with
    | None, None -> Some t
    | Some Int, Some Int
    | Some Val, Some Val
    | Some Addr, Some Addr
    | Some Float, Some Float ->
      Some t
    | Some (Addr | Val), Some Int ->
      (* print_s
        [%message
          "promoting"
            ~from:(t.kind : Cmm.machtype_component option)
            ~from_irtype:(Llvm.type_of t.value : Ir_type.t)
            ~to_:(new_machtype : Cmm.machtype_component option)
            ~to_irtype:(val_type : Ir_type.t)]; *)
      Some
        { kind = new_machtype
        ; value = Llvm.build_inttoptr t.value val_type "promote" builder
        }
    | Some Addr, Some Val -> Some { t with kind = new_machtype }
    | None, _
    | _, None
    | Some Int, Some (Addr | Val | Float)
    | Some Val, Some (Addr | Float)
    | Some Addr, Some Float
    | Some Float, Some (Addr | Val | Int) ->
      None
  ;;

  let promote_value_if_necessary_exn ~new_machtype t =
    match promote_value_if_necessary ~new_machtype t with
    | Some t -> t
    | None ->
      raise_s
        [%message
          "Unable to promote value."
            (new_machtype : Cmm.machtype_component option)
            (t : t)]
  ;;

  let cast_to_int_if_necessary_exn t =
    match t.kind with
    | None | Some Float | Some Addr ->
      raise_s [%message "Cannot demomote addresses or floats to integer."]
    | Some Int -> t
    | Some Val -> { value = build_ptrtoint t.value int_type "" builder; kind = Some Int }
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
    let left = cast_to_int_if_necessary_exn left in
    let right = cast_to_int_if_necessary_exn right in
    let value = build_llvm_op left.value right.value "binop" builder in
    { value; kind = Some Int }
  ;;

  let require_float (t : t) =
    match t.kind with
    | Some Float -> ()
    | Some (Val | Addr | Int) | None ->
      raise_s [%message "Received an float operation argument that isn't an float."]
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
    let left = promote_value_if_necessary_exn ~new_machtype:(Some Float) left in
    let right = promote_value_if_necessary_exn ~new_machtype:(Some Float) right in
    let value = build_llvm_op left.value right.value "binop" builder in
    { value; kind = Some Float }
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
    let value = promote_value_if_necessary_exn ~new_machtype:(Some Float) value in
    let value = build_llvm_op value.value "unop" builder in
    { value; kind = Some Float }
  ;;

  let rec compile_expression (expr : Cmm.expression) : t =
    (* Core.eprint_s [%message "compiling expression" (expr : Cmm.expression)]; *)
    match expr with
    | Cconst_int (value, _) -> const_int value
    | Cconst_natint (value, _) -> const_int (Nativeint.to_int_exn value)
    | Cconst_float (value, _) -> const_float value
    | Cconst_pointer (value, _) -> const_pointer value
    | Cconst_natpointer (value, _) -> const_pointer (Nativeint.to_int_exn value)
    | Cconst_symbol (name, _) ->
      (match lookup_symbol name with
      | Some (`Indirect { value = global_ptr; kind }) ->
        let new_ptr_kind = pointer_type (type_of kind) in
        let ptr = build_pointercast global_ptr new_ptr_kind "" builder in
        let real_ptr = build_load ptr "real_ptr" builder in
        { value = real_ptr; kind = Some Val }
      | Some (`Direct t) -> t
      | None -> raise_s [%message "Unknown global" (name : string)])
    | Cblockheader (value, _) ->
      (* I think this is right - chances are we'll write it down somewhere. *)
      const_int (Nativeint.to_int_exn value)
    | Cvar name ->
      let name = Backend_var.name name in
      (match String.Table.find env name with
      | Some (Pointer { ptr; underlying_kind; mutability = _ }) ->
        { kind = underlying_kind; value = build_load ptr "" builder }
      | Some (Value { kind; value }) -> { kind = Some kind; value }
      | None -> raise_s [%message "Unknown variable" (name : string)])
    | Clet (var, value, body) ->
      let var_name = Backend_var.With_provenance.name var in
      let var_value = compile_expression value in
      with_var_in_env
        ~name:var_name
        ~value:
          (Value { value = var_value.value; kind = var_value.kind |> Option.value_exn })
        ~f:(fun () -> compile_expression body)
    | Clet_mut (var, machtype, value, body) ->
      (* Maybe we should convert these to ssa? *)
      let var_name = Backend_var.With_provenance.name var in
      let var_value =
        compile_expression value
        |> promote_value_if_necessary_exn
             ~new_machtype:(machtype_option_of_array machtype)
      in
      let var_ptr = Llvm.build_alloca (Llvm.type_of var_value.value) var_name builder in
      let (_ : Ir_value.t) = build_store var_value.value var_ptr builder in
      with_var_in_env
        ~name:var_name
        ~value:
          (Pointer
             { ptr = var_ptr; underlying_kind = var_value.kind; mutability = `Mutable })
        ~f:(fun () -> compile_expression body)
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
              (env : Var.t String.Table.t)]
      | Some (Value _ | Pointer { mutability = `Immutable; _ }) ->
        raise_s
          [%message
            "Tried to assign to an immutable variable."
              (var_name : string)
              (env : Var.t String.Table.t)]
      | Some (Pointer { mutability = `Mutable; underlying_kind; ptr }) ->
        let new_value =
          compile_expression new_value_expr
          |> promote_value_if_necessary_exn ~new_machtype:underlying_kind
        in
        let (_ : llvalue) = build_store new_value.value ptr builder in
        const_int 1)
    | Ctuple [] -> const_int 1
    | Ctuple _ -> raise_s [%message "TODO: Handle larger tuples?"]
    | Cop (operation, args, debug_info) -> compile_operation operation args debug_info
    | Csequence (before, after) ->
      let (_ : t) = compile_expression before in
      compile_expression after
    | Cifthenelse (cond, _, then_, _, else_, _) ->
      let start_bb = insertion_block builder in
      let cond = compile_expression cond |> cast_to_int_if_necessary_exn in
      let cond = { cond with value = build_trunc cond.value (i1_type ctx) "" builder } in
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
        | Some l, Some r -> Some (Cmm.lub_component l r)
        | None, None -> None
        | None, _ | _, None ->
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
      let (_ : llvalue) = build_cond_br cond.value then_bb else_bb builder in
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
        let incoming = List.map incoming ~f:(fun (t, bb) -> t.value, bb) in
        position_at_end merge_bb builder;
        let value = build_phi incoming "iftmp" builder in
        { value; kind = target_kind })
    | Ccatch (_, [ (index, [], handler, _) ], body) ->
      let body_bb = insertion_block builder in
      let handler_bb = append_block ctx [%string "handler.%{index#Int}"] this_function in
      let exit_bb = append_block ctx [%string "exit.%{index#Int}"] this_function in
      Int.Table.add_exn catches ~key:index ~data:handler_bb;
      (* handler *)
      position_at_end handler_bb builder;
      let handler_value = compile_expression handler in
      let real_handler_bb = insertion_block builder in
      let incoming =
        match block_terminator real_handler_bb with
        | None ->
          let (_ : llvalue) = build_br exit_bb builder in
          [ handler_value, real_handler_bb ]
        | Some _ -> []
      in
      (* body *)
      position_at_end body_bb builder;
      let body_value = compile_expression body in
      let real_body_bb = insertion_block builder in
      Int.Table.remove catches index;
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
        let incoming = List.map incoming ~f:(fun (t, bb) -> t.value, bb) in
        position_at_end exit_bb builder;
        { value = build_phi incoming [%string "phi.%{index#Int}"] builder
        ; kind = Some Val
        })
    | Ccatch _ ->
      raise_s [%message "TODO: complex catch statements" (expr : Cmm.expression)]
    | Cexit (index, []) ->
      let (_ : llvalue) = build_br (Int.Table.find_exn catches index) builder in
      const_int 1
    | Cexit _ -> raise_s [%message "TODO: complex exits" (expr : Cmm.expression)]
    | Cswitch _ -> raise_s [%message "TODO: switch statements" (expr : Cmm.expression)]
    | Ctrywith _ -> raise_s [%message "TODO: try/with" (expr : Cmm.expression)]

  and compile_operation operation args (_ : Ocaml_common.Debuginfo.t) =
    match operation, args with
    | Capply return_type, func :: args ->
      let func = compile_expression func in
      let args = List.map args ~f:compile_expression in
      let new_func_type =
        function_type
          (type_of (machtype_option_of_array return_type))
          (List.map args ~f:(fun arg -> Llvm.type_of arg.value) |> List.to_array)
        |> pointer_type
      in
      let func = build_pointercast func.value new_func_type "func_cast" builder in
      let call =
        build_call
          func
          (List.to_array (List.map args ~f:(fun arg -> arg.value)))
          ""
          builder
      in
      set_instruction_call_conv Declarations.ocaml_calling_convention call;
      { value = call; kind = machtype_option_of_array return_type }
    | Capply _, _ -> assert false
    | Cextcall (function_name, return_type, does_alloc, _label), args ->
      let args = List.map args ~f:compile_expression in
      let return_kind = machtype_option_of_array return_type in
      let return_type = type_of (machtype_option_of_array return_type) in
      let func =
        Llvm.declare_function
          function_name
          (function_type
             return_type
             (List.map args ~f:(fun arg -> Llvm.type_of arg.value) |> Array.of_list))
          this_module
      in
      if does_alloc
      then (
        let args = [ func ] @ List.map args ~f:(fun arg -> arg.value) in
        let function_type =
          Ir_type.function_type ~arg_types:(List.map args ~f:Llvm.type_of) ~return_type
        in
        let caml_c_call = Llvm.declare_function "caml_c_call" function_type this_module in
        let call = build_call caml_c_call (Array.of_list ([ func ] @ args)) "" builder in
        set_instruction_call_conv 101 call;
        { value = call; kind = return_kind })
      else (
        let call =
          build_call
            func
            (List.map args ~f:(fun arg -> arg.value) |> Array.of_list)
            ""
            builder
        in
        set_instruction_call_conv 100 call;
        { value = call; kind = return_kind })
    | Caddv, [ left; right ] ->
      let left =
        compile_expression left |> promote_value_if_necessary_exn ~new_machtype:(Some Val)
      in
      let right =
        compile_expression right
        |> promote_value_if_necessary_exn ~new_machtype:(Some Val)
      in
      let right = build_ptrtoint right.value int_type "" builder in
      { kind = Some Addr (* ? *); value = build_gep left.value [| right |] "" builder }
    | Caddv, _ -> assert false
    | Cadda, [ left; right ] ->
      let left =
        compile_expression left
        |> promote_value_if_necessary_exn ~new_machtype:(Some Addr)
      in
      let right =
        compile_expression right
        |> promote_value_if_necessary_exn ~new_machtype:(Some Addr)
      in
      let right = build_ptrtoint right.value int_type "" builder in
      { kind = Some Addr; value = build_gep left.value [| right |] "" builder }
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
      let left = cast_to_int_if_necessary_exn (compile_expression left) in
      let right = cast_to_int_if_necessary_exn (compile_expression right) in
      { value =
          build_zext
            (build_icmp cmp left.value right.value "icmp" builder)
            int_type
            "zext"
            builder
      ; kind = Some Int
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
        |> promote_value_if_necessary_exn ~new_machtype:(Some Float)
      in
      let right =
        compile_expression right
        |> promote_value_if_necessary_exn ~new_machtype:(Some Float)
      in
      { value =
          build_zext
            (build_fcmp cmp left.value right.value "fcmp" builder)
            int_type
            "zext"
            builder
      ; kind = Some Int
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
        |> promote_value_if_necessary_exn ~new_machtype:(Some Addr)
      in
      let right =
        compile_expression right
        |> promote_value_if_necessary_exn ~new_machtype:(Some Addr)
      in
      let diff = build_ptrdiff left.value right.value "diff" builder in
      { value =
          build_zext
            (build_icmp cmp diff (const_int 0).value "icmp" builder)
            int_type
            "zext"
            builder
      ; kind = Some Int
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
      let value = compile_expression value in
      let value = promote_value_if_necessary_exn ~new_machtype:(Some Float) value in
      { value = build_bitcast value.value int_type "intoffloat" builder; kind = Some Int }
    | Cintoffloat, _ -> assert false
    | Cfloatofint, [ value ] ->
      let value = compile_expression value in
      let value = promote_value_if_necessary_exn ~new_machtype:(Some Int) value in
      { value = build_bitcast value.value float_type "floatofint" builder
      ; kind = Some Float
      }
    | Cfloatofint, _ -> assert false
    | Cload (memory_chunk, _mutability), [ src ] ->
      let ptr =
        promote_value_if_necessary_exn ~new_machtype:(Some Addr) (compile_expression src)
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
      let ptr = build_pointercast ptr.value (pointer_type mem_lltype) "load" builder in
      let value = build_load ptr "" builder in
      let value =
        match should_extend with
        | `Don't -> value
        | `Zero -> build_zext value mem_lltype "zext" builder
        | `Signed -> build_sext value mem_lltype "sext" builder
        | `Float -> build_fpext value mem_lltype "fpext" builder
      in
      { value; kind = Some kind }
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
        |> promote_value_if_necessary_exn ~new_machtype:(Some kind)
      in
      let write_value =
        match should_truncate with
        | `Don't -> value.value
        | `Trunc -> build_trunc value.value mem_lltype "trunc" builder
        | `Float -> build_fptrunc value.value mem_lltype "fptrunc" builder
      in
      let ptr =
        compile_expression dst |> promote_value_if_necessary_exn ~new_machtype:(Some Addr)
      in
      let ptr = build_pointercast ptr.value (pointer_type mem_lltype) "" builder in
      (* eprint_s [%message "building store" (ptr : Ir_value.t) (write_value : Ir_value.t)]; *)
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
        compile_expression tag |> promote_value_if_necessary_exn ~new_machtype:(Some Int)
      in
      let length = const_int (List.length data) in
      (* eprint_s
        [%message "building call" (caml_alloc : Ir_value.t) (length : t) (tag_value : t)]; *)
      (* LLVM is sad if we try to do this :( *)
      (* let ptr_ptr = build_alloca (pointer_type (i8_type ctx)) "alloc_ptr" builder in *)
      (* let (_ : llvalue) =
        build_call llvm_gcroot [| ptr_ptr; const_pointer_null val_type |] "" builder
      in *)
      let ptr =
        build_call caml_alloc [| length.value; tag_value.value |] "alloc" builder
      in
      (* let (_ : llvalue) = build_store ptr ptr_ptr builder in *)
      List.iteri data ~f:(fun i elem ->
          let elem_ptr =
            build_in_bounds_gep ptr [| (const_int (i * 8)).value |] "gep" builder
          in
          let { value; kind = _ } = compile_expression elem in
          let elem_ptr =
            build_pointercast elem_ptr (pointer_type (Llvm.type_of value)) "" builder
          in
          let (_ : llvalue) = build_store value elem_ptr builder in
          ());
      { kind = Some Val; value = ptr }
    | Calloc, _ -> assert false
    | Ccheckbound, _ -> raise_s [%message "TODO check bounds"]
    | Craise _, _ -> raise_s [%message "TODO raise exceptions"]
  ;;
end
