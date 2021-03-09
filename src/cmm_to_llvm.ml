(** Convert OCaml's internal C-- representation to LLVM. *)

open! Core
open! Import
include Cmm_to_llvm_intf
module Typed_llvm = Llambda_typedllvm

module With_context (Context : Context) = struct
  open Context
  open Typed_llvm

  let int_type = Ltype.int_type ctx I64
  let val_type = Ltype.pointer_type (Ltype.int_type ctx I8)
  let float_type = Ltype.float_type ctx Double
  let void_type = Ltype.void_type ctx

  module Intrinsics = struct
    let gcroot =
      Value.declare_function
        ~name:"llvm.gcroot"
        ~typ:Ltype.Func.(Ltype.pointer_type val_type @-> val_type @-> returns void_type)
        ~module_:this_module
    ;;

    let read_register reg builder =
      let name = match reg with `r15 -> "r15" | `r14 -> "r14" in
      let reg =
        let reg_md = Value.mdstring ~ctx name in
        Value.mdnode ~ctx [ reg_md ]
      in
      let func =
        Value.declare_function
          ~name:"llvm.read_register.i64"
          ~typ:Ltype.Func.(Ltype.mdnode ctx (Ltype.mdstring ctx) @-> returns int_type)
          ~module_:this_module
      in
      let value =
        Value.build_call
          func
          ~args:Value.Args.(reg @: nil)
          ~name:("read_" ^ name)
          ~builder
      in
      Value.build_inttoptr value ~ptr_type:val_type ~name:"" ~builder
    ;;

    let write_register reg value builder =
      let name = match reg with `r15 -> "r15" in
      let reg =
        let reg_md = Value.mdstring ~ctx name in
        Value.mdnode ~ctx [ reg_md ]
      in
      let value = Value.build_ptrtoint value ~name:"" ~builder in
      let func =
        Value.declare_function
          ~name:"llvm.write_register.i64"
          ~typ:
            Ltype.Func.(
              Ltype.mdnode ctx (Ltype.mdstring ctx) @-> int_type @-> returns void_type)
          ~module_:this_module
      in
      Value.build_call func ~args:Value.Args.(reg @: value @: nil) ~name:"" ~builder
    ;;
  end

  let raise_exn =
    Value.const_inline_asm
      ~typ:Ltype.Func.(val_type @-> val_type @-> returns void_type)
      ~assembly:"movq ($1),%rsp; popq ($1); popq %r11; jmp *%r11"
      ~constraints:"{rax},r"
      ~has_side_effects:true
      ~should_align_stack:false
  ;;

  let type_of_kind kind = Var.Kind.lltype_of_t ~ctx kind
  let unwrap v = Value.to_llvm v
  let unwrap_fn f = Value.fn_to_llvm f
  let unwrap_type t = Ltype.to_llvm t

  (* Provide ourselves with ways to cheat while we make things more typeful. *)
  let wrap v = Value.unsafe_of_llvm v

  (* let wrap_type t = Value.unsafe_of_llvm t *)

  let const_unit =
    { value = `Register (Value.const_int ctx I64 1 |> unwrap); kind = Void }
  ;;

  let const_int value =
    { value = `Register (Value.const_int ctx I64 value |> unwrap); kind = Machtype Int }
  ;;

  let const_int64 ?signed value =
    { value = `Register (Value.const_int64 ctx I64 value ?signed |> unwrap)
    ; kind = Machtype Int
    }
  ;;

  let const_pointer value =
    { value =
        `Register
          (Value.const_inttoptr (Value.const_int ctx I64 value) ~ptr_type:val_type
          |> unwrap)
    ; kind = Machtype Val
    }
  ;;

  let const_float value =
    { value = `Register (Value.const_float ctx Double value |> unwrap)
    ; kind = Machtype Float
    }
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

  let promote_value_if_necessary ?msg:_ ~(new_machtype : Var.Kind.t) t : t option =
    match new_machtype, t.kind with
    | Void, Void -> Some t
    | Machtype Int, Machtype Int
    | Machtype Val, Machtype Val
    | Machtype Addr, Machtype Addr
    | Machtype Float, Machtype Float ->
      Some t
    | Machtype (Addr | Val), Machtype Int ->
      let new_type = unwrap_type val_type in
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

  let promote_value_if_necessary_exn ?msg ~new_machtype t =
    match promote_value_if_necessary ?msg ~new_machtype t with
    | Some t -> t
    | None ->
      raise_s
        [%message
          "Unable to promote value."
            (msg : Sexp.t option)
            (new_machtype : Var.Kind.t)
            (t : t)]
  ;;

  let cast_to_int_if_necessary_exn t =
    match t.kind with
    | Void | Machtype Float | Machtype Addr ->
      raise_s [%message "Cannot demomote addresses or floats to integer."]
    | Machtype Int -> t
    | Machtype Val ->
      let new_type = unwrap_type int_type in
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
      | Cdivi -> build_sdiv
      | Cmodi -> build_srem
      | Cand -> build_and
      | Cor -> build_or
      | Cxor -> build_xor
      | Clsl -> build_shl
      | Clsr -> build_lshr
      | Casr -> build_ashr
      | Cmulhi ->
        fun left right name builder ->
          let left = build_lshr left (const_int 32 |> llvm_value) name builder in
          let right = build_lshr right (const_int 32 |> llvm_value) name builder in
          let output = build_mul left right name builder in
          let output = build_shl output (const_int 32 |> llvm_value) name builder in
          build_or output (const_int 1 |> llvm_value) name builder
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
        (Llvm.function_type (unwrap_type float_type) [| unwrap_type float_type |])
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
    | Cblockheader (value, _) -> const_int64 ~signed:false (Nativeint.to_int64 value)
    | Cvar name ->
      let name = Backend_var.unique_name name in
      (match String.Table.find env name with
      | Some t ->
        let value = llvm_value t in
        { t with value = `Register value }
      | None -> raise_s [%message "Unknown variable" (name : string)])
    | Clet (var, value, body) ->
      (* FIXME: unify this code with let_mut *)
      let var_name = Backend_var.With_provenance.var var |> Backend_var.unique_name in
      let var_value = compile_expression value in
      let insertion_block = insertion_block builder in
      let entry_bb = entry_block this_function in
      (match block_terminator entry_bb with
      | None -> position_at_end entry_bb builder
      | Some terminator -> position_before terminator builder);
      let value = llvm_value var_value in
      let var_ptr = Llvm.build_alloca (type_of value) var_name builder in
      (match var_value.kind with
      | Void | Never_returns | Machtype Int | Machtype Addr | Machtype Float -> ()
      | Machtype Val ->
        (* Add a GC root for this thing. *)
        let var_ptr =
          build_pointercast var_ptr (pointer_type (unwrap_type val_type)) "" builder
        in
        let (_ : llvalue) =
          build_call
            (unwrap_fn Intrinsics.gcroot)
            [| var_ptr; const_pointer_null (unwrap_type val_type) |]
            ""
            builder
        in
        ());
      position_at_end insertion_block builder;
      let (_ : llvalue) = build_store value var_ptr builder in
      with_var_in_env
        ~name:var_name
        ~value:{ value = `Stack var_ptr; kind = var_value.kind }
        ~f:(fun () -> compile_expression body)
    | Clet_mut (var, machtype, value, body) ->
      let var_name = Backend_var.With_provenance.var var |> Backend_var.unique_name in
      let var_value =
        compile_expression value
        |> promote_value_if_necessary_exn
             ~msg:[%message "letmut" (expr : Cmm.expression)]
             ~new_machtype:(Var.Kind.of_machtype machtype)
      in
      let insertion_block = insertion_block builder in
      let entry_bb = entry_block this_function in
      (match block_terminator entry_bb with
      | None -> position_at_end entry_bb builder
      | Some terminator -> position_before terminator builder);
      let value = llvm_value var_value in
      let var_ptr = Llvm.build_alloca (type_of value) var_name builder in
      (match var_value.kind with
      | Void | Never_returns | Machtype Int | Machtype Addr | Machtype Float -> ()
      | Machtype Val ->
        (* Add a GC root for this thing. *)
        let var_ptr =
          build_pointercast var_ptr (pointer_type (unwrap_type val_type)) "" builder
        in
        let (_ : llvalue) =
          build_call
            (unwrap_fn Intrinsics.gcroot)
            [| var_ptr; const_pointer_null (unwrap_type val_type) |]
            ""
            builder
        in
        ());
      position_at_end insertion_block builder;
      let (_ : llvalue) = build_store value var_ptr builder in
      with_var_in_env
        ~name:var_name
        ~value:{ value = `Stack var_ptr; kind = var_value.kind }
        ~f:(fun () -> compile_expression body)
    | Cphantom_let _ ->
      raise_s
        [%message
          "TODO: I don't know how to handle phantom lets yet." (expr : Cmm.expression)]
    | Cassign (var, new_value_expr) ->
      let var_name = Backend_var.unique_name var in
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
          |> promote_value_if_necessary_exn
               ~msg:[%message "assign" (expr : Cmm.expression)]
               ~new_machtype:kind
          |> llvm_value
        in
        let (_ : llvalue) = build_store new_value ptr builder in
        const_unit)
    | Ctuple [] -> const_unit
    | Ctuple _ ->
      (* TODO: Consider supporting larger tuples? *)
      raise_s [%message "Unable to support non-empty tuples." (expr : Cmm.expression)]
    | Cop (operation, args, debug_info) -> compile_operation operation args debug_info
    | Csequence (before, after) ->
      let (_ : t) = compile_expression before in
      compile_expression after
    | Cifthenelse (cond, _, then_, _, else_, _) ->
      let start_bb = insertion_block builder in
      let cond = compile_expression cond |> cast_to_int_if_necessary_exn |> llvm_value in
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
        promote_value_if_necessary_exn
          ~msg:[%message "ifthenelse" (expr : Cmm.expression)]
          ~new_machtype:target_kind
          then_value
      in
      let else_value =
        promote_value_if_necessary_exn
          ~msg:[%message "ifthenelse" (expr : Cmm.expression)]
          ~new_machtype:target_kind
          else_value
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
          let kind = then_value.kind in
          let then_value = llvm_value then_value in
          let (_ : llvalue) = build_br merge_bb builder in
          [ kind, then_value, real_then_bb ]
        | Some _ -> []
      in
      position_at_end real_else_bb builder;
      let incoming =
        match block_terminator real_else_bb with
        | None ->
          let kind = else_value.kind in
          let else_value = llvm_value else_value in
          let (_ : llvalue) = build_br merge_bb builder in
          (kind, else_value, real_else_bb) :: incoming
        | Some _ -> incoming
      in
      (match incoming with
      | [] ->
        remove_block merge_bb;
        const_unit
      | _ ->
        let incoming =
          List.filter_map incoming ~f:(fun (kind, value, bb) ->
              match kind with Void -> None | _ -> Some (value, bb))
        in
        position_at_end merge_bb builder;
        if List.is_empty incoming
        then const_unit
        else
          { value = `Register (build_phi incoming "iftmp" builder); kind = target_kind })
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
          let kind = handler_value.kind in
          let handler_value = llvm_value handler_value in
          let (_ : llvalue) = build_br exit_bb builder in
          [ kind, handler_value, real_handler_bb ]
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
          let kind = body_value.kind in
          let body_value = llvm_value body_value in
          let (_ : llvalue) = build_br exit_bb builder in
          (kind, body_value, real_body_bb) :: incoming
        | Some _ -> incoming
      in
      (match incoming with
      | [] ->
        remove_block exit_bb;
        const_unit
      | _ ->
        let incoming =
          List.filter_map incoming ~f:(fun (kind, value, bb) ->
              match kind with Void -> None | _ -> Some (value, bb))
        in
        position_at_end exit_bb builder;
        if List.is_empty incoming
        then const_unit
        else
          { value = `Register (build_phi incoming [%string "phi.%{index#Int}"] builder)
          ; kind = Machtype Val
          })
    | Ccatch _ ->
      (* TODO: consider handling more complex catch statements *)
      raise_s
        [%message "Unable to handle complex catch statements." (expr : Cmm.expression)]
    | Cexit (index, []) ->
      let (_ : llvalue) = build_br (Int.Table.find_exn catches index) builder in
      const_unit
    | Cexit _ ->
      (* TODO: consider handling more complex exits.*)
      raise_s
        [%message "Unable to handle complex exit statements." (expr : Cmm.expression)]
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
              let kind = result.kind in
              let value = llvm_value result in
              let (_ : llvalue) = build_br exit_bb builder in
              Some (kind, value, real_bb)
            | Some _ -> None)
        |> Array.to_list
        |> List.filter_opt
      in
      (match results with
      | [] ->
        remove_block exit_bb;
        const_unit
      | results ->
        let kind =
          List.map results ~f:(fun (kind, _, _) -> kind)
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
        let incoming =
          List.filter_map results ~f:(fun (kind, value, bb) ->
              match kind with Void -> None | _ -> Some (value, bb))
        in
        if List.is_empty incoming
        then const_unit
        else { value = `Register (build_phi incoming "phi" builder); kind })
    | Ctrywith (expr, var, handler, _) ->
      (* This is a sketchy implementation of exception handling that avoids
         having to teach LLVM about OCaml-style exceptions. It most likely
         breaks GC roots inside the try with, since we don't tell LLVM about the
         push and pop operations. Maybe in the future we can just use alloca? *)
      let domain_state_ptr = Intrinsics.read_register `r14 builder in
      let domain_exn_ptr =
        let offset = Ocaml_common.Domainstate.idx_of_field Domain_exception_pointer * 8 in
        Value.build_in_bounds_gep
          domain_state_ptr
          ~offsets:[ Value.const_int ctx I64 offset ]
          ~name:"domain_exn_ptr"
          ~builder
      in
      let push_handler =
        Value.const_inline_asm
          ~typ:Ltype.Func.(val_type @-> val_type @-> returns void_type)
          ~assembly:"lea $1(%rip),%r11; push %r11; push ($0); mov %rsp,($0)"
          ~constraints:"r,X,~{r11}"
          ~has_side_effects:true
          ~should_align_stack:false
      in
      let pop_handler =
        Value.const_inline_asm
          ~typ:Ltype.Func.(val_type @-> returns void_type)
          ~assembly:"pop ($0); add $$8,%rsp"
          ~constraints:"r"
          ~has_side_effects:true
          ~should_align_stack:false
      in
      (*
        We want to end up with something like this

        {v
            [entry]
               |      
               | <- push handler
               |
             .body  ======raise========> handler_bb
               |                             |
               | <- pop handler              |
               |                             |
             .merge <-----------------------/
        v}

        FIXME: Note that we don't currently model situations like:

        {v
          for (int i = 0; i < n; i++) {
            do_something_outside_the_handler();
            try {
              int x = do_something();
              if (x == 0) {
                continue;
              } else {
                do_something_else();
              }
            } catch {
              do_some_cleanup();
            }
          }
        v}

        We should pop the handler just before continue'ing to the start of the
        loop again. Fortunately, this would be a fairly unusual (impossible?)
        thing to do in OCaml, so for the time being I'll leave it.
      *)
      let body_bb = append_block ctx "body" this_function in
      let handler_bb = append_block ctx "handler" this_function in
      let merge_bb = append_block ctx "merge" this_function in
      let (_ : unit Value.t) =
        (* We kind of abuse LLVM's callbr instruction here. It's intended to
           allow assembly code to branch to LLVM labels, but here we store the
           label on the stack, and branch to it later. The semantics are similar
           enough that it doesn't break too many things. *)
        Value.build_callbr
          push_handler
          ~fallthrough:body_bb
          ~args:
            Value.Args.(
              domain_exn_ptr
              @: Value.block_address
                   ~func:(Value.unsafe_fn_of_llvm this_function)
                   handler_bb
              @: nil)
          ~targets:[ handler_bb ]
          ~name:""
          ~builder
      in
      (* compile the bit between try and with *)
      position_at_end body_bb builder;
      let good_case = compile_expression expr in
      let real_body_bb = insertion_block builder in
      let incoming =
        (* pop the handler, then continue *)
        match block_terminator real_body_bb with
        | None ->
          let (_ : unit Value.t) =
            Value.build_call
              pop_handler
              ~args:Value.Args.(domain_exn_ptr @: nil)
              ~name:""
              ~builder
          in
          let (_ : llvalue) = build_br merge_bb builder in
          [ good_case, real_body_bb ]
        | Some terminator ->
          position_before terminator builder;
          (* FIXME: This probably actually needs to happen before any exit
             nodes... I don't know how to find them though.  *)
          let (_ : unit Value.t) =
            Value.build_call
              pop_handler
              ~args:Value.Args.(domain_exn_ptr @: nil)
              ~name:""
              ~builder
          in
          []
      in
      (* compile the handler *)
      position_at_end handler_bb builder;
      let get_exn =
        Value.const_inline_asm
          ~typ:Ltype.Func.(returns val_type)
          ~assembly:""
          ~constraints:"={rax}"
          ~has_side_effects:false
          ~should_align_stack:false
      in
      let exn = Value.build_call get_exn ~args:Value.Args.(nil) ~name:"exn" ~builder in
      let handler_result =
        with_var_in_env
          ~name:(Backend_var.unique_name (Backend_var.With_provenance.var var))
          ~value:{ value = `Register (unwrap exn); kind = Machtype Val }
          ~f:(fun () -> compile_expression handler)
      in
      let real_handler_bb = insertion_block builder in
      let incoming =
        match block_terminator real_handler_bb with
        | None ->
          let (_ : llvalue) = build_br merge_bb builder in
          (handler_result, real_handler_bb) :: incoming
        | Some _ -> incoming
      in
      (match incoming with
      | [] ->
        remove_block merge_bb;
        const_unit
      | _ ->
        let target_kind =
          List.map incoming ~f:(fun (t, _) -> t.kind)
          |> List.reduce_exn ~f:(fun l r ->
                 match l, r with
                 | Machtype l, Machtype r -> Var.Kind.Machtype (Cmm.lub_component l r)
                 | Void, Void -> Void
                 | Never_returns, other | other, Never_returns -> other
                 | Void, _ | _, Void ->
                   raise_s [%message "Try/with with mis-matching branches."])
        in
        let incoming =
          List.filter_map incoming ~f:(fun (t, bb) ->
              match t.kind with
              | Void -> None
              | _ ->
                Some
                  ( promote_value_if_necessary_exn ~new_machtype:target_kind t
                    |> llvm_value
                  , bb ))
        in
        position_at_end merge_bb builder;
        if List.is_empty incoming
        then const_unit
        else
          { value = `Register (build_phi incoming [%string "phi"] builder)
          ; kind = Machtype Val
          })

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
        let args = func :: args in
        let function_type =
          function_type return_type (List.map args ~f:Llvm.type_of |> Array.of_list)
        in
        let caml_c_call = Llvm.declare_function "caml_c_call" function_type this_module in
        let call = build_call caml_c_call (Array.of_list args) "" builder in
        (* Epilogue to a c call - put r15 into *r14... *)
        let r14 = Intrinsics.read_register `r14 builder in
        let r14 =
          Value.build_pointercast
            r14
            ~new_type:(Ltype.pointer_type val_type)
            ~name:""
            ~builder
        in
        let star_r14 = Value.build_load r14 ~name:"" ~builder in
        let (_ : llvalue) = Intrinsics.write_register `r15 star_r14 builder |> unwrap in
        set_instruction_call_conv Declarations.ocaml_ext_calling_convention call;
        { value = `Register call; kind = return_kind })
      else (
        let call = build_call func (Array.of_list args) "" builder in
        { value = `Register call; kind = return_kind })
    | Caddv, [ left; right ] ->
      let left =
        compile_expression left
        |> promote_value_if_necessary_exn
             ~msg:[%message "addv" (operation : Cmm.operation)]
             ~new_machtype:(Machtype Val)
        |> llvm_value
      in
      let right =
        compile_expression right |> cast_to_int_if_necessary_exn |> llvm_value
      in
      { kind = Machtype Val; value = `Register (build_gep left [| right |] "" builder) }
    | Caddv, _ -> assert false
    | Cadda, [ left; right ] ->
      let left =
        compile_expression left
        |> promote_value_if_necessary_exn
             ~msg:[%message "adda"]
             ~new_machtype:(Machtype Addr)
        |> llvm_value
      in
      let right =
        compile_expression right |> cast_to_int_if_necessary_exn |> llvm_value
      in
      { kind = Machtype Addr; value = `Register (build_gep left [| right |] "" builder) }
    | Cadda, _ -> assert false
    | ( ( Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor | Cxor | Clsl | Clsr
        | Casr )
      , [ left; right ] ) ->
      (* We should be able to do these operations on Val too *)
      let left = compile_expression left in
      let right = compile_expression right in
      (* print_s [%message "binary operation" (left : t) (right : t)]; *)
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
               (unwrap_type int_type)
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
               (unwrap_type int_type)
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
        |> promote_value_if_necessary_exn
             ~msg:[%message "cmpa"]
             ~new_machtype:(Machtype Addr)
        |> llvm_value
      in
      let right =
        compile_expression right
        |> promote_value_if_necessary_exn
             ~msg:[%message "cmpa"]
             ~new_machtype:(Machtype Addr)
        |> llvm_value
      in
      let diff = build_ptrdiff left right "diff" builder in
      { value =
          `Register
            (build_zext
               (build_icmp cmp diff (const_int 0 |> llvm_value) "icmp" builder)
               (unwrap_type int_type)
               "zext"
               builder)
      ; kind = Machtype Int
      }
    | Ccmpa _, _ -> assert false
    | (Caddf | Csubf | Cmulf | Cdivf), [ left; right ] ->
      (* FIXME: the float operations generated here are probably not quite equivalent to ocamlopt. *)
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
      { value =
          `Register (build_bitcast value (unwrap_type int_type) "intoffloat" builder)
      ; kind = Machtype Int
      }
    | Cintoffloat, _ -> assert false
    | Cfloatofint, [ value ] ->
      let value =
        compile_expression value
        |> promote_value_if_necessary_exn ~new_machtype:(Machtype Int)
        |> llvm_value
      in
      { value =
          `Register (build_bitcast value (unwrap_type float_type) "floatofint" builder)
      ; kind = Machtype Float
      }
    | Cfloatofint, _ -> assert false
    | Cload (memory_chunk, _mutability), [ src ] ->
      let ptr =
        promote_value_if_necessary_exn
          ~msg:[%message "cload"]
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
        | Word_int -> Int, unwrap_type int_type, `Don't
        | Word_val -> Val, unwrap_type val_type, `Don't
        | Single -> Float, Llvm.float_type ctx, `Float
        | Double | Double_u -> Float, unwrap_type float_type, `Don't
      in
      let ptr = build_pointercast ptr (pointer_type mem_lltype) "load" builder in
      let value = build_load ptr "" builder in
      let result_type = type_of_kind (Machtype kind) in
      let value =
        match should_extend with
        | `Don't -> value
        | `Zero -> build_zext value result_type "zext" builder
        | `Signed -> build_sext value result_type "sext" builder
        | `Float -> build_fpext value result_type "fpext" builder
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
        | Word_int -> Int, unwrap_type int_type, `Don't
        | Word_val -> Val, unwrap_type val_type, `Don't
        | Single -> Float, Llvm.float_type ctx, `Float
        | Double | Double_u -> Float, unwrap_type float_type, `Don't
      in
      let value =
        compile_expression value
        |> promote_value_if_necessary_exn
             ~msg:[%message "store value"]
             ~new_machtype:(Machtype kind)
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
        |> promote_value_if_necessary_exn
             ~msg:[%message "store dest"]
             ~new_machtype:(Machtype Addr)
        |> llvm_value
      in
      let ptr = build_pointercast ptr (pointer_type mem_lltype) "" builder in
      let (_ : llvalue) = build_store write_value ptr builder in
      { (const_int 1) with kind = Void }
    | Cstore _, _ -> assert false
    | Calloc, tag :: data ->
      let insertion_block = insertion_block builder in
      let entry_bb = entry_block this_function in
      (match block_terminator entry_bb with
      | None -> position_at_end entry_bb builder
      | Some terminator -> position_before terminator builder);
      let ptr_ptr = build_alloca (pointer_type (i8_type ctx)) "alloc_ptr" builder in
      let (_ : llvalue) =
        build_call
          (unwrap_fn Intrinsics.gcroot)
          [| ptr_ptr; const_pointer_null (unwrap_type val_type) |]
          ""
          builder
      in
      position_at_end insertion_block builder;
      let bytes = 8 + (List.length data * 8) in
      let bytes_ll = const_int (-bytes) |> llvm_value in
      let function_to_call =
        match bytes with
        | 16 -> "caml_alloc1"
        | 24 -> "caml_alloc2"
        | 32 -> "caml_alloc3"
        | _ ->
          let r15 = Intrinsics.read_register `r15 builder in
          let new_r15 = build_gep (unwrap r15) [| bytes_ll |] "" builder in
          let (_ : llvalue) =
            Intrinsics.write_register `r15 (wrap new_r15) builder |> unwrap
          in
          "caml_allocN"
      in
      let caml_alloc =
        Llvm.declare_function
          function_to_call
          (function_type (unwrap_type void_type) [||])
          this_module
      in
      let (_ : llvalue) = build_call caml_alloc [||] "" builder in
      let r15 = Intrinsics.read_register `r15 builder |> unwrap in
      let ptr = build_gep r15 [| const_int 8 |> llvm_value |] "" builder in
      let tag_value =
        compile_expression tag
        |> promote_value_if_necessary_exn
             ~msg:[%message "calloc"]
             ~new_machtype:(Machtype Int)
        |> llvm_value
      in
      let (_ : llvalue) = build_store ptr ptr_ptr builder in
      let tag_ptr = r15 in
      let tag_ptr =
        build_pointercast tag_ptr (pointer_type (type_of tag_value)) "" builder
      in
      let (_ : llvalue) = build_store tag_value tag_ptr builder in
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
    | Ccheckbound, [ upper_bound; index ] ->
      let upper_bound =
        compile_expression upper_bound |> cast_to_int_if_necessary_exn |> llvm_value
      in
      let index =
        compile_expression index |> cast_to_int_if_necessary_exn |> llvm_value
      in
      (* {[
      if index >= bound then
        raise
      else
        ()
      ]}
      *)
      let out_of_bounds = append_block ctx "oob" this_function in
      let in_bounds = append_block ctx "inbounds" this_function in
      let cond = build_icmp Slt index upper_bound "boundscheck" builder in
      let (_ : llvalue) = build_cond_br cond in_bounds out_of_bounds builder in
      position_at_end out_of_bounds builder;
      (* FIXME: throw an exception instead. *)
      let abort =
        declare_function "abort" (function_type (unwrap_type void_type) [||]) this_module
      in
      let (_ : llvalue) = build_call abort [||] "" builder in
      let (_ : llvalue) = build_unreachable builder in
      position_at_end in_bounds builder;
      { (const_int 1) with kind = Void }
    | Ccheckbound, _ -> assert false
    | Craise raise_kind, [ exn ] ->
      let exn_val =
        compile_expression exn
        |> promote_value_if_necessary_exn
             ~msg:[%message "raise"]
             ~new_machtype:(Machtype Val)
        |> llvm_value
      in
      let domain_state_ptr = Intrinsics.read_register `r14 builder in
      let caml_raise_exn =
        declare_function
          "caml_raise_exn"
          (function_type (unwrap_type void_type) [| unwrap_type val_type |])
          this_module
      in
      (match raise_kind with
      | Raise_notrace ->
        let domain_exn_ptr =
          let offset =
            Ocaml_common.Domainstate.idx_of_field Domain_exception_pointer * 8
          in
          build_in_bounds_gep
            (unwrap domain_state_ptr)
            [| const_int offset |> llvm_value |]
            "domain_exn_ptr"
            builder
        in
        let call =
          build_call (unwrap_fn raise_exn) [| exn_val; domain_exn_ptr |] "" builder
        in
        set_instruction_call_conv Declarations.ocaml_calling_convention call;
        { kind = Never_returns; value = `Register (build_unreachable builder) }
      | Raise_reraise ->
        let call = build_call caml_raise_exn [| exn_val |] "" builder in
        set_instruction_call_conv Declarations.ocaml_calling_convention call;
        { kind = Never_returns; value = `Register (build_unreachable builder) }
      | Raise_regular ->
        let domain_backtrace_ptr =
          let offset = Ocaml_common.Domainstate.idx_of_field Domain_backtrace_pos * 8 in
          build_in_bounds_gep
            (unwrap domain_state_ptr)
            [| const_int offset |> llvm_value |]
            "domain_exn_ptr"
            builder
        in
        let (_ : llvalue) =
          build_store (const_int 0 |> llvm_value) domain_backtrace_ptr builder
        in
        let call = build_call caml_raise_exn [| exn_val |] "" builder in
        set_instruction_call_conv Declarations.ocaml_calling_convention call;
        { kind = Never_returns; value = `Register (build_unreachable builder) })
    | Craise _, _ -> assert false
  ;;
end
