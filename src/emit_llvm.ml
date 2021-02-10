open! Core
open! Import
module Backend_var = Ocaml_optcomp.Backend_var
module Debug_info = Ocaml_common.Debuginfo

let type_of_function ctx (fundecl : Cmm.fundecl) =
  let env = String.Table.create () in
  List.iter fundecl.fun_args ~f:(fun (name, machtype) ->
      String.Table.set
        env
        ~key:(Backend_var.With_provenance.name name)
        ~data:(machtype, Declarations.type_of_machtype ctx machtype));
  let return_type = Declarations.value_type ctx in
  let args =
    List.map fundecl.fun_args ~f:(fun (_, machtype) ->
        Declarations.type_of_machtype ctx machtype)
  in
  function_type return_type (List.to_array args)
;;

let value_of_data_item (data_item : Cmm.data_item) =
  match data_item with
  | Cdefine_symbol name -> "_" ^ name ^ ":"
  | Cglobal_symbol name -> ".globl _" ^ name
  | Cint8 value -> sprintf ".byte %d" value
  | Cint16 value -> sprintf ".word %d" value
  | Cint32 value -> sprintf !".long %{Nativeint}" value
  | Cint value -> sprintf !".quad %{Nativeint}" value
  | Cdouble value ->
    let int = Int64.bits_of_float value in
    sprintf !".quad %{Int64}" int
  | Csingle _ -> assert false
  | Cstring literal -> sprintf !".ascii \"%s\"" literal
  | Csymbol_address name -> sprintf !".quad _%s" name
  | Calign n -> sprintf !".align %d" n
  | Cskip n -> sprintf !".space %d" n
;;

let emit ~ctx ~this_module (cmm : Cmm.phrase list) =
  let builder = Ir_builder.create ctx in
  let env = String.Table.create () in
  List.iter (Declarations.builtin_functions ctx) ~f:(fun (name, funtype) ->
      Ir_module.declare_function' this_module ~name ~funtype);
  (* Pre-define functions and globals to avoid issues with ordering. *)
  List.iter cmm ~f:(function
      | Cfunction cfundecl ->
        (* print_s [%message "declaring function" cfundecl.fun_name]; *)
        Ir_module.declare_function'
          this_module
          ~name:cfundecl.fun_name
          ~funtype:(type_of_function ctx cfundecl)
      | Cdata _ -> ());
  let globals =
    List.concat_map cmm ~f:(function
        | Cfunction _ -> []
        | Cdata items -> ".data" :: List.map items ~f:value_of_data_item)
    |> String.concat ~sep:"\n"
  in
  List.iter cmm ~f:(function
      | Cfunction _ -> ()
      | Cdata items ->
        List.iter items ~f:(function
            | Cdefine_symbol name ->
              let (_ : llvalue) = Llvm.declare_global (i8_type ctx) name this_module in
              ()
            | _ -> ()));
  set_module_inline_asm this_module globals;
  List.iter cmm ~f:(function
      | Cfunction cfundecl ->
        (* print_s [%message "compiling function" (cfundecl.fun_name : string)]; *)
        let fundecl = Ir_module.lookup_function_exn this_module ~name:cfundecl.fun_name in
        set_function_call_conv Declarations.ocaml_calling_convention fundecl;
        set_gc (Some "ocaml") fundecl;
        (* set argument names *)
        List.iter2_exn
          (params fundecl |> Array.to_list)
          cfundecl.fun_args
          ~f:(fun arg (name, machtype) ->
            let real_name = Backend_var.With_provenance.name name in
            set_value_name real_name arg;
            (* Core.eprint_s
              [%message
                "arg" (real_name : string) (machtype : Cmm.machtype_component array)]; *)
            String.Table.set
              env
              ~key:(value_name arg)
              ~data:
                (Var.Value
                   { value = arg
                   ; kind =
                       Cmm_to_llvm.machtype_option_of_array machtype |> Option.value_exn
                   }));
        String.Table.add_exn
          env
          ~key:cfundecl.fun_name
          ~data:(Var.Value { value = fundecl; kind = Val });
        let entry = append_block ctx "entry" fundecl in
        position_at_end entry builder;
        (try
           let module Cmm_to_llvm =
             Cmm_to_llvm.With_context (struct
               let ctx = ctx
               let builder = builder
               let this_module = this_module
               let this_function = fundecl
               let env = env
               let catches = Int.Table.create ()

               let lookup_symbol name =
                 (* eprint_s
                   [%message
                     "lookup symbol"
                       (name : string)
                       ~llvm_global:
                         (Llvm.lookup_global name this_module : Ir_value.t option)
                       ~llvm_function:
                         (Llvm.lookup_function name this_module : Ir_value.t option)]; *)
                 if String.equal name cfundecl.fun_name
                 then Some (`Direct { Cmm_to_llvm.value = fundecl; kind = Some Addr })
                 else (
                   match Llvm.lookup_global name this_module with
                   | None ->
                     (match Llvm.lookup_function name this_module with
                     | Some g ->
                       let g =
                         build_pointercast g (pointer_type (i8_type ctx)) "" builder
                       in
                       Some (`Direct { Cmm_to_llvm.value = g; kind = Some Addr })
                     | None ->
                       let g = declare_global (i8_type ctx) name this_module in
                       Some (`Direct { Cmm_to_llvm.value = g; kind = Some Int }))
                   | Some g -> Some (`Direct { Cmm_to_llvm.value = g; kind = Some Val }))
               ;;
             end)
           in
           let ret_val = Cmm_to_llvm.compile_expression cfundecl.fun_body in
           build_ret
             (Cmm_to_llvm.promote_value_if_necessary_exn ~new_machtype:(Some Val) ret_val)
               .value
             builder
           |> (ignore : llvalue -> unit)
         with
        | exn ->
          let msg =
            [%message
              "exception raised while compiling function"
                ~name:(cfundecl.fun_name : string)
                ~expr:(cfundecl.fun_body : Cmm.expression)
                (exn : Exn.t)]
          in
          (* eprint_s msg; *)
          delete_function fundecl;
          raise_s msg)
      | Cdata _ -> ())
;;

let emit_llvm phrases =
  let ctx = Llvm.global_context () in
  Ir_module.with_module
    ~target_triple:"x86_64-apple-darwin19.6.0"
    ~ctx
    "melse"
    (fun this_module ->
      emit ~this_module ~ctx phrases;
      string_of_llmodule this_module |> print_endline)
;;
