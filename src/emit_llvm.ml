open! Core
open! Import
module Debug_info = Ocaml_common.Debuginfo

let type_of_function ctx (fundecl : Cmm.fundecl) =
  let env = String.Table.create () in
  List.iter fundecl.fun_args ~f:(fun (name, machtype) ->
      String.Table.set
        env
        ~key:(Backend_var.With_provenance.var name |> Backend_var.unique_name)
        ~data:(machtype, Var.Kind.lltype_of_t ~ctx (Var.Kind.of_machtype machtype)));
  let return_type = Var.Kind.lltype_of_t ~ctx (Machtype Val) in
  let args =
    List.map fundecl.fun_args ~f:(fun (_, machtype) ->
        Var.Kind.lltype_of_t ~ctx (Var.Kind.of_machtype machtype))
  in
  function_type return_type (List.to_array args)
;;

let mangle_symbol_name esc s =
  let prefix = "" in
  let mangled_name =
    String.concat_map s ~f:(function
        | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c -> Char.to_string c
        | c -> sprintf "%c%02x" esc (Char.to_int c))
  in
  prefix ^ mangled_name
;;

let value_of_data_item (data_item : Cmm.data_item) =
  let prefix =
    match Ocaml_common.Config.system with
    | "macosx" -> "_"
    | _ -> ""
  in
  match data_item with
  | Cdefine_symbol name -> prefix ^ mangle_symbol_name '$' name ^ ":"
  | Cglobal_symbol name -> ".globl " ^ prefix ^ mangle_symbol_name '$' name
  | Cint8 value -> sprintf ".byte %d" value
  | Cint16 value -> sprintf ".word %d" value
  | Cint32 value -> sprintf !".long %{Nativeint}" value
  | Cint value -> sprintf !".quad %{Nativeint}" value
  | Cdouble value ->
    let int = Int64.bits_of_float value in
    sprintf !".quad %{Int64}" int
  | Csingle _ -> assert false
  | Cstring literal -> sprintf !".ascii \"%s\"" literal
  | Csymbol_address name -> sprintf !".quad %s%s" prefix (mangle_symbol_name '$' name)
  | Calign n -> sprintf !".align %d" n
  | Cskip n -> sprintf !".space %d" n
;;

let emit ~ctx ~this_module (cmm : Cmm.phrase list) =
  let builder = builder ctx in
  let env : Cmm_to_llvm.t String.Table.t = String.Table.create () in
  (* Pre-define functions and globals to avoid issues with ordering. *)
  List.iter cmm ~f:(function
      | Cfunction cfundecl ->
        (* print_s [%message "declaring function" cfundecl.fun_name]; *)
        let name = mangle_symbol_name '$' cfundecl.fun_name in
        let (_ : llvalue) =
          declare_function name (type_of_function ctx cfundecl) this_module
        in
        ()
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
        let name = mangle_symbol_name '$' cfundecl.fun_name in
        let fundecl = lookup_function name this_module |> Option.value_exn in
        set_function_call_conv Declarations.ocaml_calling_convention fundecl;
        set_gc (Some "ocaml") fundecl;
        (* set argument names *)
        List.iter2_exn
          (params fundecl |> Array.to_list)
          cfundecl.fun_args
          ~f:(fun arg (name, machtype) ->
            let real_name =
              Backend_var.With_provenance.var name |> Backend_var.unique_name
            in
            set_value_name real_name arg;
            (* Core.eprint_s
              [%message
                "arg" (real_name : string) (machtype : Cmm.machtype_component array)]; *)
            String.Table.set
              env
              ~key:(value_name arg)
              ~data:
                { value = `Register arg
                ; kind =
                    (match machtype with
                    | [| x |] -> Machtype x
                    | [||] -> Void
                    | _ -> assert false)
                });
        String.Table.add_exn
          env
          ~key:cfundecl.fun_name
          ~data:{ value = `Register fundecl; kind = Machtype Val };
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
                         (Llvm.lookup_global name this_module : llvalue option)
                       ~llvm_function:
                         (Llvm.lookup_function name this_module : llvalue option)]; *)
                 if String.equal name cfundecl.fun_name
                 then
                   Some
                     (`Direct
                       { Cmm_to_llvm.value = `Register fundecl; kind = Machtype Addr })
                 else (
                   let name = mangle_symbol_name '$' name in
                   match Llvm.lookup_global name this_module with
                   | None ->
                     (match Llvm.lookup_function name this_module with
                     | Some g ->
                       let g =
                         build_pointercast g (pointer_type (i8_type ctx)) "" builder
                       in
                       Some
                         (`Direct
                           { Cmm_to_llvm.value = `Register g; kind = Machtype Addr })
                     | None ->
                       let g = declare_global (i8_type ctx) name this_module in
                       Some
                         (`Direct
                           { Cmm_to_llvm.value = `Register g; kind = Machtype Int }))
                   | Some g ->
                     Some
                       (`Direct { Cmm_to_llvm.value = `Register g; kind = Machtype Val }))
               ;;
             end)
           in
           let ret_val = Cmm_to_llvm.compile_expression cfundecl.fun_body in
           match ret_val.kind with
           | Never_returns -> ()
           | _ ->
             build_ret
               (Cmm_to_llvm.promote_value_if_necessary_exn
                  ~msg:[%message "promoting return value"]
                  ~new_machtype:(Machtype Val)
                  ret_val
               |> Cmm_to_llvm.llvm_value)
               builder
             |> (ignore : llvalue -> unit)
         with
        | exn ->
          let msg =
            [%message
              "exception raised while compiling function"
                ~name:(cfundecl.fun_name : string)
                ~args:
                  (cfundecl.fun_args
                    : (Backend_var.With_provenance.t * Cmm.machtype) list)
                ~expr:(cfundecl.fun_body : Cmm.expression)
                (exn : Exn.t)]
          in
          raise_s msg)
      | Cdata _ -> ())
;;
