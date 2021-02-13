open! Core

module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Ocaml_optcomp.Compilenv.symbol_for_global'
  let closure_symbol = Ocaml_optcomp.Compilenv.closure_symbol
  let really_import_approx = Ocaml_optcomp.Import_approx.really_import_approx
  let import_symbol = Ocaml_optcomp.Import_approx.import_symbol
  let size_int = 8
  let big_endian = false (* TODO *)

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Ocaml_optcomp.Proc.max_arguments_for_tailcalls - 1
  ;;
end

let backend = (module Backend : Backend_intf.S)
let loc = Location.none

let runtest ?(show_functions = [ "f" ]) structure =
  Ocaml_common.Warnings.without_warnings (fun () ->
      Clflags.native_code := true;
      Ocaml_optcomp.Compilenv.reset "Test";
      Ocaml_common.Compmisc.init_path ();
      Ocaml_common.Env.set_unit_name "test";
      let env = Ocaml_common.Compmisc.initial_env () in
      let typed, _, _, _ = Ocaml_common.Typemod.type_structure env structure loc in
      let transl =
        Ocaml_common.Translmod.transl_store_implementation "test" (typed, Tcoerce_none)
      in
      let code = Ocaml_common.Simplif.simplify_lambda transl.code in
      let lambda = { transl with code } in
      let clambda =
        Ocaml_optcomp.Closure_middle_end.lambda_to_clambda
          ~backend
          ~filename:"test.ml"
          ~prefixname:"test"
          ~ppf_dump:Format.std_formatter
          lambda
      in
      let cmm = Ocaml_optcomp.Cmmgen.compunit clambda in
      let ctx = Llvm.create_context () in
      let this_module = Llvm.create_module ctx (Ident.name lambda.module_ident) in
      Llvm.set_target_triple "x86_64-apple-macosx10.15.0" this_module;
      Llambda.Emit_llvm.emit ~ctx ~this_module cmm;
      Llvm.iter_functions
        (fun func ->
          let name = Llvm.value_name func in
          if List.exists show_functions ~f:(fun funcname ->
                 String.is_prefix name ~prefix:("camlTest__" ^ funcname))
          then Llvm.string_of_llvalue func |> print_endline)
        this_module;
      Llvm.dispose_module this_module;
      Llvm.dispose_context ctx)
;;
