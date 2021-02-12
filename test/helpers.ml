open! Core

let loc = Location.none

let runtest structure =
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
          ~backend:Llambda.For_testing.Decls.backend
          ~filename:"test.ml"
          ~prefixname:"test"
          ~ppf_dump:Format.std_formatter
          lambda
      in
      let cmm = Ocaml_optcomp.Cmmgen.compunit clambda in
      let ctx = Llvm.create_context () in
      Wrap_llvm.Ir_module.with_module ~ctx "test" (fun this_module ->
          Llambda.Emit_llvm.emit ~ctx ~this_module cmm;
          Llvm.iter_functions
            (fun func ->
              let name = Llvm.value_name func in
              if String.is_prefix name ~prefix:"camlTest__f"
              then Llvm.string_of_llvalue func |> print_endline)
            this_module);
      Llvm.dispose_context ctx)
;;
