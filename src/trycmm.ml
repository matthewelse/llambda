open! Core
open! Import
module Compilenv = Ocaml_optcomp.Compilenv
module Closure_middle_end = Ocaml_optcomp.Closure_middle_end
module Pprintast = Ocaml_common.Pprintast
module Printclambda = Ocaml_optcomp.Printclambda
module Printtyped = Ocaml_common.Printtyped
module Printlambda = Ocaml_common.Printlambda
module Compmisc = Ocaml_common.Compmisc
module Env = Ocaml_common.Env
module Translmod = Ocaml_common.Translmod
module Simplif = Ocaml_common.Simplif

let () =
  (* urgh global state *)
  Ocaml_common.Clflags.native_code := true
;;

let walk (code : Ocaml_optcomp.Cmm.fundecl) =
  eprint_s [%message "compiling cmm" ~function_name:(code.fun_name : string)]
;;

module Frontend = struct
  type t =
    { source_prefix : string (** e.g. melse for melse.ml *)
    ; dump_ast : bool
    ; dump_typed_ast : bool
    }

  let source_file t = t.source_prefix ^ ".ml"
  let module_name t = String.capitalize t.source_prefix

  let parse t source_code =
    let source = Lexing.from_string source_code in
    let structure = Ocaml_common.Parse.implementation source in
    if t.dump_ast then Pprintast.structure Format.std_formatter structure;
    structure
  ;;

  let type_impl t structure =
    Compmisc.init_path ();
    Env.set_unit_name (module_name t);
    let env = Compmisc.initial_env () in
    let typed =
      try
        Ocaml_common.Typemod.type_implementation
          (source_file t)
          t.source_prefix
          (module_name t)
          env
          structure
      with
      | Env.Error error as exn ->
        Env.report_error Format.std_formatter error;
        raise exn
    in
    if t.dump_typed_ast
    then Printtyped.implementation_with_coercion Format.std_formatter typed;
    typed
  ;;
end

module Ident = struct
  include Ocaml_common.Ident

  let sexp_of_t t = name t |> String.sexp_of_t
end

module Path = struct
  include Ocaml_common.Path

  let sexp_of_t t = name t |> String.sexp_of_t
end

module Backend = struct
  type t =
    { prefix_name : string
    ; dump_cmm : bool
    ; dump_clambda : bool
    ; dump_lambda : bool
    }

  let module_name t = String.capitalize t.prefix_name
  let filename t = t.prefix_name ^ ".ml"

  let to_cmm t (typed, coercion) =
    let lambda =
      Translmod.transl_store_implementation (module_name t) (typed, coercion)
    in
    let code = Simplif.simplify_lambda lambda.code in
    let lambda = { lambda with code } in
    if t.dump_lambda
    then (
      Printlambda.lambda Format.std_formatter code;
      print_endline "");
    let clambda =
      Closure_middle_end.lambda_to_clambda
        ~backend:Decls.backend
        ~filename:(filename t)
        ~prefixname:t.prefix_name
        ~ppf_dump:Format.std_formatter
        lambda
    in
    let ulambda, preallocated_blocks, structured_constants = clambda in
    if t.dump_clambda
    then (
      Printclambda.clambda Format.std_formatter ulambda;
      List.iter
        structured_constants
        ~f:(fun { symbol; exported; definition; provenance = _ } ->
          print_s [%message "structured constant" (symbol : string) (exported : bool)];
          match definition with
          | Uconst_block (length, constants) ->
            let len = List.length constants in
            print_s [%message "block" (length : int) (len : int)]
          | _ -> print_s [%message "other"]);
      List.iter
        preallocated_blocks
        ~f:(fun { symbol; exported; tag; fields; provenance = _ } ->
          let field_count = List.length fields in
          print_s
            [%message
              "preallocated block"
                (symbol : string)
                (exported : bool)
                (tag : int)
                (field_count : int)]));
    let cmm = Ocaml_optcomp.Cmmgen.compunit clambda in
    (* if t.dump_cmm then List.iter cmm ~f:(Printcmm.phrase Format.std_formatter); *)
    cmm
  ;;
end

let cmm_of_source ~dump_cmm source =
  Compilenv.reset "Melse";
  let config : Frontend.t =
    { source_prefix = "melse"; dump_ast = false; dump_typed_ast = false }
  in
  let structure = Frontend.parse config source in
  let typed_ast = Frontend.type_impl config structure in
  let config : Backend.t =
    { prefix_name = "melse"; dump_cmm; dump_clambda = false; dump_lambda = false }
  in
  Backend.to_cmm config typed_ast
;;
