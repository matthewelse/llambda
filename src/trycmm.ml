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

  module Longident = struct
    include Ocaml_common.Longident

    let sexp_of_t t = flatten t |> [%sexp_of: string list]
  end

  type nonrec lookup_error = Ocaml_common.Env.lookup_error =
    | Unbound_value of Longident.t * (Ocaml_common.Env.unbound_value_hint[@sexp.opaque])
    | Unbound_type of Longident.t
    | Unbound_constructor of Longident.t
    | Unbound_label of Longident.t
    | Unbound_module of Longident.t
    | Unbound_class of Longident.t
    | Unbound_modtype of Longident.t
    | Unbound_cltype of Longident.t
    | Unbound_instance_variable of string
    | Not_an_instance_variable of string
    | Masked_instance_variable of Longident.t
    | Masked_self_variable of Longident.t
    | Masked_ancestor_variable of Longident.t
    | Structure_used_as_functor of Longident.t
    | Abstract_used_as_functor of Longident.t
    | Functor_used_as_structure of Longident.t
    | Abstract_used_as_structure of Longident.t
    | Generative_used_as_applicative of Longident.t
    | Illegal_reference_to_recursive_module
    | Cannot_scrape_alias of Longident.t * (Ocaml_common.Path.t[@sexp.opaque])
  [@@deriving sexp_of]

  let type_impl t structure =
    Compmisc.init_path ();
    Env.set_unit_name (module_name t);
    let env =
      try Compmisc.initial_env () with
      | Ocaml_common.Env.Error error ->
        let error =
          match error with
          | Missing_module _ -> [%sexp Missing_module]
          | Illegal_value_name _ -> [%sexp Illegal_value_name]
          | Lookup_error (_, _, err) -> [%sexp Lookup_error (err : lookup_error)]
        in
        raise_s [%message "Error during Compmisc.initial_env" (error : Sexp.t)]
    in
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

  let make_startup_file units =
    let module Cmm_helpers = Ocaml_optcomp.Cmm_helpers in
    Ocaml_common.Location.input_name := "caml_startup";
    (* set name of "current" input *)
    Compilenv.reset "_startup";
    (* set the name of the "current" compunit *)
    let name_list = [ "Melse" ] in
    let cmm = [ Ocaml_optcomp.Cmm_helpers.entry_point name_list ] in
    let cmm = cmm @ Ocaml_optcomp.Cmm_helpers.generic_functions false units in
    let cmm =
      cmm
      @ (Array.mapi
           ~f:(fun i name -> Cmm_helpers.predef_exception i name)
           Ocaml_common.Runtimedef.builtin_exceptions
        |> Array.to_list)
    in
    let cmm = cmm @ [ Ocaml_optcomp.Cmm_helpers.global_table name_list ] in
    (* let globals_map = make_globals_map units_list ~crc_interfaces:[] in *)
    (* let cmm = Cmm_helpers.globals_map globals_map :: cmm in *)
    let cmm = cmm @ [ Cmm_helpers.data_segment_table ("_startup" :: name_list) ] in
    let cmm = cmm @ [ Cmm_helpers.code_segment_table ("_startup" :: name_list) ] in
    let all_names = "_startup" :: "_system" :: name_list in
    let cmm = cmm @ [ Cmm_helpers.frame_table all_names ] in
    cmm
  ;;

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
    (* Ident.Set.iter
      (fun ident ->
        eprint_s [%message "Required global identifier" ~ident:(Ident.name ident)];
        Compilenv.require_global ident)
      lambda.required_globals; *)
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
          eprint_s [%message "structured constant" (symbol : string) (exported : bool)];
          match definition with
          | Uconst_block (length, constants) ->
            let len = List.length constants in
            eprint_s [%message "block" (length : int) (len : int)]
          | _ -> eprint_s [%message "other"]);
      List.iter
        preallocated_blocks
        ~f:(fun { symbol; exported; tag; fields; provenance = _ } ->
          let field_count = List.length fields in
          eprint_s
            [%message
              "preallocated block"
                (symbol : string)
                (exported : bool)
                (tag : int)
                (field_count : int)]));
    let cmm = Ocaml_optcomp.Cmmgen.compunit clambda in
    (* Generate generic functions: curry/apply/tuplify etc *)
    let cmm' = make_startup_file [ Compilenv.current_unit_infos () ] in
    let cmm = cmm @ cmm' in
    if t.dump_cmm
    then List.iter cmm ~f:(Ocaml_optcomp.Printcmm.phrase Format.std_formatter);
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
