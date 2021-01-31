open Core

let () =
  (* urgh global state *)
  Clflags.native_code := true
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
      Ocaml_common.Typemod.type_implementation
        (source_file t)
        t.source_prefix
        (module_name t)
        env
        structure
    in
    if t.dump_typed_ast
    then Printtyped.implementation_with_coercion Format.std_formatter typed;
    typed
  ;;
end

module Ident = struct
  include Ident

  let sexp_of_t t = Ident.name t |> String.sexp_of_t
end

module Path = struct
  include Path

  let sexp_of_t t = Path.name t |> String.sexp_of_t
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
    let cmm = Cmmgen.compunit clambda in
    if t.dump_cmm then List.iter cmm ~f:(Printcmm.phrase Format.std_formatter);
    cmm
  ;;
end

let cmm_of_source source =
  Compilenv.reset "Melse";
  let config : Frontend.t =
    { source_prefix = "melse"; dump_ast = false; dump_typed_ast = false }
  in
  let structure = Frontend.parse config source in
  let typed_ast = Frontend.type_impl config structure in
  let config : Backend.t =
    { prefix_name = "melse"; dump_cmm = true; dump_clambda = false; dump_lambda = false }
  in
  Backend.to_cmm config typed_ast
;;

let%expect_test "compile" =
  let source = {|
    let f x = 1234 + x;;
  |} in
  let config : Frontend.t =
    { source_prefix = "melse"; dump_ast = true; dump_typed_ast = true }
  in
  let structure = Frontend.parse config source in
  [%expect {|
    let f x = 1234 + x |}];
  let typed_ast = Frontend.type_impl config structure in
  [%expect
    {|
    [
      structure_item ([2,1+4]..[2,1+22])
        Tstr_value Nonrec
        [
          <def>
            pattern ([2,1+8]..[2,1+9])
              Tpat_var "f/80"
            expression ([2,1+10]..[2,1+22]) ghost
              Texp_function
              Nolabel
              [
                <case>
                  pattern ([2,1+10]..[2,1+11])
                    Tpat_var "x/82"
                  expression ([2,1+14]..[2,1+22])
                    Texp_apply
                    expression ([2,1+19]..[2,1+20])
                      Texp_ident "Stdlib!.+"
                    [
                      <arg>
                        Nolabel
                        expression ([2,1+14]..[2,1+18])
                          Texp_constant Const_int 1234
                      <arg>
                        Nolabel
                        expression ([2,1+21]..[2,1+22])
                          Texp_ident "x/82"
                    ]
              ]
        ]
    ] |}];
  let config : Backend.t =
    { prefix_name = "melse"; dump_cmm = false; dump_clambda = true; dump_lambda = true }
  in
  let cmm = Backend.to_cmm config typed_ast in
  [%expect
    {|
    (seq
      (let (f/80 = (function x/82[int] : int (+ 1234 x/82)))
        (setfield_ptr(root-init) 0 (global Melse!) f/80))
    File "_none_", line 1:
    Warning 58: no cmx file was found in path for module Melse, and its interface was not compiled with -opaque

      0a)(seq
           (let
             (f/80 (closure  (fun caml__f_80:int 1  x/82[int] (+ 1234 x/82)) ))
             (setfield_ptr(root-init) 0 (read_symbol camlMelse) f/80))
           0a)
    ("preallocated block" (symbol caml) (exported true) (tag 0) (field_count 1)) |}];
  List.iter cmm ~f:(function
      | Cfunction { fun_name; _ } -> print_s [%message "fundecl" (fun_name : string)]
      | Cdata items ->
        print_s [%message "data"];
        List.iter items ~f:(function
            | Cint value -> print_s [%message "cint" (value : nativeint)]
            | Cdefine_symbol name -> print_s [%message "define_symbol" (name : string)]
            | Cglobal_symbol name -> print_s [%message "global_symbol" (name : string)]
            | Csymbol_address address ->
              print_s [%message "symbol_address" (address : string)]
            | _ -> print_s [%message "other"]));
  [%expect
    {|
    data
    data
    (cint (value 3063))
    (define_symbol (name caml__1))
    (symbol_address (address caml__f_80))
    (cint (value 3))
    data
    (cint (value 1792))
    (global_symbol (name caml))
    (define_symbol (name caml))
    (cint (value 1))
    data
    (global_symbol (name caml__gc_roots))
    (define_symbol (name caml__gc_roots))
    (symbol_address (address caml))
    (cint (value 0))
    (fundecl (fun_name caml__f_80))
    (fundecl (fun_name caml__entry)) |}]
;;
