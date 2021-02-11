(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Clflags
open Compenv

module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'
  let closure_symbol = Compilenv.closure_symbol
  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol
  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
  ;;
end

let backend = (module Backend : Backend_intf.S)
let usage = "Usage: ocamlopt <options> <files>\nOptions are:"

module Options = Main_args.Make_optcomp_options (Main_args.Default.Optmain)

module Optcompile = struct
  include Optcompile
  open Misc
  open Compile_common

  let tool_name = "llambda"
  let with_info = Compile_common.with_info ~native:true ~tool_name

  let clambda ~llvm_flags i backend typed =
    Clflags.use_inlining_arguments_set Clflags.classic_arguments;
    typed
    |> Profile.(record transl) (Translmod.transl_store_implementation i.module_name)
    |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
    |> Profile.(record generate) (fun program ->
           let code = Simplif.simplify_lambda program.Lambda.code in
           { program with Lambda.code }
           |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
           |> Llvm_asmgen.compile_implementation
                ~llvm_flags
                ~backend
                ~filename:i.source_file
                ~prefixname:i.output_prefix
                ~middle_end:Closure_middle_end.lambda_to_clambda
                ~ppf_dump:i.ppf_dump;
           Compilenv.save_unit_info (cmx i))
  ;;

  let flambda ~llvm_flags i backend typed =
    Clflags.use_inlining_arguments_set Clflags.classic_arguments;
    typed
    |> Profile.(record transl) (Translmod.transl_implementation_flambda i.module_name)
    |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
    |> Profile.(record generate) (fun program ->
           let code = Simplif.simplify_lambda program.Lambda.code in
           { program with Lambda.code }
           |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
           |> Llvm_asmgen.compile_implementation
                ~llvm_flags
                ~backend
                ~filename:i.source_file
                ~prefixname:i.output_prefix
                ~middle_end:Flambda_middle_end.lambda_to_clambda
                ~ppf_dump:i.ppf_dump;
           Compilenv.save_unit_info (cmx i))
  ;;

  let implementation
      ~llvm_flags
      ~use_llvm
      ~(backend : (module Backend_intf.S))
      ~source_file
      ~output_prefix
    =
    if not use_llvm
    then Optcompile.implementation ~backend ~source_file ~output_prefix
    else (
      let backend info typed =
        Compilenv.reset ?packname:!Clflags.for_package info.module_name;
        if Config.flambda
        then flambda ~llvm_flags info backend typed
        else clambda ~llvm_flags info backend typed
      in
      with_info ~source_file ~output_prefix ~dump_ext:"cmx"
      @@ fun info -> Compile_common.implementation info ~backend)
  ;;
end

let main ~use_llvm () =
  native_code := true;
  let ppf = Format.err_formatter in
  let llvm_args = ref "" in
  try
    readenv ppf Before_args;
    Clflags.add_arguments __LOC__ (Arch.command_line_options @ Options.list);
    Clflags.add_arguments
      __LOC__
      [ ( "-depend"
        , Arg.Unit Makedepend.main_from_option
        , "<options> Compute dependencies (use 'ocamlopt -depend -help' for details)" )
      ];
    Clflags.add_arguments
      __LOC__
      [ "-llvm-flags", Arg.Set_string llvm_args, "<flags> Flags to pass to LLVM." ];
    Clflags.parse_arguments anonymous usage;
    Compmisc.read_clflags_from_env ();
    if !Clflags.plugin then fatal "-plugin is only supported up to OCaml 4.08.0";
    (try
       Compenv.process_deferred_actions
         ( ppf
         , Optcompile.implementation ~llvm_flags:!llvm_args ~use_llvm ~backend
         , Optcompile.interface
         , ".cmx"
         , ".cmxa" )
     with
    | Arg.Bad msg ->
      prerr_endline msg;
      Clflags.print_arguments usage;
      exit 2);
    readenv ppf Before_link;
    (if List.length
          (List.filter
             (fun x -> !x)
             [ make_package; make_archive; shared; stop_early; output_c_object ])
        > 1
    then
      let module P = Clflags.Compiler_pass in
      match !stop_after with
      | None -> fatal "Please specify at most one of -pack, -a, -shared, -c, -output-obj"
      | Some ((P.Parsing | P.Typing | P.Scheduling) as p) ->
        assert (P.is_compilation_pass p);
        Printf.ksprintf
          fatal
          "Options -i and -stop-after (%s) are  incompatible with -pack, -a, -shared, \
           -output-obj"
          (String.concat "|" (Clflags.Compiler_pass.available_pass_names ~native:true)));
    if !make_archive
    then (
      Compmisc.init_path ();
      let target = extract_output !output_name in
      Asmlibrarian.create_archive (get_objfiles ~with_ocamlparam:false) target;
      Warnings.check_fatal ())
    else if !make_package
    then (
      Compmisc.init_path ();
      let target = extract_output !output_name in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          Asmpackager.package_files
            ~ppf_dump
            (Compmisc.initial_env ())
            (get_objfiles ~with_ocamlparam:false)
            target
            ~backend);
      Warnings.check_fatal ())
    else if !shared
    then (
      Compmisc.init_path ();
      let target = extract_output !output_name in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          Asmlink.link_shared ~ppf_dump (get_objfiles ~with_ocamlparam:false) target);
      Warnings.check_fatal ())
    else if (not !stop_early) && !objfiles <> []
    then (
      let target =
        if !output_c_object
        then (
          let s = extract_output !output_name in
          if Filename.check_suffix s Config.ext_obj
             || Filename.check_suffix s Config.ext_dll
          then s
          else
            fatal
              (Printf.sprintf
                 "The extension of the output file must be %s or %s"
                 Config.ext_obj
                 Config.ext_dll))
        else default_output !output_name
      in
      Compmisc.init_path ();
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          Asmlink.link ~ppf_dump (get_objfiles ~with_ocamlparam:true) target);
      Warnings.check_fatal ())
  with
  | x ->
    Location.report_exception ppf x;
    exit 2
;;
