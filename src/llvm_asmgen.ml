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

(* From lambda to assembly code *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Format
open Config
open Clflags
open Misc
open Cmm

type error = Assembler_error of string

exception Error of error

let should_emit () = not (should_stop_after Compiler_pass.Scheduling)
let ( ++ ) x f = f x

(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns f =
  List.filter_map
    (function Cfunction { fun_name = name } as ph when f name -> Some ph | _ -> None)
    (Cmm_helpers.generic_functions true [ Compilenv.current_unit_infos () ])
;;

let assemble_llvm_ir ~llvm_flags ~ir_filename ~obj_filename () =
  let result =
    Ccomp.command
      ("~/development/llambda/external/llvm/llvm-project/build/bin/opt -S -mem2reg "
      ^ llvm_flags
      ^ " -o "
      ^ Filename.quote ir_filename
      ^ " "
      ^ Filename.quote ir_filename)
  in
  if result = 0
  then
    Ccomp.command
      ("~/development/llambda/external/llvm/llvm-project/build/bin/llc -filetype obj "
      ^ llvm_flags
      ^ " "
      ^ String.concat " " (Misc.debug_prefix_map_flags ())
      ^ " -o "
      ^ Filename.quote obj_filename
      ^ " "
      ^ ir_filename (* urgh LLVM uses the filename as the module name :( *))
  else result
;;

let compile_unit ~llvm_flags ir_filename keep_asm obj_filename gen =
  let create_asm =
    should_emit () && (keep_asm || not !Emitaux.binary_backend_available)
  in
  Emitaux.create_asm_file := create_asm;
  Misc.try_finally
    ~exceptionally:(fun () -> remove_file obj_filename)
    (fun () ->
      if create_asm then Emitaux.output_channel := open_out ir_filename;
      Misc.try_finally
        gen
        ~always:(fun () -> if create_asm then close_out !Emitaux.output_channel)
        ~exceptionally:(fun () ->
          if create_asm && not keep_asm then remove_file ir_filename);
      if should_emit ()
      then (
        let assemble_result =
          Profile.record
            "assemble"
            (assemble_llvm_ir ~llvm_flags ~ir_filename ~obj_filename)
            ()
        in
        if assemble_result <> 0 then raise (Error (Assembler_error ir_filename)));
      if create_asm && not keep_asm then remove_file ir_filename)
;;

let end_gen_implementation ?toplevel ~ctx ~this_module (clambda : Clambda.with_constants) =
  clambda
  ++ Profile.record "cmm" Cmmgen.compunit
  ++ Profile.record "compile_phrases" (fun unit_phrases ->
         let phrases =
           (* We add explicit references to external primitive symbols.  This
              is to ensure that the object files that define these symbols,
              when part of a C library, won't be discarded by the linker.
              This is important if a module that uses such a symbol is later
              dynlinked. *)
           (Cmm_helpers.reference_symbols
              (List.filter_map
                 (fun prim ->
                   if not (Primitive.native_name_is_external prim)
                   then None
                   else Some (Primitive.native_name prim))
                 !Translmod.primitive_declarations)
           :: (match toplevel with None -> [] | Some f -> compile_genfuns f))
           @ unit_phrases
         in
         Emit_llvm.emit ~ctx ~this_module phrases)
;;

let ext_ll = ".ll"

let compile_implementation
    ?toplevel
    ~llvm_flags
    ~backend
    ~filename
    ~prefixname
    ~middle_end
    ~ppf_dump
    (program : Lambda.program)
  =
  let irfile =
    if !keep_asm_file || !Emitaux.binary_backend_available
    then prefixname ^ ext_ll
    else Filename.temp_file "camlasm" ext_ll
  in
  let ctx = Llvm.global_context () in
  let impl_module = Llvm.create_module ctx (Ident.name program.module_ident) in
  let target_triple =
    (* FIXME melse: Do something more principled here. e.g. call llvm-config --host-triple *)
    match Ocaml_common.Config.system with
    | "macosx" -> "x86_64-apple-macosx10.15.0"
    | "linux" -> "x86_64-unknown-linux-gnu"
    | _ -> Core.raise_s [%message "Unsupported system type. Maybe just add an extra target triple."]
  in
  Llvm.set_target_triple target_triple impl_module;
  compile_unit ~llvm_flags irfile !keep_asm_file (prefixname ^ ext_obj) (fun () ->
      Ident.Set.iter Compilenv.require_global program.required_globals;
      let clambda_with_constants =
        middle_end ~backend ~filename ~prefixname ~ppf_dump program
      in
      Llvm.add_named_metadata_operand
        impl_module
        "ocaml.module_name"
        (Llvm.mdnode ctx [| Llvm.mdstring ctx (Ident.name program.module_ident) |]);
      end_gen_implementation
        ?toplevel
        ~this_module:impl_module
        ~ctx
        clambda_with_constants;
      Emitaux.emit_string (Llvm.string_of_llmodule impl_module));
  Llvm.dispose_module impl_module
;;

(* Error report *)

let report_error ppf = function
  | Assembler_error file ->
    fprintf ppf "Assembler error, input left in file %a" Location.print_filename file
;;

let () =
  Location.register_error_of_exn (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None)
;;
