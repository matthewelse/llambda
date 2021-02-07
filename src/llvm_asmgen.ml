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

let liveness phrase =
  Liveness.fundecl phrase;
  phrase
;;

let dump_if ppf flag message phrase = if !flag then Printmach.phase message ppf phrase

let pass_dump_if ppf flag message phrase =
  dump_if ppf flag message phrase;
  phrase
;;

let pass_dump_linear_if ppf flag message phrase =
  if !flag then fprintf ppf "*** %s@.%a@." message Printlinear.fundecl phrase;
  phrase
;;

let should_emit () = not (should_stop_after Compiler_pass.Scheduling)
let if_emit_do f x = if should_emit () then f x else ()
let emit_begin_assembly = if_emit_do Emit.begin_assembly
let emit_end_assembly = if_emit_do Emit.end_assembly
let emit_data = if_emit_do Emit.data
let emit_fundecl = if_emit_do (Profile.record ~accumulate:true "emit" Emit.fundecl)
let ( ++ ) x f = f x

(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns f =
  List.filter_map
    (function Cfunction { fun_name = name } as ph when f name -> Some ph | _ -> None)
    (Cmm_helpers.generic_functions true [ Compilenv.current_unit_infos () ])
;;

let assemble_llvm_ir infile outfile =
  print_endline "calling llc";
  Ccomp.command
    ("/usr/local/opt/llvm@10/bin/clang -c"
    ^ " "
    ^ String.concat " " (Misc.debug_prefix_map_flags ())
    ^ " -o "
    ^ Filename.quote outfile
    ^ " "
    ^ Filename.quote infile)
;;

let compile_unit asm_filename keep_asm obj_filename gen =
  let create_asm =
    should_emit () && (keep_asm || not !Emitaux.binary_backend_available)
  in
  Emitaux.create_asm_file := create_asm;
  Misc.try_finally
    ~exceptionally:(fun () -> remove_file obj_filename)
    (fun () ->
      if create_asm then Emitaux.output_channel := open_out asm_filename;
      Misc.try_finally
        gen
        ~always:(fun () -> if create_asm then close_out !Emitaux.output_channel)
        ~exceptionally:(fun () ->
          if create_asm && not keep_asm then remove_file asm_filename);
      if should_emit ()
      then (
        let assemble_result =
          Profile.record "assemble" (assemble_llvm_ir asm_filename) obj_filename
        in
        if assemble_result <> 0 then raise (Error (Assembler_error asm_filename)));
      if create_asm && not keep_asm then remove_file asm_filename)
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

type middle_end =
  backend:(module Backend_intf.S)
  -> filename:string
  -> prefixname:string
  -> ppf_dump:Format.formatter
  -> Lambda.program
  -> Clambda.with_constants

let compile_implementation
    ?toplevel
    ~backend
    ~filename
    ~prefixname
    ~middle_end
    ~ppf_dump
    (program : Lambda.program)
  =
  let asmfile =
    if !keep_asm_file || !Emitaux.binary_backend_available
    then prefixname ^ ".ll"
    else Filename.temp_file "camlasm" ".ll"
  in
  let ctx = Llvm.global_context () in
  Wrap_llvm.Ir_module.with_module
    ~target_triple:"x86_64-apple-macosx10.15.0"
    ~ctx
    prefixname
    (fun impl_module ->
      compile_unit asmfile !keep_asm_file (prefixname ^ ext_obj) (fun () ->
          Ident.Set.iter Compilenv.require_global program.required_globals;
          let clambda_with_constants =
            middle_end ~backend ~filename ~prefixname ~ppf_dump program
          in
          end_gen_implementation
            ?toplevel
            ~this_module:impl_module
            ~ctx
            clambda_with_constants;
          Emitaux.emit_string (Llvm.string_of_llmodule impl_module)))
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
