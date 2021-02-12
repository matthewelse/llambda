open! Core
open! Import

val compile_implementation
  :  ?toplevel:(string -> bool)
  -> llvm_flags:string
  -> backend:'a
  -> filename:'b
  -> prefixname:string
  -> middle_end:
       (backend:'a
        -> filename:'b
        -> prefixname:string
        -> ppf_dump:'c
        -> Ocaml_common.Lambda.program
        -> Ocaml_optcomp.Clambda.ulambda
           * Ocaml_optcomp.Clambda.preallocated_block list
           * Ocaml_optcomp.Clambda.preallocated_constant list)
  -> ppf_dump:'c
  -> Ocaml_common.Lambda.program
  -> unit
