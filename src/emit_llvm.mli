open! Core
module Cmm = Compiler_wrappers.Wrap_cmm

val emit : Cmm.phrase list -> unit
