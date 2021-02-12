open! Core
open! Import

val emit : ctx:llcontext -> this_module:llmodule -> Cmm.phrase list -> unit
