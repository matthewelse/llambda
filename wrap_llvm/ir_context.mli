open! Core
open! Llvm

type t = llcontext [@@deriving sexp_of]

val global : unit -> t
