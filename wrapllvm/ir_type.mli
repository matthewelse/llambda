open! Core
open! Llvm

type t = lltype [@@deriving sexp_of]

val function_type : arg_types:t list -> return_type:t -> t
