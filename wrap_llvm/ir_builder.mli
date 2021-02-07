open Llvm

type t = llbuilder [@@deriving sexp_of]

val create : Ir_context.t -> t
