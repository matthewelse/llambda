open Llvm

type t = llbuilder

val create : Ir_context.t -> t
