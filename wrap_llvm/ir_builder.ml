open Llvm

type t = llbuilder

let create ctx = builder ctx
