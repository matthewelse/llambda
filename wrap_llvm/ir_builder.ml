open Llvm

type t = llbuilder

let create ctx = builder ctx
let sexp_of_t _ = [%sexp "<llvm builder>"]
