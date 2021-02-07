open Llvm

type t = llcontext

let sexp_of_t _ = [%sexp "<llvm context>"]
let global () = global_context ()
