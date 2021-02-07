open Core

type t = Llvm.llvalue

let sexp_of_t t = Llvm.string_of_llvalue t |> String.sexp_of_t
