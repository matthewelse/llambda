open Core

type t = Llvm.llbasicblock

let sexp_of_t t = Llvm.value_name (Llvm.value_of_block t) |> String.sexp_of_t
