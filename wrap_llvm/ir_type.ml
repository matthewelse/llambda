open Core
open Llvm

type t = lltype

let sexp_of_t t = string_of_lltype t |> String.sexp_of_t

let function_type ~arg_types ~return_type =
  function_type return_type (Array.of_list arg_types)
;;
