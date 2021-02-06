open Core
open Llvm

type t = llmodule

let with_module ?target_triple ?data_layout ~ctx name f =
  let t = create_module ctx name in
  Option.iter target_triple ~f:(fun name -> set_target_triple name t);
  Option.iter data_layout ~f:(fun layout -> set_data_layout layout t);
  Exn.protectx t ~f ~finally:dispose_module
;;

let sexp_of_t t = string_of_llmodule t |> String.sexp_of_t
let declare_global t ~name type_ = declare_global type_ name t
let define_global t ~name value = define_global name value t
let define_global' t ~name value = define_global t ~name value |> (ignore : llvalue -> unit)
let lookup_global t ~name = lookup_global name t
let declare_function t ~name ~funtype = declare_function name funtype t

let declare_function' t ~name ~funtype =
  declare_function t ~name ~funtype |> (ignore : llvalue -> unit)
;;

let lookup_function t ~name = lookup_function name t

let lookup_function_exn t ~name =
  lookup_function t ~name
  |> Option.value_exn ~message:[%string "Unknown function %{name}"]
;;
