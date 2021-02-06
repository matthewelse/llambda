open! Core
open! Llvm

type t = llmodule [@@deriving sexp_of]

(** Creates an LLVM module with the provided config, then disposes of the module
  once the provided function returns. *)
val with_module
  :  ?target_triple:string
  -> ?data_layout:string
  -> ctx:llcontext
  -> string
  -> (t -> 'a)
  -> 'a

val declare_global : t -> name:string -> lltype -> llvalue
val define_global : t -> name:string -> llvalue -> llvalue
val define_global' : t -> name:string -> llvalue -> unit
val lookup_global : t -> name:string -> llvalue option
val declare_function : t -> name:string -> funtype:Ir_type.t -> llvalue
val declare_function' : t -> name:string -> funtype:Ir_type.t -> unit
val lookup_function : t -> name:string -> llvalue option
val lookup_function_exn : t -> name:string -> llvalue
