open! Core
open! Import

val ocaml_calling_convention : int

(** We treat OCaml values as void pointers, or in LLVM: i8*. *)
val value_type : llcontext -> lltype

(** Basically just an i8 pointer. *)
val void_pointer_type : llcontext -> lltype

(** LLVM types for C-- types *)
val type_of_machtype : llcontext -> Cmm.machtype -> lltype

val type_of_machtype_component : llcontext -> Cmm.machtype_component -> lltype
val type_of_operation : llcontext -> Cmm.operation -> lltype
val type_of_memory_chunk : llcontext -> Cmm.memory_chunk -> lltype
