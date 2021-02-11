open! Core
open! Import

val ocaml_calling_convention : int

(** We treat OCaml values as void pointers, or in LLVM: i8*. *)
val value_type : Ir_context.t -> Ir_type.t

(** Basically just an i8 pointer. *)
val void_pointer_type : Ir_context.t -> Ir_type.t

(** LLVM types for C-- types *)
val type_of_machtype : Ir_context.t -> Cmm.machtype -> Ir_type.t

val type_of_machtype_component : Ir_context.t -> Cmm.machtype_component -> Ir_type.t
val type_of_operation : Ir_context.t -> Cmm.operation -> Ir_type.t
val type_of_memory_chunk : Ir_context.t -> Cmm.memory_chunk -> Ir_type.t
