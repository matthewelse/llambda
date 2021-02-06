open! Core
open! Wrap_llvm

(** For the time being, we'll use GHC's calling convention. *)
val ghc_calling_convention : int

(** We treat OCaml values as void pointers, or in LLVM: i8*. *)
val value_type : Ir_context.t -> Ir_type.t

(** Basically just an i8 pointer. *)
val void_pointer_type : Ir_context.t -> Ir_type.t

(** LLVM types for C-- types *)
val type_of_machtype : Ir_context.t -> Cmm.machtype -> Ir_type.t

val type_of_operation : Ir_context.t -> Cmm.operation -> Ir_type.t
val type_of_memory_chunk : Ir_context.t -> Cmm.memory_chunk -> Ir_type.t

(*_ *)
val builtin_functions : Ir_context.t -> (string * Ir_type.t) list
