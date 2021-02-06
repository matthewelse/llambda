open! Core
open! Wrapllvm

val value_type : Ir_context.t -> Ir_type.t
val void_pointer_type : Ir_context.t -> Ir_type.t
val type_of_machtype : Ir_context.t -> Cmm.machtype -> Ir_type.t
val type_of_operation : Ir_context.t -> Cmm.operation -> Ir_type.t
val type_of_memory_chunk : Ir_context.t -> Cmm.memory_chunk -> Ir_type.t
val builtin_functions : Ir_context.t -> (string * Ir_type.t) list
