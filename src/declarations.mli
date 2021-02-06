open Llvm
open Wrapllvm

val value_type : Ir_context.t -> lltype
val void_pointer_type : Ir_context.t -> lltype
val type_of_machtype : Ir_context.t -> Cmm.machtype -> lltype
val builtin_functions : Ir_context.t -> (string * lltype) list
