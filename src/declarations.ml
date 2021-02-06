open Core
open Llvm
open Wrapllvm

let value_type ctx = pointer_type (i8_type ctx)
let void_pointer_type ctx = pointer_type (i8_type ctx)

let type_of_machtype_component ctx (component : Cmm.machtype_component) =
  match component with
  | Val | Addr -> pointer_type (i8_type ctx)
  | Int -> i64_type ctx
  | Float -> float_type ctx
;;

let type_of_machtype ctx (machtype : Cmm.machtype) =
  match machtype with
  | [||] -> void_type ctx
  | [| component |] -> type_of_machtype_component ctx component
  | elems -> packed_struct_type ctx (Array.map elems ~f:(type_of_machtype_component ctx))
;;

let builtin_functions ctx =
  [ ( "caml_alloc"
    , Ir_type.function_type
        ~arg_types:[ i64_type ctx; i32_type ctx ]
        ~return_type:(void_pointer_type ctx) )
  ; ( "llvm.gcroot"
    , Ir_type.function_type
        ~arg_types:[ pointer_type (void_pointer_type ctx); void_pointer_type ctx ]
        ~return_type:(void_type ctx) )
  ]
;;
