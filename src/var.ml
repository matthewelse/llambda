open! Core
open! Import

type t =
  | Pointer of
      { ptr : Ir_value.t
      ; underlying_kind : Cmm.machtype_component option
      ; mutability : [ `Mutable | `Immutable ]
      }
  | Value of
      { value : Ir_value.t
      ; kind : Cmm.machtype_component
      }
[@@deriving sexp_of]
