open! Core
open! Import

type t =
  | Pointer of
      { ptr : Ir_value.t
      ; underlying_kind : [ `Void | `No_return | `Some of Cmm.machtype_component ]
      ; mutability : [ `Mutable | `Immutable ]
      }
  | Value of
      { value : Ir_value.t
      ; kind : Cmm.machtype_component
      }
[@@deriving sexp_of]
