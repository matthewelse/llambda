open! Core
open! Import

type t =
  | Pointer of
      { ptr : llvalue
      ; underlying_kind : [ `Void | `No_return | `Some of Cmm.machtype_component ]
      ; mutability : [ `Mutable | `Immutable ]
      }
  | Value of
      { value : llvalue
      ; kind : Cmm.machtype_component
      }
[@@deriving sexp_of]
