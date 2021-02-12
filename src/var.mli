open! Core
open! Import

module Kind : sig
  type t =
    | Void
    | Never_returns
    | Machtype of Cmm.machtype_component
  [@@deriving sexp_of]

  val lltype_of_t : ctx:llcontext -> t -> lltype
  val of_machtype : Cmm.machtype_component array -> t
end

type t =
  | Pointer of
      { ptr : Llvm.llvalue
      ; underlying_kind : Kind.t
      ; mutability : [ `Immutable | `Mutable ]
      }
  | Value of
      { value : Llvm.llvalue
      ; kind : Cmm.machtype_component
      }
[@@deriving sexp_of]
