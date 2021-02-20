open! Core
open! Import

module Kind = struct
  type t =
    | Void
    | Never_returns
    | Machtype of Cmm.machtype_component
  [@@deriving sexp_of]

  let lltype_of_t ~ctx (kind : t) =
    match kind with
    | Void -> void_type ctx
    | Machtype Int -> i64_type ctx
    | Machtype Val | Machtype Addr -> pointer_type (i8_type ctx)
    | Machtype Float -> double_type ctx
    | Never_returns -> void_type ctx
  ;;

  let of_machtype = function
    | [||] -> Void
    | [| x |] -> Machtype x
    | machtype ->
      raise_s
        [%message
          "Machtype with more than one element." (machtype : Cmm.machtype_component array)]
  ;;
end

type t =
  | Pointer of
      { ptr : llvalue
      ; underlying_kind : Kind.t
      ; mutability : [ `Mutable | `Immutable ]
      }
  | Value of
      { value : llvalue
      ; kind : Cmm.machtype_component
      }
[@@deriving sexp_of]
