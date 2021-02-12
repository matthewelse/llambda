open! Core
open! Import

type t =
  { value : llvalue
  ; kind : Var.Kind.t
  }
[@@deriving sexp_of]

module type Context = sig
  val ctx : llcontext
  val builder : llbuilder
  val this_module : llmodule
  val this_function : llvalue
  val env : Var.t String.Table.t
  val catches : llbasicblock Int.Table.t

  (** Lookup a global symbol. *)
  val lookup_symbol : string -> [ `Direct of t | `Indirect of t ] option
end

module type Cmm_to_llvm = sig
  type nonrec t = t =
    { value : llvalue
    ; kind : Var.Kind.t
    }
  [@@deriving sexp_of]

  module With_context (S : Context) : sig
    val compile_expression : Cmm.expression -> t
    val promote_value_if_necessary_exn : new_machtype:Var.Kind.t -> t -> t
  end
end
