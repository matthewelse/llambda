open! Core
open! Import

type t =
  { value : [ `Stack of llvalue | `Register of llvalue ]
  ; kind : Var.Kind.t
  }
[@@deriving sexp_of]

let raw_type t =
  match t.value with
  | `Stack value -> Llvm.type_of value
  | `Register value -> Llvm.type_of value
;;

let raw_value t =
  match t.value with
  | `Stack value -> value
  | `Register value -> value
;;

module type Context = sig
  val ctx : llcontext
  val builder : llbuilder
  val this_module : llmodule
  val this_function : llvalue
  val env : t String.Table.t
  val catches : llbasicblock Int.Table.t

  (** Lookup a global symbol. *)
  val lookup_symbol : string -> [ `Direct of t | `Indirect of t ] option
end

module type Cmm_to_llvm = sig
  type nonrec t = t =
    { value : [ `Stack of llvalue | `Register of llvalue ]
    ; kind : Var.Kind.t
    }
  [@@deriving sexp_of]

  module With_context (S : Context) : sig
    val compile_expression : Cmm.expression -> t
    val promote_value_if_necessary_exn : ?msg:Sexp.t -> new_machtype:Var.Kind.t -> t -> t
    val llvm_value : t -> llvalue
  end
end
