open! Core

type _ t [@@deriving sexp_of]

val to_llvm : 'a t -> Llvm.lltype
val unsafe_of_llvm : Llvm.lltype -> 'a t
val void_type : Llvm.llcontext -> unit t

module Int_size : sig
  type _ t =
    | I1 : [ `i1 ] t
    | I8 : [ `i8 ] t
    | I16 : [ `i16 ] t
    | I32 : [ `i32 ] t
    | I64 : [ `i64 ] t
  [@@deriving sexp_of]
end

type _ int

val int_type : Llvm.llcontext -> 'a Int_size.t -> 'a int t

module Float_size : sig
  type _ t =
    | Single : [ `f32 ] t
    | Double : [ `f64 ] t
    | X86fp80 : [ `f80 ] t
    | Quad : [ `f128 ] t
  [@@deriving sexp_of]
end

type _ float

val float_type : Llvm.llcontext -> 'a Float_size.t -> 'a float t

type _ pointer

val pointer_type : 'a t -> 'a pointer t

module Func : sig
  type _ typ
  type ('return, 'args) t [@@deriving sexp_of]

  val returns : 'a typ -> ('a, 'a) t
  val ( @-> ) : 'a typ -> ('return, 'args) t -> ('return, 'a -> 'args) t
  val to_llvm : _ t -> Llvm.lltype
end
with type 'a typ := 'a t

type _ md

(** [mdnode] should probably actually be a heterogeneous list of nodes, like a
  function.*)
val mdnode : Llvm.llcontext -> 'a md t -> 'a list md t

val mdstring : Llvm.llcontext -> string md t
