type builder = Llvm.llbuilder
type _ t

val to_llvm : _ t -> Llvm.llvalue
val unsafe_of_llvm : Llvm.llvalue -> _ t

module Args : sig
  type 'a value
  type ('return, 'args) t

  val nil : ('a, 'a) t
  val ( @: ) : 'a value -> ('return, 'largs) t -> ('return, 'a -> 'largs) t
end
with type 'a value := 'a t

type ('return, 'args) fn

val fn_to_llvm : _ fn -> Llvm.llvalue
val unsafe_fn_of_llvm : Llvm.llvalue -> _ fn
val const_int : Llvm.llcontext -> 'size Ltype.Int_size.t -> int -> 'size Ltype.int t

(** [signed] defaults to true. *)
val const_int64
  :  ?signed:bool
  -> Llvm.llcontext
  -> 'size Ltype.Int_size.t
  -> int64
  -> 'size Ltype.int t

val const_float
  :  Llvm.llcontext
  -> 'size Ltype.Float_size.t
  -> float
  -> 'size Ltype.float t

(** TODO: Maybe this should just take an integer literal. *)
val const_inttoptr
  :  ptr_type:'kind Ltype.pointer Ltype.t
  -> [ `i64 ] Ltype.int t
  -> 'kind Ltype.pointer t

val const_pointer_null
  : 'kind Ltype.pointer Ltype.t
  -> 'kind Ltype.pointer t

val build_inttoptr
  :  ptr_type:'kind Ltype.pointer Ltype.t
  -> name:string
  -> builder:builder
  -> [ `i64 ] Ltype.int t
  -> 'kind Ltype.pointer t

val build_ptrtoint
  :  name:string
  -> builder:builder
  -> _ Ltype.pointer t
  -> [ `i64 ] Ltype.int t

val build_pointercast
  :  new_type:'kind Ltype.pointer Ltype.t
  -> name:string
  -> builder:builder
  -> _ Ltype.pointer t
  -> 'kind Ltype.pointer t

val build_in_bounds_gep
  :  offsets:_ Ltype.int t list
  -> name:string
  -> builder:builder
  -> 'a Ltype.pointer t
  -> 'a Ltype.pointer t

val build_gep
  :  offsets:_ Ltype.int t list
  -> name:string
  -> builder:builder
  -> 'a Ltype.pointer t
  -> 'a Ltype.pointer t

val declare_function
  :  name:string
  -> typ:('return, 'args) Ltype.Func.t
  -> module_:Llvm.llmodule
  -> ('return, 'args) fn

val build_call
  :  ('return, 'args) fn
  -> args:('return, 'args) Args.t
  -> name:string
  -> builder:builder
  -> 'result t

val build_callbr
  :  ('return, 'args) fn
  -> fallthrough:Llvm.llbasicblock
  -> args:('return, 'args) Args.t
  -> targets:Llvm.llbasicblock list
  -> name:string
  -> builder:builder
  -> 'result t

module Int_binop : sig
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Xor
    | Lsl
    | Lsr
    | Asr
  [@@deriving sexp_of]
end

val build_int_binop
  :  name:string
  -> builder:builder
  -> op:Int_binop.t
  -> 'size Ltype.int t
  -> 'size Ltype.int t
  -> 'size Ltype.int t

module Float_binop : sig
  type t =
    | Add
    | Sub
    | Mul
    | Div
  [@@deriving sexp_of]
end

val build_float_binop
  :  name:string
  -> builder:builder
  -> op:Float_binop.t
  -> 'size Ltype.float t
  -> 'size Ltype.float t
  -> 'size Ltype.float t

val build_fneg
  :  name:string
  -> builder:builder
  -> 'size Ltype.float t
  -> 'size Ltype.float t

val const_inline_asm
  :  typ:('return, 'args) Ltype.Func.t
  -> assembly:string
  -> constraints:string
  -> has_side_effects:bool
  -> should_align_stack:bool
  -> ('return, 'args) fn

val build_alloca : name:string -> builder:builder -> 'a Ltype.t -> 'a Ltype.pointer t
val build_load : name:string -> builder:builder -> 'a Ltype.pointer t -> 'a t
val build_store : builder:builder -> dst:'a Ltype.pointer t -> 'a t -> unit t
val mdnode : ctx:Llvm.llcontext -> string Ltype.md t list -> string list Ltype.md t
val mdstring : ctx:Llvm.llcontext -> string -> string Ltype.md t

(* TODO: maybe have a specific phantom type for bb pointers? *)
val block_address : func:_ fn -> Llvm.llbasicblock -> [ `i8 ] Ltype.int Ltype.pointer t
