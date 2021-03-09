open! Core

type _ t = Llvm.lltype

let sexp_of_t _ t = Llvm.string_of_lltype t |> String.sexp_of_t
let to_llvm t = t
let unsafe_of_llvm t = t
let void_type ctx = Llvm.void_type ctx

type _ int

module Int_size = struct
  type _ t =
    | I1 : [ `i1 ] t
    | I8 : [ `i8 ] t
    | I16 : [ `i16 ] t
    | I32 : [ `i32 ] t
    | I64 : [ `i64 ] t
  [@@deriving sexp_of]
end

let int_type (type size) ctx (size : size Int_size.t) : size int t =
  match size with
  | I1 -> Llvm.i1_type ctx
  | I8 -> Llvm.i8_type ctx
  | I16 -> Llvm.i16_type ctx
  | I32 -> Llvm.i32_type ctx
  | I64 -> Llvm.i64_type ctx
;;

module Float_size = struct
  type _ t =
    | Single : [ `f32 ] t
    | Double : [ `f64 ] t
    | X86fp80 : [ `f80 ] t
    | Quad : [ `f128 ] t
  [@@deriving sexp_of]
end

type _ float

let float_type (type size) ctx (size : size Float_size.t) : size float t =
  match size with
  | Single -> Llvm.float_type ctx
  | Double -> Llvm.double_type ctx
  | X86fp80 -> Llvm.x86fp80_type ctx
  | Quad -> Llvm.fp128_type ctx
;;

type _ pointer

let pointer_type (und : 'und t) : 'und pointer t = Llvm.pointer_type und

module Func = struct
  type 'a outer = 'a t [@@deriving sexp_of]

  type ('return, 'args) t =
    | Cons : ('a outer * ('return, 'b) t) -> ('return, 'a -> 'b) t
    | Returns : 'return outer -> ('return, 'return) t

  let returns x = Returns x
  let ( @-> ) l r = Cons (l, r)

  let rec to_list : type args return. (return, args) t -> Llvm.lltype list =
   fun args ->
    match args with
    | Cons (l, r) -> l :: to_list r
    | Returns typ -> [ typ ]
 ;;

  let rec to_arg_list : type args return. (return, args) t -> Llvm.lltype list =
   fun args ->
    match args with
    | Cons (l, r) -> l :: to_arg_list r
    | Returns _ -> []
 ;;

  let rec return_type : type args return. (return, args) t -> Llvm.lltype =
   fun args ->
    match args with
    | Cons (_, r) -> return_type r
    | Returns r -> r
 ;;

  let to_llvm t = Llvm.function_type (return_type t) (to_arg_list t |> Array.of_list)

  let sexp_of_t _ _ t =
    to_list t
    |> List.map ~f:Llvm.string_of_lltype
    |> String.concat ~sep:" -> "
    |> [%sexp_of: string]
  ;;
end

type _ md

let mdnode ctx _ = Llvm.mdnode ctx [||] |> Llvm.type_of
let mdstring ctx = Llvm.mdstring ctx "" |> Llvm.type_of
