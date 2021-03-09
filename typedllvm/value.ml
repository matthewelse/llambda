open Core

type _ t = Llvm.llvalue

let to_llvm t = t
let unsafe_of_llvm t = t

type builder = Llvm.llbuilder

module Args = struct
  type 'a value = 'a t

  type ('return, 'args) t =
    | Cons : 'a value * ('return, 'b) t -> ('return, 'a -> 'b) t
    | Nil : ('return, 'return) t

  let nil = Nil
  let ( @: ) left right = Cons (left, right)

  let rec to_arg_list : type args return. (return, args) t -> Llvm.llvalue list =
   fun args -> match args with Cons (l, r) -> to_llvm l :: to_arg_list r | Nil -> []
 ;;
end

type ('return, 'args) fn = Llvm.llvalue

let fn_to_llvm t = t
let unsafe_fn_of_llvm t = t

let const_int ctx (size : 'size Ltype.Int_size.t) n : 'size Ltype.int t =
  Llvm.const_int (Ltype.int_type ctx size |> Ltype.to_llvm) n
;;

let const_int64 ?(signed = true) ctx (size : 'size Ltype.Int_size.t) n : 'size Ltype.int t
  =
  Llvm.const_of_int64 (Ltype.int_type ctx size |> Ltype.to_llvm) n signed
;;

let const_float ctx (size : 'size Ltype.Float_size.t) f : 'size Ltype.float t =
  Llvm.const_float (Ltype.float_type ctx size |> Ltype.to_llvm) f
;;

let const_inttoptr ~ptr_type t = Llvm.const_inttoptr t (Ltype.to_llvm ptr_type)

let const_inline_asm ~typ ~assembly ~constraints ~has_side_effects ~should_align_stack =
  Llvm.const_inline_asm
    (Ltype.Func.to_llvm typ)
    ~assembly
    ~constraints
    ~has_side_effects
    ~should_align_stack
;;

let declare_function ~name ~typ:ftype ~module_:this_module =
  Llvm.declare_function name (Ltype.Func.to_llvm ftype) this_module
;;

let build_call fn ~args ~name ~builder =
  Llvm.build_call fn (Args.to_arg_list args |> Array.of_list) name builder
;;

let build_callbr fn ~fallthrough ~args ~targets ~name ~builder =
  Llvm.build_callbr
    fn
    fallthrough
    (Array.of_list (Args.to_arg_list args))
    (Array.of_list targets)
    name
    builder
;;

let build_inttoptr ~ptr_type ~name ~builder t =
  Llvm.build_inttoptr t (Ltype.to_llvm ptr_type) name builder
;;

let build_ptrtoint ~name ~builder t =
  let ctx = Llvm.type_context (Llvm.type_of t) in
  Llvm.build_ptrtoint t (Ltype.to_llvm (Ltype.int_type ctx I64)) name builder
;;

let build_pointercast ~new_type ~name ~builder t =
  Llvm.build_pointercast t (Ltype.to_llvm new_type) name builder
;;

let build_in_bounds_gep ~offsets ~name ~builder t =
  Llvm.build_in_bounds_gep t (Array.of_list offsets) name builder
;;

let build_load ~name ~builder ptr = Llvm.build_load ptr name builder
let build_store ~builder ~dst t = Llvm.build_store t dst builder

module Int_binop = struct
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

let build_int_binop ~name ~builder ~(op : Int_binop.t) l r =
  let build_op =
    let open Llvm in
    match op with
    | Add -> build_add
    | Sub -> build_sub
    | Mul -> build_mul
    | Div -> build_sdiv
    | Mod -> build_srem
    | And -> build_and
    | Or -> build_or
    | Xor -> build_xor
    | Lsl -> build_shl
    | Lsr -> build_lshr
    | Asr -> build_ashr
  in
  build_op l r name builder
;;

module Float_binop = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
  [@@deriving sexp_of]
end

let build_float_binop ~name ~builder ~(op : Float_binop.t) l r =
  let build_op =
    match op with
    | Add -> Llvm.build_fadd
    | Sub -> Llvm.build_fsub
    | Mul -> Llvm.build_fmul
    | Div -> Llvm.build_fdiv
  in
  build_op l r name builder
;;

let build_fneg ~name ~builder t = Llvm.build_fneg t name builder
let mdnode ~ctx nodes = Llvm.mdnode ctx (Array.of_list nodes)
let mdstring ~ctx s = Llvm.mdstring ctx s
let block_address ~func bb = Llvm.block_address func bb
