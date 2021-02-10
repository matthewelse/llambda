open! Core
open! Import

let value_type ctx = pointer_type (i8_type ctx)
let void_pointer_type ctx = pointer_type (i8_type ctx)

let type_of_machtype_component ctx (component : Cmm.machtype_component) =
  match component with
  | Val | Addr -> pointer_type (i8_type ctx)
  | Int -> i64_type ctx
  | Float -> float_type ctx
;;

let type_of_machtype ctx (machtype : Cmm.machtype) =
  match machtype with
  | [||] -> void_type ctx
  | [| component |] -> type_of_machtype_component ctx component
  | _ ->
    raise_s
      [%message
        "Tried to get type of machtype with multiple elements. This is not supported."]
;;

let type_of_operation ctx (op : Cmm.operation) =
  match op with
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor | Cxor | Clsl | Clsr
  | Casr ->
    i64_type ctx
  | Cstore _ -> void_type ctx
  | Ccmpi _ -> i1_type ctx
  | Calloc -> pointer_type (i8_type ctx)
  | Capply machtype -> type_of_machtype ctx machtype
  | _ ->
    (* Format.print_string ("Unknown operation type: " ^ Printcmm.operation Debuginfo.none op); *)
    (* Format.print_newline (); *)
    assert false
;;

let type_of_memory_chunk ctx (chunk : Cmm.memory_chunk) =
  match chunk with
  | Byte_unsigned -> i8_type ctx
  | Byte_signed -> i8_type ctx (* signed/unsigned addition? *)
  | Sixteen_unsigned -> i16_type ctx
  | Sixteen_signed -> i16_type ctx
  | Thirtytwo_signed -> i32_type ctx
  | Thirtytwo_unsigned -> i32_type ctx
  | Word_int -> i64_type ctx
  | Word_val -> pointer_type (i8_type ctx)
  | Single -> float_type ctx
  | Double | Double_u -> double_type ctx
;;
let ocaml_calling_convention = 100
