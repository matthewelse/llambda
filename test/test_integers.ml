open Core
open Ppxlib
open Helpers

let%expect_test "addition" =
  runtest [%str let f x = x + 10];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %binop = add i64 %0, 20
      %promote = inttoptr i64 %binop to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "subtraction of constant" =
  runtest [%str let f x = x - 10];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %binop = add i64 %0, -20
      %promote = inttoptr i64 %binop to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "subtraction by variable" =
  runtest [%str let f x y = x - y];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_87, i8* %y_86) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_87 to i64
      %1 = ptrtoint i8* %y_86 to i64
      %binop = sub i64 %0, %1
      %binop1 = add i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "multiplication" =
  runtest [%str let f x = x * 10];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %binop = mul i64 %0, 10
      %binop1 = add i64 %binop, -9
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "division by constant" =
  runtest [%str let f x = x / 10];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %binop = ashr i64 %0, 1
      %dividend_88 = alloca i64, align 8
      store i64 %binop, i64* %dividend_88, align 4
      %1 = load i64, i64* %dividend_88, align 4
      %2 = sext i64 %1 to i128
      %3 = mul i128 %2, 7378697629483820647
      %4 = lshr i128 %3, 64
      %5 = trunc i128 %4 to i64
      %binop1 = ashr i64 %5, 2
      %6 = load i64, i64* %dividend_88, align 4
      %binop2 = lshr i64 %6, 63
      %binop3 = add i64 %binop1, %binop2
      %binop4 = shl i64 %binop3, 1
      %binop5 = add i64 %binop4, 1
      %promote = inttoptr i64 %binop5 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "division by variable" =
  runtest [%str let f x y = x / y];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_87, i8* %y_86) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %y_86 to i64
      %icmp = icmp eq i64 1, %0
      %zext = zext i1 %icmp to i64
      %1 = trunc i64 %zext to i1
      br i1 %1, label %then, label %else

    then:                                             ; preds = %entry
      %read_r14 = call i64 @llvm.read_register.i64(metadata !0)
      %2 = inttoptr i64 %read_r14 to i8*
      %domain_exn_ptr = getelementptr inbounds i8, i8* %2, i64 16
      call ocamlcc void asm sideeffect "movq ($1),%rsp; popq ($1); popq %r11; jmp *%r11", "{rax},r"(i8* @caml_exn_Division_by_zero, i8* %domain_exn_ptr)
      unreachable

    else:                                             ; preds = %entry
      %3 = ptrtoint i8* %x_87 to i64
      %binop = ashr i64 %3, 1
      %4 = ptrtoint i8* %y_86 to i64
      %binop1 = ashr i64 %4, 1
      %binop2 = sdiv i64 %binop, %binop1
      %binop3 = shl i64 %binop2, 1
      %binop4 = add i64 %binop3, 1
      br label %merge

    merge:                                            ; preds = %else
      %iftmp = phi i64 [ %binop4, %else ]
      %promote = inttoptr i64 %iftmp to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "modulo constant" =
  runtest [%str let f x = x mod 10];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %binop = ashr i64 %0, 1
      %dividend_88 = alloca i64, align 8
      store i64 %binop, i64* %dividend_88, align 4
      %1 = load i64, i64* %dividend_88, align 4
      %2 = load i64, i64* %dividend_88, align 4
      %3 = sext i64 %2 to i128
      %4 = mul i128 %3, 7378697629483820647
      %5 = lshr i128 %4, 64
      %6 = trunc i128 %5 to i64
      %binop1 = ashr i64 %6, 2
      %7 = load i64, i64* %dividend_88, align 4
      %binop2 = lshr i64 %7, 63
      %binop3 = add i64 %binop1, %binop2
      %binop4 = mul i64 %binop3, 10
      %binop5 = sub i64 %1, %binop4
      %binop6 = shl i64 %binop5, 1
      %binop7 = add i64 %binop6, 1
      %promote = inttoptr i64 %binop7 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "modulo variable" =
  runtest [%str let f x y = x mod y];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_87, i8* %y_86) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %y_86 to i64
      %icmp = icmp eq i64 1, %0
      %zext = zext i1 %icmp to i64
      %1 = trunc i64 %zext to i1
      br i1 %1, label %then, label %else

    then:                                             ; preds = %entry
      %read_r14 = call i64 @llvm.read_register.i64(metadata !0)
      %2 = inttoptr i64 %read_r14 to i8*
      %domain_exn_ptr = getelementptr inbounds i8, i8* %2, i64 16
      call ocamlcc void asm sideeffect "movq ($1),%rsp; popq ($1); popq %r11; jmp *%r11", "{rax},r"(i8* @caml_exn_Division_by_zero, i8* %domain_exn_ptr)
      unreachable

    else:                                             ; preds = %entry
      %3 = ptrtoint i8* %x_87 to i64
      %binop = ashr i64 %3, 1
      %4 = ptrtoint i8* %y_86 to i64
      %binop1 = ashr i64 %4, 1
      %binop2 = srem i64 %binop, %binop1
      %binop3 = shl i64 %binop2, 1
      %binop4 = add i64 %binop3, 1
      br label %merge

    merge:                                            ; preds = %else
      %iftmp = phi i64 [ %binop4, %else ]
      %promote = inttoptr i64 %iftmp to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "logical and" =
  runtest [%str let f x = x land 0xFF];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %binop = and i64 %0, 511
      %promote = inttoptr i64 %binop to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "logical or" =
  runtest [%str let f x = x lor 1];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %binop = or i64 %0, 3
      %promote = inttoptr i64 %binop to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "logical xor" =
  runtest [%str let f x = x lxor 1];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %binop = xor i64 %0, 3
      %binop1 = or i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "logical shift left" =
  runtest [%str let f x = x lsl 1];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %binop = shl i64 %0, 1
      %binop1 = add i64 %binop, -1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "logical shift right" =
  runtest [%str let f x = x lsr 2];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %binop = lshr i64 %0, 2
      %binop1 = or i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "arithmetic shift right" =
  runtest [%str let f x = x asr 4];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %binop = ashr i64 %0, 4
      %binop1 = or i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "integer comparison: eq" =
  runtest [%str let f x = x = 4];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %icmp = icmp eq i64 %0, 9
      %zext = zext i1 %icmp to i64
      %binop = shl i64 %zext, 1
      %binop1 = add i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "integer comparison: neq" =
  runtest [%str let f x = x <> 4];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %icmp = icmp ne i64 %0, 9
      %zext = zext i1 %icmp to i64
      %binop = shl i64 %zext, 1
      %binop1 = add i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "integer comparison: lt" =
  runtest [%str let f x = x < 4];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %icmp = icmp slt i64 %0, 9
      %zext = zext i1 %icmp to i64
      %binop = shl i64 %zext, 1
      %binop1 = add i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "integer comparison: gt" =
  runtest [%str let f x = x > 4];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %icmp = icmp sgt i64 %0, 9
      %zext = zext i1 %icmp to i64
      %binop = shl i64 %zext, 1
      %binop1 = add i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "integer comparison: gte" =
  runtest [%str let f x = x >= 4];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %icmp = icmp sge i64 %0, 9
      %zext = zext i1 %icmp to i64
      %binop = shl i64 %zext, 1
      %binop1 = add i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "integer comparison: lte" =
  runtest [%str let f x = x <= 4];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_85 to i64
      %icmp = icmp sle i64 %0, 9
      %zext = zext i1 %icmp to i64
      %binop = shl i64 %zext, 1
      %binop1 = add i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;
