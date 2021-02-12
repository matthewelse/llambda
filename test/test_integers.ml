open Core
open Ppxlib
open Helpers

let%expect_test "addition" =
  runtest [%str let f x = x + 10];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %binop = add i64 %0, 20
      %promote = inttoptr i64 %binop to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "subtraction of constant" =
  runtest [%str let f x = x - 10];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %binop = add i64 %0, -20
      %promote = inttoptr i64 %binop to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "subtraction by variable" =
  runtest [%str let f x y = x - y];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_80(i8* %x, i8* %y) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %1 = ptrtoint i8* %y to i64
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
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
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
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %binop = ashr i64 %0, 1
      %binop1 = mul i64 %binop, 7378697629483820647
      %binop2 = ashr i64 %binop1, 2
      %binop3 = lshr i64 %binop, 63
      %binop4 = add i64 %binop2, %binop3
      %binop5 = shl i64 %binop4, 1
      %binop6 = add i64 %binop5, 1
      %promote = inttoptr i64 %binop6 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "division by variable" =
  runtest [%str let f x y = x / y];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_80(i8* %x, i8* %y) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %y to i64
      %binop = ashr i64 %0, 1
      %1 = ptrtoint i8* %x to i64
      %binop1 = ashr i64 %1, 1
      %2 = trunc i64 %binop to i1
      br i1 %2, label %then, label %else

    then:                                             ; preds = %entry
      %binop2 = sdiv i64 %binop1, %binop
      br label %merge

    else:                                             ; preds = %entry
      call ocamlcc void @llambda_raise_exn(i8* @caml_exn_Division_by_zero)
      unreachable

    merge:                                            ; preds = %then
      %iftmp = phi i64 [ %binop2, %then ]
      %binop3 = shl i64 %iftmp, 1
      %binop4 = add i64 %binop3, 1
      %promote = inttoptr i64 %binop4 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "modulo constant" =
  runtest [%str let f x = x mod 10];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %binop = ashr i64 %0, 1
      %binop1 = mul i64 %binop, 7378697629483820647
      %binop2 = ashr i64 %binop1, 2
      %binop3 = lshr i64 %binop, 63
      %binop4 = add i64 %binop2, %binop3
      %binop5 = mul i64 %binop4, 10
      %binop6 = sub i64 %binop, %binop5
      %binop7 = shl i64 %binop6, 1
      %binop8 = add i64 %binop7, 1
      %promote = inttoptr i64 %binop8 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "modulo variable" =
  runtest [%str let f x y = x mod y];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_80(i8* %x, i8* %y) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %y to i64
      %binop = ashr i64 %0, 1
      %1 = ptrtoint i8* %x to i64
      %binop1 = ashr i64 %1, 1
      %2 = trunc i64 %binop to i1
      br i1 %2, label %then, label %else

    then:                                             ; preds = %entry
      %binop2 = srem i64 %binop1, %binop
      br label %merge

    else:                                             ; preds = %entry
      call ocamlcc void @llambda_raise_exn(i8* @caml_exn_Division_by_zero)
      unreachable

    merge:                                            ; preds = %then
      %iftmp = phi i64 [ %binop2, %then ]
      %binop3 = shl i64 %iftmp, 1
      %binop4 = add i64 %binop3, 1
      %promote = inttoptr i64 %binop4 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "logical and" =
  runtest [%str let f x = x land 0xFF];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %binop = and i64 %0, 511
      %promote = inttoptr i64 %binop to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "logical or" =
  runtest [%str let f x = x lor 1];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %binop = or i64 %0, 3
      %promote = inttoptr i64 %binop to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "logical xor" =
  runtest [%str let f x = x lxor 1];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
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
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
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
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
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
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
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
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
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
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
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
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
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
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
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
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
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
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %icmp = icmp sle i64 %0, 9
      %zext = zext i1 %icmp to i64
      %binop = shl i64 %zext, 1
      %binop1 = add i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;
