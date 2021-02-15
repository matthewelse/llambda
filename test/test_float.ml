open Core
open Ppxlib
open Helpers

let%expect_test "addition" =
  runtest [%str let f x = x +. 10.0];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      %alloc = call i8* @caml_alloc(i64 1, i64 1277)
      store i8* %alloc, i8** %alloc_ptr, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      %alloc1 = call i8* @caml_alloc(i64 1, i64 1277)
      store i8* %alloc1, i8** %alloc_ptr, align 8
      %gep = getelementptr inbounds i8, i8* %alloc1, i64 0
      %load = bitcast i8* %x to double*
      %0 = load double, double* %load, align 8
      %binop = fadd double %0, 1.000000e+01
      %1 = bitcast i8* %gep to double*
      store double %binop, double* %1, align 8
      %2 = load i8*, i8** %alloc_ptr, align 8
      ret i8* %2
    } |}]
;;

let%expect_test "subtraction of constant" =
  runtest [%str let f x = x -. 10.0];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      %alloc = call i8* @caml_alloc(i64 1, i64 1277)
      store i8* %alloc, i8** %alloc_ptr, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      %alloc1 = call i8* @caml_alloc(i64 1, i64 1277)
      store i8* %alloc1, i8** %alloc_ptr, align 8
      %gep = getelementptr inbounds i8, i8* %alloc1, i64 0
      %load = bitcast i8* %x to double*
      %0 = load double, double* %load, align 8
      %binop = fsub double %0, 1.000000e+01
      %1 = bitcast i8* %gep to double*
      store double %binop, double* %1, align 8
      %2 = load i8*, i8** %alloc_ptr, align 8
      ret i8* %2
    } |}]
;;

let%expect_test "subtraction by variable" =
  runtest [%str let f x y = x -. y];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x, i8* %y) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      %alloc = call i8* @caml_alloc(i64 1, i64 1277)
      store i8* %alloc, i8** %alloc_ptr, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      %alloc1 = call i8* @caml_alloc(i64 1, i64 1277)
      store i8* %alloc1, i8** %alloc_ptr, align 8
      %gep = getelementptr inbounds i8, i8* %alloc1, i64 0
      %load = bitcast i8* %x to double*
      %0 = load double, double* %load, align 8
      %load2 = bitcast i8* %y to double*
      %1 = load double, double* %load2, align 8
      %binop = fsub double %0, %1
      %2 = bitcast i8* %gep to double*
      store double %binop, double* %2, align 8
      %3 = load i8*, i8** %alloc_ptr, align 8
      ret i8* %3
    } |}]
;;

let%expect_test "multiplication" =
  runtest [%str let f x = x *. 10.0];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      %alloc = call i8* @caml_alloc(i64 1, i64 1277)
      store i8* %alloc, i8** %alloc_ptr, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      %alloc1 = call i8* @caml_alloc(i64 1, i64 1277)
      store i8* %alloc1, i8** %alloc_ptr, align 8
      %gep = getelementptr inbounds i8, i8* %alloc1, i64 0
      %load = bitcast i8* %x to double*
      %0 = load double, double* %load, align 8
      %binop = fmul double %0, 1.000000e+01
      %1 = bitcast i8* %gep to double*
      store double %binop, double* %1, align 8
      %2 = load i8*, i8** %alloc_ptr, align 8
      ret i8* %2
    } |}]
;;

let%expect_test "division by constant" =
  runtest [%str let f x = x /. 10.0];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      %alloc = call i8* @caml_alloc(i64 1, i64 1277)
      store i8* %alloc, i8** %alloc_ptr, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      %alloc1 = call i8* @caml_alloc(i64 1, i64 1277)
      store i8* %alloc1, i8** %alloc_ptr, align 8
      %gep = getelementptr inbounds i8, i8* %alloc1, i64 0
      %load = bitcast i8* %x to double*
      %0 = load double, double* %load, align 8
      %binop = fdiv double %0, 1.000000e+01
      %1 = bitcast i8* %gep to double*
      store double %binop, double* %1, align 8
      %2 = load i8*, i8** %alloc_ptr, align 8
      ret i8* %2
    } |}]
;;

let%expect_test "division by variable" =
  runtest [%str let f x y = x /. y];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x, i8* %y) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      %alloc = call i8* @caml_alloc(i64 1, i64 1277)
      store i8* %alloc, i8** %alloc_ptr, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      %alloc1 = call i8* @caml_alloc(i64 1, i64 1277)
      store i8* %alloc1, i8** %alloc_ptr, align 8
      %gep = getelementptr inbounds i8, i8* %alloc1, i64 0
      %load = bitcast i8* %x to double*
      %0 = load double, double* %load, align 8
      %load2 = bitcast i8* %y to double*
      %1 = load double, double* %load2, align 8
      %binop = fdiv double %0, %1
      %2 = bitcast i8* %gep to double*
      store double %binop, double* %2, align 8
      %3 = load i8*, i8** %alloc_ptr, align 8
      ret i8* %3
    } |}]
;;

let%expect_test "fp comparison: eq" =
  runtest [%str let f x = x = 4.0];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x) gc "ocaml" {
    entry:
      %load = bitcast i8* %x to double*
      %0 = load double, double* %load, align 8
      %fcmp = fcmp oeq double %0, 4.000000e+00
      %zext = zext i1 %fcmp to i64
      %binop = shl i64 %zext, 1
      %binop1 = add i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "fp comparison: neq" =
  runtest [%str let f x = x <> 4.0];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x) gc "ocaml" {
    entry:
      %load = bitcast i8* %x to double*
      %0 = load double, double* %load, align 8
      %fcmp = fcmp one double %0, 4.000000e+00
      %zext = zext i1 %fcmp to i64
      %binop = shl i64 %zext, 1
      %binop1 = add i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;
