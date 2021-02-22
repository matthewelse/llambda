open Core
open Ppxlib
open Helpers

let%expect_test "addition" =
  runtest [%str let f x = x +. 10.0];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      call void @caml_alloc1()
      %read_r15 = call i64 @llvm.read_register.i64(metadata !0)
      %0 = inttoptr i64 %read_r15 to i8*
      %1 = getelementptr i8, i8* %0, i64 8
      store i8* %1, i8** %alloc_ptr, align 8
      %2 = bitcast i8* %0 to i64*
      store i64 1277, i64* %2, align 4
      %gep = getelementptr inbounds i8, i8* %1, i64 0
      %load = bitcast i8* %x_85 to double*
      %3 = load double, double* %load, align 8
      %binop = fadd double %3, 1.000000e+01
      %4 = bitcast i8* %gep to double*
      store double %binop, double* %4, align 8
      %5 = load i8*, i8** %alloc_ptr, align 8
      ret i8* %5
    } |}]
;;

let%expect_test "subtraction of constant" =
  runtest [%str let f x = x -. 10.0];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      call void @caml_alloc1()
      %read_r15 = call i64 @llvm.read_register.i64(metadata !0)
      %0 = inttoptr i64 %read_r15 to i8*
      %1 = getelementptr i8, i8* %0, i64 8
      store i8* %1, i8** %alloc_ptr, align 8
      %2 = bitcast i8* %0 to i64*
      store i64 1277, i64* %2, align 4
      %gep = getelementptr inbounds i8, i8* %1, i64 0
      %load = bitcast i8* %x_85 to double*
      %3 = load double, double* %load, align 8
      %binop = fsub double %3, 1.000000e+01
      %4 = bitcast i8* %gep to double*
      store double %binop, double* %4, align 8
      %5 = load i8*, i8** %alloc_ptr, align 8
      ret i8* %5
    } |}]
;;

let%expect_test "subtraction by variable" =
  runtest [%str let f x y = x -. y];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_87, i8* %y_86) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      call void @caml_alloc1()
      %read_r15 = call i64 @llvm.read_register.i64(metadata !0)
      %0 = inttoptr i64 %read_r15 to i8*
      %1 = getelementptr i8, i8* %0, i64 8
      store i8* %1, i8** %alloc_ptr, align 8
      %2 = bitcast i8* %0 to i64*
      store i64 1277, i64* %2, align 4
      %gep = getelementptr inbounds i8, i8* %1, i64 0
      %load = bitcast i8* %x_87 to double*
      %3 = load double, double* %load, align 8
      %load1 = bitcast i8* %y_86 to double*
      %4 = load double, double* %load1, align 8
      %binop = fsub double %3, %4
      %5 = bitcast i8* %gep to double*
      store double %binop, double* %5, align 8
      %6 = load i8*, i8** %alloc_ptr, align 8
      ret i8* %6
    } |}]
;;

let%expect_test "multiplication" =
  runtest [%str let f x = x *. 10.0];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      call void @caml_alloc1()
      %read_r15 = call i64 @llvm.read_register.i64(metadata !0)
      %0 = inttoptr i64 %read_r15 to i8*
      %1 = getelementptr i8, i8* %0, i64 8
      store i8* %1, i8** %alloc_ptr, align 8
      %2 = bitcast i8* %0 to i64*
      store i64 1277, i64* %2, align 4
      %gep = getelementptr inbounds i8, i8* %1, i64 0
      %load = bitcast i8* %x_85 to double*
      %3 = load double, double* %load, align 8
      %binop = fmul double %3, 1.000000e+01
      %4 = bitcast i8* %gep to double*
      store double %binop, double* %4, align 8
      %5 = load i8*, i8** %alloc_ptr, align 8
      ret i8* %5
    } |}]
;;

let%expect_test "division by constant" =
  runtest [%str let f x = x /. 10.0];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      call void @caml_alloc1()
      %read_r15 = call i64 @llvm.read_register.i64(metadata !0)
      %0 = inttoptr i64 %read_r15 to i8*
      %1 = getelementptr i8, i8* %0, i64 8
      store i8* %1, i8** %alloc_ptr, align 8
      %2 = bitcast i8* %0 to i64*
      store i64 1277, i64* %2, align 4
      %gep = getelementptr inbounds i8, i8* %1, i64 0
      %load = bitcast i8* %x_85 to double*
      %3 = load double, double* %load, align 8
      %binop = fdiv double %3, 1.000000e+01
      %4 = bitcast i8* %gep to double*
      store double %binop, double* %4, align 8
      %5 = load i8*, i8** %alloc_ptr, align 8
      ret i8* %5
    } |}]
;;

let%expect_test "division by variable" =
  runtest [%str let f x y = x /. y];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_87, i8* %y_86) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      call void @caml_alloc1()
      %read_r15 = call i64 @llvm.read_register.i64(metadata !0)
      %0 = inttoptr i64 %read_r15 to i8*
      %1 = getelementptr i8, i8* %0, i64 8
      store i8* %1, i8** %alloc_ptr, align 8
      %2 = bitcast i8* %0 to i64*
      store i64 1277, i64* %2, align 4
      %gep = getelementptr inbounds i8, i8* %1, i64 0
      %load = bitcast i8* %x_87 to double*
      %3 = load double, double* %load, align 8
      %load1 = bitcast i8* %y_86 to double*
      %4 = load double, double* %load1, align 8
      %binop = fdiv double %3, %4
      %5 = bitcast i8* %gep to double*
      store double %binop, double* %5, align 8
      %6 = load i8*, i8** %alloc_ptr, align 8
      ret i8* %6
    } |}]
;;

let%expect_test "fp comparison: eq" =
  runtest [%str let f x = x = 4.0];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %load = bitcast i8* %x_85 to double*
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
    define ocamlcc i8* @camlTest__f_XXX(i8* %x_85) gc "ocaml" {
    entry:
      %load = bitcast i8* %x_85 to double*
      %0 = load double, double* %load, align 8
      %fcmp = fcmp one double %0, 4.000000e+00
      %zext = zext i1 %fcmp to i64
      %binop = shl i64 %zext, 1
      %binop1 = add i64 %binop, 1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]
;;
