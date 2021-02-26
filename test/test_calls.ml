open Core
open Ppxlib
open Helpers

let%expect_test "basic ocaml function call" =
  runtest ~show_functions:[ "f"; "g" ] [%str let f x = x + 10
                                             let g y = 2 * f y];
  [%expect
    {|
    define ocamlcc i8* @camlTest__g_XXX(i8* %y_92) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %y_92 to i64
      %binop = shl i64 %0, 1
      %binop1 = add i64 %binop, 39
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    }

    define ocamlcc i8* @camlTest__f_XXX(i8* %x_89) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_89 to i64
      %binop = add i64 %0, 20
      %promote = inttoptr i64 %binop to i8*
      ret i8* %promote
    } |}]
;;

let%expect_test "return function" =
  runtest
    ~show_functions:[ "f"; "g"; "fun" ]
    [%str
      let f x =
        let x = x * 10 in
        fun y -> x + y
      ;;

      let g y =
        let f = f y in
        f 10
      ;;];
  [%expect
    {|
    define ocamlcc i8* @camlTest__g_XXX(i8* %y_101) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %y_101 to i64
      %binop = mul i64 %0, 10
      %binop1 = add i64 %binop, 11
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    }

    define ocamlcc i8* @camlTest__f_XXX(i8* %x_93) gc "ocaml" {
    entry:
      %alloc_ptr = alloca i8*, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      call void @caml_alloc3()
      %read_r15 = call i64 @llvm.read_register.i64(metadata !0)
      %0 = inttoptr i64 %read_r15 to i8*
      %1 = getelementptr i8, i8* %0, i64 8
      store i8* %1, i8** %alloc_ptr, align 8
      %2 = bitcast i8* %0 to i64*
      store i64 3319, i64* %2, align 4
      %gep = getelementptr inbounds i8, i8* %1, i64 0
      %3 = bitcast i8* %gep to i8**
      store i8* bitcast (i8* (i8*, i8*)* @camlTest__anon_fn_1110 to i8*), i8** %3, align 8
      %gep1 = getelementptr inbounds i8, i8* %1, i64 8
      %4 = bitcast i8* %gep1 to i64*
      store i64 3, i64* %4, align 4
      %gep2 = getelementptr inbounds i8, i8* %1, i64 16
      %5 = ptrtoint i8* %x_93 to i64
      %binop = mul i64 %5, 10
      %binop3 = add i64 %binop, -9
      %6 = bitcast i8* %gep2 to i64*
      store i64 %binop3, i64* %6, align 4
      %7 = load i8*, i8** %alloc_ptr, align 8
      %set_of_closures_96 = alloca i8*, align 8
      call void @llvm.gcroot(i8** %set_of_closures_96, i8* null)
      store i8* %7, i8** %set_of_closures_96, align 8
      %8 = load i8*, i8** %set_of_closures_96, align 8
      ret i8* %8
    } |}]
;;
