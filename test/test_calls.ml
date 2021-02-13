open Core
open Ppxlib
open Helpers

let%expect_test "basic ocaml function call" =
  runtest ~show_functions:["f"; "g"] [%str 
    let f x = x + 10 

    let g y = 2 * (f y)
  ];
  [%expect {|
    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %binop = add i64 %0, 20
      %promote = inttoptr i64 %binop to i8*
      ret i8* %promote
    }

    define ocamlcc i8* @camlTest__g_83(i8* %y) gc "ocaml" {
    entry:
      %0 = load i8*, i8** bitcast (i8* @camltest to i8**), align 8
      %load = bitcast i8* %0 to i8**
      %1 = load i8*, i8** %load, align 8
      %func_cast = bitcast i8* %1 to i8* (i8*, i8*)*
      %2 = call ocamlcc i8* %func_cast(i8* %y, i8* %0)
      %3 = ptrtoint i8* %2 to i64
      %binop = shl i64 %3, 1
      %binop1 = add i64 %binop, -1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    } |}]

let%expect_test "return function" =
  runtest ~show_functions:["f"; "g"; "fun"] [%str 
    let f x = 
      let x = x * 10 in
      fun y -> 
        x + y

    let g y = 
      let f = f y in
      f 10
  ];
  [%expect {|
    define ocamlcc i8* @camlTest__fun_90(i8* %y, i8* %env) gc "ocaml" {
    entry:
      %0 = getelementptr i8, i8* %env, i64 16
      %load = bitcast i8* %0 to i8**
      %1 = load i8*, i8** %load, align 8
      %2 = ptrtoint i8* %1 to i64
      %3 = ptrtoint i8* %y to i64
      %binop = add i64 %2, %3
      %binop1 = add i64 %binop, -1
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    }

    define ocamlcc i8* @camlTest__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %binop = mul i64 %0, 10
      %binop1 = add i64 %binop, -9
      %alloc = call i8* @caml_alloc(i64 3, i64 3319)
      %gep = getelementptr inbounds i8, i8* %alloc, i64 0
      %1 = bitcast i8* %gep to i8**
      store i8* bitcast (i8* (i8*, i8*)* @camlTest__fun_90 to i8*), i8** %1, align 8
      %gep2 = getelementptr inbounds i8, i8* %alloc, i64 8
      %2 = bitcast i8* %gep2 to i64*
      store i64 3, i64* %2, align 4
      %gep3 = getelementptr inbounds i8, i8* %alloc, i64 16
      %3 = bitcast i8* %gep3 to i64*
      store i64 %binop1, i64* %3, align 4
      ret i8* %alloc
    }

    define ocamlcc i8* @camlTest__g_85(i8* %y) gc "ocaml" {
    entry:
      %0 = load i8*, i8** bitcast (i8* @camltest to i8**), align 8
      %load = bitcast i8* %0 to i8**
      %1 = load i8*, i8** %load, align 8
      %func_cast = bitcast i8* %1 to i8* (i8*, i8*)*
      %2 = call ocamlcc i8* %func_cast(i8* %y, i8* %0)
      %load1 = bitcast i8* %2 to i8**
      %3 = load i8*, i8** %load1, align 8
      %func_cast2 = bitcast i8* %3 to i8* (i64, i8*)*
      %4 = call ocamlcc i8* %func_cast2(i64 21, i8* %2)
      ret i8* %4
    } |}]
