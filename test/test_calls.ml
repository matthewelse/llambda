open Core
open Ppxlib
open Helpers

let%expect_test "basic ocaml function call" =
  runtest ~show_functions:["f"; "g"] [%str 
    let f x = x + 10 

    let g y = 2 * (f y)
  ];
  [%expect {|
    define ocamlcc i8* @camlTest__g_XXX(i8* %y) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %y to i64
      %binop = shl i64 %0, 1
      %binop1 = add i64 %binop, 39
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    }

    define ocamlcc i8* @camlTest__f_XXX(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %binop = add i64 %0, 20
      %promote = inttoptr i64 %binop to i8*
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
    define ocamlcc i8* @camlTest__g_XXX(i8* %y) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %y to i64
      %binop = mul i64 %0, 10
      %binop1 = add i64 %binop, 11
      %promote = inttoptr i64 %binop1 to i8*
      ret i8* %promote
    }

    define ocamlcc i8* @camlTest__f_XXX(i8* %x) gc "ocaml" {
    entry:
      %alloc = call i8* @caml_alloc(i64 3, i64 3319)
      %gep = getelementptr inbounds i8, i8* %alloc, i64 0
      %0 = bitcast i8* %gep to i8**
      store i8* bitcast (i8* (i8*, i8*)* @camlTest__anon_fn_873 to i8*), i8** %0, align 8
      %gep1 = getelementptr inbounds i8, i8* %alloc, i64 8
      %1 = bitcast i8* %gep1 to i64*
      store i64 3, i64* %1, align 4
      %gep2 = getelementptr inbounds i8, i8* %alloc, i64 16
      %2 = ptrtoint i8* %x to i64
      %binop = mul i64 %2, 10
      %binop3 = add i64 %binop, -9
      %3 = bitcast i8* %gep2 to i64*
      store i64 %binop3, i64* %3, align 4
      ret i8* %alloc
    } |}]
