open Core
open Ppxlib
open Helpers

let%expect_test "if statement" =
  runtest [%str let f x = if x > 10 then print_endline "Hello, world"];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %icmp = icmp sgt i64 %0, 21
      %zext = zext i1 %icmp to i64
      %1 = trunc i64 %zext to i1
      br i1 %1, label %then, label %else

    then:                                             ; preds = %entry
      %2 = call ocamlcc i8* bitcast (i8* @camlStdlib__print_endline_1181 to i8* (i8*)*)(i8* @camlTest__const_immstring_500)
      br label %merge

    else:                                             ; preds = %entry
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ inttoptr (i64 1 to i8*), %else ], [ %2, %then ]
      ret i8* %iftmp
    } |}]
;;

let%expect_test "if/else" =
  runtest [%str let f x = if x > 10 then `Hello else `World];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %icmp = icmp sgt i64 %0, 21
      %zext = zext i1 %icmp to i64
      %1 = trunc i64 %zext to i1
      br i1 %1, label %then, label %else

    then:                                             ; preds = %entry
      br label %merge

    else:                                             ; preds = %entry
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ inttoptr (i64 -1021047707 to i8*), %else ], [ inttoptr (i64 1876772325 to i8*), %then ]
      ret i8* %iftmp
    } |}]
;;

let%expect_test "match" =
  runtest
    [%str
      type t =
        | A
        | B
        | C
        | D
        | E
        | F
        | G
        | H

      let f x =
        match x with
        | A -> "A"
        | B -> "B"
        | C -> "C"
        | D -> "D"
        | E -> "E"
        | F -> "F"
        | G -> "G"
        | H -> "H"
      ;;];
  (* We can do better with code-gen here, though llc does do a decent job of
  optimising this 

  {{  ; what we should be generating is:
      %n = shr i64 %0, 1
      %1 = getelementptr i8, i8* @camlPleasefortheloveofgodoptimisethis__10, i64 %n
      %load = bitcast i8* %2 to i8**
      %3 = load i8*, i8** %load, align 8
      ret i8* %3
  ]}
  *)
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %binop = or i64 %0, 1
      %binop1 = shl i64 %binop, 2
      %1 = getelementptr i8, i8* @camlTest__1, i64 %binop1
      %2 = getelementptr i8, i8* %1, i64 -4
      %load = bitcast i8* %2 to i8**
      %3 = load i8*, i8** %load, align 8
      ret i8* %3
    } |}]
;;

let%expect_test "match+" =
  runtest
    [%str
      let f x y =
        match x with
        | `Hello -> "hello"
        | `World when y > 10 -> "woooorld"
        | `World -> "world"
      ;;];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %x, i8* %y) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %icmp = icmp sge i64 %0, 1876772325
      %zext = zext i1 %icmp to i64
      %1 = trunc i64 %zext to i1
      br i1 %1, label %then, label %else

    then:                                             ; preds = %entry
      br label %merge5

    else:                                             ; preds = %entry
      %2 = ptrtoint i8* %y to i64
      %icmp1 = icmp sgt i64 %2, 21
      %zext2 = zext i1 %icmp1 to i64
      %3 = trunc i64 %zext2 to i1
      br i1 %3, label %then3, label %else4

    then3:                                            ; preds = %else
      br label %merge

    else4:                                            ; preds = %else
      br label %merge

    merge:                                            ; preds = %else4, %then3
      %iftmp = phi i8* [ @camlTest__const_immstring_611, %else4 ], [ @camlTest__const_immstring_613, %then3 ]
      br label %merge5

    merge5:                                           ; preds = %merge, %then
      %iftmp6 = phi i8* [ %iftmp, %merge ], [ @camlTest__const_immstring_615, %then ]
      ret i8* %iftmp6
    } |}]
;;
