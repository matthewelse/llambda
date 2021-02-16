open Core
open Ppxlib
open Helpers

let%expect_test "for loop" =
  runtest [%str
    let f n f =
      for i = 0 to n do
        f ()
      done
    ;;
  ];
  [%expect {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %n, i8* %f) gc "ocaml" {
    entry:
      %i = alloca i64, align 8
      store i64 1, i64* %i, align 4
      %0 = load i64, i64* %i, align 4
      %1 = ptrtoint i8* %n to i64
      %icmp = icmp sgt i64 %0, %1
      %zext = zext i1 %icmp to i64
      %2 = trunc i64 %zext to i1
      br i1 %2, label %then, label %else

    handler.1:                                        ; preds = %then3, %then
      br label %exit.1

    exit.1:                                           ; preds = %handler.1
      %phi.1 = phi i64 [ 1, %handler.1 ]
      ret i8* inttoptr (i64 1 to i8*)

    then:                                             ; preds = %entry
      br label %handler.1

    else:                                             ; preds = %entry
      br label %handler.2

    handler.2:                                        ; preds = %else, %merge
      %load = bitcast i8* %f to i8**
      %3 = load i8*, i8** %load, align 8
      %func_cast = bitcast i8* %3 to void (i8*, i8*)*
      call ocamlcc void %func_cast(i8* inttoptr (i64 1 to i8*), i8* %f)
      %4 = load i64, i64* %i, align 4
      %binop = add i64 %4, 2
      store i64 %binop, i64* %i, align 4
      %5 = load i64, i64* %i, align 4
      %6 = ptrtoint i8* %n to i64
      %icmp1 = icmp eq i64 %5, %6
      %zext2 = zext i1 %icmp1 to i64
      %7 = trunc i64 %zext2 to i1
      br i1 %7, label %then3, label %else4

    then3:                                            ; preds = %handler.2
      br label %handler.1

    else4:                                            ; preds = %handler.2
      br label %merge

    merge:                                            ; preds = %else4
      %iftmp = phi i64 [ 1, %else4 ]
      br label %handler.2
    } |}]
