open Core
open Ppxlib
open Helpers

let%expect_test "for loop" =
  runtest
    [%str
      let f n f =
        for i = 0 to n do
          f ()
        done
      ;;];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %n_88, i8* %f_87) gc "ocaml" {
    entry:
      %i_90 = alloca i64, align 8
      store i64 1, i64* %i_90, align 4
      %bound_93 = alloca i8*, align 8
      call void @llvm.gcroot(i8** %bound_93, i8* null)
      store i8* %n_88, i8** %bound_93, align 8
      %0 = load i64, i64* %i_90, align 4
      %1 = load i8*, i8** %bound_93, align 8
      %2 = ptrtoint i8* %1 to i64
      %icmp = icmp sgt i64 %0, %2
      %zext = zext i1 %icmp to i64
      %3 = trunc i64 %zext to i1
      %"*id_prev*_92" = alloca i64, align 8
      br i1 %3, label %then, label %else

    handler.2:                                        ; preds = %then3, %then
      br label %exit.2

    exit.2:                                           ; preds = %handler.2
      ret i8* inttoptr (i64 1 to i8*)

    then:                                             ; preds = %entry
      br label %handler.2

    else:                                             ; preds = %entry
      br label %handler.3

    handler.3:                                        ; preds = %else, %merge
      %load = bitcast i8* %f_87 to i8**
      %4 = load i8*, i8** %load, align 8
      %func_cast = bitcast i8* %4 to void (i8*, i8*)*
      call ocamlcc void %func_cast(i8* inttoptr (i64 1 to i8*), i8* %f_87)
      %5 = load i64, i64* %i_90, align 4
      store i64 %5, i64* %"*id_prev*_92", align 4
      %6 = load i64, i64* %i_90, align 4
      %binop = add i64 %6, 2
      store i64 %binop, i64* %i_90, align 4
      %7 = load i64, i64* %"*id_prev*_92", align 4
      %8 = load i8*, i8** %bound_93, align 8
      %9 = ptrtoint i8* %8 to i64
      %icmp1 = icmp eq i64 %7, %9
      %zext2 = zext i1 %icmp1 to i64
      %10 = trunc i64 %zext2 to i1
      br i1 %10, label %then3, label %else4

    then3:                                            ; preds = %handler.3
      br label %handler.2

    else4:                                            ; preds = %handler.3
      br label %merge

    merge:                                            ; preds = %else4
      br label %handler.3
    } |}]
;;
