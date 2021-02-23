open Core
open Ppxlib
open Helpers

let%expect_test "post-order visit" =
  runtest
    ~show_functions:[ "postorder" ]
    [%str
      type 'a t =
        | Leaf of 'a
        | Branch of 'a * 'a t * 'a t

      let rec postorder ~f t =
        match t with
        | Leaf x -> f x
        | Branch (value, left, right) ->
          postorder ~f left;
          postorder ~f right;
          f value
      ;;];
  [%expect
    {|
    define ocamlcc i8* @camlTest__postorder_XXX(i8* %f_92, i8* %t_91) gc "ocaml" {
    entry:
      %0 = getelementptr i8, i8* %t_91, i64 -8
      %1 = load i8, i8* %0, align 1
      %zext = zext i8 %1 to i64
      %2 = trunc i64 %zext to i1
      br i1 %2, label %then, label %else

    then:                                             ; preds = %entry
      %3 = getelementptr i8, i8* %t_91, i64 8
      %load = bitcast i8* %3 to i8**
      %4 = load i8*, i8** %load, align 8
      %5 = call ocamlcc i8* @camlTest__postorder_XXX(i8* %f_92, i8* %4)
      %6 = getelementptr i8, i8* %t_91, i64 16
      %load1 = bitcast i8* %6 to i8**
      %7 = load i8*, i8** %load1, align 8
      %8 = call ocamlcc i8* @camlTest__postorder_XXX(i8* %f_92, i8* %7)
      %load2 = bitcast i8* %f_92 to i8**
      %9 = load i8*, i8** %load2, align 8
      %load3 = bitcast i8* %t_91 to i8**
      %10 = load i8*, i8** %load3, align 8
      %func_cast = bitcast i8* %9 to i8* (i8*, i8*)*
      %11 = call ocamlcc i8* %func_cast(i8* %10, i8* %f_92)
      br label %merge

    else:                                             ; preds = %entry
      %load4 = bitcast i8* %f_92 to i8**
      %12 = load i8*, i8** %load4, align 8
      %load5 = bitcast i8* %t_91 to i8**
      %13 = load i8*, i8** %load5, align 8
      %func_cast6 = bitcast i8* %12 to i8* (i8*, i8*)*
      %14 = call ocamlcc i8* %func_cast6(i8* %13, i8* %f_92)
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ %14, %else ], [ %11, %then ]
      ret i8* %iftmp
    } |}]
;;

let%expect_test "hello world" =
  runtest ~show_functions:[ "f" ] [%str let f () = print_endline "hello, world!"];
  [%expect
    {|
    define ocamlcc i8* @camlTest__f_XXX(i8* %param_85) gc "ocaml" {
    entry:
      %0 = call ocamlcc i8* bitcast (i8* @camlStdlib__print_endline_1181 to i8* (i8*)*)(i8* @camlTest__const_immstring_949)
      ret i8* %0
    } |}]
;;
