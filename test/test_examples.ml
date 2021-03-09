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
      %switcher_101 = alloca i64, align 8
      store i64 %zext, i64* %switcher_101, align 4
      %2 = load i64, i64* %switcher_101, align 4
      %3 = trunc i64 %2 to i1
      %apply_arg_94 = alloca i8*, align 8
      call void @llvm.gcroot(i8** %apply_arg_94, i8* null)
      %sequence_96 = alloca i8*, align 8
      call void @llvm.gcroot(i8** %sequence_96, i8* null)
      %apply_arg_97 = alloca i8*, align 8
      call void @llvm.gcroot(i8** %apply_arg_97, i8* null)
      %sequence_99 = alloca i8*, align 8
      call void @llvm.gcroot(i8** %sequence_99, i8* null)
      br i1 %3, label %then, label %else

    then:                                             ; preds = %entry
      %4 = getelementptr i8, i8* %t_91, i64 8
      %load = bitcast i8* %4 to i8**
      %5 = load i8*, i8** %load, align 8
      store i8* %5, i8** %apply_arg_94, align 8
      %6 = load i8*, i8** %apply_arg_94, align 8
      %7 = call ocamlcc i8* @camlTest__postorder_XXX(i8* %f_92, i8* %6)
      store i8* %7, i8** %sequence_96, align 8
      %8 = getelementptr i8, i8* %t_91, i64 16
      %load1 = bitcast i8* %8 to i8**
      %9 = load i8*, i8** %load1, align 8
      store i8* %9, i8** %apply_arg_97, align 8
      %10 = load i8*, i8** %apply_arg_97, align 8
      %11 = call ocamlcc i8* @camlTest__postorder_XXX(i8* %f_92, i8* %10)
      store i8* %11, i8** %sequence_99, align 8
      %load2 = bitcast i8* %f_92 to i8**
      %12 = load i8*, i8** %load2, align 8
      %load3 = bitcast i8* %t_91 to i8**
      %13 = load i8*, i8** %load3, align 8
      %func_cast = bitcast i8* %12 to i8* (i8*, i8*)*
      %14 = call ocamlcc i8* %func_cast(i8* %13, i8* %f_92)
      br label %merge

    else:                                             ; preds = %entry
      %load4 = bitcast i8* %f_92 to i8**
      %15 = load i8*, i8** %load4, align 8
      %load5 = bitcast i8* %t_91 to i8**
      %16 = load i8*, i8** %load5, align 8
      %func_cast6 = bitcast i8* %15 to i8* (i8*, i8*)*
      %17 = call ocamlcc i8* %func_cast6(i8* %16, i8* %f_92)
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ %17, %else ], [ %14, %then ]
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

let%expect_test "allocations" =
  runtest
    ~show_functions:[ "hd"; "print_int_option" ]
    [%str
      (* Return 0, rather than none here to just test pattern matching, rather than allocations. *)
      let hd (x : 'a list) : 'a option =
        match x with
        | x :: _ -> Some x
        | [] -> None
      ;;

      let print_int_option t =
        match t with
        | None -> print_endline "None"
        | Some x ->
          print_string "Some ";
          print_int x;
          print_char '\n'
      ;;];
  [%expect
    {|
    define ocamlcc i8* @camlTest__print_int_option_XXX(i8* %t_92) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %t_92 to i64
      %icmp = icmp ne i64 %0, 1
      %zext = zext i1 %icmp to i64
      %1 = trunc i64 %zext to i1
      %sequence_98 = alloca i8*, align 8
      call void @llvm.gcroot(i8** %sequence_98, i8* null)
      %sequence_101 = alloca i8*, align 8
      call void @llvm.gcroot(i8** %sequence_101, i8* null)
      br i1 %1, label %then, label %else

    then:                                             ; preds = %entry
      %2 = load i8*, i8** bitcast (i8* @camlStdlib__Pccall_1806 to i8**), align 8
      %3 = call ocamlcc i8* bitcast (i8* @camlStdlib__output_string_761 to i8* (i8*, i8*)*)(i8* %2, i8* @camlTest__const_immstring_1007)
      store i8* %3, i8** %sequence_98, align 8
      %load = bitcast i8* %t_92 to i8**
      %4 = load i8*, i8** %load, align 8
      %5 = call ocamlcc i8* bitcast (i8* @camlStdlib__print_int_1157 to i8* (i8*)*)(i8* %4)
      store i8* %5, i8** %sequence_101, align 8
      %6 = call ocamlcc i8* bitcast (i8* @camlStdlib__print_char_1131 to i8* (i64)*)(i64 21)
      br label %merge

    else:                                             ; preds = %entry
      %7 = call ocamlcc i8* bitcast (i8* @camlStdlib__print_endline_1181 to i8* (i8*)*)(i8* @camlTest__const_immstring_999)
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ %7, %else ], [ %6, %then ]
      ret i8* %iftmp
    }

    define ocamlcc i8* @camlTest__hd_XXX(i8* %x_104) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_104 to i64
      %icmp = icmp ne i64 %0, 1
      %zext = zext i1 %icmp to i64
      %1 = trunc i64 %zext to i1
      %alloc_ptr = alloca i8*, align 8
      call void @llvm.gcroot(i8** %alloc_ptr, i8* null)
      br i1 %1, label %then, label %else

    then:                                             ; preds = %entry
      call void @caml_alloc1()
      %read_r15 = call i64 @llvm.read_register.i64(metadata !0)
      %2 = inttoptr i64 %read_r15 to i8*
      %3 = getelementptr i8, i8* %2, i64 8
      store i8* %3, i8** %alloc_ptr, align 8
      %4 = bitcast i8* %2 to i64*
      store i64 1024, i64* %4, align 4
      %gep = getelementptr inbounds i8, i8* %3, i64 0
      %load = bitcast i8* %x_104 to i8**
      %5 = load i8*, i8** %load, align 8
      %6 = bitcast i8* %gep to i8**
      store i8* %5, i8** %6, align 8
      %7 = load i8*, i8** %alloc_ptr, align 8
      br label %merge

    else:                                             ; preds = %entry
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ inttoptr (i64 1 to i8*), %else ], [ %7, %then ]
      ret i8* %iftmp
    } |}]
;;
