open Core
open Llambda
open Llambda.Emit_llvm

let%expect_test "" =
  let source = {| let f x = 10 + x;; |} in
  let cmm = Trycmm.cmm_of_source ~dump_cmm:false source in
  [%expect {| |}];
  emit cmm;
  [%expect
    {|
    ; ModuleID = 'melse'
    source_filename = "melse"

    @camlMelse__3 = global i64 3
    @0 = global { i64, i64 } { i64 3063, i64 3 }
    @camlMelse = global i64 1
    @camlMelse.1 = global i64 1
    @1 = global { i64, i64 } { i64 1792, i64 1 }
    @camlMelse__gc_roots = global i64 0
    @camlMelse__gc_roots.2 = global i64 0
    @2 = global i64 0

    declare i8* @caml_alloc(i64, i32)

    ; Function Attrs: nounwind
    declare void @llvm.gcroot(i8**, i8*) #0

    define i64 @camlMelse__f_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %1 = add i64 %0, 20
      ret i64 %1
    }

    define i8* @camlMelse__entry() gc "ocaml" {
    entry:
      %0 = alloca i64*
      store i64* @camlMelse__3, i64** %0
      %1 = load i64*, i64** %0
      store i64* @camlMelse, i64* %1
      ret i8* inttoptr (i64 1 to i8*)
    }

    attributes #0 = { nounwind } |}]
;;

let%expect_test "" =
  let source = {| let rec sum x = match x with | [] -> 0 | x :: xs -> x + (sum xs);; |} in
  let cmm = Trycmm.cmm_of_source ~dump_cmm:false source in
  [%expect {| |}];
  emit cmm;
  [%expect
    {|
    i8* %x
    i64 8
    ; ModuleID = 'melse'
    source_filename = "melse"

    @camlMelse__4 = global i64 3
    @0 = global { i64, i64 } { i64 3063, i64 3 }
    @camlMelse = global i64 1
    @camlMelse.1 = global i64 1
    @1 = global { i64, i64 } { i64 1792, i64 1 }
    @camlMelse__gc_roots = global i64 0
    @camlMelse__gc_roots.2 = global i64 0
    @2 = global i64 0

    declare i8* @caml_alloc(i64, i32)

    ; Function Attrs: nounwind
    declare void @llvm.gcroot(i8**, i8*) #0

    define i64 @camlMelse__sum_80(i8* %x) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x to i64
      %1 = sub i64 %0, 1
      %2 = sdiv exact i64 %1, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64)
      %3 = icmp ne i64 %2, 0
      br i1 %3, label %then, label %else

    then:                                             ; preds = %entry
      %4 = bitcast i8* %x to i8**
      %5 = load i8*, i8** %4
      %6 = ptrtoint i8* %5 to i64
      %7 = getelementptr i8, i8* %x, i64 8
      %8 = bitcast i8* %7 to i8**
      %9 = load i8*, i8** %8
      %10 = call i64 @camlMelse__sum_80(i8* %9)
      %11 = add i64 %6, %10
      %12 = add i64 %11, -1
      br label %merge

    else:                                             ; preds = %entry
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i64 [ 1, %else ], [ %12, %then ]
      ret i64 %iftmp
    }

    define i8* @camlMelse__entry() gc "ocaml" {
    entry:
      %0 = alloca i64*
      store i64* @camlMelse__4, i64** %0
      %1 = load i64*, i64** %0
      store i64* @camlMelse, i64* %1
      ret i8* inttoptr (i64 1 to i8*)
    }

    attributes #0 = { nounwind } |}]
;;

let%expect_test "" =
  let source =
    {|  type t = { x : int; y : int; z : int }
let create x y z = { x; y; z } |}
  in
  let cmm = Trycmm.cmm_of_source ~dump_cmm:false source in
  [%expect {| |}];
  emit cmm;
  [%expect
    {|
    ; ModuleID = 'melse'
    source_filename = "melse"

    @camlMelse__5 = global i64 7
    @0 = global { i64, i64 } { i64 4087, i64 7 }
    @camlMelse = global i64 1
    @camlMelse.1 = global i64 1
    @1 = global { i64, i64 } { i64 1792, i64 1 }
    @camlMelse__gc_roots = global i64 0
    @camlMelse__gc_roots.2 = global i64 0
    @2 = global i64 0

    declare i8* @caml_alloc(i64, i32)

    ; Function Attrs: nounwind
    declare void @llvm.gcroot(i8**, i8*) #0

    define i8* @camlMelse__create_84(i8* %x, i8* %y, i8* %z) gc "ocaml" {
    entry:
      %0 = alloca i8*
      %1 = call i8* @caml_alloc(i64 4, i32 0)
      store i8* %1, i8** %0
      call void @llvm.gcroot(i8** %0, i8* null)
      ret i8* %1
    }

    define i8* @camlMelse__entry() gc "ocaml" {
    entry:
      %0 = alloca i64*
      store i64* @camlMelse__5, i64** %0
      %1 = load i64*, i64** %0
      store i64* @camlMelse, i64* %1
      ret i8* inttoptr (i64 1 to i8*)
    }

    attributes #0 = { nounwind } |}]
;;
