open Core
open Llambda
open Llambda.Emit_llvm

let%expect_test "" =
  let source = {| let f x = 10 + x;; |} in
  let cmm = Trycmm.cmm_of_source ~dump_cmm:false source in
  [%expect {| |}];
  emit cmm;
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Not_found_s ("Hashtbl.find_exn: not found" x))
  Raised at Base__Exn.protectx in file "src/exn.ml", line 71, characters 4-114
  Called from Llambda_test__Test_emit_llvm.(fun) in file "test/test_emit_llvm.ml", line 9, characters 2-10
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]
;;

let%expect_test "" =
  let source = {| let rec sum x = match x with | [] -> 0 | x :: xs -> x + (sum xs);; |} in
  let cmm = Trycmm.cmm_of_source ~dump_cmm:false source in
  [%expect {| |}];
  emit cmm;
  [%expect
    {|
    ("declaring function" (cfundecl.fun_name camlMelse__sum_80))
    ("declaring function" (cfundecl.fun_name camlMelse__entry))
    ("function call" (types (Val)) "i8* (i8*)")
    ; ModuleID = 'melse'
    source_filename = "melse"

    @0 = global { i64, i8* (i8*)*, i64 } { i64 3063, i8* (i8*)* @camlMelse__sum_80, i64 3 }
    @camlMelse__5 = global i8* bitcast (i8* (i8*)** getelementptr inbounds ({ i64, i8* (i8*)*, i64 }, { i64, i8* (i8*)*, i64 }* @0, i32 0, i32 1) to i8*)
    @1 = global { i64, i64 } { i64 1792, i64 1 }
    @camlMelse = global i8* bitcast (i64* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @1, i32 0, i32 1) to i8*)
    @2 = global { i8**, i64 } { i8** @camlMelse, i64 0 }
    @camlMelse__gc_roots = global i8* bitcast ({ i8**, i64 }* @2 to i8*)

    declare i8* @caml_alloc(i64, i32)

    ; Function Attrs: nounwind
    declare void @llvm.gcroot(i8**, i8*) #0

    define ghccc i8* @camlMelse__sum_80(i8* %x) {
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
      %10 = call ghccc i8* @camlMelse__sum_80(i8* %9)
      %11 = ptrtoint i8* %10 to i64
      %12 = add i64 %6, %11
      %13 = add i64 %12, -1
      br label %merge

    else:                                             ; preds = %entry
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i64 [ 1, %else ], [ %13, %then ]
      ret i64 %iftmp
    }

    define ghccc i8* @camlMelse__entry() {
    entry:
      %0 = alloca i8**
      store i8** @camlMelse__5, i8*** %0
      %1 = load i8**, i8*** %0
      store i8* bitcast (i8** @camlMelse to i8*), i8** %1
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
    ("declaring function" (cfundecl.fun_name camlMelse__create_84))
    ("declaring function" (cfundecl.fun_name camlMelse__entry))
    ; ModuleID = 'melse'
    source_filename = "melse"

    @caml_curry3 = external global i8*
    @0 = global { i64, i8**, i64, i8* (i8*, i8*, i8*)* } { i64 4087, i8** @caml_curry3, i64 7, i8* (i8*, i8*, i8*)* @camlMelse__create_84 }
    @camlMelse__6 = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i8* (i8*, i8*, i8*)* }, { i64, i8**, i64, i8* (i8*, i8*, i8*)* }* @0, i32 0, i32 1) to i8*)
    @1 = global { i64, i64 } { i64 1792, i64 1 }
    @camlMelse = global i8* bitcast (i64* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @1, i32 0, i32 1) to i8*)
    @2 = global { i8**, i64 } { i8** @camlMelse, i64 0 }
    @camlMelse__gc_roots = global i8* bitcast ({ i8**, i64 }* @2 to i8*)

    declare i8* @caml_alloc(i64, i32)

    ; Function Attrs: nounwind
    declare void @llvm.gcroot(i8**, i8*) #0

    define ghccc i8* @camlMelse__create_84(i8* %x, i8* %y, i8* %z) {
    entry:
      %0 = alloca i8*
      %1 = call i8* @caml_alloc(i64 4, i32 0)
      store i8* %1, i8** %0
      call void @llvm.gcroot(i8** %0, i8* null)
      ret i8* %1
    }

    define ghccc i8* @camlMelse__entry() {
    entry:
      %0 = alloca i8**
      store i8** @camlMelse__6, i8*** %0
      %1 = load i8**, i8*** %0
      store i8* bitcast (i8** @camlMelse to i8*), i8** %1
      ret i8* inttoptr (i64 1 to i8*)
    }

    attributes #0 = { nounwind } |}]
;;
