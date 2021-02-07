(** Just test the "while" example, to see how the LLVM differs. *)
open! Core

module Trycmm = Llambda.Trycmm

let emit = Llambda.Emit_llvm.emit

let%expect_test "while example" =
  let code =
    {|
    let f n =
      let x = ref 0 in
      while 10 > !x do
        x := !x + 1
      done;
      !x
    ;; 
  |}
  in
  let cmm = Trycmm.cmm_of_source ~dump_cmm:false code in
  [%expect {| |}];
  emit cmm;
  ();
  [%expect
    {|
    ("declaring function" (cfundecl.fun_name camlMelse__f_80))
    ("declaring function" (cfundecl.fun_name camlMelse__entry))
    ; ModuleID = 'melse'
    source_filename = "melse"

    @0 = global { i64, i64 (i8*)*, i64 } { i64 3063, i64 (i8*)* @camlMelse__f_80, i64 3 }
    @camlMelse__3 = global i8* bitcast (i64 (i8*)** getelementptr inbounds ({ i64, i64 (i8*)*, i64 }, { i64, i64 (i8*)*, i64 }* @0, i32 0, i32 1) to i8*)
    @1 = global { i64, i64 } { i64 1792, i64 1 }
    @camlMelse = global i8* bitcast (i64* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @1, i32 0, i32 1) to i8*)
    @2 = global { i8**, i64 } { i8** @camlMelse, i64 0 }
    @camlMelse__gc_roots = global i8* bitcast ({ i8**, i64 }* @2 to i8*)

    declare i8* @caml_alloc(i64, i32)

    ; Function Attrs: nounwind
    declare void @llvm.gcroot(i8**, i8*) #0

    define ghccc i64 @camlMelse__f_80(i8* %n) {
    entry:
      %0 = alloca i64
      store i64 1, i64* %0
      br label %handler.2

    handler.1:                                        ; preds = %else
      br label %exit.1

    exit.1:                                           ; preds = %handler.1
      %phi.1 = phi i64 [ 1, %handler.1 ]
      %1 = load i64, i64* %0
      ret i64 %1

    handler.2:                                        ; preds = %entry, %merge
      %2 = load i64, i64* %0
      %3 = icmp sgt i64 21, %2
      br i1 %3, label %then, label %else

    then:                                             ; preds = %handler.2
      %4 = load i64, i64* %0
      %5 = add i64 %4, 2
      store i64 %5, i64* %0
      br label %merge

    else:                                             ; preds = %handler.2
      br label %handler.1

    merge:                                            ; preds = %then
      %iftmp = phi i64 [ 1, %then ]
      br label %handler.2
    }

    define ghccc i8* @camlMelse__entry() {
    entry:
      %0 = alloca i8**
      store i8** @camlMelse__3, i8*** %0
      %1 = load i8**, i8*** %0
      store i8* bitcast (i8** @camlMelse to i8*), i8** %1
      ret i8* inttoptr (i64 1 to i8*)
    }

    attributes #0 = { nounwind } |}]
;;
