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
    (codegen_expr
     (expr
       "(let_mut x/83: int 1\
      \n  (catch\
      \n    (catch rec (exit 2) with(2)\
      \n      (if (> 21 x/83) (assign x/83 (+ x/83 2)) (exit 1)) (exit 2))\
      \n  with(1) []) x/83)"))
    (codegen_expr (expr 1))
    (codegen_expr
     (expr
       "(seq\
      \n  (catch\
      \n    (catch rec (exit 2) with(2)\
      \n      (if (> 21 x/83) (assign x/83 (+ x/83 2)) (exit 1)) (exit 2))\
      \n  with(1) []) x/83)"))
    (codegen_expr
     (expr
       "(catch\
      \n  (catch rec (exit 2) with(2)\
      \n    (if (> 21 x/83) (assign x/83 (+ x/83 2)) (exit 1)) (exit 2))\
      \nwith(1) [])"))
    (codegen_expr (expr []))
    (codegen_expr
     (expr
       "(catch rec (exit 2) with(2)\
      \n  (if (> 21 x/83) (assign x/83 (+ x/83 2)) (exit 1)) (exit 2))"))
    (codegen_expr
     (expr "(seq (if (> 21 x/83) (assign x/83 (+ x/83 2)) (exit 1)) (exit 2))"))
    (codegen_expr (expr "(if (> 21 x/83) (assign x/83 (+ x/83 2)) (exit 1))"))
    (codegen_expr (expr "(> 21 x/83)"))
    (codegen_expr (expr 21))
    (codegen_expr (expr x/83))
    (codegen_expr (expr "(assign x/83 (+ x/83 2))"))
    (codegen_expr (expr "(+ x/83 2)"))
    (codegen_expr (expr x/83))
    (codegen_expr (expr 2))
    (codegen_expr (expr "(exit 1)"))
    (codegen_expr (expr "(exit 2)"))
    (codegen_expr (expr "(exit 2)"))
    (codegen_expr (expr x/83))
    ("finished codegen_expr" (cfundecl.fun_name camlMelse__f_80))
    (codegen_expr
     (expr
      "(seq (let f/80 \"camlMelse__3\" (store val(root-init) \"camlMelse\" f/80)) 1a)"))
    (codegen_expr
     (expr
      "(let f/80 \"camlMelse__3\" (store val(root-init) \"camlMelse\" f/80))"))
    (codegen_expr (expr "\"camlMelse__3\""))
    ("Reading const symbol" (name camlMelse__3)
     (ptr
      "@camlMelse__3 = global i8* bitcast (i64 (i8*)** getelementptr inbounds ({ i64, i64 (i8*)*, i64 }, { i64, i64 (i8*)*, i64 }* @0, i32 0, i32 1) to i8*)"))
    "did we get here?"
    (codegen_expr (expr "(store val(root-init) \"camlMelse\" f/80)"))
    (codegen_expr (expr "\"camlMelse\""))
    ("Reading const symbol" (name camlMelse)
     (ptr
      "@camlMelse = global i8* bitcast (i64* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @1, i32 0, i32 1) to i8*)"))
    "did we get here?"
    (codegen_expr (expr f/80))
    (codegen_expr (expr 1a))
    ("finished codegen_expr" (cfundecl.fun_name camlMelse__entry))
    ; ModuleID = 'melse'
    source_filename = "melse"
    target triple = "x86_64-apple-macosx10.15.0"

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
      %0 = alloca i8*
      store i8* bitcast (i8** @camlMelse__3 to i8*), i8** %0
      %1 = load i8*, i8** %0
      store i8* %1, i8** @camlMelse
      ret i8* inttoptr (i64 1 to i8*)
    }

    attributes #0 = { nounwind } |}]
;;
