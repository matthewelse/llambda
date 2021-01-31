open Core
open Llambda
open Llambda.Emit_llvm

let%expect_test "" =
  let source = {| let f x = 10 + x;; |} in
  let cmm = Trycmm.cmm_of_source source in
  [%expect
    {|
    (data)(data int 3063 "camlMelse__3": addr "camlMelse__f_80" int 3)(data
                                                                       int 1792
                                                                       global "camlMelse"
                                                                       "camlMelse":
                                                                       int 1)
    (data
     global "camlMelse__gc_roots"
     "camlMelse__gc_roots":
     addr "camlMelse"
     int 0)(function{:1,7-17} camlMelse__f_80 (x/82: val) (+ x/82 20))
    (function camlMelse__entry ()
     (let f/80 "camlMelse__3" (store val(root-init) "camlMelse" f/80)) 1a) |}];
  emit cmm;
  [%expect
    {|
    ; ModuleID = 'melse'
    source_filename = "melse"

    @0 = global {} zeroinitializer
    @1 = global { i64, i64 } { i64 3063, i64 3 }
    @camlMelse__3 = private global { i64, i64 }* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @1, i64 1)
    @2 = global { i64, i64 } { i64 1792, i64 1 }
    @camlMelse = global { i64, i64 }* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @2, i64 1)
    @camlMelse.1 = private global { i64, i64 }* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @2, i64 1)
    @3 = global { i64 } zeroinitializer
    @camlMelse__gc_roots = global { i64 }* @3
    @camlMelse__gc_roots.2 = private global { i64 }* @3

    define i64 @camlMelse__f_80(i64* %x) {
    entry:
      %0 = add i64* %x, i64 20
      ret i64* %0
    }

    define i64* @camlMelse__entry() {
    entry:
      store { i64, i64 }** @camlMelse, { i64, i64 }** @camlMelse__3
      ret i64* inttoptr (i64 1 to i64*)
    } |}]
;;

let%expect_test "" =
  let source = {| let rec sum x = match x with | [] -> 0 | x :: xs -> x + (sum xs);; |} in
  let cmm = Trycmm.cmm_of_source source in
  [%expect
    {|
    (data)(data int 3063 "camlMelse__4": addr "camlMelse__sum_80" int 3)(data
                                                                        int 1792
                                                                        global "camlMelse"
                                                                        "camlMelse":
                                                                        int 1)
    (data
     global "camlMelse__gc_roots"
     "camlMelse__gc_roots":
     addr "camlMelse"
     int 0)(function{:1,13-65} camlMelse__sum_80 (x/81: val)
            (if (!= x/81 1)
              (+
                (+ (load_mut val x/81)
                  (app{:1,57-65} "camlMelse__sum_80" (load_mut val (+a x/81 8))
                    val))
                -1)
              1))
    (function camlMelse__entry ()
     (let clos/86 "camlMelse__4" (store val(root-init) "camlMelse" clos/86)) 1a) |}];
  emit cmm;
  [%expect
    {|
    ; ModuleID = 'melse'
    source_filename = "melse"

    @0 = global {} zeroinitializer
    @1 = global { i64, i64 } { i64 3063, i64 3 }
    @camlMelse__4 = private global { i64, i64 }* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @1, i64 1)
    @2 = global { i64, i64 } { i64 1792, i64 1 }
    @camlMelse = global { i64, i64 }* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @2, i64 1)
    @camlMelse.1 = private global { i64, i64 }* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @2, i64 1)
    @3 = global { i64 } zeroinitializer
    @camlMelse__gc_roots = global { i64 }* @3
    @camlMelse__gc_roots.2 = private global { i64 }* @3

    define i64 @camlMelse__sum_80(i64* %x) {
    entry:
      %0 = icmp ne i64* %x, i64 1
      br i1 %0, label %then, label %else

    then:                                             ; preds = %entry
      %1 = load i64, i64* %x
      %2 = add i64* %x, i64 8
      %3 = load i64, i64* %2
      %4 = call i64 @camlMelse__sum_80(i64 %3)
      %5 = add i64 %1, %4
      %6 = add i64 %5, -1
      br label %merge

    else:                                             ; preds = %entry
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i64 [ %6, %then ], [ 1, %else ]
      ret i64 %iftmp
    }

    define i64* @camlMelse__entry() {
    entry:
      store { i64, i64 }** @camlMelse, { i64, i64 }** @camlMelse__4
      ret i64* inttoptr (i64 1 to i64*)
    } |}]
;;
