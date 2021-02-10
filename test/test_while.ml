(** Just test the "while" example, to see how the LLVM differs. *)
open! Core

module Trycmm = Llambda.Trycmm

let emit = Llambda.Emit_llvm.emit_llvm

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
    ; ModuleID = 'melse'
    source_filename = "melse"
    target triple = "x86_64-apple-darwin19.6.0"

    module asm ".data"
    module asm ".data"
    module asm ".quad 3063"
    module asm "_camlMelse__27:"
    module asm ".quad _camlMelse__f_80"
    module asm ".quad 3"
    module asm ".data"
    module asm ".quad 1792"
    module asm ".globl _camlMelse"
    module asm "_camlMelse:"
    module asm ".quad 1"
    module asm ".data"
    module asm ".globl _camlMelse__gc_roots"
    module asm "_camlMelse__gc_roots:"
    module asm ".quad _camlMelse"
    module asm ".quad 0"
    module asm ".data"
    module asm ".quad 3064"
    module asm ".globl _caml_exn_Out_of_memory"
    module asm "_caml_exn_Out_of_memory:"
    module asm ".quad _caml_startup__28"
    module asm ".quad -1"
    module asm ".quad 3068"
    module asm "_caml_startup__28:"
    module asm ".ascii \22Out_of_memory\22"
    module asm ".space 2"
    module asm ".byte 2"
    module asm ".data"
    module asm ".quad 3064"
    module asm ".globl _caml_exn_Sys_error"
    module asm "_caml_exn_Sys_error:"
    module asm ".quad _caml_startup__29"
    module asm ".quad -3"
    module asm ".quad 3068"
    module asm "_caml_startup__29:"
    module asm ".ascii \22Sys_error\22"
    module asm ".space 6"
    module asm ".byte 6"
    module asm ".data"
    module asm ".quad 3064"
    module asm ".globl _caml_exn_Failure"
    module asm "_caml_exn_Failure:"
    module asm ".quad _caml_startup__30"
    module asm ".quad -5"
    module asm ".quad 2044"
    module asm "_caml_startup__30:"
    module asm ".ascii \22Failure\22"
    module asm ".space 0"
    module asm ".byte 0"
    module asm ".data"
    module asm ".quad 3064"
    module asm ".globl _caml_exn_Invalid_argument"
    module asm "_caml_exn_Invalid_argument:"
    module asm ".quad _caml_startup__31"
    module asm ".quad -7"
    module asm ".quad 4092"
    module asm "_caml_startup__31:"
    module asm ".ascii \22Invalid_argument\22"
    module asm ".space 7"
    module asm ".byte 7"
    module asm ".data"
    module asm ".quad 3064"
    module asm ".globl _caml_exn_End_of_file"
    module asm "_caml_exn_End_of_file:"
    module asm ".quad _caml_startup__32"
    module asm ".quad -9"
    module asm ".quad 3068"
    module asm "_caml_startup__32:"
    module asm ".ascii \22End_of_file\22"
    module asm ".space 4"
    module asm ".byte 4"
    module asm ".data"
    module asm ".quad 3064"
    module asm ".globl _caml_exn_Division_by_zero"
    module asm "_caml_exn_Division_by_zero:"
    module asm ".quad _caml_startup__33"
    module asm ".quad -11"
    module asm ".quad 4092"
    module asm "_caml_startup__33:"
    module asm ".ascii \22Division_by_zero\22"
    module asm ".space 7"
    module asm ".byte 7"
    module asm ".data"
    module asm ".quad 3064"
    module asm ".globl _caml_exn_Not_found"
    module asm "_caml_exn_Not_found:"
    module asm ".quad _caml_startup__34"
    module asm ".quad -13"
    module asm ".quad 3068"
    module asm "_caml_startup__34:"
    module asm ".ascii \22Not_found\22"
    module asm ".space 6"
    module asm ".byte 6"
    module asm ".data"
    module asm ".quad 3064"
    module asm ".globl _caml_exn_Match_failure"
    module asm "_caml_exn_Match_failure:"
    module asm ".quad _caml_startup__35"
    module asm ".quad -15"
    module asm ".quad 3068"
    module asm "_caml_startup__35:"
    module asm ".ascii \22Match_failure\22"
    module asm ".space 2"
    module asm ".byte 2"
    module asm ".data"
    module asm ".quad 3064"
    module asm ".globl _caml_exn_Stack_overflow"
    module asm "_caml_exn_Stack_overflow:"
    module asm ".quad _caml_startup__36"
    module asm ".quad -17"
    module asm ".quad 3068"
    module asm "_caml_startup__36:"
    module asm ".ascii \22Stack_overflow\22"
    module asm ".space 1"
    module asm ".byte 1"
    module asm ".data"
    module asm ".quad 3064"
    module asm ".globl _caml_exn_Sys_blocked_io"
    module asm "_caml_exn_Sys_blocked_io:"
    module asm ".quad _caml_startup__37"
    module asm ".quad -19"
    module asm ".quad 3068"
    module asm "_caml_startup__37:"
    module asm ".ascii \22Sys_blocked_io\22"
    module asm ".space 1"
    module asm ".byte 1"
    module asm ".data"
    module asm ".quad 3064"
    module asm ".globl _caml_exn_Assert_failure"
    module asm "_caml_exn_Assert_failure:"
    module asm ".quad _caml_startup__38"
    module asm ".quad -21"
    module asm ".quad 3068"
    module asm "_caml_startup__38:"
    module asm ".ascii \22Assert_failure\22"
    module asm ".space 1"
    module asm ".byte 1"
    module asm ".data"
    module asm ".quad 3064"
    module asm ".globl _caml_exn_Undefined_recursive_module"
    module asm "_caml_exn_Undefined_recursive_module:"
    module asm ".quad _caml_startup__39"
    module asm ".quad -23"
    module asm ".quad 5116"
    module asm "_caml_startup__39:"
    module asm ".ascii \22Undefined_recursive_module\22"
    module asm ".space 5"
    module asm ".byte 5"
    module asm ".data"
    module asm ".globl _caml_globals"
    module asm "_caml_globals:"
    module asm ".quad _camlMelse__gc_roots"
    module asm ".quad 0"
    module asm ".data"
    module asm ".globl _caml_data_segments"
    module asm "_caml_data_segments:"
    module asm ".quad _caml_startup__data_begin"
    module asm ".quad _caml_startup__data_end"
    module asm ".quad _camlMelse__data_begin"
    module asm ".quad _camlMelse__data_end"
    module asm ".quad 0"
    module asm ".data"
    module asm ".globl _caml_code_segments"
    module asm "_caml_code_segments:"
    module asm ".quad _caml_startup__code_begin"
    module asm ".quad _caml_startup__code_end"
    module asm ".quad _camlMelse__code_begin"
    module asm ".quad _camlMelse__code_end"
    module asm ".quad 0"
    module asm ".data"
    module asm ".globl _caml_frametable"
    module asm "_caml_frametable:"
    module asm ".quad _caml_startup__frametable"
    module asm ".quad _caml_system__frametable"
    module asm ".quad _camlMelse__frametable"
    module asm ".quad 0"

    @camlMelse__27 = external global i8
    @camlMelse = external global i8
    @camlMelse__gc_roots = external global i8
    @caml_exn_Out_of_memory = external global i8
    @caml_startup__28 = external global i8
    @caml_exn_Sys_error = external global i8
    @caml_startup__29 = external global i8
    @caml_exn_Failure = external global i8
    @caml_startup__30 = external global i8
    @caml_exn_Invalid_argument = external global i8
    @caml_startup__31 = external global i8
    @caml_exn_End_of_file = external global i8
    @caml_startup__32 = external global i8
    @caml_exn_Division_by_zero = external global i8
    @caml_startup__33 = external global i8
    @caml_exn_Not_found = external global i8
    @caml_startup__34 = external global i8
    @caml_exn_Match_failure = external global i8
    @caml_startup__35 = external global i8
    @caml_exn_Stack_overflow = external global i8
    @caml_startup__36 = external global i8
    @caml_exn_Sys_blocked_io = external global i8
    @caml_startup__37 = external global i8
    @caml_exn_Assert_failure = external global i8
    @caml_startup__38 = external global i8
    @caml_exn_Undefined_recursive_module = external global i8
    @caml_startup__39 = external global i8
    @caml_globals = external global i8
    @caml_data_segments = external global i8
    @caml_code_segments = external global i8
    @caml_frametable = external global i8
    @caml_globals_inited = external global i8

    ; Function Attrs: nounwind
    declare void @llvm.gcroot(i8**, i8*) #0

    define ocamlcc i8* @camlMelse__f_80(i8* %n) gc "ocaml" {
    entry:
      %x = alloca i64
      store i64 1, i64* %x
      br label %handler.2

    handler.1:                                        ; preds = %else
      br label %exit.1

    exit.1:                                           ; preds = %handler.1
      %phi.1 = phi i64 [ 1, %handler.1 ]
      %0 = load i64, i64* %x
      %promote = inttoptr i64 %0 to i8*
      ret i8* %promote

    handler.2:                                        ; preds = %entry, %merge
      %1 = load i64, i64* %x
      %icmp = icmp sgt i64 21, %1
      %zext = zext i1 %icmp to i64
      %2 = trunc i64 %zext to i1
      br i1 %2, label %then, label %else

    then:                                             ; preds = %handler.2
      %3 = load i64, i64* %x
      %binop = add i64 %3, 2
      store i64 %binop, i64* %x
      br label %merge

    else:                                             ; preds = %handler.2
      br label %handler.1

    merge:                                            ; preds = %then
      %iftmp = phi i64 [ 1, %then ]
      br label %handler.2
    }

    define ocamlcc i8* @camlMelse__entry() gc "ocaml" {
    entry:
      %f = alloca i8*
      store i8* @camlMelse__27, i8** %f
      %0 = load i8*, i8** %f
      store i8* %0, i8** bitcast (i8* @camlMelse to i8**)
      ret i8* inttoptr (i64 1 to i8*)
    }

    define ocamlcc i8* @caml_program() gc "ocaml" {
    entry:
      call ocamlcc void bitcast (i8* ()* @camlMelse__entry to void ()*)()
      %0 = load i64, i64* bitcast (i8* @caml_globals_inited to i64*)
      %binop = add i64 %0, 1
      store i64 %binop, i64* bitcast (i8* @caml_globals_inited to i64*)
      ret i8* inttoptr (i64 1 to i8*)
    }

    define ocamlcc i8* @caml_apply3(i8* %arg, i8* %arg1, i8* %arg2, i8* %clos) gc "ocaml" {
    entry:
      %0 = getelementptr i8, i8* %clos, i64 8
      %load = bitcast i8* %0 to i8**
      %1 = load i8*, i8** %load
      %2 = ptrtoint i8* %1 to i64
      %icmp = icmp eq i64 %2, 7
      %zext = zext i1 %icmp to i64
      %3 = trunc i64 %zext to i1
      br i1 %3, label %then, label %else

    then:                                             ; preds = %entry
      %4 = getelementptr i8, i8* %clos, i64 16
      %load3 = bitcast i8* %4 to i8**
      %5 = load i8*, i8** %load3
      %func_cast = bitcast i8* %5 to i8* (i8*, i8*, i8*, i8*)*
      %6 = call ocamlcc i8* %func_cast(i8* %arg, i8* %arg, i8* %arg, i8* %clos)
      br label %merge

    else:                                             ; preds = %entry
      %load4 = bitcast i8* %clos to i8**
      %7 = load i8*, i8** %load4
      %func_cast5 = bitcast i8* %7 to i8* (i8*, i8*)*
      %8 = call ocamlcc i8* %func_cast5(i8* %arg, i8* %clos)
      %clos6 = alloca i8*
      store i8* %8, i8** %clos6
      %9 = load i8*, i8** %clos6
      %load7 = bitcast i8* %9 to i8**
      %10 = load i8*, i8** %load7
      %11 = load i8*, i8** %clos6
      %func_cast8 = bitcast i8* %10 to i8* (i8*, i8*)*
      %12 = call ocamlcc i8* %func_cast8(i8* %arg, i8* %11)
      %clos9 = alloca i8*
      store i8* %12, i8** %clos9
      %13 = load i8*, i8** %clos9
      %load10 = bitcast i8* %13 to i8**
      %14 = load i8*, i8** %load10
      %15 = load i8*, i8** %clos9
      %func_cast11 = bitcast i8* %14 to i8* (i8*, i8*)*
      %16 = call ocamlcc i8* %func_cast11(i8* %arg, i8* %15)
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ %16, %else ], [ %6, %then ]
      ret i8* %iftmp
    }

    define ocamlcc i8* @caml_apply2(i8* %arg, i8* %arg1, i8* %clos) gc "ocaml" {
    entry:
      %0 = getelementptr i8, i8* %clos, i64 8
      %load = bitcast i8* %0 to i8**
      %1 = load i8*, i8** %load
      %2 = ptrtoint i8* %1 to i64
      %icmp = icmp eq i64 %2, 5
      %zext = zext i1 %icmp to i64
      %3 = trunc i64 %zext to i1
      br i1 %3, label %then, label %else

    then:                                             ; preds = %entry
      %4 = getelementptr i8, i8* %clos, i64 16
      %load2 = bitcast i8* %4 to i8**
      %5 = load i8*, i8** %load2
      %func_cast = bitcast i8* %5 to i8* (i8*, i8*, i8*)*
      %6 = call ocamlcc i8* %func_cast(i8* %arg, i8* %arg, i8* %clos)
      br label %merge

    else:                                             ; preds = %entry
      %load3 = bitcast i8* %clos to i8**
      %7 = load i8*, i8** %load3
      %func_cast4 = bitcast i8* %7 to i8* (i8*, i8*)*
      %8 = call ocamlcc i8* %func_cast4(i8* %arg, i8* %clos)
      %clos5 = alloca i8*
      store i8* %8, i8** %clos5
      %9 = load i8*, i8** %clos5
      %load6 = bitcast i8* %9 to i8**
      %10 = load i8*, i8** %load6
      %11 = load i8*, i8** %clos5
      %func_cast7 = bitcast i8* %10 to i8* (i8*, i8*)*
      %12 = call ocamlcc i8* %func_cast7(i8* %arg, i8* %11)
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ %12, %else ], [ %6, %then ]
      ret i8* %iftmp
    }

    attributes #0 = { nounwind } |}]
;;
