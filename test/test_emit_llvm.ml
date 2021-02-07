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
    ("function args" (name camlMelse__f_80) (args ((x (Value "i8* %x")))))
    ("function args" (name camlMelse__entry) (args ()))
    ("function args" (name caml_program) (args ()))
    ("function args" (name caml_apply3)
     (args
      ((arg (Value "i8* %arg")) (arg1 (Value "i8* %arg1"))
       (arg2 (Value "i8* %arg2")) (clos (Value "i8* %clos")))))
    ("function args" (name caml_apply2)
     (args
      ((arg (Value "i8* %arg")) (arg1 (Value "i8* %arg1"))
       (clos (Value "i8* %clos")))))
    ; ModuleID = 'melse'
    source_filename = "melse"
    target triple = "x86_64-apple-macosx10.15.0"

    @0 = global { i64, i8* (i8*)*, i64 } { i64 3063, i8* (i8*)* @camlMelse__f_80, i64 3 }
    @camlMelse__40 = global i8* bitcast (i8* (i8*)** getelementptr inbounds ({ i64, i8* (i8*)*, i64 }, { i64, i8* (i8*)*, i64 }* @0, i32 0, i32 1) to i8*)
    @1 = global { i64, i64 } { i64 1792, i64 1 }
    @camlMelse = global i8* bitcast (i64* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @1, i32 0, i32 1) to i8*)
    @2 = global { i8**, i64 } { i8** @camlMelse, i64 0 }
    @camlMelse__gc_roots = global i8* bitcast ({ i8**, i64 }* @2 to i8*)
    @caml_startup__41 = external global i8*
    @3 = global { i64, i8**, i64, i64, [14 x i8], i8 } { i64 3064, i8** @caml_startup__41, i64 -1, i64 3068, [14 x i8] c"Out_of_memory\00", i8 2 }
    @caml_exn_Out_of_memory = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [14 x i8], i8 }, { i64, i8**, i64, i64, [14 x i8], i8 }* @3, i32 0, i32 1) to i8*)
    @caml_startup__41.1 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [14 x i8], i8 }, { i64, i8**, i64, i64, [14 x i8], i8 }* @3, i32 0, i32 4, i32 0)
    @caml_startup__42 = external global i8*
    @4 = global { i64, i8**, i64, i64, [10 x i8], i8 } { i64 3064, i8** @caml_startup__42, i64 -3, i64 3068, [10 x i8] c"Sys_error\00", i8 6 }
    @caml_exn_Sys_error = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [10 x i8], i8 }, { i64, i8**, i64, i64, [10 x i8], i8 }* @4, i32 0, i32 1) to i8*)
    @caml_startup__42.2 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [10 x i8], i8 }, { i64, i8**, i64, i64, [10 x i8], i8 }* @4, i32 0, i32 4, i32 0)
    @caml_startup__43 = external global i8*
    @5 = global { i64, i8**, i64, i64, [8 x i8], i8 } { i64 3064, i8** @caml_startup__43, i64 -5, i64 2044, [8 x i8] c"Failure\00", i8 0 }
    @caml_exn_Failure = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [8 x i8], i8 }, { i64, i8**, i64, i64, [8 x i8], i8 }* @5, i32 0, i32 1) to i8*)
    @caml_startup__43.3 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [8 x i8], i8 }, { i64, i8**, i64, i64, [8 x i8], i8 }* @5, i32 0, i32 4, i32 0)
    @caml_startup__44 = external global i8*
    @6 = global { i64, i8**, i64, i64, [17 x i8], i8 } { i64 3064, i8** @caml_startup__44, i64 -7, i64 4092, [17 x i8] c"Invalid_argument\00", i8 7 }
    @caml_exn_Invalid_argument = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [17 x i8], i8 }, { i64, i8**, i64, i64, [17 x i8], i8 }* @6, i32 0, i32 1) to i8*)
    @caml_startup__44.4 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [17 x i8], i8 }, { i64, i8**, i64, i64, [17 x i8], i8 }* @6, i32 0, i32 4, i32 0)
    @caml_startup__45 = external global i8*
    @7 = global { i64, i8**, i64, i64, [12 x i8], i8 } { i64 3064, i8** @caml_startup__45, i64 -9, i64 3068, [12 x i8] c"End_of_file\00", i8 4 }
    @caml_exn_End_of_file = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [12 x i8], i8 }, { i64, i8**, i64, i64, [12 x i8], i8 }* @7, i32 0, i32 1) to i8*)
    @caml_startup__45.5 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [12 x i8], i8 }, { i64, i8**, i64, i64, [12 x i8], i8 }* @7, i32 0, i32 4, i32 0)
    @caml_startup__46 = external global i8*
    @8 = global { i64, i8**, i64, i64, [17 x i8], i8 } { i64 3064, i8** @caml_startup__46, i64 -11, i64 4092, [17 x i8] c"Division_by_zero\00", i8 7 }
    @caml_exn_Division_by_zero = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [17 x i8], i8 }, { i64, i8**, i64, i64, [17 x i8], i8 }* @8, i32 0, i32 1) to i8*)
    @caml_startup__46.6 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [17 x i8], i8 }, { i64, i8**, i64, i64, [17 x i8], i8 }* @8, i32 0, i32 4, i32 0)
    @caml_startup__47 = external global i8*
    @9 = global { i64, i8**, i64, i64, [10 x i8], i8 } { i64 3064, i8** @caml_startup__47, i64 -13, i64 3068, [10 x i8] c"Not_found\00", i8 6 }
    @caml_exn_Not_found = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [10 x i8], i8 }, { i64, i8**, i64, i64, [10 x i8], i8 }* @9, i32 0, i32 1) to i8*)
    @caml_startup__47.7 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [10 x i8], i8 }, { i64, i8**, i64, i64, [10 x i8], i8 }* @9, i32 0, i32 4, i32 0)
    @caml_startup__48 = external global i8*
    @10 = global { i64, i8**, i64, i64, [14 x i8], i8 } { i64 3064, i8** @caml_startup__48, i64 -15, i64 3068, [14 x i8] c"Match_failure\00", i8 2 }
    @caml_exn_Match_failure = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [14 x i8], i8 }, { i64, i8**, i64, i64, [14 x i8], i8 }* @10, i32 0, i32 1) to i8*)
    @caml_startup__48.8 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [14 x i8], i8 }, { i64, i8**, i64, i64, [14 x i8], i8 }* @10, i32 0, i32 4, i32 0)
    @caml_startup__49 = external global i8*
    @11 = global { i64, i8**, i64, i64, [15 x i8], i8 } { i64 3064, i8** @caml_startup__49, i64 -17, i64 3068, [15 x i8] c"Stack_overflow\00", i8 1 }
    @caml_exn_Stack_overflow = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @11, i32 0, i32 1) to i8*)
    @caml_startup__49.9 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @11, i32 0, i32 4, i32 0)
    @caml_startup__50 = external global i8*
    @12 = global { i64, i8**, i64, i64, [15 x i8], i8 } { i64 3064, i8** @caml_startup__50, i64 -19, i64 3068, [15 x i8] c"Sys_blocked_io\00", i8 1 }
    @caml_exn_Sys_blocked_io = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @12, i32 0, i32 1) to i8*)
    @caml_startup__50.10 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @12, i32 0, i32 4, i32 0)
    @caml_startup__51 = external global i8*
    @13 = global { i64, i8**, i64, i64, [15 x i8], i8 } { i64 3064, i8** @caml_startup__51, i64 -21, i64 3068, [15 x i8] c"Assert_failure\00", i8 1 }
    @caml_exn_Assert_failure = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @13, i32 0, i32 1) to i8*)
    @caml_startup__51.11 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @13, i32 0, i32 4, i32 0)
    @caml_startup__52 = external global i8*
    @14 = global { i64, i8**, i64, i64, [27 x i8], i8 } { i64 3064, i8** @caml_startup__52, i64 -23, i64 5116, [27 x i8] c"Undefined_recursive_module\00", i8 5 }
    @caml_exn_Undefined_recursive_module = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [27 x i8], i8 }, { i64, i8**, i64, i64, [27 x i8], i8 }* @14, i32 0, i32 1) to i8*)
    @caml_startup__52.12 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [27 x i8], i8 }, { i64, i8**, i64, i64, [27 x i8], i8 }* @14, i32 0, i32 4, i32 0)
    @15 = global { i8**, i64 } { i8** @camlMelse__gc_roots, i64 0 }
    @caml_globals = global i8* bitcast ({ i8**, i64 }* @15 to i8*)
    @caml_startup__data_begin = external global i8*
    @caml_startup__data_end = external global i8*
    @camlMelse__data_begin = external global i8*
    @camlMelse__data_end = external global i8*
    @16 = global { i8**, i8**, i8**, i8**, i64 } { i8** @caml_startup__data_begin, i8** @caml_startup__data_end, i8** @camlMelse__data_begin, i8** @camlMelse__data_end, i64 0 }
    @caml_data_segments = global i8* bitcast ({ i8**, i8**, i8**, i8**, i64 }* @16 to i8*)
    @caml_startup__code_begin = external global i8*
    @caml_startup__code_end = external global i8*
    @camlMelse__code_begin = external global i8*
    @camlMelse__code_end = external global i8*
    @17 = global { i8**, i8**, i8**, i8**, i64 } { i8** @caml_startup__code_begin, i8** @caml_startup__code_end, i8** @camlMelse__code_begin, i8** @camlMelse__code_end, i64 0 }
    @caml_code_segments = global i8* bitcast ({ i8**, i8**, i8**, i8**, i64 }* @17 to i8*)
    @caml_startup__frametable = external global i8*
    @caml_system__frametable = external global i8*
    @camlMelse__frametable = external global i8*
    @18 = global { i8**, i8**, i8**, i64 } { i8** @caml_startup__frametable, i8** @caml_system__frametable, i8** @camlMelse__frametable, i64 0 }
    @caml_frametable = global i8* bitcast ({ i8**, i8**, i8**, i64 }* @18 to i8*)
    @caml_globals_inited = external global i8*

    declare i8* @caml_alloc(i64, i32)

    ; Function Attrs: nounwind
    declare void @llvm.gcroot(i8**, i8*) #0

    define ghccc i8* @camlMelse__f_80(i8* %x) {
    entry:
      %0 = ptrtoint i8* %x to i64
      %1 = add i64 %0, 20
      ret i64 %1
    }

    define ghccc i8* @camlMelse__entry() {
    entry:
      %0 = alloca i8*
      store i8* bitcast (i8** @camlMelse__40 to i8*), i8** %0
      %1 = load i8*, i8** %0
      store i8* %1, i8** @camlMelse
      ret i8* inttoptr (i64 1 to i8*)
    }

    define ghccc i64 @caml_program() {
    entry:
      call ghccc void bitcast (i8* ()* @camlMelse__entry to void ()*)()
      %0 = load i64, i64* bitcast (i8** @caml_globals_inited to i64*)
      %1 = add i64 %0, 1
      store i64 %1, i64* bitcast (i8** @caml_globals_inited to i64*)
      ret i64 1
    }

    define ghccc i8* @caml_apply3(i8* %arg, i8* %arg1, i8* %arg2, i8* %clos) {
    entry:
      %0 = getelementptr i8, i8* %clos, i64 8
      %1 = bitcast i8* %0 to i8**
      %2 = load i8*, i8** %1
      %3 = ptrtoint i8* %2 to i64
      %4 = sub i64 %3, 7
      %5 = sdiv exact i64 %4, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64)
      %6 = icmp eq i64 %5, 0
      br i1 %6, label %then, label %else

    then:                                             ; preds = %entry
      %7 = getelementptr i8, i8* %clos, i64 16
      %8 = bitcast i8* %7 to i8**
      %9 = load i8*, i8** %8
      %func_cast = bitcast i8* %9 to i8* (i8*, i8*, i8*, i8*)*
      %10 = call ghccc i8* %func_cast(i8* %arg, i8* %arg, i8* %arg, i8* %clos)
      br label %merge

    else:                                             ; preds = %entry
      %11 = bitcast i8* %clos to i8**
      %12 = load i8*, i8** %11
      %func_cast3 = bitcast i8* %12 to i8* (i8*, i8*)*
      %13 = call ghccc i8* %func_cast3(i8* %arg, i8* %clos)
      %14 = alloca i8*
      store i8* %13, i8** %14
      %15 = load i8*, i8** %14
      %16 = bitcast i8* %15 to i8**
      %17 = load i8*, i8** %16
      %18 = load i8*, i8** %14
      %func_cast4 = bitcast i8* %17 to i8* (i8*, i8*)*
      %19 = call ghccc i8* %func_cast4(i8* %arg, i8* %18)
      %20 = alloca i8*
      store i8* %19, i8** %20
      %21 = load i8*, i8** %20
      %22 = bitcast i8* %21 to i8**
      %23 = load i8*, i8** %22
      %24 = load i8*, i8** %20
      %func_cast5 = bitcast i8* %23 to i8* (i8*, i8*)*
      %25 = call ghccc i8* %func_cast5(i8* %arg, i8* %24)
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ %25, %else ], [ %10, %then ]
      ret i8* %iftmp
    }

    define ghccc i8* @caml_apply2(i8* %arg, i8* %arg1, i8* %clos) {
    entry:
      %0 = getelementptr i8, i8* %clos, i64 8
      %1 = bitcast i8* %0 to i8**
      %2 = load i8*, i8** %1
      %3 = ptrtoint i8* %2 to i64
      %4 = sub i64 %3, 5
      %5 = sdiv exact i64 %4, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64)
      %6 = icmp eq i64 %5, 0
      br i1 %6, label %then, label %else

    then:                                             ; preds = %entry
      %7 = getelementptr i8, i8* %clos, i64 16
      %8 = bitcast i8* %7 to i8**
      %9 = load i8*, i8** %8
      %func_cast = bitcast i8* %9 to i8* (i8*, i8*, i8*)*
      %10 = call ghccc i8* %func_cast(i8* %arg, i8* %arg, i8* %clos)
      br label %merge

    else:                                             ; preds = %entry
      %11 = bitcast i8* %clos to i8**
      %12 = load i8*, i8** %11
      %func_cast2 = bitcast i8* %12 to i8* (i8*, i8*)*
      %13 = call ghccc i8* %func_cast2(i8* %arg, i8* %clos)
      %14 = alloca i8*
      store i8* %13, i8** %14
      %15 = load i8*, i8** %14
      %16 = bitcast i8* %15 to i8**
      %17 = load i8*, i8** %16
      %18 = load i8*, i8** %14
      %func_cast3 = bitcast i8* %17 to i8* (i8*, i8*)*
      %19 = call ghccc i8* %func_cast3(i8* %arg, i8* %18)
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ %19, %else ], [ %10, %then ]
      ret i8* %iftmp
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
    ("function args" (name camlMelse__sum_80) (args ((x (Value "i8* %x")))))
    ("function args" (name camlMelse__entry) (args ()))
    ("function args" (name caml_program) (args ()))
    ("function args" (name caml_apply3)
     (args
      ((arg (Value "i8* %arg")) (arg1 (Value "i8* %arg1"))
       (arg2 (Value "i8* %arg2")) (clos (Value "i8* %clos")))))
    ("function args" (name caml_apply2)
     (args
      ((arg (Value "i8* %arg")) (arg1 (Value "i8* %arg1"))
       (clos (Value "i8* %clos")))))
    ; ModuleID = 'melse'
    source_filename = "melse"
    target triple = "x86_64-apple-macosx10.15.0"

    @0 = global { i64, i8* (i8*)*, i64 } { i64 3063, i8* (i8*)* @camlMelse__sum_80, i64 3 }
    @camlMelse__53 = global i8* bitcast (i8* (i8*)** getelementptr inbounds ({ i64, i8* (i8*)*, i64 }, { i64, i8* (i8*)*, i64 }* @0, i32 0, i32 1) to i8*)
    @1 = global { i64, i64 } { i64 1792, i64 1 }
    @camlMelse = global i8* bitcast (i64* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @1, i32 0, i32 1) to i8*)
    @2 = global { i8**, i64 } { i8** @camlMelse, i64 0 }
    @camlMelse__gc_roots = global i8* bitcast ({ i8**, i64 }* @2 to i8*)
    @caml_startup__54 = external global i8*
    @3 = global { i64, i8**, i64, i64, [14 x i8], i8 } { i64 3064, i8** @caml_startup__54, i64 -1, i64 3068, [14 x i8] c"Out_of_memory\00", i8 2 }
    @caml_exn_Out_of_memory = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [14 x i8], i8 }, { i64, i8**, i64, i64, [14 x i8], i8 }* @3, i32 0, i32 1) to i8*)
    @caml_startup__54.1 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [14 x i8], i8 }, { i64, i8**, i64, i64, [14 x i8], i8 }* @3, i32 0, i32 4, i32 0)
    @caml_startup__55 = external global i8*
    @4 = global { i64, i8**, i64, i64, [10 x i8], i8 } { i64 3064, i8** @caml_startup__55, i64 -3, i64 3068, [10 x i8] c"Sys_error\00", i8 6 }
    @caml_exn_Sys_error = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [10 x i8], i8 }, { i64, i8**, i64, i64, [10 x i8], i8 }* @4, i32 0, i32 1) to i8*)
    @caml_startup__55.2 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [10 x i8], i8 }, { i64, i8**, i64, i64, [10 x i8], i8 }* @4, i32 0, i32 4, i32 0)
    @caml_startup__56 = external global i8*
    @5 = global { i64, i8**, i64, i64, [8 x i8], i8 } { i64 3064, i8** @caml_startup__56, i64 -5, i64 2044, [8 x i8] c"Failure\00", i8 0 }
    @caml_exn_Failure = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [8 x i8], i8 }, { i64, i8**, i64, i64, [8 x i8], i8 }* @5, i32 0, i32 1) to i8*)
    @caml_startup__56.3 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [8 x i8], i8 }, { i64, i8**, i64, i64, [8 x i8], i8 }* @5, i32 0, i32 4, i32 0)
    @caml_startup__57 = external global i8*
    @6 = global { i64, i8**, i64, i64, [17 x i8], i8 } { i64 3064, i8** @caml_startup__57, i64 -7, i64 4092, [17 x i8] c"Invalid_argument\00", i8 7 }
    @caml_exn_Invalid_argument = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [17 x i8], i8 }, { i64, i8**, i64, i64, [17 x i8], i8 }* @6, i32 0, i32 1) to i8*)
    @caml_startup__57.4 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [17 x i8], i8 }, { i64, i8**, i64, i64, [17 x i8], i8 }* @6, i32 0, i32 4, i32 0)
    @caml_startup__58 = external global i8*
    @7 = global { i64, i8**, i64, i64, [12 x i8], i8 } { i64 3064, i8** @caml_startup__58, i64 -9, i64 3068, [12 x i8] c"End_of_file\00", i8 4 }
    @caml_exn_End_of_file = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [12 x i8], i8 }, { i64, i8**, i64, i64, [12 x i8], i8 }* @7, i32 0, i32 1) to i8*)
    @caml_startup__58.5 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [12 x i8], i8 }, { i64, i8**, i64, i64, [12 x i8], i8 }* @7, i32 0, i32 4, i32 0)
    @caml_startup__59 = external global i8*
    @8 = global { i64, i8**, i64, i64, [17 x i8], i8 } { i64 3064, i8** @caml_startup__59, i64 -11, i64 4092, [17 x i8] c"Division_by_zero\00", i8 7 }
    @caml_exn_Division_by_zero = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [17 x i8], i8 }, { i64, i8**, i64, i64, [17 x i8], i8 }* @8, i32 0, i32 1) to i8*)
    @caml_startup__59.6 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [17 x i8], i8 }, { i64, i8**, i64, i64, [17 x i8], i8 }* @8, i32 0, i32 4, i32 0)
    @caml_startup__60 = external global i8*
    @9 = global { i64, i8**, i64, i64, [10 x i8], i8 } { i64 3064, i8** @caml_startup__60, i64 -13, i64 3068, [10 x i8] c"Not_found\00", i8 6 }
    @caml_exn_Not_found = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [10 x i8], i8 }, { i64, i8**, i64, i64, [10 x i8], i8 }* @9, i32 0, i32 1) to i8*)
    @caml_startup__60.7 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [10 x i8], i8 }, { i64, i8**, i64, i64, [10 x i8], i8 }* @9, i32 0, i32 4, i32 0)
    @caml_startup__61 = external global i8*
    @10 = global { i64, i8**, i64, i64, [14 x i8], i8 } { i64 3064, i8** @caml_startup__61, i64 -15, i64 3068, [14 x i8] c"Match_failure\00", i8 2 }
    @caml_exn_Match_failure = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [14 x i8], i8 }, { i64, i8**, i64, i64, [14 x i8], i8 }* @10, i32 0, i32 1) to i8*)
    @caml_startup__61.8 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [14 x i8], i8 }, { i64, i8**, i64, i64, [14 x i8], i8 }* @10, i32 0, i32 4, i32 0)
    @caml_startup__62 = external global i8*
    @11 = global { i64, i8**, i64, i64, [15 x i8], i8 } { i64 3064, i8** @caml_startup__62, i64 -17, i64 3068, [15 x i8] c"Stack_overflow\00", i8 1 }
    @caml_exn_Stack_overflow = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @11, i32 0, i32 1) to i8*)
    @caml_startup__62.9 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @11, i32 0, i32 4, i32 0)
    @caml_startup__63 = external global i8*
    @12 = global { i64, i8**, i64, i64, [15 x i8], i8 } { i64 3064, i8** @caml_startup__63, i64 -19, i64 3068, [15 x i8] c"Sys_blocked_io\00", i8 1 }
    @caml_exn_Sys_blocked_io = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @12, i32 0, i32 1) to i8*)
    @caml_startup__63.10 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @12, i32 0, i32 4, i32 0)
    @caml_startup__64 = external global i8*
    @13 = global { i64, i8**, i64, i64, [15 x i8], i8 } { i64 3064, i8** @caml_startup__64, i64 -21, i64 3068, [15 x i8] c"Assert_failure\00", i8 1 }
    @caml_exn_Assert_failure = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @13, i32 0, i32 1) to i8*)
    @caml_startup__64.11 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @13, i32 0, i32 4, i32 0)
    @caml_startup__65 = external global i8*
    @14 = global { i64, i8**, i64, i64, [27 x i8], i8 } { i64 3064, i8** @caml_startup__65, i64 -23, i64 5116, [27 x i8] c"Undefined_recursive_module\00", i8 5 }
    @caml_exn_Undefined_recursive_module = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [27 x i8], i8 }, { i64, i8**, i64, i64, [27 x i8], i8 }* @14, i32 0, i32 1) to i8*)
    @caml_startup__65.12 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [27 x i8], i8 }, { i64, i8**, i64, i64, [27 x i8], i8 }* @14, i32 0, i32 4, i32 0)
    @15 = global { i8**, i64 } { i8** @camlMelse__gc_roots, i64 0 }
    @caml_globals = global i8* bitcast ({ i8**, i64 }* @15 to i8*)
    @caml_startup__data_begin = external global i8*
    @caml_startup__data_end = external global i8*
    @camlMelse__data_begin = external global i8*
    @camlMelse__data_end = external global i8*
    @16 = global { i8**, i8**, i8**, i8**, i64 } { i8** @caml_startup__data_begin, i8** @caml_startup__data_end, i8** @camlMelse__data_begin, i8** @camlMelse__data_end, i64 0 }
    @caml_data_segments = global i8* bitcast ({ i8**, i8**, i8**, i8**, i64 }* @16 to i8*)
    @caml_startup__code_begin = external global i8*
    @caml_startup__code_end = external global i8*
    @camlMelse__code_begin = external global i8*
    @camlMelse__code_end = external global i8*
    @17 = global { i8**, i8**, i8**, i8**, i64 } { i8** @caml_startup__code_begin, i8** @caml_startup__code_end, i8** @camlMelse__code_begin, i8** @camlMelse__code_end, i64 0 }
    @caml_code_segments = global i8* bitcast ({ i8**, i8**, i8**, i8**, i64 }* @17 to i8*)
    @caml_startup__frametable = external global i8*
    @caml_system__frametable = external global i8*
    @camlMelse__frametable = external global i8*
    @18 = global { i8**, i8**, i8**, i64 } { i8** @caml_startup__frametable, i8** @caml_system__frametable, i8** @camlMelse__frametable, i64 0 }
    @caml_frametable = global i8* bitcast ({ i8**, i8**, i8**, i64 }* @18 to i8*)
    @caml_globals_inited = external global i8*

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
      %0 = alloca i8*
      store i8* bitcast (i8** @camlMelse__53 to i8*), i8** %0
      %1 = load i8*, i8** %0
      store i8* %1, i8** @camlMelse
      ret i8* inttoptr (i64 1 to i8*)
    }

    define ghccc i64 @caml_program() {
    entry:
      call ghccc void bitcast (i8* ()* @camlMelse__entry to void ()*)()
      %0 = load i64, i64* bitcast (i8** @caml_globals_inited to i64*)
      %1 = add i64 %0, 1
      store i64 %1, i64* bitcast (i8** @caml_globals_inited to i64*)
      ret i64 1
    }

    define ghccc i8* @caml_apply3(i8* %arg, i8* %arg1, i8* %arg2, i8* %clos) {
    entry:
      %0 = getelementptr i8, i8* %clos, i64 8
      %1 = bitcast i8* %0 to i8**
      %2 = load i8*, i8** %1
      %3 = ptrtoint i8* %2 to i64
      %4 = sub i64 %3, 7
      %5 = sdiv exact i64 %4, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64)
      %6 = icmp eq i64 %5, 0
      br i1 %6, label %then, label %else

    then:                                             ; preds = %entry
      %7 = getelementptr i8, i8* %clos, i64 16
      %8 = bitcast i8* %7 to i8**
      %9 = load i8*, i8** %8
      %func_cast = bitcast i8* %9 to i8* (i8*, i8*, i8*, i8*)*
      %10 = call ghccc i8* %func_cast(i8* %arg, i8* %arg, i8* %arg, i8* %clos)
      br label %merge

    else:                                             ; preds = %entry
      %11 = bitcast i8* %clos to i8**
      %12 = load i8*, i8** %11
      %func_cast3 = bitcast i8* %12 to i8* (i8*, i8*)*
      %13 = call ghccc i8* %func_cast3(i8* %arg, i8* %clos)
      %14 = alloca i8*
      store i8* %13, i8** %14
      %15 = load i8*, i8** %14
      %16 = bitcast i8* %15 to i8**
      %17 = load i8*, i8** %16
      %18 = load i8*, i8** %14
      %func_cast4 = bitcast i8* %17 to i8* (i8*, i8*)*
      %19 = call ghccc i8* %func_cast4(i8* %arg, i8* %18)
      %20 = alloca i8*
      store i8* %19, i8** %20
      %21 = load i8*, i8** %20
      %22 = bitcast i8* %21 to i8**
      %23 = load i8*, i8** %22
      %24 = load i8*, i8** %20
      %func_cast5 = bitcast i8* %23 to i8* (i8*, i8*)*
      %25 = call ghccc i8* %func_cast5(i8* %arg, i8* %24)
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ %25, %else ], [ %10, %then ]
      ret i8* %iftmp
    }

    define ghccc i8* @caml_apply2(i8* %arg, i8* %arg1, i8* %clos) {
    entry:
      %0 = getelementptr i8, i8* %clos, i64 8
      %1 = bitcast i8* %0 to i8**
      %2 = load i8*, i8** %1
      %3 = ptrtoint i8* %2 to i64
      %4 = sub i64 %3, 5
      %5 = sdiv exact i64 %4, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64)
      %6 = icmp eq i64 %5, 0
      br i1 %6, label %then, label %else

    then:                                             ; preds = %entry
      %7 = getelementptr i8, i8* %clos, i64 16
      %8 = bitcast i8* %7 to i8**
      %9 = load i8*, i8** %8
      %func_cast = bitcast i8* %9 to i8* (i8*, i8*, i8*)*
      %10 = call ghccc i8* %func_cast(i8* %arg, i8* %arg, i8* %clos)
      br label %merge

    else:                                             ; preds = %entry
      %11 = bitcast i8* %clos to i8**
      %12 = load i8*, i8** %11
      %func_cast2 = bitcast i8* %12 to i8* (i8*, i8*)*
      %13 = call ghccc i8* %func_cast2(i8* %arg, i8* %clos)
      %14 = alloca i8*
      store i8* %13, i8** %14
      %15 = load i8*, i8** %14
      %16 = bitcast i8* %15 to i8**
      %17 = load i8*, i8** %16
      %18 = load i8*, i8** %14
      %func_cast3 = bitcast i8* %17 to i8* (i8*, i8*)*
      %19 = call ghccc i8* %func_cast3(i8* %arg, i8* %18)
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ %19, %else ], [ %10, %then ]
      ret i8* %iftmp
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
    ("function args" (name camlMelse__create_84)
     (args ((x (Value "i8* %x")) (y (Value "i8* %y")) (z (Value "i8* %z")))))
    ("function args" (name camlMelse__entry) (args ()))
    ("function args" (name caml_program) (args ()))
    ("function args" (name caml_apply3)
     (args
      ((arg (Value "i8* %arg")) (arg1 (Value "i8* %arg1"))
       (arg2 (Value "i8* %arg2")) (clos (Value "i8* %clos")))))
    ("function args" (name caml_apply2)
     (args
      ((arg (Value "i8* %arg")) (arg1 (Value "i8* %arg1"))
       (clos (Value "i8* %clos")))))
    ; ModuleID = 'melse'
    source_filename = "melse"
    target triple = "x86_64-apple-macosx10.15.0"

    @caml_curry3 = external global i8*
    @0 = global { i64, i8**, i64, i8* (i8*, i8*, i8*)* } { i64 4087, i8** @caml_curry3, i64 7, i8* (i8*, i8*, i8*)* @camlMelse__create_84 }
    @camlMelse__66 = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i8* (i8*, i8*, i8*)* }, { i64, i8**, i64, i8* (i8*, i8*, i8*)* }* @0, i32 0, i32 1) to i8*)
    @1 = global { i64, i64 } { i64 1792, i64 1 }
    @camlMelse = global i8* bitcast (i64* getelementptr inbounds ({ i64, i64 }, { i64, i64 }* @1, i32 0, i32 1) to i8*)
    @2 = global { i8**, i64 } { i8** @camlMelse, i64 0 }
    @camlMelse__gc_roots = global i8* bitcast ({ i8**, i64 }* @2 to i8*)
    @caml_startup__67 = external global i8*
    @3 = global { i64, i8**, i64, i64, [14 x i8], i8 } { i64 3064, i8** @caml_startup__67, i64 -1, i64 3068, [14 x i8] c"Out_of_memory\00", i8 2 }
    @caml_exn_Out_of_memory = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [14 x i8], i8 }, { i64, i8**, i64, i64, [14 x i8], i8 }* @3, i32 0, i32 1) to i8*)
    @caml_startup__67.1 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [14 x i8], i8 }, { i64, i8**, i64, i64, [14 x i8], i8 }* @3, i32 0, i32 4, i32 0)
    @caml_startup__68 = external global i8*
    @4 = global { i64, i8**, i64, i64, [10 x i8], i8 } { i64 3064, i8** @caml_startup__68, i64 -3, i64 3068, [10 x i8] c"Sys_error\00", i8 6 }
    @caml_exn_Sys_error = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [10 x i8], i8 }, { i64, i8**, i64, i64, [10 x i8], i8 }* @4, i32 0, i32 1) to i8*)
    @caml_startup__68.2 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [10 x i8], i8 }, { i64, i8**, i64, i64, [10 x i8], i8 }* @4, i32 0, i32 4, i32 0)
    @caml_startup__69 = external global i8*
    @5 = global { i64, i8**, i64, i64, [8 x i8], i8 } { i64 3064, i8** @caml_startup__69, i64 -5, i64 2044, [8 x i8] c"Failure\00", i8 0 }
    @caml_exn_Failure = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [8 x i8], i8 }, { i64, i8**, i64, i64, [8 x i8], i8 }* @5, i32 0, i32 1) to i8*)
    @caml_startup__69.3 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [8 x i8], i8 }, { i64, i8**, i64, i64, [8 x i8], i8 }* @5, i32 0, i32 4, i32 0)
    @caml_startup__70 = external global i8*
    @6 = global { i64, i8**, i64, i64, [17 x i8], i8 } { i64 3064, i8** @caml_startup__70, i64 -7, i64 4092, [17 x i8] c"Invalid_argument\00", i8 7 }
    @caml_exn_Invalid_argument = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [17 x i8], i8 }, { i64, i8**, i64, i64, [17 x i8], i8 }* @6, i32 0, i32 1) to i8*)
    @caml_startup__70.4 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [17 x i8], i8 }, { i64, i8**, i64, i64, [17 x i8], i8 }* @6, i32 0, i32 4, i32 0)
    @caml_startup__71 = external global i8*
    @7 = global { i64, i8**, i64, i64, [12 x i8], i8 } { i64 3064, i8** @caml_startup__71, i64 -9, i64 3068, [12 x i8] c"End_of_file\00", i8 4 }
    @caml_exn_End_of_file = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [12 x i8], i8 }, { i64, i8**, i64, i64, [12 x i8], i8 }* @7, i32 0, i32 1) to i8*)
    @caml_startup__71.5 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [12 x i8], i8 }, { i64, i8**, i64, i64, [12 x i8], i8 }* @7, i32 0, i32 4, i32 0)
    @caml_startup__72 = external global i8*
    @8 = global { i64, i8**, i64, i64, [17 x i8], i8 } { i64 3064, i8** @caml_startup__72, i64 -11, i64 4092, [17 x i8] c"Division_by_zero\00", i8 7 }
    @caml_exn_Division_by_zero = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [17 x i8], i8 }, { i64, i8**, i64, i64, [17 x i8], i8 }* @8, i32 0, i32 1) to i8*)
    @caml_startup__72.6 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [17 x i8], i8 }, { i64, i8**, i64, i64, [17 x i8], i8 }* @8, i32 0, i32 4, i32 0)
    @caml_startup__73 = external global i8*
    @9 = global { i64, i8**, i64, i64, [10 x i8], i8 } { i64 3064, i8** @caml_startup__73, i64 -13, i64 3068, [10 x i8] c"Not_found\00", i8 6 }
    @caml_exn_Not_found = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [10 x i8], i8 }, { i64, i8**, i64, i64, [10 x i8], i8 }* @9, i32 0, i32 1) to i8*)
    @caml_startup__73.7 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [10 x i8], i8 }, { i64, i8**, i64, i64, [10 x i8], i8 }* @9, i32 0, i32 4, i32 0)
    @caml_startup__74 = external global i8*
    @10 = global { i64, i8**, i64, i64, [14 x i8], i8 } { i64 3064, i8** @caml_startup__74, i64 -15, i64 3068, [14 x i8] c"Match_failure\00", i8 2 }
    @caml_exn_Match_failure = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [14 x i8], i8 }, { i64, i8**, i64, i64, [14 x i8], i8 }* @10, i32 0, i32 1) to i8*)
    @caml_startup__74.8 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [14 x i8], i8 }, { i64, i8**, i64, i64, [14 x i8], i8 }* @10, i32 0, i32 4, i32 0)
    @caml_startup__75 = external global i8*
    @11 = global { i64, i8**, i64, i64, [15 x i8], i8 } { i64 3064, i8** @caml_startup__75, i64 -17, i64 3068, [15 x i8] c"Stack_overflow\00", i8 1 }
    @caml_exn_Stack_overflow = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @11, i32 0, i32 1) to i8*)
    @caml_startup__75.9 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @11, i32 0, i32 4, i32 0)
    @caml_startup__76 = external global i8*
    @12 = global { i64, i8**, i64, i64, [15 x i8], i8 } { i64 3064, i8** @caml_startup__76, i64 -19, i64 3068, [15 x i8] c"Sys_blocked_io\00", i8 1 }
    @caml_exn_Sys_blocked_io = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @12, i32 0, i32 1) to i8*)
    @caml_startup__76.10 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @12, i32 0, i32 4, i32 0)
    @caml_startup__77 = external global i8*
    @13 = global { i64, i8**, i64, i64, [15 x i8], i8 } { i64 3064, i8** @caml_startup__77, i64 -21, i64 3068, [15 x i8] c"Assert_failure\00", i8 1 }
    @caml_exn_Assert_failure = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @13, i32 0, i32 1) to i8*)
    @caml_startup__77.11 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [15 x i8], i8 }, { i64, i8**, i64, i64, [15 x i8], i8 }* @13, i32 0, i32 4, i32 0)
    @caml_startup__78 = external global i8*
    @14 = global { i64, i8**, i64, i64, [27 x i8], i8 } { i64 3064, i8** @caml_startup__78, i64 -23, i64 5116, [27 x i8] c"Undefined_recursive_module\00", i8 5 }
    @caml_exn_Undefined_recursive_module = global i8* bitcast (i8*** getelementptr inbounds ({ i64, i8**, i64, i64, [27 x i8], i8 }, { i64, i8**, i64, i64, [27 x i8], i8 }* @14, i32 0, i32 1) to i8*)
    @caml_startup__78.12 = global i8* getelementptr inbounds ({ i64, i8**, i64, i64, [27 x i8], i8 }, { i64, i8**, i64, i64, [27 x i8], i8 }* @14, i32 0, i32 4, i32 0)
    @15 = global { i8**, i64 } { i8** @camlMelse__gc_roots, i64 0 }
    @caml_globals = global i8* bitcast ({ i8**, i64 }* @15 to i8*)
    @caml_startup__data_begin = external global i8*
    @caml_startup__data_end = external global i8*
    @camlMelse__data_begin = external global i8*
    @camlMelse__data_end = external global i8*
    @16 = global { i8**, i8**, i8**, i8**, i64 } { i8** @caml_startup__data_begin, i8** @caml_startup__data_end, i8** @camlMelse__data_begin, i8** @camlMelse__data_end, i64 0 }
    @caml_data_segments = global i8* bitcast ({ i8**, i8**, i8**, i8**, i64 }* @16 to i8*)
    @caml_startup__code_begin = external global i8*
    @caml_startup__code_end = external global i8*
    @camlMelse__code_begin = external global i8*
    @camlMelse__code_end = external global i8*
    @17 = global { i8**, i8**, i8**, i8**, i64 } { i8** @caml_startup__code_begin, i8** @caml_startup__code_end, i8** @camlMelse__code_begin, i8** @camlMelse__code_end, i64 0 }
    @caml_code_segments = global i8* bitcast ({ i8**, i8**, i8**, i8**, i64 }* @17 to i8*)
    @caml_startup__frametable = external global i8*
    @caml_system__frametable = external global i8*
    @camlMelse__frametable = external global i8*
    @18 = global { i8**, i8**, i8**, i64 } { i8** @caml_startup__frametable, i8** @caml_system__frametable, i8** @camlMelse__frametable, i64 0 }
    @caml_frametable = global i8* bitcast ({ i8**, i8**, i8**, i64 }* @18 to i8*)
    @caml_globals_inited = external global i8*

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
      %0 = alloca i8*
      store i8* bitcast (i8** @camlMelse__66 to i8*), i8** %0
      %1 = load i8*, i8** %0
      store i8* %1, i8** @camlMelse
      ret i8* inttoptr (i64 1 to i8*)
    }

    define ghccc i64 @caml_program() {
    entry:
      call ghccc void bitcast (i8* ()* @camlMelse__entry to void ()*)()
      %0 = load i64, i64* bitcast (i8** @caml_globals_inited to i64*)
      %1 = add i64 %0, 1
      store i64 %1, i64* bitcast (i8** @caml_globals_inited to i64*)
      ret i64 1
    }

    define ghccc i8* @caml_apply3(i8* %arg, i8* %arg1, i8* %arg2, i8* %clos) {
    entry:
      %0 = getelementptr i8, i8* %clos, i64 8
      %1 = bitcast i8* %0 to i8**
      %2 = load i8*, i8** %1
      %3 = ptrtoint i8* %2 to i64
      %4 = sub i64 %3, 7
      %5 = sdiv exact i64 %4, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64)
      %6 = icmp eq i64 %5, 0
      br i1 %6, label %then, label %else

    then:                                             ; preds = %entry
      %7 = getelementptr i8, i8* %clos, i64 16
      %8 = bitcast i8* %7 to i8**
      %9 = load i8*, i8** %8
      %func_cast = bitcast i8* %9 to i8* (i8*, i8*, i8*, i8*)*
      %10 = call ghccc i8* %func_cast(i8* %arg, i8* %arg, i8* %arg, i8* %clos)
      br label %merge

    else:                                             ; preds = %entry
      %11 = bitcast i8* %clos to i8**
      %12 = load i8*, i8** %11
      %func_cast3 = bitcast i8* %12 to i8* (i8*, i8*)*
      %13 = call ghccc i8* %func_cast3(i8* %arg, i8* %clos)
      %14 = alloca i8*
      store i8* %13, i8** %14
      %15 = load i8*, i8** %14
      %16 = bitcast i8* %15 to i8**
      %17 = load i8*, i8** %16
      %18 = load i8*, i8** %14
      %func_cast4 = bitcast i8* %17 to i8* (i8*, i8*)*
      %19 = call ghccc i8* %func_cast4(i8* %arg, i8* %18)
      %20 = alloca i8*
      store i8* %19, i8** %20
      %21 = load i8*, i8** %20
      %22 = bitcast i8* %21 to i8**
      %23 = load i8*, i8** %22
      %24 = load i8*, i8** %20
      %func_cast5 = bitcast i8* %23 to i8* (i8*, i8*)*
      %25 = call ghccc i8* %func_cast5(i8* %arg, i8* %24)
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ %25, %else ], [ %10, %then ]
      ret i8* %iftmp
    }

    define ghccc i8* @caml_apply2(i8* %arg, i8* %arg1, i8* %clos) {
    entry:
      %0 = getelementptr i8, i8* %clos, i64 8
      %1 = bitcast i8* %0 to i8**
      %2 = load i8*, i8** %1
      %3 = ptrtoint i8* %2 to i64
      %4 = sub i64 %3, 5
      %5 = sdiv exact i64 %4, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64)
      %6 = icmp eq i64 %5, 0
      br i1 %6, label %then, label %else

    then:                                             ; preds = %entry
      %7 = getelementptr i8, i8* %clos, i64 16
      %8 = bitcast i8* %7 to i8**
      %9 = load i8*, i8** %8
      %func_cast = bitcast i8* %9 to i8* (i8*, i8*, i8*)*
      %10 = call ghccc i8* %func_cast(i8* %arg, i8* %arg, i8* %clos)
      br label %merge

    else:                                             ; preds = %entry
      %11 = bitcast i8* %clos to i8**
      %12 = load i8*, i8** %11
      %func_cast2 = bitcast i8* %12 to i8* (i8*, i8*)*
      %13 = call ghccc i8* %func_cast2(i8* %arg, i8* %clos)
      %14 = alloca i8*
      store i8* %13, i8** %14
      %15 = load i8*, i8** %14
      %16 = bitcast i8* %15 to i8**
      %17 = load i8*, i8** %16
      %18 = load i8*, i8** %14
      %func_cast3 = bitcast i8* %17 to i8* (i8*, i8*)*
      %19 = call ghccc i8* %func_cast3(i8* %arg, i8* %18)
      br label %merge

    merge:                                            ; preds = %else, %then
      %iftmp = phi i8* [ %19, %else ], [ %10, %then ]
      ret i8* %iftmp
    }

    attributes #0 = { nounwind } |}]
;;
