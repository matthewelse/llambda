open Core
open Ppxlib
open Helpers

let%expect_test "try/with" =
  runtest
    ~show_functions:[ "f"; "g" ]
    [%str
      exception Sad_times of int

      let g x = if x = 0 then raise (Sad_times 0) else x * 10

      let f n m =
        let result = try g n with Sad_times res -> res in
        m * result
      ;;];
  [%expect
    {|
    (Ctrywith (expr "(app \"camlTest__g_20\" n/108 val)") (var exn_111)
     (handler
       "(if (== (load_mut val exn/111) \"camlTest__Pmakeblock_70\")\
      \n  (load_mut val (+a exn/111 8)) (raise_notrace exn/111))"))
    ("allocate stack space"
     (domain_state_ptr
      ((value "  %0 = inttoptr i64 %read_r15 to i8*") (typ i8*)))
     (domain_exn_ptr
      ((value "  %domain_exn_ptr = getelementptr inbounds i8, i8* %0, i64 16")
       (typ i8*))))
    ("done some more stuff"
     (prev_stack
      ((value "  %prev_stack = call i8* @llvm.stacksave()") (typ i8*)))
     (handler_ptr ((value "  %handler = alloca i8*, align 8") (typ i8**))))
    (build_call
     (llambda_setjmp
      ((value "declare i8* @llambda_setjmp(i8**, i8*)\n")
       (typ "i8* (i8**, i8*)*")))
     (handler_ptr ((value "  %handler = alloca i8*, align 8") (typ i8**)))
     (domain_exn_ptr
      ((value "  %domain_exn_ptr = getelementptr inbounds i8, i8* %0, i64 16")
       (typ i8*))))
    ("done even more stuff"
     (result
      ((value
        "  %result = call ocamlcc i8* @llambda_setjmp(i8** %handler, i8* %domain_exn_ptr)")
       (typ i8*))))
    define ocamlcc i8* @camlTest__f_XXX(i8* %n_108, i8* %m_107) gc "ocaml" {
    entry:
      %read_r15 = call i64 @llvm.read_register.i64(metadata !0)
      %0 = inttoptr i64 %read_r15 to i8*
      %domain_exn_ptr = getelementptr inbounds i8, i8* %0, i64 16
      %prev_stack = call i8* @llvm.stacksave()
      %handler = alloca i8*, align 8
      %old_handler = alloca i8*, align 8
      store i8* %domain_exn_ptr, i8** %old_handler, align 8
      %result = call ocamlcc i8* @llambda_setjmp(i8** %handler, i8* %domain_exn_ptr)
      %exception_was_raised = icmp eq i8* %result, null
      br i1 %exception_was_raised, label %body, label %handler1

    body:                                             ; preds = %entry
      %1 = call ocamlcc i8* @camlTest__g_20(i8* %n_108)
      br label %merge

    handler1:                                         ; preds = %entry
      call void @llvm.stackrestore(i8* %prev_stack)
      %load = bitcast i8* %result to i8**
      %2 = load i8*, i8** %load, align 8
      %3 = ptrtoint i8* %2 to i64
      %icmp = icmp eq i64 %3, ptrtoint (i8* @camlTest__Pmakeblock_70 to i64)
      %zext = zext i1 %icmp to i64
      %4 = trunc i64 %zext to i1
      br i1 %4, label %then, label %else

    merge:                                            ; preds = %merge3, %body
      %phi = phi i8* [ %iftmp, %merge3 ], [ %1, %body ]
      call void @llvm.stackrestore(i8* %prev_stack)
      %5 = ptrtoint i8* %m_107 to i64
      %binop = add i64 %5, -1
      %6 = ptrtoint i8* %phi to i64
      %binop4 = ashr i64 %6, 1
      %binop5 = mul i64 %binop, %binop4
      %binop6 = add i64 %binop5, 1
      %promote = inttoptr i64 %binop6 to i8*
      ret i8* %promote

    then:                                             ; preds = %handler1
      %7 = getelementptr i8, i8* %result, i64 8
      %load2 = bitcast i8* %7 to i8**
      %8 = load i8*, i8** %load2, align 8
      br label %merge3

    else:                                             ; preds = %handler1
      call ocamlcc void @llambda_raise_exn(i8* %result)
      unreachable

    merge3:                                           ; preds = %then
      %iftmp = phi i8* [ %8, %then ]
      br label %merge
    }

    define ocamlcc i8* @camlTest__g_XXX(i8* %x_98) gc "ocaml" {
    entry:
      %0 = ptrtoint i8* %x_98 to i64
      %icmp = icmp eq i64 %0, 1
      %zext = zext i1 %icmp to i64
      %1 = trunc i64 %zext to i1
      br i1 %1, label %then, label %else

    then:                                             ; preds = %entry
      call ocamlcc void @llambda_raise_exn(i8* @camlTest__simplify_fv_90)
      unreachable

    else:                                             ; preds = %entry
      %2 = ptrtoint i8* %x_98 to i64
      %binop = mul i64 %2, 10
      %binop1 = add i64 %binop, -9
      br label %merge

    merge:                                            ; preds = %else
      %iftmp = phi i64 [ %binop1, %else ]
      %promote = inttoptr i64 %iftmp to i8*
      ret i8* %promote
    } |}]
;;
