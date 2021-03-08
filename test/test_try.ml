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
    define ocamlcc i8* @camlTest__f_XXX(i8* %n_108, i8* %m_107) gc "ocaml" {
    entry:
      %read_r14 = call i64 @llvm.read_register.i64(metadata !0)
      %0 = inttoptr i64 %read_r14 to i8*
      %domain_exn_ptr = getelementptr inbounds i8, i8* %0, i64 16
      %result_109 = alloca i8*, align 8
      call void @llvm.gcroot(i8** %result_109, i8* null)
      callbr void asm sideeffect "lea $1(%rip),%r11; push %r11; push ($0); mov %rsp,($0)", "r,X,~{r11}"(i8* %domain_exn_ptr, i8* blockaddress(@camlTest__f_XXX, %handler))
              to label %body [label %handler]

    body:                                             ; preds = %entry
      %1 = call ocamlcc i8* @camlTest__g_20(i8* %n_108)
      call void asm sideeffect "pop ($0); add $$8,%rsp", "r"(i8* %domain_exn_ptr)
      br label %merge

    handler:                                          ; preds = %entry
      %exn = call i8* asm "", "={rax}"()
      %load = bitcast i8* %exn to i8**
      %2 = load i8*, i8** %load, align 8
      %3 = ptrtoint i8* %2 to i64
      %icmp = icmp eq i64 %3, ptrtoint (i8* @camlTest__Pmakeblock_70 to i64)
      %zext = zext i1 %icmp to i64
      %4 = trunc i64 %zext to i1
      br i1 %4, label %then, label %else

    merge:                                            ; preds = %merge4, %body
      %phi = phi i8* [ %iftmp, %merge4 ], [ %1, %body ]
      store i8* %phi, i8** %result_109, align 8
      %5 = ptrtoint i8* %m_107 to i64
      %binop = add i64 %5, -1
      %6 = load i8*, i8** %result_109, align 8
      %7 = ptrtoint i8* %6 to i64
      %binop5 = ashr i64 %7, 1
      %binop6 = mul i64 %binop, %binop5
      %binop7 = add i64 %binop6, 1
      %promote = inttoptr i64 %binop7 to i8*
      ret i8* %promote

    then:                                             ; preds = %handler
      %8 = getelementptr i8, i8* %exn, i64 8
      %load1 = bitcast i8* %8 to i8**
      %9 = load i8*, i8** %load1, align 8
      br label %merge4

    else:                                             ; preds = %handler
      %read_r142 = call i64 @llvm.read_register.i64(metadata !0)
      %10 = inttoptr i64 %read_r142 to i8*
      %domain_exn_ptr3 = getelementptr inbounds i8, i8* %10, i64 16
      call ocamlcc void asm sideeffect "movq ($1),%rsp; popq ($1); popq %r11; jmp *%r11", "{rax},r"(i8* %exn, i8* %domain_exn_ptr3)
      unreachable

    merge4:                                           ; preds = %then
      %iftmp = phi i8* [ %9, %then ]
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
      %read_r14 = call i64 @llvm.read_register.i64(metadata !0)
      %2 = inttoptr i64 %read_r14 to i8*
      %domain_exn_ptr = getelementptr inbounds i8, i8* %2, i64 16
      call ocamlcc void asm sideeffect "movq ($1),%rsp; popq ($1); popq %r11; jmp *%r11", "{rax},r"(i8* @camlTest__simplify_fv_90, i8* %domain_exn_ptr)
      unreachable

    else:                                             ; preds = %entry
      %3 = ptrtoint i8* %x_98 to i64
      %binop = mul i64 %3, 10
      %binop1 = add i64 %binop, -9
      br label %merge

    merge:                                            ; preds = %else
      %iftmp = phi i64 [ %binop1, %else ]
      %promote = inttoptr i64 %iftmp to i8*
      ret i8* %promote
    } |}]
;;
