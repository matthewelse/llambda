## LLVM Exception Handling

Add three intrinsics to LLVM, reflecting the machine pseudo-instructions used in
the OCaml compiler:

<!-- TODO: Add tests for these intrinsics -->

```
declare void @llvm.ocaml.push_handler(i8* tgt_block)

declare void @llvm.ocaml.pop_handler()

declare i8* @llvm.ocaml.landing_pad()
```

These are target-specific, so in X86, these should expand to:

1. `llvm.ocaml.push_handler`

```asm
mov {handler}, {scratch}
push {scratch}
push (r14 + 16) ; domain_state.domain_exn_pointer
```

2. `llvm.ocaml.pop_handler` 

```asm
pop (r14 + 16) ; domain_state.domain_exn_pointer
add $8,%rsp
```

3. `llvm.ocaml.landing_pad`

```asm
mov %rax,{result}
```

> I wonder to what extent these could just be implemented using inline assembly?

It seems like it may be possible to cheat, and implement a (very likely broken)
version of OCaml exception handling in inline assembly. I guess we'll see how
broken it is, and implement custom intrinsics later.

```llvm
define i64 @main(i64 %input) {
    %domain_ptr = call i64* @read_r14()
    %exn_ptr = getelementptr i64, i64* %domain_ptr, i32 2

    ; push handler
    callbr void asm "push ${1:c}; push ($0)", "r,X"(i64* %exn_ptr, i8* blockaddress(@main, %handler)) to label %fallthrough [label %handler]

handler:
    %exn = call i64 asm "", "={rax}"()
    ret i64 %exn

fallthrough:
    %result = call i64 @maybe_throw(i64 %input)

    ; pop handler
    call void asm "pop ($0); add $$8,%rsp", "r"(i64* %exn_ptr)

    ret i64 %result
}
```

This almost works, but we need to teach LLVM about the fact that we change the
stack pointer somehow.
