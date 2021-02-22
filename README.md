# Llambda: An LLVM backend for OCaml

Big hairy audacious goal: interop with the existing flambda/lambda OCaml
compilers for optimising straight-line memory-access-intensive code that could
benefit from LLVM's peephole optimisations.

```ocaml
let[@llvm] memcpy (src : Iobuf.t) (dst : Iobuf.t) =
    let size = Int.min (Iobuf.length src) (Iobuf.length dst) in
    for i = 0 to size - 1 do
        Iobuf.Unsafe.Poke.char dst ~pos:i (Iobuf.Unsafe.Peek.char src ~pos:i)
    done
;;

let () =
    let x = Iobuf.of_string "hello, world" in
    let y = Iobuf.create ~len:(Iobuf.length x) in
    memcpy x y;
    print_s [%message "Copied by llambda" (y : (_, _) Iobuf.Hexdump.t)]
;;
```

The OCaml compiler doesn't currently do a great job of optimising functions like
this -- hopefully llambda would allow us to take advantage of e.g. AVX
instructions, loop unrolling to make this much faster than it would otherwise
be. Hopefully these benefits should extend to OCaml code more broadly.

## Plan

For now, just compile whole OCaml files, don't try to do this fancy thing with
attributes attached to functions.

Use ocaml-compilers-lib to parse and type the input source code, then pass it
through clambda or flambda to produce c--. Compile directly from c-- to LLVM.

- [x] Integer constants, floating-point constants
- [x] Immutable values
- [x] Mutable values
- [x] if/then/else
- [x] for loops & while loops
- [x] recursive function calls?
- [x] closures?
- [x] C function calls
- [x] Calls to OCaml functions compiled with llambda
- [x] Use GHC's calling convention for external-facing functions (similar to
  OCaml's), maybe use fastcc for internal things <- unfortunately, GHC's calling
  convention is wrong...
- [ ] exception handling
- [x] raise exceptions
- [x] handle allocations properly (GC segfaults right now)
- [ ] figure out how to make tests stable across platforms (right now we end up
  with subtly different test output on Linux vs. macos)

## LLVM changes

- [x] Need to add an OCaml calling convention
- [x] Some way of avoiding using R15 (and R14?)
- [x] Use the fork of LLVM in the submodule (and the ocaml bindings via dune)

## Building LLVM & OCaml bindings

This is tested on macos and Linux, with OCaml 4.11.1 with and without flambda.
You may need `-DBUILD_SHARED_LIBS=On` if you don't have enough RAM to statically
link libLLVM.

### Build LLVM:

```bash
cd external/llvm/llvm-project
mkdir build
cd build
cmake -G Ninja -DLLVM_ENABLE_PROJECTS='' -DCMAKE_BUILD_TYPE=Debug \
      -DLLVM_ENABLE_ASSERTIONS=On -DLLVM_TARGETS_TO_BUILD="X86" ../llvm
ninja # this is a good time to go and get some lunch
```

### Set up bindings

```bash
cd external/llvm
./setup.sh
dune build
```

## Potential optimisations

- If we can tag variables as being integers (in 2n+1) form, we can have a custom
  pass based on instcombine that assumes that the value begins with an |1
  operation.
- More accurate types for global variables (i.e. not i8*)
- Fewer `inttoptr`s, since they just make other optimisations harder. Emit GEPs
  for addi when the first element is an LLVM pointer type (I think if we ever do
  multiplications etc. we know that it's an integer anyway). <- maybe?
- Try with flambda
