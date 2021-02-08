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
- [ ] Calls to OCaml functions compiled with llambda
- [x] Use GHC's calling convention for external-facing functions (similar to
  OCaml's), maybe use fastcc for internal things <- unfortunately, GHC's calling
  convention is wrong...
- [ ] exception handling
- [ ] more expressive types for llvalues - distingui

## Issues with c--

It's hard to distinguish between integers and pointers in C--, it basically
doesn't have enough type information for something like LLVM.

## LLVM changes

- [x] Need to add an OCaml calling convention
- [ ] Some way of avoiding using R15 (and R14?)
- [ ] Use the fork of LLVM in the submodule (and the ocaml bindings via dune)

## Building LLVM & OCaml bindings

### Build LLVM:

```bash
cd external/llvm/llvm-project
mkdir build
cd build
cmake -G Ninja -DLLVM_ENABLE_PROJECTS='' -DCMAKE_BUILD_TYPE=Debug -DLLVM_ENABLE_ASSERTIONS=On -DLLVM_TARGETS_TO_BUILD="X86" ../llvm
ninja # this is a good time to go and get some lunch
```

### Set up bindings

```bash
cd external/llvm
./setup.sh
dune build
```

