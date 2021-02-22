# Llambda: An LLVM backend for OCaml

This is a crazy (and unimaginative) experiment to build an LLVM backend for
OCaml. It is intended to be correct enough to satisfy my need to see the right
numbers printed to standard output. It is (probably very) buggy and certainly
doesn't generate great code.

Big hairy audacious goal: interop with the existing flambda/lambda OCaml
compilers for optimising straight-line memory-access-intensive code that could
benefit from LLVM's peephole optimisations, as well as things like vector
instructions where possible.

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
