# Ideas

- [ ] Optimisations that use the fact that we know certain values are OCaml (2n+1) integers.
- [ ] Pass r14 and r15 as arguments to functions, and return them from functions. This would
  allow us to relax ocamlopt's use of r14 and r15 as global variables.
