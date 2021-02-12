(** LLVM backend for OCaml *)

(* Want to get from a .ml file to cmm *)
module Emit_llvm = Emit_llvm
module Compat_driver = Compat_driver

module For_testing = struct
  module Decls = Decls
end
