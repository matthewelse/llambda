open Core
module B = Backend_intf
include Ocaml_shadow
include Llvm
include Wrap_llvm
module Backend_intf = B

module Cmm = struct
  include Ocaml_optcomp.Cmm

  let sexp_of_phrase t =
    let s =
      Ocaml_optcomp.Printcmm.phrase Format.str_formatter t;
      Format.flush_str_formatter ()
    in
    Sexp.Atom s
  ;;

  let sexp_of_expression t =
    let s =
      Ocaml_optcomp.Printcmm.expression Format.str_formatter t;
      Format.flush_str_formatter ()
    in
    Sexp.Atom s
  ;;

  let sexp_of_operation t =
    let s = Ocaml_optcomp.Printcmm.operation Ocaml_common.Debuginfo.none t in
    Sexp.Atom s
  ;;

  let sexp_of_machtype_component t =
    let s =
      Ocaml_optcomp.Printcmm.machtype_component Format.str_formatter t;
      Format.flush_str_formatter ()
    in
    Sexp.Atom s
  ;;
end

module Cmmgen = Ocaml_optcomp.Cmmgen
