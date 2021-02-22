open Core
module B = Backend_intf
include Ocaml_shadow
include Llvm
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

  let sexp_of_machtype = [%sexp_of: machtype_component array]
end

module Backend_var = struct
  include Ocaml_optcomp.Backend_var

  module With_provenance = struct
    include With_provenance

    let sexp_of_t t = Sexp.Atom (unique_name (var t))
  end

  let sexp_of_t t = Sexp.Atom (unique_name t)
end

module Cmmgen = Ocaml_optcomp.Cmmgen

let sexp_of_lltype lltype : Sexp.t = Atom (string_of_lltype lltype)
let sexp_of_llvalue llvalue : Sexp.t =
  let value =  (string_of_llvalue llvalue) in
  [%message (value : string) ~typ:((type_of llvalue) : lltype)]
let sexp_of_llcontext (_ : llcontext) : Sexp.t = [%sexp `llcontext]

let sexp_of_llbasicblock block = sexp_of_llvalue (value_of_block block)
