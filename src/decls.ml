open! Core
open! Import

module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Ocaml_optcomp.Compilenv.symbol_for_global'
  let closure_symbol = Ocaml_optcomp.Compilenv.closure_symbol
  let really_import_approx = Ocaml_optcomp.Import_approx.really_import_approx
  let import_symbol = Ocaml_optcomp.Import_approx.import_symbol
  let size_int = 8
  let big_endian = false (* TODO *)

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Ocaml_optcomp.Proc.max_arguments_for_tailcalls - 1
  ;;
end

let backend = (module Backend : Backend_intf.S)
