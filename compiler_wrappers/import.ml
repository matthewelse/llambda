open! Core

module Targetint = struct
  include Targetint

  let sexp_of_t t = to_string t |> String.sexp_of_t
end

module Backend_var = struct
  include Backend_var

  let sexp_of_t t = name t |> String.sexp_of_t

  module With_provenance = struct
    include With_provenance

    let sexp_of_t t = name t |> String.sexp_of_t
  end
end

module Asttypes = struct
  type nonrec private_flag = Asttypes.private_flag =
    | Private
    | Public
  [@@deriving sexp_of]

  type nonrec mutable_flag = Asttypes.mutable_flag =
    | Immutable
    | Mutable
  [@@deriving sexp_of]
end

module Lambda = struct
  include Lambda

  type nonrec initialization_or_assignment = initialization_or_assignment =
    | Assignment
    (* Initialization of in heap values, like [caml_initialize] C primitive.  The
     field should not have been read before and initialization should happen
     only once. *)
    | Heap_initialization
    (* Initialization of roots only. Compiles to a simple store.
     No checks are done to preserve GC invariants.  *)
    | Root_initialization
  [@@deriving sexp_of]

  type nonrec raise_kind = raise_kind =
    | Raise_regular
    | Raise_reraise
    | Raise_notrace
  [@@deriving sexp_of]
end

module Debuginfo = struct
  include Debuginfo

  module Scoped_location = struct
    include Scoped_location

    let sexp_of_scopes t = string_of_scopes t |> String.sexp_of_t
  end

  type nonrec item = item = private
    { dinfo_file : string
    ; dinfo_line : int
    ; dinfo_char_start : int
    ; dinfo_char_end : int
    ; dinfo_start_bol : int
    ; dinfo_end_bol : int
    ; dinfo_end_line : int
    ; dinfo_scopes : Scoped_location.scopes
    }
  [@@deriving sexp_of]

  type t = item list [@@deriving sexp_of]
end
