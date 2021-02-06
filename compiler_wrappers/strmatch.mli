(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation of string matching from closed lambda to C-- *)

module type I = sig
  val string_block_length : Wrap_cmm.expression -> Wrap_cmm.expression

  val transl_switch
    :  Debuginfo.t
    -> Wrap_cmm.expression
    -> int
    -> int
    -> (int * Wrap_cmm.expression) list
    -> Wrap_cmm.expression
    -> Wrap_cmm.expression
end

module Make (_ : I) : sig
  (* Compile stringswitch (arg,cases,d)
       Note: cases should not contain string duplicates *)
  val compile
    :  Debuginfo.t
    -> Wrap_cmm.expression (* arg *)
    -> Wrap_cmm.expression option (* d *)
    -> (string * Wrap_cmm.expression) list (* cases *)
    -> Wrap_cmm.expression
end
