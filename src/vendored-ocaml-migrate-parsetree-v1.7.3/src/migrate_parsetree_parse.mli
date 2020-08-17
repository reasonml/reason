(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Parser entry points that migrate to a specified version of OCaml.

   The parser used is the one from current compiler-libs.  The resulting AST is
   then converted to the desired version.

   These parsing functions can raise Migration_errors.
*)

open Migrate_parsetree_versions

val implementation  : 'types ocaml_version -> Lexing.lexbuf -> 'types get_structure
val interface       : 'types ocaml_version -> Lexing.lexbuf -> 'types get_signature
val toplevel_phrase : 'types ocaml_version -> Lexing.lexbuf -> 'types get_toplevel_phrase
val use_file        : 'types ocaml_version -> Lexing.lexbuf -> 'types get_toplevel_phrase list
val core_type       : 'types ocaml_version -> Lexing.lexbuf -> 'types get_core_type
val expression      : 'types ocaml_version -> Lexing.lexbuf -> 'types get_expression
val pattern         : 'types ocaml_version -> Lexing.lexbuf -> 'types get_pattern
