
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

let implementation version =
  let { copy_structure; _ } = migrate ocaml_current version in
  fun lexbuf -> copy_structure (Parse.implementation lexbuf)

let interface version =
  let { copy_signature; _ } = migrate ocaml_current version in
  fun lexbuf -> copy_signature (Parse.interface lexbuf)

let toplevel_phrase version =
  let { copy_toplevel_phrase; _ } = migrate ocaml_current version in
  fun lexbuf -> copy_toplevel_phrase (Parse.toplevel_phrase lexbuf)

let use_file version =
  let { copy_toplevel_phrase; _ } = migrate ocaml_current version in
  fun lexbuf -> List.map copy_toplevel_phrase (Parse.use_file lexbuf)

let core_type version =
  let { copy_core_type; _ } = migrate ocaml_current version in
  fun lexbuf -> copy_core_type (Parse.core_type lexbuf)

let expression version =
  let { copy_expression; _ } = migrate ocaml_current version in
  fun lexbuf -> copy_expression (Parse.expression lexbuf)

let pattern version =
  let { copy_pattern; _ } = migrate ocaml_current version in
  fun lexbuf -> copy_pattern (Parse.pattern lexbuf)
