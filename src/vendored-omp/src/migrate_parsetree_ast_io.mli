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

(** A marshalled ast packs the ast with the corresponding version of the
    frontend *)
type ast =
  | Impl : (module Migrate_parsetree_versions.OCaml_version with
             type Ast.Parsetree.structure = 'concrete) * 'concrete -> ast
  | Intf : (module Migrate_parsetree_versions.OCaml_version with
             type Ast.Parsetree.signature = 'concrete) * 'concrete -> ast

(** A simple alias used for the filename of the source that produced an AST *)
type filename = string

type read_error =
  | Not_a_binary_ast of string
  (** The input doesn't contain a binary AST. The argument corresponds
      to the bytes from the input that were consumed. *)
  | Unknown_version of string
  (** The input contains a binary AST for an unknown version of OCaml.
      The argument is the unknown magic number. *)

(** Load a marshalled AST from a channel

    Any exception raised during unmarshalling (see [Marshal]) can escape.  *)
val from_channel : in_channel -> (filename * ast, read_error) result

(** Load a marshalled AST from a byte string.

    See [from_channel] description for exception that can be raised. *)
val from_bytes : bytes -> int -> (filename * ast, read_error) result

(** Marshal an AST to a channel *)
val to_channel : out_channel -> filename -> ast -> unit

(** Marshal an AST to a byte string *)
val to_bytes : filename -> ast -> bytes
