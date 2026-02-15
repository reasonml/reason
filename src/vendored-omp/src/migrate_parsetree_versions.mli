(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                   Jérémie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*$ #use "src/cinaps_helpers" $*)

(** {1 Abstracting an OCaml frontend} *)

(** Abstract view of a version of an OCaml Ast *)
module type Ast = sig
  (*$ foreach_module (fun m types ->
      printf "module %s : sig\n" m;
      List.iter types ~f:(printf "type %s\n");
      printf "end\n"
    )
  *)
  module Outcometree : sig
    type out_value
    type out_type
    type out_class_type
    type out_module_type
    type out_sig_item
    type out_type_extension
    type out_phrase
  end
  (*$*)
end

(* Shortcuts for talking about ast types outside of the module language *)

type 'a _types = 'a constraint 'a
  = <
    (*$ foreach_type (fun _ s -> printf "%-21s : _;\n" s) *)
    out_value             : _;
    out_type              : _;
    out_class_type        : _;
    out_module_type       : _;
    out_sig_item          : _;
    out_type_extension    : _;
    out_phrase            : _;
    (*$*)
  >
;;

(*$ foreach_type (fun _ s ->
    printf "type 'a get_%s = 'x constraint 'a _types = < %s : 'x; .. >\n" s s
  );
  printf ";;\n" *)
type 'a get_out_value = 'x constraint 'a _types = < out_value : 'x; .. >
type 'a get_out_type = 'x constraint 'a _types = < out_type : 'x; .. >
type 'a get_out_class_type = 'x constraint 'a _types = < out_class_type : 'x; .. >
type 'a get_out_module_type = 'x constraint 'a _types = < out_module_type : 'x; .. >
type 'a get_out_sig_item = 'x constraint 'a _types = < out_sig_item : 'x; .. >
type 'a get_out_type_extension = 'x constraint 'a _types = < out_type_extension : 'x; .. >
type 'a get_out_phrase = 'x constraint 'a _types = < out_phrase : 'x; .. >
;;
(*$*)

(** A version of the OCaml frontend packs the ast with type witnesses
    so that equalities can be recovered dynamically. *)
type _ witnesses (*IF_AT_LEAST 406 = private ..*)

(** [migration_info] is an opaque type that is used to generate migration
    functions. *)
type _ migration_info

(** An OCaml frontend versions an Ast, version number and some witnesses for
    conversion. *)
module type OCaml_version = sig

  (** Ast definition for this version *)
  module Ast : Ast

  (* Version number as an integer, 402, 403, 404, ... *)
  val version : int

  (* Version number as a user-friendly string *)
  val string_version : string (* 4.02, 4.03, 4.04, ... *)

  (** Shortcut for talking about Ast types *)
  type types = <
    (*$ foreach_type (fun m s -> printf "%-21s : Ast.%s.%s;\n" s m s) *)
    out_value             : Ast.Outcometree.out_value;
    out_type              : Ast.Outcometree.out_type;
    out_class_type        : Ast.Outcometree.out_class_type;
    out_module_type       : Ast.Outcometree.out_module_type;
    out_sig_item          : Ast.Outcometree.out_sig_item;
    out_type_extension    : Ast.Outcometree.out_type_extension;
    out_phrase            : Ast.Outcometree.out_phrase;
    (*$*)
  > _types

  (** A construtor for recovering type equalities between two arbitrary
      versions. *)
  type _ witnesses += Version : types witnesses

  (** Information used to derive migration functions, see below *)
  val migration_info : types migration_info
end

(** Representing an ocaml version in type language *)
type 'types ocaml_version =
  (module OCaml_version
    (*$ let sep = with_then_and () in
      foreach_type (fun m s ->
          printf "%t type Ast.%s.%s = 'types get_%s\n" sep m s s) *)
    with type Ast.Outcometree.out_value = 'types get_out_value
     and type Ast.Outcometree.out_type = 'types get_out_type
     and type Ast.Outcometree.out_class_type = 'types get_out_class_type
     and type Ast.Outcometree.out_module_type = 'types get_out_module_type
     and type Ast.Outcometree.out_sig_item = 'types get_out_sig_item
     and type Ast.Outcometree.out_type_extension = 'types get_out_type_extension
     and type Ast.Outcometree.out_phrase = 'types get_out_phrase
     (*$*)
  )

(** {1 Concrete frontend instances} *)

(*$foreach_version (fun suffix _ ->
    printf "module OCaml_%s : OCaml_version with module Ast = Ast_%s\n"
      suffix suffix;
    printf "val ocaml_%s : OCaml_%s.types ocaml_version\n" suffix suffix;
  )*)
module OCaml_408 : OCaml_version with module Ast = Ast_408
val ocaml_408 : OCaml_408.types ocaml_version
module OCaml_409 : OCaml_version with module Ast = Ast_409
val ocaml_409 : OCaml_409.types ocaml_version
module OCaml_410 : OCaml_version with module Ast = Ast_410
val ocaml_410 : OCaml_410.types ocaml_version
module OCaml_411 : OCaml_version with module Ast = Ast_411
val ocaml_411 : OCaml_411.types ocaml_version
module OCaml_412 : OCaml_version with module Ast = Ast_412
val ocaml_412 : OCaml_412.types ocaml_version
module OCaml_413 : OCaml_version with module Ast = Ast_413
val ocaml_413 : OCaml_413.types ocaml_version
module OCaml_414 : OCaml_version with module Ast = Ast_414
val ocaml_414 : OCaml_414.types ocaml_version
module OCaml_500 : OCaml_version with module Ast = Ast_500
val ocaml_500 : OCaml_500.types ocaml_version
module OCaml_51 : OCaml_version with module Ast = Ast_51
val ocaml_51 : OCaml_51.types ocaml_version
module OCaml_52 : OCaml_version with module Ast = Ast_52
val ocaml_52 : OCaml_52.types ocaml_version
module OCaml_53 : OCaml_version with module Ast = Ast_53
val ocaml_53 : OCaml_53.types ocaml_version
module OCaml_54 : OCaml_version with module Ast = Ast_54
val ocaml_54 : OCaml_54.types ocaml_version
(*$*)

(* An alias to the current compiler version *)
module OCaml_current = OCaml_OCAML_VERSION
val ocaml_current : OCaml_current.types ocaml_version

val all_versions : (module OCaml_version) list

(** {1 Migrating between different versions} *)

type ('a, 'b) type_comparison =
  | Lt : ('a, 'b) type_comparison
  | Eq : ('a, 'a) type_comparison
  | Gt : ('a, 'b) type_comparison

val compare_ocaml_version : 'a ocaml_version -> 'b ocaml_version -> ('a, 'b) type_comparison

(** A record for migrating each AST construct between two known versions *)
type ('from, 'to_) migration_functions = {
  (*$ foreach_type (fun _ s ->
      printf "copy_%s: 'from get_%s -> 'to_ get_%s;\n" s s s) *)
  copy_out_value: 'from get_out_value -> 'to_ get_out_value;
  copy_out_type: 'from get_out_type -> 'to_ get_out_type;
  copy_out_class_type: 'from get_out_class_type -> 'to_ get_out_class_type;
  copy_out_module_type: 'from get_out_module_type -> 'to_ get_out_module_type;
  copy_out_sig_item: 'from get_out_sig_item -> 'to_ get_out_sig_item;
  copy_out_type_extension: 'from get_out_type_extension -> 'to_ get_out_type_extension;
  copy_out_phrase: 'from get_out_phrase -> 'to_ get_out_phrase;
  (*$*)
}

(** Migrating to the same version is no-op *)
val migration_identity : ('a, 'a) migration_functions

(** Migrations can be composed *)
val migration_compose : ('a, 'b) migration_functions -> ('b, 'c) migration_functions -> ('a, 'c) migration_functions

(** Represent the next or previous version of an Ast *)

type 'from immediate_migration =
  | No_migration : 'from immediate_migration
  (** Cannot migrate earliest or latest supported version *)
  |
    Immediate_migration :
      ('from, 'to_) migration_functions * 'to_ ocaml_version -> 'from immediate_migration
  (** Pack the migration functions and the new version *)

val immediate_migration : 'types ocaml_version -> [< `Next | `Previous ] -> 'types immediate_migration

val migrate : 'from ocaml_version -> 'to_ ocaml_version -> ('from, 'to_) migration_functions

(** {1 Convenience definitions} *)

(** Module level migration *)
module Convert (A : OCaml_version) (B : OCaml_version) : sig
  (*$ foreach_type (fun m s ->
      let fq = sprintf "%s.%s" m s in
      printf "  val copy_%-21s : A.Ast.%-31s -> B.Ast.%s\n" s fq fq) *)
  val copy_out_value             : A.Ast.Outcometree.out_value           -> B.Ast.Outcometree.out_value
  val copy_out_type              : A.Ast.Outcometree.out_type            -> B.Ast.Outcometree.out_type
  val copy_out_class_type        : A.Ast.Outcometree.out_class_type      -> B.Ast.Outcometree.out_class_type
  val copy_out_module_type       : A.Ast.Outcometree.out_module_type     -> B.Ast.Outcometree.out_module_type
  val copy_out_sig_item          : A.Ast.Outcometree.out_sig_item        -> B.Ast.Outcometree.out_sig_item
  val copy_out_type_extension    : A.Ast.Outcometree.out_type_extension  -> B.Ast.Outcometree.out_type_extension
  val copy_out_phrase            : A.Ast.Outcometree.out_phrase          -> B.Ast.Outcometree.out_phrase
  (*$*)
end
