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
  module Parsetree : sig
    type structure
    type signature
    type toplevel_phrase
    type core_type
    type expression
    type pattern
    type case
    type type_declaration
    type type_extension
    type extension_constructor
  end
  module Outcometree : sig
    type out_value
    type out_type
    type out_class_type
    type out_module_type
    type out_sig_item
    type out_type_extension
    type out_phrase
  end
  module Ast_mapper : sig
    type mapper
  end
  (*$*)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
  val shallow_identity : Ast_mapper.mapper
  val map_signature : Ast_mapper.mapper -> Parsetree.signature -> Parsetree.signature
  val map_structure : Ast_mapper.mapper -> Parsetree.structure -> Parsetree.structure
  val make_top_mapper
    :  signature:(Parsetree.signature -> Parsetree.signature)
    -> structure:(Parsetree.structure -> Parsetree.structure)
    -> Ast_mapper.mapper
end

(* Shortcuts for talking about ast types outside of the module language *)

type 'a _types = 'a constraint 'a
  = <
    (*$ foreach_type (fun _ s -> printf "%-21s : _;\n" s) *)
    structure             : _;
    signature             : _;
    toplevel_phrase       : _;
    core_type             : _;
    expression            : _;
    pattern               : _;
    case                  : _;
    type_declaration      : _;
    type_extension        : _;
    extension_constructor : _;
    out_value             : _;
    out_type              : _;
    out_class_type        : _;
    out_module_type       : _;
    out_sig_item          : _;
    out_type_extension    : _;
    out_phrase            : _;
    mapper                : _;
    (*$*)
  >
;;

(*$ foreach_type (fun _ s ->
    printf "type 'a get_%s = 'x constraint 'a _types = < %s : 'x; .. >\n" s s
  );
  printf ";;\n" *)
type 'a get_structure = 'x constraint 'a _types = < structure : 'x; .. >
type 'a get_signature = 'x constraint 'a _types = < signature : 'x; .. >
type 'a get_toplevel_phrase = 'x constraint 'a _types = < toplevel_phrase : 'x; .. >
type 'a get_core_type = 'x constraint 'a _types = < core_type : 'x; .. >
type 'a get_expression = 'x constraint 'a _types = < expression : 'x; .. >
type 'a get_pattern = 'x constraint 'a _types = < pattern : 'x; .. >
type 'a get_case = 'x constraint 'a _types = < case : 'x; .. >
type 'a get_type_declaration = 'x constraint 'a _types = < type_declaration : 'x; .. >
type 'a get_type_extension = 'x constraint 'a _types = < type_extension : 'x; .. >
type 'a get_extension_constructor = 'x constraint 'a _types = < extension_constructor : 'x; .. >
type 'a get_out_value = 'x constraint 'a _types = < out_value : 'x; .. >
type 'a get_out_type = 'x constraint 'a _types = < out_type : 'x; .. >
type 'a get_out_class_type = 'x constraint 'a _types = < out_class_type : 'x; .. >
type 'a get_out_module_type = 'x constraint 'a _types = < out_module_type : 'x; .. >
type 'a get_out_sig_item = 'x constraint 'a _types = < out_sig_item : 'x; .. >
type 'a get_out_type_extension = 'x constraint 'a _types = < out_type_extension : 'x; .. >
type 'a get_out_phrase = 'x constraint 'a _types = < out_phrase : 'x; .. >
type 'a get_mapper = 'x constraint 'a _types = < mapper : 'x; .. >
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
    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
    out_value             : Ast.Outcometree.out_value;
    out_type              : Ast.Outcometree.out_type;
    out_class_type        : Ast.Outcometree.out_class_type;
    out_module_type       : Ast.Outcometree.out_module_type;
    out_sig_item          : Ast.Outcometree.out_sig_item;
    out_type_extension    : Ast.Outcometree.out_type_extension;
    out_phrase            : Ast.Outcometree.out_phrase;
    mapper                : Ast.Ast_mapper.mapper;
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
    with type Ast.Parsetree.structure = 'types get_structure
     and type Ast.Parsetree.signature = 'types get_signature
     and type Ast.Parsetree.toplevel_phrase = 'types get_toplevel_phrase
     and type Ast.Parsetree.core_type = 'types get_core_type
     and type Ast.Parsetree.expression = 'types get_expression
     and type Ast.Parsetree.pattern = 'types get_pattern
     and type Ast.Parsetree.case = 'types get_case
     and type Ast.Parsetree.type_declaration = 'types get_type_declaration
     and type Ast.Parsetree.type_extension = 'types get_type_extension
     and type Ast.Parsetree.extension_constructor = 'types get_extension_constructor
     and type Ast.Outcometree.out_value = 'types get_out_value
     and type Ast.Outcometree.out_type = 'types get_out_type
     and type Ast.Outcometree.out_class_type = 'types get_out_class_type
     and type Ast.Outcometree.out_module_type = 'types get_out_module_type
     and type Ast.Outcometree.out_sig_item = 'types get_out_sig_item
     and type Ast.Outcometree.out_type_extension = 'types get_out_type_extension
     and type Ast.Outcometree.out_phrase = 'types get_out_phrase
     and type Ast.Ast_mapper.mapper = 'types get_mapper
     (*$*)
  )

(** {1 Concrete frontend instances} *)

(*$foreach_version (fun suffix _ ->
    printf "module OCaml_%s : OCaml_version with module Ast = Ast_%s\n"
      suffix suffix;
    printf "val ocaml_%s : OCaml_%s.types ocaml_version\n" suffix suffix;
  )*)
module OCaml_402 : OCaml_version with module Ast = Ast_402
val ocaml_402 : OCaml_402.types ocaml_version
module OCaml_403 : OCaml_version with module Ast = Ast_403
val ocaml_403 : OCaml_403.types ocaml_version
module OCaml_404 : OCaml_version with module Ast = Ast_404
val ocaml_404 : OCaml_404.types ocaml_version
module OCaml_405 : OCaml_version with module Ast = Ast_405
val ocaml_405 : OCaml_405.types ocaml_version
module OCaml_406 : OCaml_version with module Ast = Ast_406
val ocaml_406 : OCaml_406.types ocaml_version
module OCaml_407 : OCaml_version with module Ast = Ast_407
val ocaml_407 : OCaml_407.types ocaml_version
module OCaml_408 : OCaml_version with module Ast = Ast_408
val ocaml_408 : OCaml_408.types ocaml_version
module OCaml_409 : OCaml_version with module Ast = Ast_409
val ocaml_409 : OCaml_409.types ocaml_version
module OCaml_410 : OCaml_version with module Ast = Ast_410
val ocaml_410 : OCaml_410.types ocaml_version
module OCaml_411 : OCaml_version with module Ast = Ast_411
val ocaml_411 : OCaml_411.types ocaml_version
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
  copy_structure: 'from get_structure -> 'to_ get_structure;
  copy_signature: 'from get_signature -> 'to_ get_signature;
  copy_toplevel_phrase: 'from get_toplevel_phrase -> 'to_ get_toplevel_phrase;
  copy_core_type: 'from get_core_type -> 'to_ get_core_type;
  copy_expression: 'from get_expression -> 'to_ get_expression;
  copy_pattern: 'from get_pattern -> 'to_ get_pattern;
  copy_case: 'from get_case -> 'to_ get_case;
  copy_type_declaration: 'from get_type_declaration -> 'to_ get_type_declaration;
  copy_type_extension: 'from get_type_extension -> 'to_ get_type_extension;
  copy_extension_constructor: 'from get_extension_constructor -> 'to_ get_extension_constructor;
  copy_out_value: 'from get_out_value -> 'to_ get_out_value;
  copy_out_type: 'from get_out_type -> 'to_ get_out_type;
  copy_out_class_type: 'from get_out_class_type -> 'to_ get_out_class_type;
  copy_out_module_type: 'from get_out_module_type -> 'to_ get_out_module_type;
  copy_out_sig_item: 'from get_out_sig_item -> 'to_ get_out_sig_item;
  copy_out_type_extension: 'from get_out_type_extension -> 'to_ get_out_type_extension;
  copy_out_phrase: 'from get_out_phrase -> 'to_ get_out_phrase;
  copy_mapper: 'from get_mapper -> 'to_ get_mapper;
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
  val copy_structure             : A.Ast.Parsetree.structure             -> B.Ast.Parsetree.structure
  val copy_signature             : A.Ast.Parsetree.signature             -> B.Ast.Parsetree.signature
  val copy_toplevel_phrase       : A.Ast.Parsetree.toplevel_phrase       -> B.Ast.Parsetree.toplevel_phrase
  val copy_core_type             : A.Ast.Parsetree.core_type             -> B.Ast.Parsetree.core_type
  val copy_expression            : A.Ast.Parsetree.expression            -> B.Ast.Parsetree.expression
  val copy_pattern               : A.Ast.Parsetree.pattern               -> B.Ast.Parsetree.pattern
  val copy_case                  : A.Ast.Parsetree.case                  -> B.Ast.Parsetree.case
  val copy_type_declaration      : A.Ast.Parsetree.type_declaration      -> B.Ast.Parsetree.type_declaration
  val copy_type_extension        : A.Ast.Parsetree.type_extension        -> B.Ast.Parsetree.type_extension
  val copy_extension_constructor : A.Ast.Parsetree.extension_constructor -> B.Ast.Parsetree.extension_constructor
  val copy_out_value             : A.Ast.Outcometree.out_value           -> B.Ast.Outcometree.out_value
  val copy_out_type              : A.Ast.Outcometree.out_type            -> B.Ast.Outcometree.out_type
  val copy_out_class_type        : A.Ast.Outcometree.out_class_type      -> B.Ast.Outcometree.out_class_type
  val copy_out_module_type       : A.Ast.Outcometree.out_module_type     -> B.Ast.Outcometree.out_module_type
  val copy_out_sig_item          : A.Ast.Outcometree.out_sig_item        -> B.Ast.Outcometree.out_sig_item
  val copy_out_type_extension    : A.Ast.Outcometree.out_type_extension  -> B.Ast.Outcometree.out_type_extension
  val copy_out_phrase            : A.Ast.Outcometree.out_phrase          -> B.Ast.Outcometree.out_phrase
  val copy_mapper                : A.Ast.Ast_mapper.mapper               -> B.Ast.Ast_mapper.mapper
  (*$*)
end
