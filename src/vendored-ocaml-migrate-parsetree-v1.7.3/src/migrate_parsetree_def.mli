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

(** Features which are not available in all versions of the frontend *)
type missing_feature =
    Pexp_letexception
  | Ppat_open
  | Pexp_unreachable
  | PSig
  | Pcstr_record
  | Pconst_integer
  | Pconst_float
  | Pcl_open
  | Pcty_open
  | Oinherit
  | Pwith_typesubst_longident
  | Pwith_modsubst_longident
  | Pexp_open
  | Pexp_letop
  | Psig_typesubst
  | Psig_modsubst
  | Otyp_module
  | Immediate64
  | Anonymous_let_module
  | Anonymous_unpack
  | Anonymous_module_binding
  | Anonymous_module_declaration

(** Exception thrown by migration functions when a feature is not supported. *)
exception Migration_error of missing_feature * Location.t

(** [missing_feature_description x] is a text describing the feature [x]. *)
val missing_feature_description : missing_feature -> string

(** [missing_feature_minimal_version x] is the OCaml version where x was
    introduced. *)
val missing_feature_minimal_version : missing_feature -> string

(** Turn a missing feature into a reasonable error message. *)
val migration_error_message : missing_feature -> string
