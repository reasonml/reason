(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                         Frédéric Bour, Facebook                        *)
(*            Jérémie Dimino and Leo White, Jane Street Europe            *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                         Alain Frisch, LexiFi                           *)
(*       Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Location = Location
module Longident = Longident

[@@@warning "-9"]

module Asttypes = struct
  type constant (*IF_CURRENT = Asttypes.constant *) =
      Const_int of int
    | Const_char of char
    | Const_string of string * Location.t * string option
    | Const_float of string
    | Const_int32 of int32
    | Const_int64 of int64
    | Const_nativeint of nativeint

  type rec_flag (*IF_CURRENT = Asttypes.rec_flag *) = Nonrecursive | Recursive

  type direction_flag (*IF_CURRENT = Asttypes.direction_flag *) = Upto | Downto

  (* Order matters, used in polymorphic comparison *)
  type private_flag (*IF_CURRENT = Asttypes.private_flag *) = Private | Public

  type mutable_flag (*IF_CURRENT = Asttypes.mutable_flag *) = Immutable | Mutable

  type virtual_flag (*IF_CURRENT = Asttypes.virtual_flag *) = Virtual | Concrete

  type override_flag (*IF_CURRENT = Asttypes.override_flag *) = Override | Fresh

  type closed_flag (*IF_CURRENT = Asttypes.closed_flag *) = Closed | Open

  type label = string

  type arg_label (*IF_CURRENT = Asttypes.arg_label *) =
      Nolabel
    | Labelled of string (*  label:T -> ... *)
    | Optional of string (* ?label:T -> ... *)

  type 'a loc = 'a Location.loc = {
    txt : 'a;
    loc : Location.t;
  }


  type variance (*IF_CURRENT = Asttypes.variance *) =
    | Covariant
    | Contravariant
    | Invariant
end

module Type_immediacy = struct
  type t (*IF_CURRENT = Type_immediacy.t *) =
    | Unknown
    | Always
    | Always_on_64bits
end

module Outcometree = struct
  (* Module [Outcometree]: results displayed by the toplevel *)

  (* These types represent messages that the toplevel displays as normal
     results or errors. The real displaying is customisable using the hooks:
      [Toploop.print_out_value]
      [Toploop.print_out_type]
      [Toploop.print_out_sig_item]
      [Toploop.print_out_phrase] *)

  (** An [out_name] is a string representation of an identifier which can be
      rewritten on the fly to avoid name collisions *)
  type out_name (*IF_CURRENT = Outcometree.out_name *) = { mutable printed_name: string }

  type out_ident (*IF_CURRENT = Outcometree.out_ident *) =
    | Oide_apply of out_ident * out_ident
    | Oide_dot of out_ident * string
    | Oide_ident of out_name

  type out_string (*IF_CURRENT = Outcometree.out_string *) =
    | Ostr_string
    | Ostr_bytes

  type out_attribute (*IF_CURRENT = Outcometree.out_attribute *) =
    { oattr_name: string }

  type out_value (*IF_CURRENT = Outcometree.out_value *) =
    | Oval_array of out_value list
    | Oval_char of char
    | Oval_constr of out_ident * out_value list
    | Oval_ellipsis
    | Oval_float of float
    | Oval_int of int
    | Oval_int32 of int32
    | Oval_int64 of int64
    | Oval_nativeint of nativeint
    | Oval_list of out_value list
    | Oval_printer of (Format.formatter -> unit)
    | Oval_record of (out_ident * out_value) list
    | Oval_string of string * int * out_string (* string, size-to-print, kind *)
    | Oval_stuff of string
    | Oval_tuple of out_value list
    | Oval_variant of string * out_value option

  type out_type (*IF_CURRENT = Outcometree.out_type *) =
    | Otyp_abstract
    | Otyp_open
    | Otyp_alias of out_type * string
    | Otyp_arrow of string * out_type * out_type
    | Otyp_class of bool * out_ident * out_type list
    | Otyp_constr of out_ident * out_type list
    | Otyp_manifest of out_type * out_type
    | Otyp_object of (string * out_type) list * bool option
    | Otyp_record of (string * bool * out_type) list
    | Otyp_stuff of string
    | Otyp_sum of (string * out_type list * out_type option) list
    | Otyp_tuple of out_type list
    | Otyp_var of bool * string
    | Otyp_variant of
        bool * out_variant * bool * (string list) option
    | Otyp_poly of string list * out_type
    | Otyp_module of out_ident * string list * out_type list
    | Otyp_attribute of out_type * out_attribute

  and out_variant (*IF_CURRENT = Outcometree.out_variant *) =
    | Ovar_fields of (string * bool * out_type list) list
    | Ovar_typ of out_type

  type out_class_type (*IF_CURRENT = Outcometree.out_class_type *) =
    | Octy_constr of out_ident * out_type list
    | Octy_arrow of string * out_type * out_class_type
    | Octy_signature of out_type option * out_class_sig_item list
  and out_class_sig_item (*IF_CURRENT = Outcometree.out_class_sig_item *) =
    | Ocsg_constraint of out_type * out_type
    | Ocsg_method of string * bool * bool * out_type
    | Ocsg_value of string * bool * bool * out_type

  type out_module_type (*IF_CURRENT = Outcometree.out_module_type *) =
    | Omty_abstract
    | Omty_functor of (string option * out_module_type) option * out_module_type
    | Omty_ident of out_ident
    | Omty_signature of out_sig_item list
    | Omty_alias of out_ident
  and out_sig_item (*IF_CURRENT = Outcometree.out_sig_item *) =
    | Osig_class of
        bool * string * (string * (bool * bool)) list * out_class_type *
        out_rec_status
    | Osig_class_type of
        bool * string * (string * (bool * bool)) list * out_class_type *
        out_rec_status
    | Osig_typext of out_extension_constructor * out_ext_status
    | Osig_modtype of string * out_module_type
    | Osig_module of string * out_module_type * out_rec_status
    | Osig_type of out_type_decl * out_rec_status
    | Osig_value of out_val_decl
    | Osig_ellipsis
  and out_type_decl (*IF_CURRENT = Outcometree.out_type_decl *) =
    { otype_name: string;
      otype_params: (string * (bool * bool)) list;
      otype_type: out_type;
      otype_private: Asttypes.private_flag;
      otype_immediate: Type_immediacy.t;
      otype_unboxed: bool;
      otype_cstrs: (out_type * out_type) list }
  and out_extension_constructor (*IF_CURRENT = Outcometree.out_extension_constructor *) =
    { oext_name: string;
      oext_type_name: string;
      oext_type_params: string list;
      oext_args: out_type list;
      oext_ret_type: out_type option;
      oext_private: Asttypes.private_flag }
  and out_type_extension (*IF_CURRENT = Outcometree.out_type_extension *) =
    { otyext_name: string;
      otyext_params: string list;
      otyext_constructors: (string * out_type list * out_type option) list;
      otyext_private: Asttypes.private_flag }
  and out_val_decl (*IF_CURRENT = Outcometree.out_val_decl *) =
    { oval_name: string;
      oval_type: out_type;
      oval_prims: string list;
      oval_attributes: out_attribute list }
  and out_rec_status (*IF_CURRENT = Outcometree.out_rec_status *) =
    | Orec_not
    | Orec_first
    | Orec_next
  and out_ext_status (*IF_CURRENT = Outcometree.out_ext_status *) =
    | Oext_first
    | Oext_next
    | Oext_exception

  type out_phrase (*IF_CURRENT = Outcometree.out_phrase *) =
    | Ophr_eval of out_value * out_type
    | Ophr_signature of (out_sig_item * out_value option) list
    | Ophr_exception of (exn * out_value)
end
