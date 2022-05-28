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

open Stdlib0
open Ast_409_helper

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

module Parsetree = struct
  open Asttypes

  type constant (*IF_CURRENT = Parsetree.constant *) =
      Pconst_integer of string * char option
    (* 3 3l 3L 3n

       Suffixes [g-z][G-Z] are accepted by the parser.
       Suffixes except 'l', 'L' and 'n' are rejected by the typechecker
    *)
    | Pconst_char of char
    (* 'c' *)
    | Pconst_string of string * Location.t * string option
    (* "constant"
       {delim|other constant|delim}

       The location span the content of the string, without the delimiters.
    *)
    | Pconst_float of string * char option
    (* 3.4 2e5 1.4e-4

       Suffixes [g-z][G-Z] are accepted by the parser.
       Suffixes are rejected by the typechecker.
    *)

  type location_stack = Location.t list

  (** {1 Extension points} *)

  type attribute (*IF_CURRENT = Parsetree.attribute *) = {
    attr_name : string loc;
    attr_payload : payload;
    attr_loc : Location.t;
  }
  (* [@id ARG]
     [@@id ARG]

     Metadata containers passed around within the AST.
     The compiler ignores unknown attributes.
  *)

  and extension = string loc * payload
  (* [%id ARG]
     [%%id ARG]

     Sub-language placeholder -- rejected by the typechecker.
  *)

  and attributes = attribute list

  and payload (*IF_CURRENT = Parsetree.payload *) =
    | PStr of structure
    | PSig of signature (* : SIG *)
    | PTyp of core_type  (* : T *)
    | PPat of pattern * expression option  (* ? P  or  ? P when E *)

  (** {1 Core language} *)

  (* Type expressions *)

  and core_type (*IF_CURRENT = Parsetree.core_type *) =
    {
      ptyp_desc: core_type_desc;
      ptyp_loc: Location.t;
      ptyp_loc_stack: location_stack;
      ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
    }

  and core_type_desc (*IF_CURRENT = Parsetree.core_type_desc *) =
    | Ptyp_any
    (*  _ *)
    | Ptyp_var of string
    (* 'a *)
    | Ptyp_arrow of arg_label * core_type * core_type
    (* T1 -> T2       Simple
       ~l:T1 -> T2    Labelled
       ?l:T1 -> T2    Optional
    *)
    | Ptyp_tuple of core_type list
    (* T1 * ... * Tn

       Invariant: n >= 2
    *)
    | Ptyp_constr of Longident.t loc * core_type list
    (* tconstr
       T tconstr
       (T1, ..., Tn) tconstr
    *)
    | Ptyp_object of object_field list * closed_flag
    (* < l1:T1; ...; ln:Tn >     (flag = Closed)
       < l1:T1; ...; ln:Tn; .. > (flag = Open)
    *)
    | Ptyp_class of Longident.t loc * core_type list
    (* #tconstr
       T #tconstr
       (T1, ..., Tn) #tconstr
    *)
    | Ptyp_alias of core_type * string
    (* T as 'a *)
    | Ptyp_variant of row_field list * closed_flag * label list option
    (* [ `A|`B ]         (flag = Closed; labels = None)
       [> `A|`B ]        (flag = Open;   labels = None)
       [< `A|`B ]        (flag = Closed; labels = Some [])
       [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
    *)
    | Ptyp_poly of string loc list * core_type
    (* 'a1 ... 'an. T

       Can only appear in the following context:

       - As the core_type of a Ppat_constraint node corresponding
         to a constraint on a let-binding: let x : 'a1 ... 'an. T
         = e ...

       - Under Cfk_virtual for methods (not values).

       - As the core_type of a Pctf_method node.

       - As the core_type of a Pexp_poly node.

       - As the pld_type field of a label_declaration.

       - As a core_type of a Ptyp_object node.
    *)

    | Ptyp_package of package_type
    (* (module S) *)
    | Ptyp_extension of extension
    (* [%id] *)

  and package_type = Longident.t loc * (Longident.t loc * core_type) list
      (*
        (module S)
        (module S with type t1 = T1 and ... and tn = Tn)
       *)

  and row_field (*IF_CURRENT = Parsetree.row_field *) = {
    prf_desc : row_field_desc;
    prf_loc : Location.t;
    prf_attributes : attributes;
  }

  and row_field_desc (*IF_CURRENT = Parsetree.row_field_desc *) =
    | Rtag of label loc * bool * core_type list
    (* [`A]                   ( true,  [] )
       [`A of T]              ( false, [T] )
       [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
       [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

       - The 'bool' field is true if the tag contains a
         constant (empty) constructor.
       - '&' occurs when several types are used for the same constructor
         (see 4.2 in the manual)
    *)
    | Rinherit of core_type
    (* [ T ] *)

  and object_field (*IF_CURRENT = Parsetree.object_field *) = {
    pof_desc : object_field_desc;
    pof_loc : Location.t;
    pof_attributes : attributes;
  }

  and object_field_desc (*IF_CURRENT = Parsetree.object_field_desc *) =
    | Otag of label loc * core_type
    | Oinherit of core_type

  (* Patterns *)

  and pattern (*IF_CURRENT = Parsetree.pattern *) =
    {
      ppat_desc: pattern_desc;
      ppat_loc: Location.t;
      ppat_loc_stack: location_stack;
      ppat_attributes: attributes; (* ... [@id1] [@id2] *)
    }

  and pattern_desc (*IF_CURRENT = Parsetree.pattern_desc *) =
    | Ppat_any
    (* _ *)
    | Ppat_var of string loc
    (* x *)
    | Ppat_alias of pattern * string loc
    (* P as 'a *)
    | Ppat_constant of constant
    (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Ppat_interval of constant * constant
    (* 'a'..'z'

       Other forms of interval are recognized by the parser
       but rejected by the type-checker. *)
    | Ppat_tuple of pattern list
    (* (P1, ..., Pn)

       Invariant: n >= 2
    *)
    | Ppat_construct of Longident.t loc * pattern option
    (* C                None
       C P              Some P
       C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
    *)
    | Ppat_variant of label * pattern option
    (* `A             (None)
       `A P           (Some P)
    *)
    | Ppat_record of (Longident.t loc * pattern) list * closed_flag
    (* { l1=P1; ...; ln=Pn }     (flag = Closed)
       { l1=P1; ...; ln=Pn; _}   (flag = Open)

       Invariant: n > 0
    *)
    | Ppat_array of pattern list
    (* [| P1; ...; Pn |] *)
    | Ppat_or of pattern * pattern
    (* P1 | P2 *)
    | Ppat_constraint of pattern * core_type
    (* (P : T) *)
    | Ppat_type of Longident.t loc
    (* #tconst *)
    | Ppat_lazy of pattern
    (* lazy P *)
    | Ppat_unpack of string option loc
    (* (module P)        Some "P"
       (module _)        None

       Note: (module P : S) is represented as
       Ppat_constraint(Ppat_unpack, Ptyp_package)
    *)
    | Ppat_exception of pattern
    (* exception P *)
    | Ppat_extension of extension
    (* [%id] *)
    | Ppat_open of Longident.t loc * pattern
    (* M.(P) *)

  (* Value expressions *)

  and expression (*IF_CURRENT = Parsetree.expression *) =
    {
      pexp_desc: expression_desc;
      pexp_loc: Location.t;
      pexp_loc_stack: location_stack;
      pexp_attributes: attributes; (* ... [@id1] [@id2] *)
    }

  and expression_desc (*IF_CURRENT = Parsetree.expression_desc *) =
    | Pexp_ident of Longident.t loc
    (* x
       M.x
    *)
    | Pexp_constant of constant
    (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Pexp_let of rec_flag * value_binding list * expression
    (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
       let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
    *)
    | Pexp_function of case list
    (* function P1 -> E1 | ... | Pn -> En *)
    | Pexp_fun of arg_label * expression option * pattern * expression
    (* fun P -> E1                          (Simple, None)
       fun ~l:P -> E1                       (Labelled l, None)
       fun ?l:P -> E1                       (Optional l, None)
       fun ?l:(P = E0) -> E1                (Optional l, Some E0)

       Notes:
       - If E0 is provided, only Optional is allowed.
       - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
       - "let f P = E" is represented using Pexp_fun.
    *)
    | Pexp_apply of expression * (arg_label * expression) list
    (* E0 ~l1:E1 ... ~ln:En
       li can be empty (non labeled argument) or start with '?'
       (optional argument).

       Invariant: n > 0
    *)
    | Pexp_match of expression * case list
    (* match E0 with P1 -> E1 | ... | Pn -> En *)
    | Pexp_try of expression * case list
    (* try E0 with P1 -> E1 | ... | Pn -> En *)
    | Pexp_tuple of expression list
    (* (E1, ..., En)

       Invariant: n >= 2
    *)
    | Pexp_construct of Longident.t loc * expression option
    (* C                None
       C E              Some E
       C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
    *)
    | Pexp_variant of label * expression option
    (* `A             (None)
       `A E           (Some E)
    *)
    | Pexp_record of (Longident.t loc * expression) list * expression option
    (* { l1=P1; ...; ln=Pn }     (None)
       { E0 with l1=P1; ...; ln=Pn }   (Some E0)

       Invariant: n > 0
    *)
    | Pexp_field of expression * Longident.t loc
    (* E.l *)
    | Pexp_setfield of expression * Longident.t loc * expression
    (* E1.l <- E2 *)
    | Pexp_array of expression list
    (* [| E1; ...; En |] *)
    | Pexp_ifthenelse of expression * expression * expression option
    (* if E1 then E2 else E3 *)
    | Pexp_sequence of expression * expression
    (* E1; E2 *)
    | Pexp_while of expression * expression
    (* while E1 do E2 done *)
    | Pexp_for of
        pattern *  expression * expression * direction_flag * expression
    (* for i = E1 to E2 do E3 done      (flag = Upto)
       for i = E1 downto E2 do E3 done  (flag = Downto)
    *)
    | Pexp_constraint of expression * core_type
    (* (E : T) *)
    | Pexp_coerce of expression * core_type option * core_type
    (* (E :> T)        (None, T)
       (E : T0 :> T)   (Some T0, T)
    *)
    | Pexp_send of expression * label loc
    (*  E # m *)
    | Pexp_new of Longident.t loc
    (* new M.c *)
    | Pexp_setinstvar of label loc * expression
    (* x <- 2 *)
    | Pexp_override of (label loc * expression) list
    (* {< x1 = E1; ...; Xn = En >} *)
    | Pexp_letmodule of string option loc * module_expr * expression
    (* let module M = ME in E *)
    | Pexp_letexception of extension_constructor * expression
    (* let exception C in E *)
    | Pexp_assert of expression
    (* assert E
       Note: "assert false" is treated in a special way by the
       type-checker. *)
    | Pexp_lazy of expression
    (* lazy E *)
    | Pexp_poly of expression * core_type option
    (* Used for method bodies.

       Can only be used as the expression under Cfk_concrete
       for methods (not values). *)
    | Pexp_object of class_structure
    (* object ... end *)
    | Pexp_newtype of string loc * expression
    (* fun (type t) -> E *)
    | Pexp_pack of module_expr
    (* (module ME)

       (module ME : S) is represented as
       Pexp_constraint(Pexp_pack, Ptyp_package S) *)
    | Pexp_open of open_declaration * expression
    (* M.(E)
       let open M in E
       let! open M in E *)
    | Pexp_letop of letop
    (* let* P = E in E
       let* P = E and* P = E in E *)
    | Pexp_extension of extension
    (* [%id] *)
    | Pexp_unreachable
    (* . *)

  and case (*IF_CURRENT = Parsetree.case *) =   (* (P -> E) or (P when E0 -> E) *)
    {
      pc_lhs: pattern;
      pc_guard: expression option;
      pc_rhs: expression;
    }

  and letop (*IF_CURRENT = Parsetree.letop *) =
    {
      let_ : binding_op;
      ands : binding_op list;
      body : expression;
    }

  and binding_op (*IF_CURRENT = Parsetree.binding_op *) =
    {
      pbop_op : string loc;
      pbop_pat : pattern;
      pbop_exp : expression;
      pbop_loc : Location.t;
    }

  (* Value descriptions *)

  and value_description (*IF_CURRENT = Parsetree.value_description *) =
    {
      pval_name: string loc;
      pval_type: core_type;
      pval_prim: string list;
      pval_attributes: attributes;  (* ... [@@id1] [@@id2] *)
      pval_loc: Location.t;
    }

(*
  val x: T                            (prim = [])
  external x: T = "s1" ... "sn"       (prim = ["s1";..."sn"])
*)

  (* Type declarations *)

  and type_declaration (*IF_CURRENT = Parsetree.type_declaration *) =
    {
      ptype_name: string loc;
      ptype_params: (core_type * variance) list;
      (* ('a1,...'an) t; None represents  _*)
      ptype_cstrs: (core_type * core_type * Location.t) list;
      (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
      ptype_kind: type_kind;
      ptype_private: private_flag;   (* = private ... *)
      ptype_manifest: core_type option;  (* = T *)
      ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
      ptype_loc: Location.t;
    }

(*
  type t                     (abstract, no manifest)
  type t = T0                (abstract, manifest=T0)
  type t = C of T | ...      (variant,  no manifest)
  type t = T0 = C of T | ... (variant,  manifest=T0)
  type t = {l: T; ...}       (record,   no manifest)
  type t = T0 = {l : T; ...} (record,   manifest=T0)
  type t = ..                (open,     no manifest)
*)

  and type_kind (*IF_CURRENT = Parsetree.type_kind *) =
    | Ptype_abstract
    | Ptype_variant of constructor_declaration list
    | Ptype_record of label_declaration list
    (* Invariant: non-empty list *)
    | Ptype_open

  and label_declaration (*IF_CURRENT = Parsetree.label_declaration *) =
    {
      pld_name: string loc;
      pld_mutable: mutable_flag;
      pld_type: core_type;
      pld_loc: Location.t;
      pld_attributes: attributes; (* l : T [@id1] [@id2] *)
    }

  (*  { ...; l: T; ... }            (mutable=Immutable)
      { ...; mutable l: T; ... }    (mutable=Mutable)

      Note: T can be a Ptyp_poly.
  *)

  and constructor_declaration (*IF_CURRENT = Parsetree.constructor_declaration *) =
    {
      pcd_name: string loc;
      pcd_args: constructor_arguments;
      pcd_res: core_type option;
      pcd_loc: Location.t;
      pcd_attributes: attributes; (* C of ... [@id1] [@id2] *)
    }

  and constructor_arguments (*IF_CURRENT = Parsetree.constructor_arguments *) =
    | Pcstr_tuple of core_type list
    | Pcstr_record of label_declaration list

(*
  | C of T1 * ... * Tn     (res = None,    args = Pcstr_tuple [])
  | C: T0                  (res = Some T0, args = [])
  | C: T1 * ... * Tn -> T0 (res = Some T0, args = Pcstr_tuple)
  | C of {...}             (res = None,    args = Pcstr_record)
  | C: {...} -> T0         (res = Some T0, args = Pcstr_record)
  | C of {...} as t        (res = None,    args = Pcstr_record)
*)

  and type_extension (*IF_CURRENT = Parsetree.type_extension *) =
    {
      ptyext_path: Longident.t loc;
      ptyext_params: (core_type * variance) list;
      ptyext_constructors: extension_constructor list;
      ptyext_private: private_flag;
      ptyext_loc: Location.t;
      ptyext_attributes: attributes;   (* ... [@@id1] [@@id2] *)
    }
(*
  type t += ...
*)

  and extension_constructor (*IF_CURRENT = Parsetree.extension_constructor *) =
    {
      pext_name: string loc;
      pext_kind : extension_constructor_kind;
      pext_loc : Location.t;
      pext_attributes: attributes; (* C of ... [@id1] [@id2] *)
    }

  (* exception E *)
  and type_exception (*IF_CURRENT = Parsetree.type_exception *) =
    {
      ptyexn_constructor: extension_constructor;
      ptyexn_loc: Location.t;
      ptyexn_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

  and extension_constructor_kind (*IF_CURRENT = Parsetree.extension_constructor_kind *) =
      Pext_decl of constructor_arguments * core_type option
      (*
         | C of T1 * ... * Tn     ([T1; ...; Tn], None)
         | C: T0                  ([], Some T0)
         | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
       *)
    | Pext_rebind of Longident.t loc
      (*
         | C = D
       *)

  (** {1 Class language} *)

  (* Type expressions for the class language *)

  and class_type (*IF_CURRENT = Parsetree.class_type *) =
    {
      pcty_desc: class_type_desc;
      pcty_loc: Location.t;
      pcty_attributes: attributes; (* ... [@id1] [@id2] *)
    }

  and class_type_desc (*IF_CURRENT = Parsetree.class_type_desc *) =
    | Pcty_constr of Longident.t loc * core_type list
    (* c
       ['a1, ..., 'an] c *)
    | Pcty_signature of class_signature
    (* object ... end *)
    | Pcty_arrow of arg_label * core_type * class_type
    (* T -> CT       Simple
       ~l:T -> CT    Labelled l
       ?l:T -> CT    Optional l
    *)
    | Pcty_extension of extension
    (* [%id] *)
    | Pcty_open of open_description * class_type
    (* let open M in CT *)

  and class_signature (*IF_CURRENT = Parsetree.class_signature *) =
    {
      pcsig_self: core_type;
      pcsig_fields: class_type_field list;
    }
  (* object('selfpat) ... end
     object ... end             (self = Ptyp_any)
  *)

  and class_type_field (*IF_CURRENT = Parsetree.class_type_field *) =
    {
      pctf_desc: class_type_field_desc;
      pctf_loc: Location.t;
      pctf_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

  and class_type_field_desc (*IF_CURRENT = Parsetree.class_type_field_desc *) =
    | Pctf_inherit of class_type
    (* inherit CT *)
    | Pctf_val of (label loc * mutable_flag * virtual_flag * core_type)
    (* val x: T *)
    | Pctf_method  of (label loc * private_flag * virtual_flag * core_type)
    (* method x: T

       Note: T can be a Ptyp_poly.
    *)
    | Pctf_constraint  of (core_type * core_type)
    (* constraint T1 = T2 *)
    | Pctf_attribute of attribute
    (* [@@@id] *)
    | Pctf_extension of extension
    (* [%%id] *)

  and 'a class_infos (*IF_CURRENT = 'a Parsetree.class_infos *) =
    {
      pci_virt: virtual_flag;
      pci_params: (core_type * variance) list;
      pci_name: string loc;
      pci_expr: 'a;
      pci_loc: Location.t;
      pci_attributes: attributes;  (* ... [@@id1] [@@id2] *)
    }
  (* class c = ...
     class ['a1,...,'an] c = ...
     class virtual c = ...

     Also used for "class type" declaration.
  *)

  and class_description = class_type class_infos

  and class_type_declaration = class_type class_infos

  (* Value expressions for the class language *)

  and class_expr (*IF_CURRENT = Parsetree.class_expr *) =
    {
      pcl_desc: class_expr_desc;
      pcl_loc: Location.t;
      pcl_attributes: attributes; (* ... [@id1] [@id2] *)
    }

  and class_expr_desc (*IF_CURRENT = Parsetree.class_expr_desc *) =
    | Pcl_constr of Longident.t loc * core_type list
    (* c
       ['a1, ..., 'an] c *)
    | Pcl_structure of class_structure
    (* object ... end *)
    | Pcl_fun of arg_label * expression option * pattern * class_expr
    (* fun P -> CE                          (Simple, None)
       fun ~l:P -> CE                       (Labelled l, None)
       fun ?l:P -> CE                       (Optional l, None)
       fun ?l:(P = E0) -> CE                (Optional l, Some E0)
    *)
    | Pcl_apply of class_expr * (arg_label * expression) list
    (* CE ~l1:E1 ... ~ln:En
       li can be empty (non labeled argument) or start with '?'
       (optional argument).

       Invariant: n > 0
    *)
    | Pcl_let of rec_flag * value_binding list * class_expr
    (* let P1 = E1 and ... and Pn = EN in CE      (flag = Nonrecursive)
       let rec P1 = E1 and ... and Pn = EN in CE  (flag = Recursive)
    *)
    | Pcl_constraint of class_expr * class_type
    (* (CE : CT) *)
    | Pcl_extension of extension
    (* [%id] *)
    | Pcl_open of open_description * class_expr
    (* let open M in CE *)


  and class_structure (*IF_CURRENT = Parsetree.class_structure *) =
    {
      pcstr_self: pattern;
      pcstr_fields: class_field list;
    }
  (* object(selfpat) ... end
     object ... end           (self = Ppat_any)
  *)

  and class_field (*IF_CURRENT = Parsetree.class_field *) =
    {
      pcf_desc: class_field_desc;
      pcf_loc: Location.t;
      pcf_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

  and class_field_desc (*IF_CURRENT = Parsetree.class_field_desc *) =
    | Pcf_inherit of override_flag * class_expr * string loc option
    (* inherit CE
       inherit CE as x
       inherit! CE
       inherit! CE as x
    *)
    | Pcf_val of (label loc * mutable_flag * class_field_kind)
    (* val x = E
       val virtual x: T
    *)
    | Pcf_method of (label loc * private_flag * class_field_kind)
    (* method x = E            (E can be a Pexp_poly)
       method virtual x: T     (T can be a Ptyp_poly)
    *)
    | Pcf_constraint of (core_type * core_type)
    (* constraint T1 = T2 *)
    | Pcf_initializer of expression
    (* initializer E *)
    | Pcf_attribute of attribute
    (* [@@@id] *)
    | Pcf_extension of extension
    (* [%%id] *)

  and class_field_kind (*IF_CURRENT = Parsetree.class_field_kind *) =
    | Cfk_virtual of core_type
    | Cfk_concrete of override_flag * expression

  and class_declaration = class_expr class_infos

  (** {1 Module language} *)

  (* Type expressions for the module language *)

  and module_type (*IF_CURRENT = Parsetree.module_type *) =
    {
      pmty_desc: module_type_desc;
      pmty_loc: Location.t;
      pmty_attributes: attributes; (* ... [@id1] [@id2] *)
    }

  and module_type_desc (*IF_CURRENT = Parsetree.module_type_desc *) =
    | Pmty_ident of Longident.t loc
    (* S *)
    | Pmty_signature of signature
    (* sig ... end *)
    | Pmty_functor of functor_parameter * module_type
    (* functor(X : MT1) -> MT2 *)
    | Pmty_with of module_type * with_constraint list
    (* MT with ... *)
    | Pmty_typeof of module_expr
    (* module type of ME *)
    | Pmty_extension of extension
    (* [%id] *)
    | Pmty_alias of Longident.t loc
    (* (module M) *)

  and functor_parameter (*IF_CURRENT = Parsetree.functor_parameter *) =
    | Unit
    (* () *)
    | Named of string option loc * module_type
    (* (X : MT)          Some X, MT
       (_ : MT)          None, MT *)

  and signature = signature_item list

  and signature_item (*IF_CURRENT = Parsetree.signature_item *) =
    {
      psig_desc: signature_item_desc;
      psig_loc: Location.t;
    }

  and signature_item_desc (*IF_CURRENT = Parsetree.signature_item_desc *) =
    | Psig_value of value_description
        (*
          val x: T
          external x: T = "s1" ... "sn"
         *)
    | Psig_type of rec_flag * type_declaration list
    (* type t1 = ... and ... and tn  = ... *)
    | Psig_typesubst of type_declaration list
    (* type t1 := ... and ... and tn := ...  *)
    | Psig_typext of type_extension
    (* type t1 += ... *)
    | Psig_exception of type_exception
    (* exception C of T *)
    | Psig_module of module_declaration
    (* module X = M
       module X : MT *)
    | Psig_modsubst of module_substitution
    (* module X := M *)
    | Psig_recmodule of module_declaration list
    (* module rec X1 : MT1 and ... and Xn : MTn *)
    | Psig_modtype of module_type_declaration
    (* module type S = MT
       module type S *)
    | Psig_open of open_description
    (* open X *)
    | Psig_include of include_description
    (* include MT *)
    | Psig_class of class_description list
    (* class c1 : ... and ... and cn : ... *)
    | Psig_class_type of class_type_declaration list
    (* class type ct1 = ... and ... and ctn = ... *)
    | Psig_attribute of attribute
    (* [@@@id] *)
    | Psig_extension of extension * attributes
    (* [%%id] *)

  and module_declaration (*IF_CURRENT = Parsetree.module_declaration *) =
    {
      pmd_name: string option loc;
      pmd_type: module_type;
      pmd_attributes: attributes; (* ... [@@id1] [@@id2] *)
      pmd_loc: Location.t;
    }
  (* S : MT *)

  and module_substitution (*IF_CURRENT = Parsetree.module_substitution *) =
    {
      pms_name: string loc;
      pms_manifest: Longident.t loc;
      pms_attributes: attributes; (* ... [@@id1] [@@id2] *)
      pms_loc: Location.t;
    }

  and module_type_declaration (*IF_CURRENT = Parsetree.module_type_declaration *) =
    {
      pmtd_name: string loc;
      pmtd_type: module_type option;
      pmtd_attributes: attributes; (* ... [@@id1] [@@id2] *)
      pmtd_loc: Location.t;
    }
  (* S = MT
     S       (abstract module type declaration, pmtd_type = None)
  *)

  and 'a open_infos (*IF_CURRENT = 'a Parsetree.open_infos *) =
    {
      popen_expr: 'a;
      popen_override: override_flag;
      popen_loc: Location.t;
      popen_attributes: attributes;
    }
  (* open! X - popen_override = Override (silences the 'used identifier
                                shadowing' warning)
     open  X - popen_override = Fresh
  *)

  and open_description = Longident.t loc open_infos
  (* open M.N
     open M(N).O *)

  and open_declaration = module_expr open_infos
  (* open M.N
     open M(N).O
     open struct ... end *)

  and 'a include_infos (*IF_CURRENT = 'a Parsetree.include_infos *) =
    {
      pincl_mod: 'a;
      pincl_loc: Location.t;
      pincl_attributes: attributes;
    }

  and include_description = module_type include_infos
  (* include MT *)

  and include_declaration = module_expr include_infos
  (* include ME *)

  and with_constraint (*IF_CURRENT = Parsetree.with_constraint *) =
    | Pwith_type of Longident.t loc * type_declaration
    (* with type X.t = ...

       Note: the last component of the longident must match
       the name of the type_declaration. *)
    | Pwith_module of Longident.t loc * Longident.t loc
    (* with module X.Y = Z *)
    | Pwith_typesubst of Longident.t loc * type_declaration
    (* with type X.t := ..., same format as [Pwith_type] *)
    | Pwith_modsubst of Longident.t loc * Longident.t loc
    (* with module X.Y := Z *)

  (* Value expressions for the module language *)

  and module_expr (*IF_CURRENT = Parsetree.module_expr *) =
    {
      pmod_desc: module_expr_desc;
      pmod_loc: Location.t;
      pmod_attributes: attributes; (* ... [@id1] [@id2] *)
    }

  and module_expr_desc (*IF_CURRENT = Parsetree.module_expr_desc *) =
    | Pmod_ident of Longident.t loc
    (* X *)
    | Pmod_structure of structure
    (* struct ... end *)
    | Pmod_functor of functor_parameter * module_expr
    (* functor(X : MT1) -> ME *)
    | Pmod_apply of module_expr * module_expr
    (* ME1(ME2) *)
    | Pmod_constraint of module_expr * module_type
    (* (ME : MT) *)
    | Pmod_unpack of expression
    (* (val E) *)
    | Pmod_extension of extension
    (* [%id] *)

  and structure = structure_item list

  and structure_item (*IF_CURRENT = Parsetree.structure_item *) =
    {
      pstr_desc: structure_item_desc;
      pstr_loc: Location.t;
    }

  and structure_item_desc (*IF_CURRENT = Parsetree.structure_item_desc *) =
    | Pstr_eval of expression * attributes
    (* E *)
    | Pstr_value of rec_flag * value_binding list
    (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
       let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
    *)
    | Pstr_primitive of value_description
    (*  val x: T
        external x: T = "s1" ... "sn" *)
    | Pstr_type of rec_flag * type_declaration list
    (* type t1 = ... and ... and tn = ... *)
    | Pstr_typext of type_extension
    (* type t1 += ... *)
    | Pstr_exception of type_exception
    (* exception C of T
       exception C = M.X *)
    | Pstr_module of module_binding
    (* module X = ME *)
    | Pstr_recmodule of module_binding list
    (* module rec X1 = ME1 and ... and Xn = MEn *)
    | Pstr_modtype of module_type_declaration
    (* module type S = MT *)
    | Pstr_open of open_declaration
    (* open X *)
    | Pstr_class of class_declaration list
    (* class c1 = ... and ... and cn = ... *)
    | Pstr_class_type of class_type_declaration list
    (* class type ct1 = ... and ... and ctn = ... *)
    | Pstr_include of include_declaration
    (* include ME *)
    | Pstr_attribute of attribute
    (* [@@@id] *)
    | Pstr_extension of extension * attributes
    (* [%%id] *)

  and value_binding (*IF_CURRENT = Parsetree.value_binding *) =
    {
      pvb_pat: pattern;
      pvb_expr: expression;
      pvb_attributes: attributes;
      pvb_loc: Location.t;
    }

  and module_binding (*IF_CURRENT = Parsetree.module_binding *) =
    {
      pmb_name: string option loc;
      pmb_expr: module_expr;
      pmb_attributes: attributes;
      pmb_loc: Location.t;
    }
  (* X = ME *)

  (** {1 Toplevel} *)

  (* Toplevel phrases *)

  type toplevel_phrase (*IF_CURRENT = Parsetree.toplevel_phrase *) =
    | Ptop_def of structure
    | Ptop_dir of toplevel_directive
    (* #use, #load ... *)

  and toplevel_directive (*IF_CURRENT = Parsetree.toplevel_directive *) =
    {
      pdir_name : string loc;
      pdir_arg : directive_argument option;
      pdir_loc : Location.t;
    }

  and directive_argument (*IF_CURRENT = Parsetree.directive_argument *) =
    {
      pdira_desc : directive_argument_desc;
      pdira_loc : Location.t;
    }

  and directive_argument_desc (*IF_CURRENT = Parsetree.directive_argument_desc *) =
    | Pdir_string of string
    | Pdir_int of string * char option
    | Pdir_ident of Longident.t
    | Pdir_bool of bool
end

module Docstrings : sig
  (** (Re)Initialise all docstring state *)
  val init : unit -> unit

  (** Emit warnings for unattached and ambiguous docstrings *)
  val warn_bad_docstrings : unit -> unit

  (** {2 Docstrings} *)

  (** Documentation comments *)
  type docstring

  (** Create a docstring *)
  val docstring : string -> Location.t -> docstring

  (** Register a docstring *)
  val register : docstring -> unit

  (** Get the text of a docstring *)
  val docstring_body : docstring -> string

  (** Get the location of a docstring *)
  val docstring_loc : docstring -> Location.t

  (** {2 Set functions}

      These functions are used by the lexer to associate docstrings to
      the locations of tokens. *)

  (** Docstrings immediately preceding a token *)
  val set_pre_docstrings : Lexing.position -> docstring list -> unit

  (** Docstrings immediately following a token *)
  val set_post_docstrings : Lexing.position -> docstring list -> unit

  (** Docstrings not immediately adjacent to a token *)
  val set_floating_docstrings : Lexing.position -> docstring list -> unit

  (** Docstrings immediately following the token which precedes this one *)
  val set_pre_extra_docstrings : Lexing.position -> docstring list -> unit

  (** Docstrings immediately preceding the token which follows this one *)
  val set_post_extra_docstrings : Lexing.position -> docstring list -> unit

  (** {2 Items}

      The {!docs} type represents documentation attached to an item. *)

  type docs =
    { docs_pre: docstring option;
      docs_post: docstring option; }

  val empty_docs : docs

  val docs_attr : docstring -> Parsetree.attribute

  (** Convert item documentation to attributes and add them to an
      attribute list *)
  val add_docs_attrs : docs -> Parsetree.attributes -> Parsetree.attributes

  (** Fetch the item documentation for the current symbol. This also
      marks this documentation (for ambiguity warnings). *)
  val symbol_docs : unit -> docs
  val symbol_docs_lazy : unit -> docs Lazy.t

  (** Fetch the item documentation for the symbols between two
      positions. This also marks this documentation (for ambiguity
      warnings). *)
  val rhs_docs : int -> int -> docs
  val rhs_docs_lazy : int -> int -> docs Lazy.t

  (** Mark the item documentation for the current symbol (for ambiguity
      warnings). *)
  val mark_symbol_docs : unit -> unit

  (** Mark as associated the item documentation for the symbols between
      two positions (for ambiguity warnings) *)
  val mark_rhs_docs : int -> int -> unit

  (** {2 Fields and constructors}

      The {!info} type represents documentation attached to a field or
      constructor. *)

  type info = docstring option

  val empty_info : info

  val info_attr : docstring -> Parsetree.attribute

  (** Convert field info to attributes and add them to an
      attribute list *)
  val add_info_attrs : info -> Parsetree.attributes -> Parsetree.attributes

  (** Fetch the field info for the current symbol. *)
  val symbol_info : unit -> info

  (** Fetch the field info following the symbol at a given position. *)
  val rhs_info : int -> info

  (** {2 Unattached comments}

      The {!text} type represents documentation which is not attached to
      anything. *)

  type text = docstring list

  val empty_text : text
  val empty_text_lazy : text Lazy.t

  val text_attr : docstring -> Parsetree.attribute

  (** Convert text to attributes and add them to an attribute list *)
  val add_text_attrs : text -> Parsetree.attributes -> Parsetree.attributes

  (** Fetch the text preceding the current symbol. *)
  val symbol_text : unit -> text
  val symbol_text_lazy : unit -> text Lazy.t

  (** Fetch the text preceding the symbol at the given position. *)
  val rhs_text : int -> text
  val rhs_text_lazy : int -> text Lazy.t

  (** {2 Extra text}

      There may be additional text attached to the delimiters of a block
      (e.g. [struct] and [end]). This is fetched by the following
      functions, which are applied to the contents of the block rather
      than the delimiters. *)

  (** Fetch additional text preceding the current symbol *)
  val symbol_pre_extra_text : unit -> text

  (** Fetch additional text following the current symbol *)
  val symbol_post_extra_text : unit -> text

  (** Fetch additional text preceding the symbol at the given position *)
  val rhs_pre_extra_text : int -> text

  (** Fetch additional text following the symbol at the given position *)
  val rhs_post_extra_text : int -> text

  (** Fetch text following the symbol at the given position *)
  val rhs_post_text : int -> text

  module WithMenhir: sig
    (** Fetch the item documentation for the current symbol. This also
        marks this documentation (for ambiguity warnings). *)
    val symbol_docs : Lexing.position * Lexing.position -> docs
    val symbol_docs_lazy : Lexing.position * Lexing.position -> docs Lazy.t

    (** Fetch the item documentation for the symbols between two
        positions. This also marks this documentation (for ambiguity
        warnings). *)
    val rhs_docs : Lexing.position -> Lexing.position -> docs
    val rhs_docs_lazy : Lexing.position -> Lexing.position -> docs Lazy.t

    (** Mark the item documentation for the current symbol (for ambiguity
        warnings). *)
    val mark_symbol_docs : Lexing.position * Lexing.position -> unit

    (** Mark as associated the item documentation for the symbols between
        two positions (for ambiguity warnings) *)
    val mark_rhs_docs : Lexing.position -> Lexing.position -> unit

    (** Fetch the field info for the current symbol. *)
    val symbol_info : Lexing.position -> info

    (** Fetch the field info following the symbol at a given position. *)
    val rhs_info : Lexing.position -> info

    (** Fetch the text preceding the current symbol. *)
    val symbol_text : Lexing.position -> text
    val symbol_text_lazy : Lexing.position -> text Lazy.t

    (** Fetch the text preceding the symbol at the given position. *)
    val rhs_text : Lexing.position -> text
    val rhs_text_lazy : Lexing.position -> text Lazy.t

    (** {3 Extra text}

        There may be additional text attached to the delimiters of a block
        (e.g. [struct] and [end]). This is fetched by the following
        functions, which are applied to the contents of the block rather
        than the delimiters. *)

    (** Fetch additional text preceding the current symbol *)
    val symbol_pre_extra_text : Lexing.position -> text

    (** Fetch additional text following the current symbol *)
    val symbol_post_extra_text : Lexing.position -> text

    (** Fetch additional text preceding the symbol at the given position *)
    val rhs_pre_extra_text : Lexing.position -> text

    (** Fetch additional text following the symbol at the given position *)
    val rhs_post_extra_text : Lexing.position -> text

    (** Fetch text following the symbol at the given position *)
    val rhs_post_text : Lexing.position -> text

  end
end = struct
  open Location

  (* Docstrings *)

  (* A docstring is "attached" if it has been inserted in the AST. This
     is used for generating unexpected docstring warnings. *)
  type ds_attached =
    | Unattached   (* Not yet attached anything.*)
    | Info         (* Attached to a field or constructor. *)
    | Docs         (* Attached to an item or as floating text. *)

  (* A docstring is "associated" with an item if there are no blank lines between
     them. This is used for generating docstring ambiguity warnings. *)
  type ds_associated =
    | Zero             (* Not associated with an item *)
    | One              (* Associated with one item *)
    | Many             (* Associated with multiple items (ambiguity) *)

  type docstring =
    { ds_body: string;
      ds_loc: Location.t;
      mutable ds_attached: ds_attached;
      mutable ds_associated: ds_associated; }

  (* List of docstrings *)

  let docstrings : docstring list ref = ref []

  (* Warn for unused and ambiguous docstrings *)

  let warn_bad_docstrings () =
    if Warnings.is_active (Migrate_parsetree_compiler_functions.bad_docstring true) then begin
      List.iter
        (fun ds ->
           match ds.ds_attached with
           | Info -> ()
           | Unattached ->
               prerr_warning ds.ds_loc (Migrate_parsetree_compiler_functions.bad_docstring true)
           | Docs ->
               match ds.ds_associated with
               | Zero | One -> ()
               | Many ->
                   prerr_warning ds.ds_loc (Migrate_parsetree_compiler_functions.bad_docstring false))
        (List.rev !docstrings)
    end

  (* Docstring constructors and destructors *)

  let docstring body loc =
    let ds =
      { ds_body = body;
        ds_loc = loc;
        ds_attached = Unattached;
        ds_associated = Zero; }
    in
    ds

  let register ds =
    docstrings := ds :: !docstrings

  let docstring_body ds = ds.ds_body

  let docstring_loc ds = ds.ds_loc

  (* Docstrings attached to items *)

  type docs =
    { docs_pre: docstring option;
      docs_post: docstring option; }

  let empty_docs = { docs_pre = None; docs_post = None }

  let doc_loc = {txt = "ocaml.doc"; loc = Location.none}

  let docs_attr ds =
    let open Parsetree in
    let exp =
      { pexp_desc = Pexp_constant (Pconst_string(ds.ds_body, ds.ds_loc, None));
        pexp_loc = ds.ds_loc;
        pexp_loc_stack = [];
        pexp_attributes = []; }
    in
    let item =
      { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
    in
    { attr_name = doc_loc;
      attr_payload = PStr [item];
      attr_loc = Location.none }

  let add_docs_attrs docs attrs =
    let attrs =
      match docs.docs_pre with
      | None | Some { ds_body=""; _ } -> attrs
      | Some ds -> docs_attr ds :: attrs
    in
    let attrs =
      match docs.docs_post with
      | None | Some { ds_body=""; _ } -> attrs
      | Some ds -> attrs @ [docs_attr ds]
    in
    attrs

  (* Docstrings attached to constructors or fields *)

  type info = docstring option

  let empty_info = None

  let info_attr = docs_attr

  let add_info_attrs info attrs =
    match info with
    | None | Some {ds_body=""; _} -> attrs
    | Some ds -> attrs @ [info_attr ds]

  (* Docstrings not attached to a specific item *)

  type text = docstring list

  let empty_text = []
  let empty_text_lazy = lazy []

  let text_loc = {txt = "ocaml.text"; loc = Location.none}

  let text_attr ds =
    let open Parsetree in
    let exp =
      { pexp_desc = Pexp_constant (Pconst_string(ds.ds_body, ds.ds_loc, None));
        pexp_loc = ds.ds_loc;
        pexp_loc_stack = [];
        pexp_attributes = []; }
    in
    let item =
      { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
    in
    { attr_name = text_loc;
      attr_payload = PStr [item];
      attr_loc = Location.none }

  let add_text_attrs dsl attrs =
    let fdsl = List.filter (function {ds_body=""} -> false| _ ->true) dsl in
    (List.map text_attr fdsl) @ attrs

  (* Find the first non-info docstring in a list, attach it and return it *)
  let get_docstring ~info dsl =
    let rec loop = function
      | [] -> None
      | {ds_attached = Info; _} :: rest -> loop rest
      | ds :: _ ->
          ds.ds_attached <- if info then Info else Docs;
          Some ds
    in
    loop dsl

  (* Find all the non-info docstrings in a list, attach them and return them *)
  let get_docstrings dsl =
    let rec loop acc = function
      | [] -> List.rev acc
      | {ds_attached = Info; _} :: rest -> loop acc rest
      | ds :: rest ->
          ds.ds_attached <- Docs;
          loop (ds :: acc) rest
    in
    loop [] dsl

  (* "Associate" all the docstrings in a list *)
  let associate_docstrings dsl =
    List.iter
      (fun ds ->
         match ds.ds_associated with
         | Zero -> ds.ds_associated <- One
         | (One | Many) -> ds.ds_associated <- Many)
      dsl

  (* Map from positions to pre docstrings *)

  let pre_table : (Lexing.position, docstring list) Hashtbl.t =
    Hashtbl.create 50

  let set_pre_docstrings pos dsl =
    if dsl <> [] then Hashtbl.add pre_table pos dsl

  let get_pre_docs pos =
    try
      let dsl = Hashtbl.find pre_table pos in
      associate_docstrings dsl;
      get_docstring ~info:false dsl
    with Not_found -> None

  let mark_pre_docs pos =
    try
      let dsl = Hashtbl.find pre_table pos in
      associate_docstrings dsl
    with Not_found -> ()

  (* Map from positions to post docstrings *)

  let post_table : (Lexing.position, docstring list) Hashtbl.t =
    Hashtbl.create 50

  let set_post_docstrings pos dsl =
    if dsl <> [] then Hashtbl.add post_table pos dsl

  let get_post_docs pos =
    try
      let dsl = Hashtbl.find post_table pos in
      associate_docstrings dsl;
      get_docstring ~info:false dsl
    with Not_found -> None

  let mark_post_docs pos =
    try
      let dsl = Hashtbl.find post_table pos in
      associate_docstrings dsl
    with Not_found -> ()

  let get_info pos =
    try
      let dsl = Hashtbl.find post_table pos in
      get_docstring ~info:true dsl
    with Not_found -> None

  (* Map from positions to floating docstrings *)

  let floating_table : (Lexing.position, docstring list) Hashtbl.t =
    Hashtbl.create 50

  let set_floating_docstrings pos dsl =
    if dsl <> [] then Hashtbl.add floating_table pos dsl

  let get_text pos =
    try
      let dsl = Hashtbl.find floating_table pos in
      get_docstrings dsl
    with Not_found -> []

  let get_post_text pos =
    try
      let dsl = Hashtbl.find post_table pos in
      get_docstrings dsl
    with Not_found -> []

  (* Maps from positions to extra docstrings *)

  let pre_extra_table : (Lexing.position, docstring list) Hashtbl.t =
    Hashtbl.create 50

  let set_pre_extra_docstrings pos dsl =
    if dsl <> [] then Hashtbl.add pre_extra_table pos dsl

  let get_pre_extra_text pos =
    try
      let dsl = Hashtbl.find pre_extra_table pos in
      get_docstrings dsl
    with Not_found -> []

  let post_extra_table : (Lexing.position, docstring list) Hashtbl.t =
    Hashtbl.create 50

  let set_post_extra_docstrings pos dsl =
    if dsl <> [] then Hashtbl.add post_extra_table pos dsl

  let get_post_extra_text pos =
    try
      let dsl = Hashtbl.find post_extra_table pos in
      get_docstrings dsl
    with Not_found -> []

  (* Docstrings from parser actions *)
  module WithParsing = struct
    let symbol_docs () =
      { docs_pre = get_pre_docs (Parsing.symbol_start_pos ());
        docs_post = get_post_docs (Parsing.symbol_end_pos ()); }

    let symbol_docs_lazy () =
      let p1 = Parsing.symbol_start_pos () in
      let p2 = Parsing.symbol_end_pos () in
      lazy { docs_pre = get_pre_docs p1;
             docs_post = get_post_docs p2; }

    let rhs_docs pos1 pos2 =
      { docs_pre = get_pre_docs (Parsing.rhs_start_pos pos1);
        docs_post = get_post_docs (Parsing.rhs_end_pos pos2); }

    let rhs_docs_lazy pos1 pos2 =
      let p1 = Parsing.rhs_start_pos pos1 in
      let p2 = Parsing.rhs_end_pos pos2 in
      lazy { docs_pre = get_pre_docs p1;
             docs_post = get_post_docs p2; }

    let mark_symbol_docs () =
      mark_pre_docs (Parsing.symbol_start_pos ());
      mark_post_docs (Parsing.symbol_end_pos ())

    let mark_rhs_docs pos1 pos2 =
      mark_pre_docs (Parsing.rhs_start_pos pos1);
      mark_post_docs (Parsing.rhs_end_pos pos2)

    let symbol_info () =
      get_info (Parsing.symbol_end_pos ())

    let rhs_info pos =
      get_info (Parsing.rhs_end_pos pos)

    let symbol_text () =
      get_text (Parsing.symbol_start_pos ())

    let symbol_text_lazy () =
      let pos = Parsing.symbol_start_pos () in
      lazy (get_text pos)

    let rhs_text pos =
      get_text (Parsing.rhs_start_pos pos)

    let rhs_post_text pos =
      get_post_text (Parsing.rhs_end_pos pos)

    let rhs_text_lazy pos =
      let pos = Parsing.rhs_start_pos pos in
      lazy (get_text pos)

    let symbol_pre_extra_text () =
      get_pre_extra_text (Parsing.symbol_start_pos ())

    let symbol_post_extra_text () =
      get_post_extra_text (Parsing.symbol_end_pos ())

    let rhs_pre_extra_text pos =
      get_pre_extra_text (Parsing.rhs_start_pos pos)

    let rhs_post_extra_text pos =
      get_post_extra_text (Parsing.rhs_end_pos pos)
  end

  include WithParsing

  module WithMenhir = struct
    let symbol_docs (startpos, endpos) =
      { docs_pre = get_pre_docs startpos;
        docs_post = get_post_docs endpos; }

    let symbol_docs_lazy (p1, p2) =
      lazy { docs_pre = get_pre_docs p1;
             docs_post = get_post_docs p2; }

    let rhs_docs pos1 pos2 =
      { docs_pre = get_pre_docs pos1;
        docs_post = get_post_docs pos2; }

    let rhs_docs_lazy p1 p2 =
      lazy { docs_pre = get_pre_docs p1;
             docs_post = get_post_docs p2; }

    let mark_symbol_docs (startpos, endpos) =
      mark_pre_docs startpos;
      mark_post_docs endpos;
      ()

    let mark_rhs_docs pos1 pos2 =
      mark_pre_docs pos1;
      mark_post_docs pos2;
      ()

    let symbol_info endpos =
      get_info endpos

    let rhs_info endpos =
      get_info endpos

    let symbol_text startpos =
      get_text startpos

    let symbol_text_lazy startpos =
      lazy (get_text startpos)

    let rhs_text pos =
      get_text pos

    let rhs_post_text pos =
      get_post_text pos

    let rhs_text_lazy pos =
      lazy (get_text pos)

    let symbol_pre_extra_text startpos =
      get_pre_extra_text startpos

    let symbol_post_extra_text endpos =
      get_post_extra_text endpos

    let rhs_pre_extra_text pos =
      get_pre_extra_text pos

    let rhs_post_extra_text pos =
      get_post_extra_text pos
  end

  (* (Re)Initialise all comment state *)

  let init () =
    docstrings := [];
    Hashtbl.reset pre_table;
    Hashtbl.reset post_table;
    Hashtbl.reset floating_table;
    Hashtbl.reset pre_extra_table;
    Hashtbl.reset post_extra_table
end

module Ast_helper : sig
  open Asttypes
  open Docstrings
  open Parsetree

  type 'a with_loc = 'a Location.loc
  type loc = Location.t

  type lid = Longident.t with_loc
  type str = string with_loc
  type str_opt = string option with_loc
  type attrs = attribute list

  (** {1 Default locations} *)

  val default_loc: loc ref
  (** Default value for all optional location arguments. *)

  val with_default_loc: loc -> (unit -> 'a) -> 'a
  (** Set the [default_loc] within the scope of the execution
      of the provided function. *)

  (** {1 Constants} *)

  module Const : sig
    val char : char -> constant
    val string :
      ?quotation_delimiter:string -> ?loc:Location.t -> string -> constant
    val integer : ?suffix:char -> string -> constant
    val int : ?suffix:char -> int -> constant
    val int32 : ?suffix:char -> int32 -> constant
    val int64 : ?suffix:char -> int64 -> constant
    val nativeint : ?suffix:char -> nativeint -> constant
    val float : ?suffix:char -> string -> constant
  end

  (** {1 Attributes} *)
  module Attr : sig
    val mk: ?loc:loc -> str -> payload -> attribute
  end

  (** {1 Core language} *)

  (** Type expressions *)
  module Typ :
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
    val attr: core_type -> attribute -> core_type

    val any: ?loc:loc -> ?attrs:attrs -> unit -> core_type
    val var: ?loc:loc -> ?attrs:attrs -> string -> core_type
    val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type -> core_type
      -> core_type
    val tuple: ?loc:loc -> ?attrs:attrs -> core_type list -> core_type
    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
    val object_: ?loc:loc -> ?attrs:attrs -> object_field list
      -> closed_flag -> core_type
    val class_: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
    val alias: ?loc:loc -> ?attrs:attrs -> core_type -> string -> core_type
    val variant: ?loc:loc -> ?attrs:attrs -> row_field list -> closed_flag
      -> label list option -> core_type
    val poly: ?loc:loc -> ?attrs:attrs -> str list -> core_type -> core_type
    val package: ?loc:loc -> ?attrs:attrs -> lid -> (lid * core_type) list
      -> core_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> core_type

    val force_poly: core_type -> core_type

    val varify_constructors: str list -> core_type -> core_type
    (** [varify_constructors newtypes te] is type expression [te], of which
        any of nullary type constructor [tc] is replaced by type variable of
        the same name, if [tc]'s name appears in [newtypes].
        Raise [Syntaxerr.Variable_in_scope] if any type variable inside [te]
        appears in [newtypes].
        @since 4.05
    *)
  end

  (** Patterns *)
  module Pat:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> pattern_desc -> pattern
    val attr:pattern -> attribute -> pattern

    val any: ?loc:loc -> ?attrs:attrs -> unit -> pattern
    val var: ?loc:loc -> ?attrs:attrs -> str -> pattern
    val alias: ?loc:loc -> ?attrs:attrs -> pattern -> str -> pattern
    val constant: ?loc:loc -> ?attrs:attrs -> constant -> pattern
    val interval: ?loc:loc -> ?attrs:attrs -> constant -> constant -> pattern
    val tuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
    val construct: ?loc:loc -> ?attrs:attrs -> lid -> pattern option -> pattern
    val variant: ?loc:loc -> ?attrs:attrs -> label -> pattern option -> pattern
    val record: ?loc:loc -> ?attrs:attrs -> (lid * pattern) list -> closed_flag
      -> pattern
    val array: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
    val or_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
    val constraint_: ?loc:loc -> ?attrs:attrs -> pattern -> core_type -> pattern
    val type_: ?loc:loc -> ?attrs:attrs -> lid -> pattern
    val lazy_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
    val unpack: ?loc:loc -> ?attrs:attrs -> str_opt -> pattern
    val open_: ?loc:loc -> ?attrs:attrs  -> lid -> pattern -> pattern
    val exception_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> pattern
  end

  (** Expressions *)
  module Exp:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> expression_desc -> expression
    val attr: expression -> attribute -> expression

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> expression
    val constant: ?loc:loc -> ?attrs:attrs -> constant -> expression
    val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list
      -> expression -> expression
    val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option
      -> pattern -> expression -> expression
    val function_: ?loc:loc -> ?attrs:attrs -> case list -> expression
    val apply: ?loc:loc -> ?attrs:attrs -> expression
      -> (arg_label * expression) list -> expression
    val match_: ?loc:loc -> ?attrs:attrs -> expression -> case list
      -> expression
    val try_: ?loc:loc -> ?attrs:attrs -> expression -> case list -> expression
    val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression
    val construct: ?loc:loc -> ?attrs:attrs -> lid -> expression option
      -> expression
    val variant: ?loc:loc -> ?attrs:attrs -> label -> expression option
      -> expression
    val record: ?loc:loc -> ?attrs:attrs -> (lid * expression) list
      -> expression option -> expression
    val field: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
    val setfield: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
      -> expression
    val array: ?loc:loc -> ?attrs:attrs -> expression list -> expression
    val ifthenelse: ?loc:loc -> ?attrs:attrs -> expression -> expression
      -> expression option -> expression
    val sequence: ?loc:loc -> ?attrs:attrs -> expression -> expression
      -> expression
    val while_: ?loc:loc -> ?attrs:attrs -> expression -> expression
      -> expression
    val for_: ?loc:loc -> ?attrs:attrs -> pattern -> expression -> expression
      -> direction_flag -> expression -> expression
    val coerce: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
      -> core_type -> expression
    val constraint_: ?loc:loc -> ?attrs:attrs -> expression -> core_type
      -> expression
    val send: ?loc:loc -> ?attrs:attrs -> expression -> str -> expression
    val new_: ?loc:loc -> ?attrs:attrs -> lid -> expression
    val setinstvar: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
    val override: ?loc:loc -> ?attrs:attrs -> (str * expression) list
      -> expression
    val letmodule: ?loc:loc -> ?attrs:attrs -> str_opt -> module_expr
      -> expression -> expression
    val letexception:
      ?loc:loc -> ?attrs:attrs -> extension_constructor -> expression
      -> expression
    val assert_: ?loc:loc -> ?attrs:attrs -> expression -> expression
    val lazy_: ?loc:loc -> ?attrs:attrs -> expression -> expression
    val poly: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
      -> expression
    val object_: ?loc:loc -> ?attrs:attrs -> class_structure -> expression
    val newtype: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
    val pack: ?loc:loc -> ?attrs:attrs -> module_expr -> expression
    val open_: ?loc:loc -> ?attrs:attrs -> open_declaration -> expression
      -> expression
    val letop: ?loc:loc -> ?attrs:attrs -> binding_op
      -> binding_op list -> expression -> expression
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> expression
    val unreachable: ?loc:loc -> ?attrs:attrs -> unit -> expression

    val case: pattern -> ?guard:expression -> expression -> case
    val binding_op: str -> pattern -> expression -> loc -> binding_op
  end

  (** Value declarations *)
  module Val:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      ?prim:string list -> str -> core_type -> value_description
  end

  (** Type declarations *)
  module Type:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?params:(core_type * variance) list ->
      ?cstrs:(core_type * core_type * loc) list ->
      ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type -> str ->
      type_declaration

    val constructor: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?args:constructor_arguments -> ?res:core_type -> str ->
      constructor_declaration
    val field: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?mut:mutable_flag -> str -> core_type -> label_declaration
  end

  (** Type extensions *)
  module Te:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      ?params:(core_type * variance) list -> ?priv:private_flag ->
      lid -> extension_constructor list -> type_extension

    val mk_exception: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      extension_constructor -> type_exception

    val constructor: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str -> extension_constructor_kind -> extension_constructor

    val decl: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      ?args:constructor_arguments -> ?res:core_type -> str ->
      extension_constructor
    val rebind: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str -> lid -> extension_constructor
  end

  (** {1 Module language} *)

  (** Module type expressions *)
  module Mty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_type_desc -> module_type
    val attr: module_type -> attribute -> module_type

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val alias: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val signature: ?loc:loc -> ?attrs:attrs -> signature -> module_type
    val functor_: ?loc:loc -> ?attrs:attrs ->
      functor_parameter -> module_type -> module_type
    val with_: ?loc:loc -> ?attrs:attrs -> module_type ->
      with_constraint list -> module_type
    val typeof_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_type
  end

  (** Module expressions *)
  module Mod:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_expr_desc -> module_expr
    val attr: module_expr -> attribute -> module_expr

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_expr
    val structure: ?loc:loc -> ?attrs:attrs -> structure -> module_expr
    val functor_: ?loc:loc -> ?attrs:attrs ->
      functor_parameter -> module_expr -> module_expr
    val apply: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr ->
      module_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type ->
      module_expr
    val unpack: ?loc:loc -> ?attrs:attrs -> expression -> module_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_expr
  end

  (** Signature items *)
  module Sig:
  sig
    val mk: ?loc:loc -> signature_item_desc -> signature_item

    val value: ?loc:loc -> value_description -> signature_item
    val type_: ?loc:loc -> rec_flag -> type_declaration list -> signature_item
    val type_subst: ?loc:loc -> type_declaration list -> signature_item
    val type_extension: ?loc:loc -> type_extension -> signature_item
    val exception_: ?loc:loc -> type_exception -> signature_item
    val module_: ?loc:loc -> module_declaration -> signature_item
    val mod_subst: ?loc:loc -> module_substitution -> signature_item
    val rec_module: ?loc:loc -> module_declaration list -> signature_item
    val modtype: ?loc:loc -> module_type_declaration -> signature_item
    val open_: ?loc:loc -> open_description -> signature_item
    val include_: ?loc:loc -> include_description -> signature_item
    val class_: ?loc:loc -> class_description list -> signature_item
    val class_type: ?loc:loc -> class_type_declaration list -> signature_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> signature_item
    val attribute: ?loc:loc -> attribute -> signature_item
    val text: text -> signature_item list
  end

  (** Structure items *)
  module Str:
  sig
    val mk: ?loc:loc -> structure_item_desc -> structure_item

    val eval: ?loc:loc -> ?attrs:attributes -> expression -> structure_item
    val value: ?loc:loc -> rec_flag -> value_binding list -> structure_item
    val primitive: ?loc:loc -> value_description -> structure_item
    val type_: ?loc:loc -> rec_flag -> type_declaration list -> structure_item
    val type_extension: ?loc:loc -> type_extension -> structure_item
    val exception_: ?loc:loc -> type_exception -> structure_item
    val module_: ?loc:loc -> module_binding -> structure_item
    val rec_module: ?loc:loc -> module_binding list -> structure_item
    val modtype: ?loc:loc -> module_type_declaration -> structure_item
    val open_: ?loc:loc -> open_declaration -> structure_item
    val class_: ?loc:loc -> class_declaration list -> structure_item
    val class_type: ?loc:loc -> class_type_declaration list -> structure_item
    val include_: ?loc:loc -> include_declaration -> structure_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> structure_item
    val attribute: ?loc:loc -> attribute -> structure_item
    val text: text -> structure_item list
  end

  (** Module declarations *)
  module Md:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str_opt -> module_type -> module_declaration
  end

  (** Module substitutions *)
  module Ms:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str -> lid -> module_substitution
  end

  (** Module type declarations *)
  module Mtd:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?typ:module_type -> str -> module_type_declaration
  end

  (** Module bindings *)
  module Mb:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str_opt -> module_expr -> module_binding
  end

  (** Opens *)
  module Opn:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs ->
      ?override:override_flag -> 'a -> 'a open_infos
  end

  (** Includes *)
  module Incl:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> 'a -> 'a include_infos
  end

  (** Value bindings *)
  module Vb:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      pattern -> expression -> value_binding
  end


  (** {1 Class language} *)

  (** Class type expressions *)
  module Cty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_type_desc -> class_type
    val attr: class_type -> attribute -> class_type

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_type
    val signature: ?loc:loc -> ?attrs:attrs -> class_signature -> class_type
    val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type ->
      class_type -> class_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type
    val open_: ?loc:loc -> ?attrs:attrs -> open_description -> class_type
      -> class_type
  end

  (** Class type fields *)
  module Ctf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      class_type_field_desc -> class_type_field
    val attr: class_type_field -> attribute -> class_type_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> class_type -> class_type_field
    val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag ->
      virtual_flag -> core_type -> class_type_field
    val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag ->
      virtual_flag -> core_type -> class_type_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
      class_type_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type_field
    val attribute: ?loc:loc -> attribute -> class_type_field
    val text: text -> class_type_field list
  end

  (** Class expressions *)
  module Cl:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_expr_desc -> class_expr
    val attr: class_expr -> attribute -> class_expr

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_expr
    val structure: ?loc:loc -> ?attrs:attrs -> class_structure -> class_expr
    val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option ->
      pattern -> class_expr -> class_expr
    val apply: ?loc:loc -> ?attrs:attrs -> class_expr ->
      (arg_label * expression) list -> class_expr
    val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list ->
      class_expr -> class_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> class_expr -> class_type ->
      class_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_expr
    val open_: ?loc:loc -> ?attrs:attrs -> open_description -> class_expr
      -> class_expr
  end

  (** Class fields *)
  module Cf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> class_field_desc ->
      class_field
    val attr: class_field -> attribute -> class_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> override_flag -> class_expr ->
      str option -> class_field
    val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag ->
      class_field_kind -> class_field
    val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag ->
      class_field_kind -> class_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
      class_field
    val initializer_: ?loc:loc -> ?attrs:attrs -> expression -> class_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_field
    val attribute: ?loc:loc -> attribute -> class_field
    val text: text -> class_field list

    val virtual_: core_type -> class_field_kind
    val concrete: override_flag -> expression -> class_field_kind

  end

  (** Classes *)
  module Ci:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?virt:virtual_flag -> ?params:(core_type * variance) list ->
      str -> 'a -> 'a class_infos
  end

  (** Class signatures *)
  module Csig:
  sig
    val mk: core_type -> class_type_field list -> class_signature
  end

  (** Class structures *)
  module Cstr:
  sig
    val mk: pattern -> class_field list -> class_structure
  end

  (** Row fields *)
  module Rf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> row_field_desc -> row_field
    val tag: ?loc:loc -> ?attrs:attrs ->
      label with_loc -> bool -> core_type list -> row_field
    val inherit_: ?loc:loc -> core_type -> row_field
  end

  (** Object fields *)
  module Of:
  sig
    val mk: ?loc:loc -> ?attrs:attrs ->
      object_field_desc -> object_field
    val tag: ?loc:loc -> ?attrs:attrs ->
      label with_loc -> core_type -> object_field
    val inherit_: ?loc:loc -> core_type -> object_field
  end
end = struct
  open Asttypes
  open Parsetree
  open Docstrings

  type 'a with_loc = 'a Location.loc
  type loc = Location.t

  type lid = Longident.t with_loc
  type str = string with_loc
  type str_opt = string option with_loc
  type attrs = attribute list

  let default_loc = ref Location.none

  let with_default_loc l f =
    Misc.protect_refs [Misc.R (default_loc, l)] f

  module Const = struct
    let integer ?suffix i = Pconst_integer (i, suffix)
    let int ?suffix i = integer ?suffix (Int.to_string i)
    let int32 ?(suffix='l') i = integer ~suffix (Int32.to_string i)
    let int64 ?(suffix='L') i = integer ~suffix (Int64.to_string i)
    let nativeint ?(suffix='n') i = integer ~suffix (Nativeint.to_string i)
    let float ?suffix f = Pconst_float (f, suffix)
    let char c = Pconst_char c
    let string ?quotation_delimiter ?(loc= !default_loc) s =
      Pconst_string (s, loc, quotation_delimiter)
  end

  module Attr = struct
    let mk ?(loc= !default_loc) name payload =
      { attr_name = name;
        attr_payload = payload;
        attr_loc = loc }
  end

  module Typ = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {ptyp_desc = d;
       ptyp_loc = loc;
       ptyp_loc_stack = [];
       ptyp_attributes = attrs}

    let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
    let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_arrow (a, b, c))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
    let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
    let object_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_object (a, b))
    let class_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_class (a, b))
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
    let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
    let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
    let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)

    let force_poly t =
      match t.ptyp_desc with
      | Ptyp_poly _ -> t
      | _ -> poly ~loc:t.ptyp_loc [] t (* -> ghost? *)

    let varify_constructors var_names t =
      let check_variable vl loc v =
        if List.mem v vl then
          raise Syntaxerr.(Error(Variable_in_scope(loc,v))) in
      let var_names = List.map (fun v -> v.txt) var_names in
      let rec loop t =
        let desc =
          match t.ptyp_desc with
          | Ptyp_any -> Ptyp_any
          | Ptyp_var x ->
              check_variable var_names t.ptyp_loc x;
              Ptyp_var x
          | Ptyp_arrow (label,core_type,core_type') ->
              Ptyp_arrow(label, loop core_type, loop core_type')
          | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
          | Ptyp_constr( { txt = Longident.Lident s }, [])
            when List.mem s var_names ->
              Ptyp_var s
          | Ptyp_constr(longident, lst) ->
              Ptyp_constr(longident, List.map loop lst)
          | Ptyp_object (lst, o) ->
              Ptyp_object (List.map loop_object_field lst, o)
          | Ptyp_class (longident, lst) ->
              Ptyp_class (longident, List.map loop lst)
          | Ptyp_alias(core_type, string) ->
              check_variable var_names t.ptyp_loc string;
              Ptyp_alias(loop core_type, string)
          | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
              Ptyp_variant(List.map loop_row_field row_field_list,
                           flag, lbl_lst_option)
          | Ptyp_poly(string_lst, core_type) ->
              List.iter (fun v ->
                  check_variable var_names t.ptyp_loc v.txt) string_lst;
              Ptyp_poly(string_lst, loop core_type)
          | Ptyp_package(longident,lst) ->
              Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
          | Ptyp_extension (s, arg) ->
              Ptyp_extension (s, arg)
        in
        {t with ptyp_desc = desc}
      and loop_row_field field =
        let prf_desc = match field.prf_desc with
          | Rtag(label,flag,lst) ->
              Rtag(label,flag,List.map loop lst)
          | Rinherit t ->
              Rinherit (loop t)
        in
        { field with prf_desc; }
      and loop_object_field field =
        let pof_desc = match field.pof_desc with
          | Otag(label, t) ->
              Otag(label, loop t)
          | Oinherit t ->
              Oinherit (loop t)
        in
        { field with pof_desc; }
      in
      loop t

  end

  module Pat = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {ppat_desc = d;
       ppat_loc = loc;
       ppat_loc_stack = [];
       ppat_attributes = attrs}
    let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
    let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
    let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
    let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
    let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
    let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
    let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
    let open_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_open (a, b))
    let exception_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_exception a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
  end

  module Exp = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pexp_desc = d;
       pexp_loc = loc;
       pexp_loc_stack = [];
       pexp_attributes = attrs}
    let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

    let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
    let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
    let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))
    let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pexp_fun (a, b, c, d))
    let function_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_function a)
    let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
    let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
    let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
    let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
    let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
    let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
    let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
    let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
    let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
    let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, b))
    let coerce ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_coerce (a, b, c))
    let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
    let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
    let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
    let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
    let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
    let letexception ?loc ?attrs a b = mk ?loc ?attrs (Pexp_letexception (a, b))
    let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
    let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
    let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
    let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
    let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
    let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_open (a, b))
    let letop ?loc ?attrs let_ ands body =
      mk ?loc ?attrs (Pexp_letop {let_; ands; body})
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)
    let unreachable ?loc ?attrs () = mk ?loc ?attrs Pexp_unreachable

    let case lhs ?guard rhs =
      {
        pc_lhs = lhs;
        pc_guard = guard;
        pc_rhs = rhs;
      }

    let binding_op op pat exp loc =
      {
        pbop_op = op;
        pbop_pat = pat;
        pbop_exp = exp;
        pbop_loc = loc;
      }
  end

  module Mty = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
    let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

    let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
    let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_alias a)
    let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
    let functor_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_functor (a, b))
    let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
    let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
  end

  module Mod = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
    let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]}

    let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
    let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
    let functor_ ?loc ?attrs arg body =
      mk ?loc ?attrs (Pmod_functor (arg, body))
    let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
    let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
    let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
  end

  module Sig = struct
    let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

    let value ?loc a = mk ?loc (Psig_value a)
    let type_ ?loc rec_flag a = mk ?loc (Psig_type (rec_flag, a))
    let type_subst ?loc a = mk ?loc (Psig_typesubst a)
    let type_extension ?loc a = mk ?loc (Psig_typext a)
    let exception_ ?loc a = mk ?loc (Psig_exception a)
    let module_ ?loc a = mk ?loc (Psig_module a)
    let mod_subst ?loc a = mk ?loc (Psig_modsubst a)
    let rec_module ?loc a = mk ?loc (Psig_recmodule a)
    let modtype ?loc a = mk ?loc (Psig_modtype a)
    let open_ ?loc a = mk ?loc (Psig_open a)
    let include_ ?loc a = mk ?loc (Psig_include a)
    let class_ ?loc a = mk ?loc (Psig_class a)
    let class_type ?loc a = mk ?loc (Psig_class_type a)
    let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
    let attribute ?loc a = mk ?loc (Psig_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt
  end

  module Str = struct
    let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

    let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval (a, attrs))
    let value ?loc a b = mk ?loc (Pstr_value (a, b))
    let primitive ?loc a = mk ?loc (Pstr_primitive a)
    let type_ ?loc rec_flag a = mk ?loc (Pstr_type (rec_flag, a))
    let type_extension ?loc a = mk ?loc (Pstr_typext a)
    let exception_ ?loc a = mk ?loc (Pstr_exception a)
    let module_ ?loc a = mk ?loc (Pstr_module a)
    let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
    let modtype ?loc a = mk ?loc (Pstr_modtype a)
    let open_ ?loc a = mk ?loc (Pstr_open a)
    let class_ ?loc a = mk ?loc (Pstr_class a)
    let class_type ?loc a = mk ?loc (Pstr_class_type a)
    let include_ ?loc a = mk ?loc (Pstr_include a)
    let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
    let attribute ?loc a = mk ?loc (Pstr_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt
  end

  module Cl = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {
        pcl_desc = d;
        pcl_loc = loc;
        pcl_attributes = attrs;
      }
    let attr d a = {d with pcl_attributes = d.pcl_attributes @ [a]}

    let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constr (a, b))
    let structure ?loc ?attrs a = mk ?loc ?attrs (Pcl_structure a)
    let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pcl_fun (a, b, c, d))
    let apply ?loc ?attrs a b = mk ?loc ?attrs (Pcl_apply (a, b))
    let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcl_let (a, b, c))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constraint (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcl_extension a)
    let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_open (a, b))
  end

  module Cty = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {
        pcty_desc = d;
        pcty_loc = loc;
        pcty_attributes = attrs;
      }
    let attr d a = {d with pcty_attributes = d.pcty_attributes @ [a]}

    let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcty_constr (a, b))
    let signature ?loc ?attrs a = mk ?loc ?attrs (Pcty_signature a)
    let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Pcty_arrow (a, b, c))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcty_extension a)
    let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pcty_open (a, b))
  end

  module Ctf = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) d =
      {
        pctf_desc = d;
        pctf_loc = loc;
        pctf_attributes = add_docs_attrs docs attrs;
      }

    let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inherit a)
    let val_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_val (a, b, c, d))
    let method_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_method (a, b, c, d))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_constraint (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pctf_extension a)
    let attribute ?loc a = mk ?loc (Pctf_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt

    let attr d a = {d with pctf_attributes = d.pctf_attributes @ [a]}

  end

  module Cf = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) d =
      {
        pcf_desc = d;
        pcf_loc = loc;
        pcf_attributes = add_docs_attrs docs attrs;
      }

    let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
    let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
    let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
    let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)
    let attribute ?loc a = mk ?loc (Pcf_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt

    let virtual_ ct = Cfk_virtual ct
    let concrete o e = Cfk_concrete (o, e)

    let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]}

  end

  module Val = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(prim = []) name typ =
      {
        pval_name = name;
        pval_type = typ;
        pval_attributes = add_docs_attrs docs attrs;
        pval_loc = loc;
        pval_prim = prim;
      }
  end

  module Md = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name typ =
      {
        pmd_name = name;
        pmd_type = typ;
        pmd_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        pmd_loc = loc;
      }
  end

  module Ms = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name syn =
      {
        pms_name = name;
        pms_manifest = syn;
        pms_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        pms_loc = loc;
      }
  end

  module Mtd = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) ?typ name =
      {
        pmtd_name = name;
        pmtd_type = typ;
        pmtd_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        pmtd_loc = loc;
      }
  end

  module Mb = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name expr =
      {
        pmb_name = name;
        pmb_expr = expr;
        pmb_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        pmb_loc = loc;
      }
  end

  module Opn = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(override = Fresh) expr =
      {
        popen_expr = expr;
        popen_override = override;
        popen_loc = loc;
        popen_attributes = add_docs_attrs docs attrs;
      }
  end

  module Incl = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs) mexpr =
      {
        pincl_mod = mexpr;
        pincl_loc = loc;
        pincl_attributes = add_docs_attrs docs attrs;
      }

  end

  module Vb = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(text = []) pat expr =
      {
        pvb_pat = pat;
        pvb_expr = expr;
        pvb_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        pvb_loc = loc;
      }
  end

  module Ci = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = [])
        ?(virt = Concrete) ?(params = []) name expr =
      {
        pci_virt = virt;
        pci_params = params;
        pci_name = name;
        pci_expr = expr;
        pci_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        pci_loc = loc;
      }
  end

  module Type = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = [])
        ?(params = [])
        ?(cstrs = [])
        ?(kind = Ptype_abstract)
        ?(priv = Public)
        ?manifest
        name =
      {
        ptype_name = name;
        ptype_params = params;
        ptype_cstrs = cstrs;
        ptype_kind = kind;
        ptype_private = priv;
        ptype_manifest = manifest;
        ptype_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        ptype_loc = loc;
      }

    let constructor ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
        ?(args = Pcstr_tuple []) ?res name =
      {
        pcd_name = name;
        pcd_args = args;
        pcd_res = res;
        pcd_loc = loc;
        pcd_attributes = add_info_attrs info attrs;
      }

    let field ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
        ?(mut = Immutable) name typ =
      {
        pld_name = name;
        pld_mutable = mut;
        pld_type = typ;
        pld_loc = loc;
        pld_attributes = add_info_attrs info attrs;
      }

  end

  (** Type extensions *)
  module Te = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(params = []) ?(priv = Public) path constructors =
      {
        ptyext_path = path;
        ptyext_params = params;
        ptyext_constructors = constructors;
        ptyext_private = priv;
        ptyext_loc = loc;
        ptyext_attributes = add_docs_attrs docs attrs;
      }

    let mk_exception ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        constructor =
      {
        ptyexn_constructor = constructor;
        ptyexn_loc = loc;
        ptyexn_attributes = add_docs_attrs docs attrs;
      }

    let constructor ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) name kind =
      {
        pext_name = name;
        pext_kind = kind;
        pext_loc = loc;
        pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

    let decl ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(info = empty_info) ?(args = Pcstr_tuple []) ?res name =
      {
        pext_name = name;
        pext_kind = Pext_decl(args, res);
        pext_loc = loc;
        pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

    let rebind ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) name lid =
      {
        pext_name = name;
        pext_kind = Pext_rebind lid;
        pext_loc = loc;
        pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

  end

  module Csig = struct
    let mk self fields =
      {
        pcsig_self = self;
        pcsig_fields = fields;
      }
  end

  module Cstr = struct
    let mk self fields =
      {
        pcstr_self = self;
        pcstr_fields = fields;
      }
  end

  (** Row fields *)
  module Rf = struct
    let mk ?(loc = !default_loc) ?(attrs = []) desc = {
      prf_desc = desc;
      prf_loc = loc;
      prf_attributes = attrs;
    }
    let tag ?loc ?attrs label const tys =
      mk ?loc ?attrs (Rtag (label, const, tys))
    let inherit_?loc ty =
      mk ?loc (Rinherit ty)
  end

  (** Object fields *)
  module Of = struct
    let mk ?(loc = !default_loc) ?(attrs=[]) desc = {
      pof_desc = desc;
      pof_loc = loc;
      pof_attributes = attrs;
    }
    let tag ?loc ?attrs label ty =
      mk ?loc ?attrs (Otag (label, ty))
    let inherit_ ?loc ty =
      mk ?loc (Oinherit ty)
  end
end

module Ast_mapper : sig
  open Parsetree

  (** {1 A generic Parsetree mapper} *)

  type mapper (*IF_CURRENT = Ast_mapper.mapper *) = {
    attribute: mapper -> attribute -> attribute;
    attributes: mapper -> attribute list -> attribute list;
    binding_op: mapper -> binding_op -> binding_op;
    case: mapper -> case -> case;
    cases: mapper -> case list -> case list;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration
      -> class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    constant: mapper -> constant -> constant;
    constructor_declaration: mapper -> constructor_declaration
      -> constructor_declaration;
    expr: mapper -> expression -> expression;
    extension: mapper -> extension -> extension;
    extension_constructor: mapper -> extension_constructor
      -> extension_constructor;
    include_declaration: mapper -> include_declaration -> include_declaration;
    include_description: mapper -> include_description -> include_description;
    label_declaration: mapper -> label_declaration -> label_declaration;
    location: mapper -> Location.t -> Location.t;
    module_binding: mapper -> module_binding -> module_binding;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_substitution: mapper -> module_substitution -> module_substitution;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration: mapper -> module_type_declaration
      -> module_type_declaration;
    open_declaration: mapper -> open_declaration -> open_declaration;
    open_description: mapper -> open_description -> open_description;
    pat: mapper -> pattern -> pattern;
    payload: mapper -> payload -> payload;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
    type_extension: mapper -> type_extension -> type_extension;
    type_exception: mapper -> type_exception -> type_exception;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }
  (** A mapper record implements one "method" per syntactic category,
      using an open recursion style: each method takes as its first
      argument the mapper to be applied to children in the syntax
      tree. *)

  val default_mapper: mapper
  (** A default mapper, which implements a "deep identity" mapping. *)

  (** {1 Apply mappers to compilation units} *)

  val tool_name: unit -> string
  (** Can be used within a ppx preprocessor to know which tool is
      calling it ["ocamlc"], ["ocamlopt"], ["ocamldoc"], ["ocamldep"],
      ["ocaml"], ...  Some global variables that reflect command-line
      options are automatically synchronized between the calling tool
      and the ppx preprocessor: {!Clflags.include_dirs},
      {!Load_path}, {!Clflags.open_modules}, {!Clflags.for_package},
      {!Clflags.debug}. *)


  val apply: source:string -> target:string -> mapper -> unit
  (** Apply a mapper (parametrized by the unit name) to a dumped
      parsetree found in the [source] file and put the result in the
      [target] file. The [structure] or [signature] field of the mapper
      is applied to the implementation or interface.  *)

  val run_main: (string list -> mapper) -> unit
  (** Entry point to call to implement a standalone -ppx rewriter from a
      mapper, parametrized by the command line arguments.  The current
      unit name can be obtained from {!Location.input_name}.  This
      function implements proper error reporting for uncaught
      exceptions. *)

  (** {1 Registration API} *)

  val register_function: (string -> (string list -> mapper) -> unit) ref

  val register: string -> (string list -> mapper) -> unit
  (** Apply the [register_function].  The default behavior is to run the
      mapper immediately, taking arguments from the process command
      line.  This is to support a scenario where a mapper is linked as a
      stand-alone executable.

      It is possible to overwrite the [register_function] to define
      "-ppx drivers", which combine several mappers in a single process.
      Typically, a driver starts by defining [register_function] to a
      custom implementation, then lets ppx rewriters (linked statically
      or dynamically) register themselves, and then run all or some of
      them.  It is also possible to have -ppx drivers apply rewriters to
      only specific parts of an AST.

      The first argument to [register] is a symbolic name to be used by
      the ppx driver.  *)


  (** {1 Convenience functions to write mappers} *)

  val map_opt: ('a -> 'b) -> 'a option -> 'b option

  val extension_of_error: Locations.location_error -> extension
  (** Encode an error into an 'ocaml.error' extension node which can be
      inserted in a generated Parsetree.  The compiler will be
      responsible for reporting the error. *)

  val attribute_of_warning: Location.t -> string -> attribute
  (** Encode a warning message into an 'ocaml.ppwarning' attribute which can be
      inserted in a generated Parsetree.  The compiler will be
      responsible for reporting the warning. *)

  include Locations.Helpers_intf

  (** {1 Helper functions to call external mappers} *)

  val add_ppx_context_str:
    tool_name:string -> Parsetree.structure -> Parsetree.structure
  (** Extract information from the current environment and encode it
      into an attribute which is prepended to the list of structure
      items in order to pass the information to an external
      processor. *)

  val add_ppx_context_sig:
    tool_name:string -> Parsetree.signature -> Parsetree.signature
  (** Same as [add_ppx_context_str], but for signatures. *)

  val drop_ppx_context_str:
    restore:bool -> Parsetree.structure -> Parsetree.structure
  (** Drop the ocaml.ppx.context attribute from a structure.  If
      [restore] is true, also restore the associated data in the current
      process. *)

  val drop_ppx_context_sig:
    restore:bool -> Parsetree.signature -> Parsetree.signature
  (** Same as [drop_ppx_context_str], but for signatures. *)

  (** {1 Cookies} *)

  (** Cookies are used to pass information from a ppx processor to
      a further invocation of itself, when called from the OCaml
      toplevel (or other tools that support cookies). *)

  val set_cookie: string -> Parsetree.expression -> unit
  val get_cookie: string -> Parsetree.expression option
end = struct
  open Parsetree
  open Ast_helper
  open Location

  module String = Misc.Stdlib.String

  type mapper (*IF_CURRENT = Ast_mapper.mapper *) = {
    attribute: mapper -> attribute -> attribute;
    attributes: mapper -> attribute list -> attribute list;
    binding_op: mapper -> binding_op -> binding_op;
    case: mapper -> case -> case;
    cases: mapper -> case list -> case list;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration
      -> class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    constant: mapper -> constant -> constant;
    constructor_declaration: mapper -> constructor_declaration
      -> constructor_declaration;
    expr: mapper -> expression -> expression;
    extension: mapper -> extension -> extension;
    extension_constructor: mapper -> extension_constructor
      -> extension_constructor;
    include_declaration: mapper -> include_declaration -> include_declaration;
    include_description: mapper -> include_description -> include_description;
    label_declaration: mapper -> label_declaration -> label_declaration;
    location: mapper -> Location.t -> Location.t;
    module_binding: mapper -> module_binding -> module_binding;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_substitution: mapper -> module_substitution -> module_substitution;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration: mapper -> module_type_declaration
      -> module_type_declaration;
    open_declaration: mapper -> open_declaration -> open_declaration;
    open_description: mapper -> open_description -> open_description;
    pat: mapper -> pattern -> pattern;
    payload: mapper -> payload -> payload;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
    type_extension: mapper -> type_extension -> type_extension;
    type_exception: mapper -> type_exception -> type_exception;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }

  let map_fst f (x, y) = (f x, y)
  let map_snd f (x, y) = (x, f y)
  let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
  let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
  let map_opt f = function None -> None | Some x -> Some (f x)

  let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

  module C = struct
    (* Constants *)

    let map sub c = match c with
      | Pconst_integer _
      | Pconst_char _
      | Pconst_float _
        -> c
      | Pconst_string (s, loc, quotation_delimiter) ->
          let loc = sub.location sub loc in
          Const.string ~loc ?quotation_delimiter s
  end

  module T = struct
    (* Type expressions for the core language *)

    let row_field sub {
        prf_desc;
        prf_loc;
        prf_attributes;
      } =
      let loc = sub.location sub prf_loc in
      let attrs = sub.attributes sub prf_attributes in
      let desc = match prf_desc with
        | Rtag (l, b, tl) -> Rtag (map_loc sub l, b, List.map (sub.typ sub) tl)
        | Rinherit t -> Rinherit (sub.typ sub t)
      in
      Rf.mk ~loc ~attrs desc

    let object_field sub {
        pof_desc;
        pof_loc;
        pof_attributes;
      } =
      let loc = sub.location sub pof_loc in
      let attrs = sub.attributes sub pof_attributes in
      let desc = match pof_desc with
        | Otag (l, t) -> Otag (map_loc sub l, sub.typ sub t)
        | Oinherit t -> Oinherit (sub.typ sub t)
      in
      Of.mk ~loc ~attrs desc

    let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
      let open Typ in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Ptyp_any -> any ~loc ~attrs ()
      | Ptyp_var s -> var ~loc ~attrs s
      | Ptyp_arrow (lab, t1, t2) ->
          arrow ~loc ~attrs lab (sub.typ sub t1) (sub.typ sub t2)
      | Ptyp_tuple tyl -> tuple ~loc ~attrs (List.map (sub.typ sub) tyl)
      | Ptyp_constr (lid, tl) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
      | Ptyp_object (l, o) ->
          object_ ~loc ~attrs (List.map (object_field sub) l) o
      | Ptyp_class (lid, tl) ->
          class_ ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
      | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub.typ sub t) s
      | Ptyp_variant (rl, b, ll) ->
          variant ~loc ~attrs (List.map (row_field sub) rl) b ll
      | Ptyp_poly (sl, t) -> poly ~loc ~attrs
                               (List.map (map_loc sub) sl) (sub.typ sub t)
      | Ptyp_package (lid, l) ->
          package ~loc ~attrs (map_loc sub lid)
            (List.map (map_tuple (map_loc sub) (sub.typ sub)) l)
      | Ptyp_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_type_declaration sub
        {ptype_name; ptype_params; ptype_cstrs;
         ptype_kind;
         ptype_private;
         ptype_manifest;
         ptype_attributes;
         ptype_loc} =
      let loc = sub.location sub ptype_loc in
      let attrs = sub.attributes sub ptype_attributes in
      Type.mk ~loc ~attrs (map_loc sub ptype_name)
        ~params:(List.map (map_fst (sub.typ sub)) ptype_params)
        ~priv:ptype_private
        ~cstrs:(List.map
                  (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
                  ptype_cstrs)
        ~kind:(sub.type_kind sub ptype_kind)
        ?manifest:(map_opt (sub.typ sub) ptype_manifest)

    let map_type_kind sub = function
      | Ptype_abstract -> Ptype_abstract
      | Ptype_variant l ->
          Ptype_variant (List.map (sub.constructor_declaration sub) l)
      | Ptype_record l -> Ptype_record (List.map (sub.label_declaration sub) l)
      | Ptype_open -> Ptype_open

    let map_constructor_arguments sub = function
      | Pcstr_tuple l -> Pcstr_tuple (List.map (sub.typ sub) l)
      | Pcstr_record l ->
          Pcstr_record (List.map (sub.label_declaration sub) l)

    let map_type_extension sub
        {ptyext_path; ptyext_params;
         ptyext_constructors;
         ptyext_private;
         ptyext_loc;
         ptyext_attributes} =
      let loc = sub.location sub ptyext_loc in
      let attrs = sub.attributes sub ptyext_attributes in
      Te.mk ~loc ~attrs
        (map_loc sub ptyext_path)
        (List.map (sub.extension_constructor sub) ptyext_constructors)
        ~params:(List.map (map_fst (sub.typ sub)) ptyext_params)
        ~priv:ptyext_private

    let map_type_exception sub
        {ptyexn_constructor; ptyexn_loc; ptyexn_attributes} =
      let loc = sub.location sub ptyexn_loc in
      let attrs = sub.attributes sub ptyexn_attributes in
      Te.mk_exception ~loc ~attrs
        (sub.extension_constructor sub ptyexn_constructor)

    let map_extension_constructor_kind sub = function
        Pext_decl(ctl, cto) ->
          Pext_decl(map_constructor_arguments sub ctl, map_opt (sub.typ sub) cto)
      | Pext_rebind li ->
          Pext_rebind (map_loc sub li)

    let map_extension_constructor sub
        {pext_name;
         pext_kind;
         pext_loc;
         pext_attributes} =
      let loc = sub.location sub pext_loc in
      let attrs = sub.attributes sub pext_attributes in
      Te.constructor ~loc ~attrs
        (map_loc sub pext_name)
        (map_extension_constructor_kind sub pext_kind)

  end

  module CT = struct
    (* Type expressions for the class language *)

    let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
      let open Cty in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcty_constr (lid, tys) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
      | Pcty_signature x -> signature ~loc ~attrs (sub.class_signature sub x)
      | Pcty_arrow (lab, t, ct) ->
          arrow ~loc ~attrs lab (sub.typ sub t) (sub.class_type sub ct)
      | Pcty_extension x -> extension ~loc ~attrs (sub.extension sub x)
      | Pcty_open (o, ct) ->
          open_ ~loc ~attrs (sub.open_description sub o) (sub.class_type sub ct)

    let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
      =
      let open Ctf in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pctf_inherit ct -> inherit_ ~loc ~attrs (sub.class_type sub ct)
      | Pctf_val (s, m, v, t) ->
          val_ ~loc ~attrs (map_loc sub s) m v (sub.typ sub t)
      | Pctf_method (s, p, v, t) ->
          method_ ~loc ~attrs (map_loc sub s) p v (sub.typ sub t)
      | Pctf_constraint (t1, t2) ->
          constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
      | Pctf_attribute x -> attribute ~loc (sub.attribute sub x)
      | Pctf_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_signature sub {pcsig_self; pcsig_fields} =
      Csig.mk
        (sub.typ sub pcsig_self)
        (List.map (sub.class_type_field sub) pcsig_fields)
  end

  let map_functor_param sub = function
    | Unit -> Unit
    | Named (s, mt) -> Named (map_loc sub s, sub.module_type sub mt)

  module MT = struct
    (* Type expressions for the module language *)

    let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
      let open Mty in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
      | Pmty_alias s -> alias ~loc ~attrs (map_loc sub s)
      | Pmty_signature sg -> signature ~loc ~attrs (sub.signature sub sg)
      | Pmty_functor (param, mt) ->
          functor_ ~loc ~attrs
            (map_functor_param sub param)
            (sub.module_type sub mt)
      | Pmty_with (mt, l) ->
          with_ ~loc ~attrs (sub.module_type sub mt)
            (List.map (sub.with_constraint sub) l)
      | Pmty_typeof me -> typeof_ ~loc ~attrs (sub.module_expr sub me)
      | Pmty_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_with_constraint sub = function
      | Pwith_type (lid, d) ->
          Pwith_type (map_loc sub lid, sub.type_declaration sub d)
      | Pwith_module (lid, lid2) ->
          Pwith_module (map_loc sub lid, map_loc sub lid2)
      | Pwith_typesubst (lid, d) ->
          Pwith_typesubst (map_loc sub lid, sub.type_declaration sub d)
      | Pwith_modsubst (s, lid) ->
          Pwith_modsubst (map_loc sub s, map_loc sub lid)

    let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
      let open Sig in
      let loc = sub.location sub loc in
      match desc with
      | Psig_value vd -> value ~loc (sub.value_description sub vd)
      | Psig_type (rf, l) ->
          type_ ~loc rf (List.map (sub.type_declaration sub) l)
      | Psig_typesubst l ->
          type_subst ~loc (List.map (sub.type_declaration sub) l)
      | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
      | Psig_exception ed -> exception_ ~loc (sub.type_exception sub ed)
      | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
      | Psig_modsubst x -> mod_subst ~loc (sub.module_substitution sub x)
      | Psig_recmodule l ->
          rec_module ~loc (List.map (sub.module_declaration sub) l)
      | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
      | Psig_open x -> open_ ~loc (sub.open_description sub x)
      | Psig_include x -> include_ ~loc (sub.include_description sub x)
      | Psig_class l -> class_ ~loc (List.map (sub.class_description sub) l)
      | Psig_class_type l ->
          class_type ~loc (List.map (sub.class_type_declaration sub) l)
      | Psig_extension (x, attrs) ->
          let attrs = sub.attributes sub attrs in
          extension ~loc ~attrs (sub.extension sub x)
      | Psig_attribute x -> attribute ~loc (sub.attribute sub x)
  end


  module M = struct
    (* Value expressions for the module language *)

    let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
      let open Mod in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
      | Pmod_structure str -> structure ~loc ~attrs (sub.structure sub str)
      | Pmod_functor (param, body) ->
          functor_ ~loc ~attrs
            (map_functor_param sub param)
            (sub.module_expr sub body)
      | Pmod_apply (m1, m2) ->
          apply ~loc ~attrs (sub.module_expr sub m1) (sub.module_expr sub m2)
      | Pmod_constraint (m, mty) ->
          constraint_ ~loc ~attrs (sub.module_expr sub m)
            (sub.module_type sub mty)
      | Pmod_unpack e -> unpack ~loc ~attrs (sub.expr sub e)
      | Pmod_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
      let open Str in
      let loc = sub.location sub loc in
      match desc with
      | Pstr_eval (x, attrs) ->
          let attrs = sub.attributes sub attrs in
          eval ~loc ~attrs (sub.expr sub x)
      | Pstr_value (r, vbs) -> value ~loc r (List.map (sub.value_binding sub) vbs)
      | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
      | Pstr_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
      | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
      | Pstr_exception ed -> exception_ ~loc (sub.type_exception sub ed)
      | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
      | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
      | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
      | Pstr_open x -> open_ ~loc (sub.open_declaration sub x)
      | Pstr_class l -> class_ ~loc (List.map (sub.class_declaration sub) l)
      | Pstr_class_type l ->
          class_type ~loc (List.map (sub.class_type_declaration sub) l)
      | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
      | Pstr_extension (x, attrs) ->
          let attrs = sub.attributes sub attrs in
          extension ~loc ~attrs (sub.extension sub x)
      | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
  end

  module E = struct
    (* Value expressions for the core language *)

    let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
      let open Exp in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
      | Pexp_constant x -> constant ~loc ~attrs (sub.constant sub x)
      | Pexp_let (r, vbs, e) ->
          let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
            (sub.expr sub e)
      | Pexp_fun (lab, def, p, e) ->
          fun_ ~loc ~attrs lab (map_opt (sub.expr sub) def) (sub.pat sub p)
            (sub.expr sub e)
      | Pexp_function pel -> function_ ~loc ~attrs (sub.cases sub pel)
      | Pexp_apply (e, l) ->
          apply ~loc ~attrs (sub.expr sub e) (List.map (map_snd (sub.expr sub)) l)
      | Pexp_match (e, pel) ->
          match_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
      | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
      | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub.expr sub) el)
      | Pexp_construct (lid, arg) ->
          construct ~loc ~attrs (map_loc sub lid) (map_opt (sub.expr sub) arg)
      | Pexp_variant (lab, eo) ->
          variant ~loc ~attrs lab (map_opt (sub.expr sub) eo)
      | Pexp_record (l, eo) ->
          record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub.expr sub)) l)
            (map_opt (sub.expr sub) eo)
      | Pexp_field (e, lid) ->
          field ~loc ~attrs (sub.expr sub e) (map_loc sub lid)
      | Pexp_setfield (e1, lid, e2) ->
          setfield ~loc ~attrs (sub.expr sub e1) (map_loc sub lid)
            (sub.expr sub e2)
      | Pexp_array el -> array ~loc ~attrs (List.map (sub.expr sub) el)
      | Pexp_ifthenelse (e1, e2, e3) ->
          ifthenelse ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
            (map_opt (sub.expr sub) e3)
      | Pexp_sequence (e1, e2) ->
          sequence ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
      | Pexp_while (e1, e2) ->
          while_ ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
      | Pexp_for (p, e1, e2, d, e3) ->
          for_ ~loc ~attrs (sub.pat sub p) (sub.expr sub e1) (sub.expr sub e2) d
            (sub.expr sub e3)
      | Pexp_coerce (e, t1, t2) ->
          coerce ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t1)
            (sub.typ sub t2)
      | Pexp_constraint (e, t) ->
          constraint_ ~loc ~attrs (sub.expr sub e) (sub.typ sub t)
      | Pexp_send (e, s) ->
          send ~loc ~attrs (sub.expr sub e) (map_loc sub s)
      | Pexp_new lid -> new_ ~loc ~attrs (map_loc sub lid)
      | Pexp_setinstvar (s, e) ->
          setinstvar ~loc ~attrs (map_loc sub s) (sub.expr sub e)
      | Pexp_override sel ->
          override ~loc ~attrs
            (List.map (map_tuple (map_loc sub) (sub.expr sub)) sel)
      | Pexp_letmodule (s, me, e) ->
          letmodule ~loc ~attrs (map_loc sub s) (sub.module_expr sub me)
            (sub.expr sub e)
      | Pexp_letexception (cd, e) ->
          letexception ~loc ~attrs
            (sub.extension_constructor sub cd)
            (sub.expr sub e)
      | Pexp_assert e -> assert_ ~loc ~attrs (sub.expr sub e)
      | Pexp_lazy e -> lazy_ ~loc ~attrs (sub.expr sub e)
      | Pexp_poly (e, t) ->
          poly ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t)
      | Pexp_object cls -> object_ ~loc ~attrs (sub.class_structure sub cls)
      | Pexp_newtype (s, e) ->
          newtype ~loc ~attrs (map_loc sub s) (sub.expr sub e)
      | Pexp_pack me -> pack ~loc ~attrs (sub.module_expr sub me)
      | Pexp_open (o, e) ->
          open_ ~loc ~attrs (sub.open_declaration sub o) (sub.expr sub e)
      | Pexp_letop {let_; ands; body} ->
          letop ~loc ~attrs (sub.binding_op sub let_)
            (List.map (sub.binding_op sub) ands) (sub.expr sub body)
      | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
      | Pexp_unreachable -> unreachable ~loc ~attrs ()

    let map_binding_op sub {pbop_op; pbop_pat; pbop_exp; pbop_loc} =
      let open Exp in
      let op = map_loc sub pbop_op in
      let pat = sub.pat sub pbop_pat in
      let exp = sub.expr sub pbop_exp in
      let loc = sub.location sub pbop_loc in
      binding_op op pat exp loc

  end

  module P = struct
    (* Patterns *)

    let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
      let open Pat in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Ppat_any -> any ~loc ~attrs ()
      | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
      | Ppat_alias (p, s) -> alias ~loc ~attrs (sub.pat sub p) (map_loc sub s)
      | Ppat_constant c -> constant ~loc ~attrs (sub.constant sub c)
      | Ppat_interval (c1, c2) -> interval ~loc ~attrs c1 c2
      | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub.pat sub) pl)
      | Ppat_construct (l, p) ->
          construct ~loc ~attrs (map_loc sub l) (map_opt (sub.pat sub) p)
      | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub.pat sub) p)
      | Ppat_record (lpl, cf) ->
          record ~loc ~attrs
            (List.map (map_tuple (map_loc sub) (sub.pat sub)) lpl) cf
      | Ppat_array pl -> array ~loc ~attrs (List.map (sub.pat sub) pl)
      | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub.pat sub p1) (sub.pat sub p2)
      | Ppat_constraint (p, t) ->
          constraint_ ~loc ~attrs (sub.pat sub p) (sub.typ sub t)
      | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
      | Ppat_lazy p -> lazy_ ~loc ~attrs (sub.pat sub p)
      | Ppat_unpack s -> unpack ~loc ~attrs (map_loc sub s)
      | Ppat_open (lid,p) -> open_ ~loc ~attrs (map_loc sub lid) (sub.pat sub p)
      | Ppat_exception p -> exception_ ~loc ~attrs (sub.pat sub p)
      | Ppat_extension x -> extension ~loc ~attrs (sub.extension sub x)
  end

  module CE = struct
    (* Value expressions for the class language *)

    let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
      let open Cl in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcl_constr (lid, tys) ->
          constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
      | Pcl_structure s ->
          structure ~loc ~attrs (sub.class_structure sub s)
      | Pcl_fun (lab, e, p, ce) ->
          fun_ ~loc ~attrs lab
            (map_opt (sub.expr sub) e)
            (sub.pat sub p)
            (sub.class_expr sub ce)
      | Pcl_apply (ce, l) ->
          apply ~loc ~attrs (sub.class_expr sub ce)
            (List.map (map_snd (sub.expr sub)) l)
      | Pcl_let (r, vbs, ce) ->
          let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
            (sub.class_expr sub ce)
      | Pcl_constraint (ce, ct) ->
          constraint_ ~loc ~attrs (sub.class_expr sub ce) (sub.class_type sub ct)
      | Pcl_extension x -> extension ~loc ~attrs (sub.extension sub x)
      | Pcl_open (o, ce) ->
          open_ ~loc ~attrs (sub.open_description sub o) (sub.class_expr sub ce)

    let map_kind sub = function
      | Cfk_concrete (o, e) -> Cfk_concrete (o, sub.expr sub e)
      | Cfk_virtual t -> Cfk_virtual (sub.typ sub t)

    let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
      let open Cf in
      let loc = sub.location sub loc in
      let attrs = sub.attributes sub attrs in
      match desc with
      | Pcf_inherit (o, ce, s) ->
          inherit_ ~loc ~attrs o (sub.class_expr sub ce)
            (map_opt (map_loc sub) s)
      | Pcf_val (s, m, k) -> val_ ~loc ~attrs (map_loc sub s) m (map_kind sub k)
      | Pcf_method (s, p, k) ->
          method_ ~loc ~attrs (map_loc sub s) p (map_kind sub k)
      | Pcf_constraint (t1, t2) ->
          constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
      | Pcf_initializer e -> initializer_ ~loc ~attrs (sub.expr sub e)
      | Pcf_attribute x -> attribute ~loc (sub.attribute sub x)
      | Pcf_extension x -> extension ~loc ~attrs (sub.extension sub x)

    let map_structure sub {pcstr_self; pcstr_fields} =
      {
        pcstr_self = sub.pat sub pcstr_self;
        pcstr_fields = List.map (sub.class_field sub) pcstr_fields;
      }

    let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                           pci_loc; pci_attributes} =
      let loc = sub.location sub pci_loc in
      let attrs = sub.attributes sub pci_attributes in
      Ci.mk ~loc ~attrs
        ~virt:pci_virt
        ~params:(List.map (map_fst (sub.typ sub)) pl)
        (map_loc sub pci_name)
        (f pci_expr)
  end

  (* Now, a generic AST mapper, to be extended to cover all kinds and
     cases of the OCaml grammar.  The default behavior of the mapper is
     the identity. *)

  let default_mapper =
    {
      constant = C.map;
      structure = (fun this l -> List.map (this.structure_item this) l);
      structure_item = M.map_structure_item;
      module_expr = M.map;
      signature = (fun this l -> List.map (this.signature_item this) l);
      signature_item = MT.map_signature_item;
      module_type = MT.map;
      with_constraint = MT.map_with_constraint;
      class_declaration =
        (fun this -> CE.class_infos this (this.class_expr this));
      class_expr = CE.map;
      class_field = CE.map_field;
      class_structure = CE.map_structure;
      class_type = CT.map;
      class_type_field = CT.map_field;
      class_signature = CT.map_signature;
      class_type_declaration =
        (fun this -> CE.class_infos this (this.class_type this));
      class_description =
        (fun this -> CE.class_infos this (this.class_type this));
      type_declaration = T.map_type_declaration;
      type_kind = T.map_type_kind;
      typ = T.map;
      type_extension = T.map_type_extension;
      type_exception = T.map_type_exception;
      extension_constructor = T.map_extension_constructor;
      value_description =
        (fun this {pval_name; pval_type; pval_prim; pval_loc;
                   pval_attributes} ->
          Val.mk
            (map_loc this pval_name)
            (this.typ this pval_type)
            ~attrs:(this.attributes this pval_attributes)
            ~loc:(this.location this pval_loc)
            ~prim:pval_prim
        );

      pat = P.map;
      expr = E.map;
      binding_op = E.map_binding_op;

      module_declaration =
        (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
           Md.mk
             (map_loc this pmd_name)
             (this.module_type this pmd_type)
             ~attrs:(this.attributes this pmd_attributes)
             ~loc:(this.location this pmd_loc)
        );

      module_substitution =
        (fun this {pms_name; pms_manifest; pms_attributes; pms_loc} ->
           Ms.mk
             (map_loc this pms_name)
             (map_loc this pms_manifest)
             ~attrs:(this.attributes this pms_attributes)
             ~loc:(this.location this pms_loc)
        );

      module_type_declaration =
        (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
           Mtd.mk
             (map_loc this pmtd_name)
             ?typ:(map_opt (this.module_type this) pmtd_type)
             ~attrs:(this.attributes this pmtd_attributes)
             ~loc:(this.location this pmtd_loc)
        );

      module_binding =
        (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
           Mb.mk (map_loc this pmb_name) (this.module_expr this pmb_expr)
             ~attrs:(this.attributes this pmb_attributes)
             ~loc:(this.location this pmb_loc)
        );


      open_declaration =
        (fun this {popen_expr; popen_override; popen_attributes; popen_loc} ->
           Opn.mk (this.module_expr this popen_expr)
             ~override:popen_override
             ~loc:(this.location this popen_loc)
             ~attrs:(this.attributes this popen_attributes)
        );

      open_description =
        (fun this {popen_expr; popen_override; popen_attributes; popen_loc} ->
           Opn.mk (map_loc this popen_expr)
             ~override:popen_override
             ~loc:(this.location this popen_loc)
             ~attrs:(this.attributes this popen_attributes)
        );

      include_description =
        (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
           Incl.mk (this.module_type this pincl_mod)
             ~loc:(this.location this pincl_loc)
             ~attrs:(this.attributes this pincl_attributes)
        );

      include_declaration =
        (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
           Incl.mk (this.module_expr this pincl_mod)
             ~loc:(this.location this pincl_loc)
             ~attrs:(this.attributes this pincl_attributes)
        );


      value_binding =
        (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
           Vb.mk
             (this.pat this pvb_pat)
             (this.expr this pvb_expr)
             ~loc:(this.location this pvb_loc)
             ~attrs:(this.attributes this pvb_attributes)
        );


      constructor_declaration =
        (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
           Type.constructor
             (map_loc this pcd_name)
             ~args:(T.map_constructor_arguments this pcd_args)
             ?res:(map_opt (this.typ this) pcd_res)
             ~loc:(this.location this pcd_loc)
             ~attrs:(this.attributes this pcd_attributes)
        );

      label_declaration =
        (fun this {pld_name; pld_type; pld_loc; pld_mutable; pld_attributes} ->
           Type.field
             (map_loc this pld_name)
             (this.typ this pld_type)
             ~mut:pld_mutable
             ~loc:(this.location this pld_loc)
             ~attrs:(this.attributes this pld_attributes)
        );

      cases = (fun this l -> List.map (this.case this) l);
      case =
        (fun this {pc_lhs; pc_guard; pc_rhs} ->
           {
             pc_lhs = this.pat this pc_lhs;
             pc_guard = map_opt (this.expr this) pc_guard;
             pc_rhs = this.expr this pc_rhs;
           }
        );



      location = (fun _this l -> l);

      extension = (fun this (s, e) -> (map_loc this s, this.payload this e));
      attribute = (fun this a ->
          {
            attr_name = map_loc this a.attr_name;
            attr_payload = this.payload this a.attr_payload;
            attr_loc = this.location this a.attr_loc
          }
        );
      attributes = (fun this l -> List.map (this.attribute this) l);
      payload =
        (fun this -> function
           | PStr x -> PStr (this.structure this x)
           | PSig x -> PSig (this.signature this x)
           | PTyp x -> PTyp (this.typ this x)
           | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g)
        );
    }

  let extension_of_error (error : Locations.location_error) : extension =
    Locations.extension_of_error
      ~mk_pstr:(fun x -> PStr x)
      ~mk_extension:(fun x -> Str.extension x)
      ~mk_string_constant:(fun x -> Str.eval (Exp.constant (Pconst_string (x, Location.none (* XXX *), None))))
      error

  let attribute_of_warning loc s =
    Attr.mk
      {loc; txt = "ocaml.ppwarning" }
      (PStr ([Str.eval ~loc (Exp.constant (Pconst_string (s, loc, None)))]))

  include Locations.Helpers_impl

  let cookies = ref String.Map.empty

  let get_cookie k =
    try Some (String.Map.find k !cookies)
    with Not_found -> None

  let set_cookie k v =
    cookies := String.Map.add k v !cookies

  let tool_name_ref = ref "_none_"

  let tool_name () = !tool_name_ref


  module PpxContext = struct
    open Longident
    open Asttypes
    open Ast_helper

    let lid name = { txt = Lident name; loc = Location.none }

    let make_string s = Exp.constant (Const.string s)

    let make_bool x =
      if x
      then Exp.construct (lid "true") None
      else Exp.construct (lid "false") None

    let rec make_list f lst =
      match lst with
      | x :: rest ->
          Exp.construct (lid "::") (Some (Exp.tuple [f x; make_list f rest]))
      | [] ->
          Exp.construct (lid "[]") None

    let make_pair f1 f2 (x1, x2) =
      Exp.tuple [f1 x1; f2 x2]

    let make_option f opt =
      match opt with
      | Some x -> Exp.construct (lid "Some") (Some (f x))
      | None   -> Exp.construct (lid "None") None

    let get_cookies () =
      lid "cookies",
      make_list (make_pair make_string (fun x -> x))
        (String.Map.bindings !cookies)

    let mk fields =
      {
        attr_name = { txt = "ocaml.ppx.context"; loc = Location.none };
        attr_payload = Parsetree.PStr [Str.eval (Exp.record fields None)];
        attr_loc = Location.none
      }

    let make ~tool_name () =
      let fields =
        [
          lid "tool_name",    make_string tool_name;
          lid "include_dirs", make_list make_string !Clflags.include_dirs;
          lid "load_path",    make_list make_string (Migrate_parsetree_compiler_functions.get_load_paths ());
          lid "open_modules", make_list make_string !Clflags.open_modules;
          lid "for_package",  make_option make_string !Clflags.for_package;
          lid "debug",        make_bool !Clflags.debug;
          lid "use_threads",  make_bool !Clflags.use_threads;
          lid "use_vmthreads", make_bool false;
          lid "recursive_types", make_bool !Clflags.recursive_types;
          lid "principal", make_bool !Clflags.principal;
          lid "transparent_modules", make_bool !Clflags.transparent_modules;
          lid "unboxed_types", make_bool (Migrate_parsetree_compiler_functions.get_unboxed_types ());
          get_cookies ()
        ]
      in
      mk fields

    let get_fields = function
      | PStr [{pstr_desc = Pstr_eval
                   ({ pexp_desc = Pexp_record (fields, None) }, [])}] ->
          fields
      | _ ->
          raise_errorf "Internal error: invalid [@@@ocaml.ppx.context] syntax"

    let restore fields =
      let field name payload =
        let rec get_string = function
          | { pexp_desc = Pexp_constant (Pconst_string (str, _, None)) } -> str
          | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                               { %s }] string syntax" name
        and get_bool pexp =
          match pexp with
          | {pexp_desc = Pexp_construct ({txt = Longident.Lident "true"},
                                         None)} ->
              true
          | {pexp_desc = Pexp_construct ({txt = Longident.Lident "false"},
                                         None)} ->
              false
          | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                               { %s }] bool syntax" name
        and get_list elem = function
          | {pexp_desc =
               Pexp_construct ({txt = Longident.Lident "::"},
                               Some {pexp_desc = Pexp_tuple [exp; rest]}) } ->
              elem exp :: get_list elem rest
          | {pexp_desc =
               Pexp_construct ({txt = Longident.Lident "[]"}, None)} ->
              []
          | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                               { %s }] list syntax" name
        and get_pair f1 f2 = function
          | {pexp_desc = Pexp_tuple [e1; e2]} ->
              (f1 e1, f2 e2)
          | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                               { %s }] pair syntax" name
        and get_option elem = function
          | { pexp_desc =
                Pexp_construct ({ txt = Longident.Lident "Some" }, Some exp) } ->
              Some (elem exp)
          | { pexp_desc =
                Pexp_construct ({ txt = Longident.Lident "None" }, None) } ->
              None
          | _ -> raise_errorf "Internal error: invalid [@@@ocaml.ppx.context \
                               { %s }] option syntax" name
        in
        match name with
        | "tool_name" ->
            tool_name_ref := get_string payload
        | "include_dirs" ->
            Clflags.include_dirs := get_list get_string payload
        | "load_path" ->
            Migrate_parsetree_compiler_functions.load_path_init (get_list get_string payload)
        | "open_modules" ->
            Clflags.open_modules := get_list get_string payload
        | "for_package" ->
            Clflags.for_package := get_option get_string payload
        | "debug" ->
            Clflags.debug := get_bool payload
        | "use_threads" ->
            Clflags.use_threads := get_bool payload
        | "use_vmthreads" ->
            if get_bool payload then
              raise_errorf "Internal error: vmthreads not supported after 4.09.0"
        | "recursive_types" ->
            Clflags.recursive_types := get_bool payload
        | "principal" ->
            Clflags.principal := get_bool payload
        | "transparent_modules" ->
            Clflags.transparent_modules := get_bool payload
        | "unboxed_types" ->
            Migrate_parsetree_compiler_functions.set_unboxed_types (get_bool payload)
        | "cookies" ->
            let l = get_list (get_pair get_string (fun x -> x)) payload in
            cookies :=
              List.fold_left
                (fun s (k, v) -> String.Map.add k v s) String.Map.empty
                l
        | _ ->
            ()
      in
      List.iter (function ({txt=Lident name}, x) -> field name x | _ -> ()) fields

    let update_cookies fields =
      let fields =
        List.filter
          (function ({txt=Lident "cookies"}, _) -> false | _ -> true)
          fields
      in
      fields @ [get_cookies ()]
  end

  let ppx_context = PpxContext.make

  let extension_of_exn exn = extension_of_error (Locations.location_error_of_exn exn)

  let apply_lazy ~source ~target mapper =
    let implem ast =
      let fields, ast =
        match ast with
        | {pstr_desc = Pstr_attribute ({attr_name = {txt = "ocaml.ppx.context"};
                                        attr_payload = x})} :: l ->
            PpxContext.get_fields x, l
        | _ -> [], ast
      in
      PpxContext.restore fields;
      let ast =
        try
          let mapper = mapper () in
          mapper.structure mapper ast
        with exn ->
          [{pstr_desc = Pstr_extension (extension_of_exn exn, []);
            pstr_loc  = Location.none}]
      in
      let fields = PpxContext.update_cookies fields in
      Str.attribute (PpxContext.mk fields) :: ast
    in
    let iface ast =
      let fields, ast =
        match ast with
        | {psig_desc = Psig_attribute ({attr_name = {txt = "ocaml.ppx.context"};
                                        attr_payload = x;
                                        attr_loc = _})} :: l ->
            PpxContext.get_fields x, l
        | _ -> [], ast
      in
      PpxContext.restore fields;
      let ast =
        try
          let mapper = mapper () in
          mapper.signature mapper ast
        with exn ->
          [{psig_desc = Psig_extension (extension_of_exn exn, []);
            psig_loc  = Location.none}]
      in
      let fields = PpxContext.update_cookies fields in
      Sig.attribute (PpxContext.mk fields) :: ast
    in

    let ic = open_in_bin source in
    let magic =
      really_input_string ic (String.length Config.ast_impl_magic_number)
    in

    let rewrite transform =
      Location.input_name := input_value ic;
      let ast = input_value ic in
      close_in ic;
      let ast = transform ast in
      let oc = open_out_bin target in
      output_string oc magic;
      output_value oc !Location.input_name;
      output_value oc ast;
      close_out oc
    and fail () =
      close_in ic;
      failwith "Ast_mapper: OCaml version mismatch or malformed input";
    in

    if magic = Config.ast_impl_magic_number then
      rewrite (implem : structure -> structure)
    else if magic = Config.ast_intf_magic_number then
      rewrite (iface : signature -> signature)
    else fail ()

  let drop_ppx_context_str ~restore = function
    | {pstr_desc = Pstr_attribute
           {attr_name = {Location.txt = "ocaml.ppx.context"};
            attr_payload = a;
            attr_loc = _}}
      :: items ->
        if restore then
          PpxContext.restore (PpxContext.get_fields a);
        items
    | items -> items

  let drop_ppx_context_sig ~restore = function
    | {psig_desc = Psig_attribute
           {attr_name = {Location.txt = "ocaml.ppx.context"};
            attr_payload = a;
            attr_loc = _}}
      :: items ->
        if restore then
          PpxContext.restore (PpxContext.get_fields a);
        items
    | items -> items

  let add_ppx_context_str ~tool_name ast =
    Ast_helper.Str.attribute (ppx_context ~tool_name ()) :: ast

  let add_ppx_context_sig ~tool_name ast =
    Ast_helper.Sig.attribute (ppx_context ~tool_name ()) :: ast


  let apply ~source ~target mapper =
    apply_lazy ~source ~target (fun () -> mapper)

  let run_main mapper =
    try
      let a = Sys.argv in
      let n = Array.length a in
      if n > 2 then
        let mapper () =
          try mapper (Array.to_list (Array.sub a 1 (n - 3)))
          with exn ->
            (* PR#6463 *)
            let f _ _ = raise exn in
            {default_mapper with structure = f; signature = f}
        in
        apply_lazy ~source:a.(n - 2) ~target:a.(n - 1) mapper
      else begin
        Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
          Sys.executable_name;
        exit 2
      end
    with exn ->
      prerr_endline (Printexc.to_string exn);
      exit 2

  let register_function = ref (fun _name f -> run_main f)
  let register name f = !register_function name f
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

module Config = struct
  let ast_impl_magic_number = "Caml1999M028"
  let ast_intf_magic_number = "Caml1999N028"
end

let map_signature mapper = mapper.Ast_mapper.signature mapper
let map_structure mapper = mapper.Ast_mapper.structure mapper

let shallow_identity =
  let id _ x = x in
  {
    Ast_mapper.
    structure               = id;
    structure_item          = id;
    module_expr             = id;
    signature               = id;
    signature_item          = id;
    module_type             = id;
    with_constraint         = id;
    class_declaration       = id;
    class_expr              = id;
    class_field             = id;
    class_structure         = id;
    class_type              = id;
    class_type_field        = id;
    class_signature         = id;
    class_type_declaration  = id;
    class_description       = id;
    type_declaration        = id;
    type_kind               = id;
    typ                     = id;
    type_extension          = id;
    extension_constructor   = id;
    value_description       = id;
    pat                     = id;
    expr                    = id;
    module_declaration      = id;
    module_type_declaration = id;
    module_binding          = id;
    open_description        = id;
    include_description     = id;
    include_declaration     = id;
    value_binding           = id;
    constructor_declaration = id;
    label_declaration       = id;
    cases                   = id;
    case                    = id;
    location                = id;
    extension               = id;
    attribute               = id;
    attributes              = id;
    payload                 = id;
    binding_op              = id;
    module_substitution     = id;
    open_declaration        = id;
    type_exception          = id;
    constant                = id;
  }

let failing_mapper =
  let fail _ _ =
    invalid_arg "failing_mapper: this mapper function should never get called"
  in
  {
    Ast_mapper.
    structure               = fail;
    structure_item          = fail;
    module_expr             = fail;
    signature               = fail;
    signature_item          = fail;
    module_type             = fail;
    with_constraint         = fail;
    class_declaration       = fail;
    class_expr              = fail;
    class_field             = fail;
    class_structure         = fail;
    class_type              = fail;
    class_type_field        = fail;
    class_signature         = fail;
    class_type_declaration  = fail;
    class_description       = fail;
    type_declaration        = fail;
    type_kind               = fail;
    typ                     = fail;
    type_extension          = fail;
    extension_constructor   = fail;
    value_description       = fail;
    pat                     = fail;
    expr                    = fail;
    module_declaration      = fail;
    module_type_declaration = fail;
    module_binding          = fail;
    open_description        = fail;
    include_description     = fail;
    include_declaration     = fail;
    value_binding           = fail;
    constructor_declaration = fail;
    label_declaration       = fail;
    cases                   = fail;
    case                    = fail;
    location                = fail;
    extension               = fail;
    attribute               = fail;
    attributes              = fail;
    payload                 = fail;
    binding_op              = fail;
    module_substitution     = fail;
    open_declaration        = fail;
    type_exception          = fail;
    constant                = fail;
  }

let make_top_mapper ~signature ~structure =
  {failing_mapper with Ast_mapper.
                    signature = (fun _ x -> signature x);
                    structure = (fun _ x -> structure x) }
