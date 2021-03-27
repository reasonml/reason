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
    | NoVariance

  type injectivity (*IF_CURRENT = Asttypes.injectivity *) =
    | Injective
    | NoInjectivity
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
      ptype_params: (core_type * (variance * injectivity)) list;
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
      ptyext_params: (core_type * (variance * injectivity)) list;
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
      pci_params: (core_type * (variance * injectivity)) list;
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

module Ast_mapper = struct
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

  type out_type_param = string * (Asttypes.variance * Asttypes.injectivity)

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
        bool * string * out_type_param list * out_class_type *
        out_rec_status
    | Osig_class_type of
        bool * string * out_type_param list * out_class_type *
        out_rec_status
    | Osig_typext of out_extension_constructor * out_ext_status
    | Osig_modtype of string * out_module_type
    | Osig_module of string * out_module_type * out_rec_status
    | Osig_type of out_type_decl * out_rec_status
    | Osig_value of out_val_decl
    | Osig_ellipsis
  and out_type_decl (*IF_CURRENT = Outcometree.out_type_decl *) =
    { otype_name: string;
      otype_params: out_type_param list;
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
  let ast_impl_magic_number = "Caml1999M029"
  let ast_intf_magic_number = "Caml1999N029"
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
