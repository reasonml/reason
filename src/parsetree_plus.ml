(* This file is the result of copying several files from https://github.com/ocaml/ocaml/parsing *)
(**
 * asttypes_plus.mli
 *)
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

(** Auxiliary AST types used by parsetree and typedtree. *)
type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

(* Order matters, used in polymorphic comparison *)
type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type label = string

type arg_label =
    Nolabel
  | Labelled of string (*  label:T -> ... *)
  | Optional of string (* ?label:T -> ... *)

type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : Location.t;
}

type variance =
  | Covariant
  | Contravariant
  | Invariant

(**
 * parsetree_plus.mli
 *)
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

(** Abstract syntax tree produced by parsing *)

module Int_literal_converter = struct
(* To convert integer literals, allowing max_int + 1 (PR#4210) *)
  let cvt_int_aux str neg of_string =
    if String.length str = 0 || str.[0]= '-'
    then of_string str
    else neg (of_string ("-" ^ str))
  let int s = cvt_int_aux s (~-) int_of_string
  let int32 s = cvt_int_aux s Int32.neg Int32.of_string
  let int64 s = cvt_int_aux s Int64.neg Int64.of_string
  let nativeint s = cvt_int_aux s Nativeint.neg Nativeint.of_string
end

exception Unknown_literal of string * char

type constant =
    Pconst_integer of string * char option
  (* 3 3l 3L 3n

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes except 'l', 'L' and 'n' are rejected by the typechecker
  *)
  | Pconst_char of char
  (* 'c' *)
  | Pconst_string of string * string option
  (* "constant"
     {delim|other constant|delim}
  *)
  | Pconst_float of string * char option
  (* 3.4 2e5 1.4e-4

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes are rejected by the typechecker.
  *)

(** {2 Extension points} *)

type attribute = string loc * payload
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

and payload =
  | PStr of structure
  | PSig of signature (* : SIG *)
  | PTyp of core_type  (* : T *)
  | PPat of pattern * expression option  (* ? P  or  ? P when E *)

(** {2 Core language} *)

(* Type expressions *)

and core_type =
    {
     ptyp_desc: core_type_desc;
     ptyp_loc: Location.t;
     ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and core_type_desc =
  | Ptyp_any
        (*  _ *)
  | Ptyp_var of string
        (* 'a *)
  | Ptyp_arrow of arg_label * core_type * core_type
        (* T1 -> T2       Simple
           ~l:T1 -> T2    Labelled
           ?l:T1 -> T2    Otional
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
  | Ptyp_object of (string loc * attributes * core_type) list * closed_flag
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

and row_field =
  | Rtag of label * attributes * bool * core_type list
        (* [`A]                   ( true,  [] )
           [`A of T]              ( false, [T] )
           [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
           [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

          - The 2nd field is true if the tag contains a
            constant (empty) constructor.
          - '&' occurs when several types are used for the same constructor
            (see 4.2 in the manual)

          - TODO: switch to a record representation, and keep location
        *)
  | Rinherit of core_type
        (* [ T ] *)

(* Patterns *)

and pattern =
    {
     ppat_desc: pattern_desc;
     ppat_loc: Location.t;
     ppat_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and pattern_desc =
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
  | Ppat_unpack of string loc
        (* (module P)
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

and expression =
    {
     pexp_desc: expression_desc;
     pexp_loc: Location.t;
     pexp_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and expression_desc =
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
  | Pexp_send of expression * string loc
        (*  E # m *)
  | Pexp_new of Longident.t loc
        (* new M.c *)
  | Pexp_setinstvar of string loc * expression
        (* x <- 2 *)
  | Pexp_override of (string loc * expression) list
        (* {< x1 = E1; ...; Xn = En >} *)
  | Pexp_letmodule of string loc * module_expr * expression
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
  | Pexp_open of override_flag * Longident.t loc * expression
        (* M.(E)
           let open M in E
           let! open M in E *)
  | Pexp_extension of extension
        (* [%id] *)
  | Pexp_unreachable
        (* . *)

and case =   (* (P -> E) or (P when E0 -> E) *)
    {
     pc_lhs: pattern;
     pc_guard: expression option;
     pc_rhs: expression;
    }

(* Value descriptions *)

and value_description =
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

and type_declaration =
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

and type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
        (* Invariant: non-empty list *)
  | Ptype_record of label_declaration list
        (* Invariant: non-empty list *)
  | Ptype_open

and label_declaration =
    {
     pld_name: string loc;
     pld_mutable: mutable_flag;
     pld_type: core_type;
     pld_loc: Location.t;
     pld_attributes: attributes; (* l [@id1] [@id2] : T *)
    }

(*  { ...; l: T; ... }            (mutable=Immutable)
    { ...; mutable l: T; ... }    (mutable=Mutable)

    Note: T can be a Ptyp_poly.
*)

and constructor_declaration =
    {
     pcd_name: string loc;
     pcd_args: constructor_arguments;
     pcd_res: core_type option;
     pcd_loc: Location.t;
     pcd_attributes: attributes; (* C [@id1] [@id2] of ... *)
    }

and constructor_arguments =
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

and type_extension =
    {
     ptyext_path: Longident.t loc;
     ptyext_params: (core_type * variance) list;
     ptyext_constructors: extension_constructor list;
     ptyext_private: private_flag;
     ptyext_attributes: attributes;   (* ... [@@id1] [@@id2] *)
    }
(*
  type t += ...
*)

and extension_constructor =
    {
     pext_name: string loc;
     pext_kind : extension_constructor_kind;
     pext_loc : Location.t;
     pext_attributes: attributes; (* C [@id1] [@id2] of ... *)
    }

and extension_constructor_kind =
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

(** {2 Class language} *)

(* Type expressions for the class language *)

and class_type =
    {
     pcty_desc: class_type_desc;
     pcty_loc: Location.t;
     pcty_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and class_type_desc =
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

and class_signature =
    {
     pcsig_self: core_type;
     pcsig_fields: class_type_field list;
    }
(* object('selfpat) ... end
   object ... end             (self = Ptyp_any)
 *)

and class_type_field =
    {
     pctf_desc: class_type_field_desc;
     pctf_loc: Location.t;
     pctf_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

and class_type_field_desc =
  | Pctf_inherit of class_type
        (* inherit CT *)
  | Pctf_val of (string loc * mutable_flag * virtual_flag * core_type)
        (* val x: T *)
  | Pctf_method  of (string loc * private_flag * virtual_flag * core_type)
        (* method x: T

           Note: T can be a Ptyp_poly.
         *)
  | Pctf_constraint  of (core_type * core_type)
        (* constraint T1 = T2 *)
  | Pctf_attribute of attribute
        (* [@@@id] *)
  | Pctf_extension of extension
        (* [%%id] *)

and 'a class_infos =
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

and class_expr =
    {
     pcl_desc: class_expr_desc;
     pcl_loc: Location.t;
     pcl_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and class_expr_desc =
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

and class_structure =
    {
     pcstr_self: pattern;
     pcstr_fields: class_field list;
    }
(* object(selfpat) ... end
   object ... end           (self = Ppat_any)
 *)

and class_field =
    {
     pcf_desc: class_field_desc;
     pcf_loc: Location.t;
     pcf_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

and class_field_desc =
  | Pcf_inherit of override_flag * class_expr * string loc option
        (* inherit CE
           inherit CE as x
           inherit! CE
           inherit! CE as x
         *)
  | Pcf_val of (string loc * mutable_flag * class_field_kind)
        (* val x = E
           val virtual x: T
         *)
  | Pcf_method of (string loc * private_flag * class_field_kind)
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

and class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag * expression

and class_declaration = class_expr class_infos

(** {2 Module language} *)

(* Type expressions for the module language *)

and module_type =
    {
     pmty_desc: module_type_desc;
     pmty_loc: Location.t;
     pmty_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and module_type_desc =
  | Pmty_ident of Longident.t loc
        (* S *)
  | Pmty_signature of signature
        (* sig ... end *)
  | Pmty_functor of string loc * module_type option * module_type
        (* functor(X : MT1) -> MT2 *)
  | Pmty_with of module_type * with_constraint list
        (* MT with ... *)
  | Pmty_typeof of module_expr
        (* module type of ME *)
  | Pmty_extension of extension
        (* [%id] *)
  | Pmty_alias of Longident.t loc
        (* (module M) *)

and signature = signature_item list

and signature_item =
    {
     psig_desc: signature_item_desc;
     psig_loc: Location.t;
    }

and signature_item_desc =
  | Psig_value of value_description
        (*
          val x: T
          external x: T = "s1" ... "sn"
         *)
  | Psig_type of rec_flag * type_declaration list
        (* type t1 = ... and ... and tn = ... *)
  | Psig_typext of type_extension
        (* type t1 += ... *)
  | Psig_exception of extension_constructor
        (* exception C of T *)
  | Psig_module of module_declaration
        (* module X : MT *)
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

and module_declaration =
    {
     pmd_name: string loc;
     pmd_type: module_type;
     pmd_attributes: attributes; (* ... [@@id1] [@@id2] *)
     pmd_loc: Location.t;
    }
(* S : MT *)

and module_type_declaration =
    {
     pmtd_name: string loc;
     pmtd_type: module_type option;
     pmtd_attributes: attributes; (* ... [@@id1] [@@id2] *)
     pmtd_loc: Location.t;
    }
(* S = MT
   S       (abstract module type declaration, pmtd_type = None)
*)

and open_description =
    {
     popen_lid: Longident.t loc;
     popen_override: override_flag;
     popen_loc: Location.t;
     popen_attributes: attributes;
    }
(* open! X - popen_override = Override (silences the 'used identifier
                              shadowing' warning)
   open  X - popen_override = Fresh
 *)

and 'a include_infos =
    {
     pincl_mod: 'a;
     pincl_loc: Location.t;
     pincl_attributes: attributes;
    }

and include_description = module_type include_infos
(* include MT *)

and include_declaration = module_expr include_infos
(* include ME *)

and with_constraint =
  | Pwith_type of Longident.t loc * type_declaration
        (* with type X.t = ...

           Note: the last component of the longident must match
           the name of the type_declaration. *)
  | Pwith_module of Longident.t loc * Longident.t loc
        (* with module X.Y = Z *)
  | Pwith_typesubst of type_declaration
        (* with type t := ... *)
  | Pwith_modsubst of string loc * Longident.t loc
        (* with module X := Z *)

(* Value expressions for the module language *)

and module_expr =
    {
     pmod_desc: module_expr_desc;
     pmod_loc: Location.t;
     pmod_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and module_expr_desc =
  | Pmod_ident of Longident.t loc
        (* X *)
  | Pmod_structure of structure
        (* struct ... end *)
  | Pmod_functor of string loc * module_type option * module_expr
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

and structure_item =
    {
     pstr_desc: structure_item_desc;
     pstr_loc: Location.t;
    }

and structure_item_desc =
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
  | Pstr_exception of extension_constructor
        (* exception C of T
           exception C = M.X *)
  | Pstr_module of module_binding
        (* module X = ME *)
  | Pstr_recmodule of module_binding list
        (* module rec X1 = ME1 and ... and Xn = MEn *)
  | Pstr_modtype of module_type_declaration
        (* module type S = MT *)
  | Pstr_open of open_description
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

and value_binding =
  {
    pvb_pat: pattern;
    pvb_expr: expression;
    pvb_attributes: attributes;
    pvb_loc: Location.t;
  }

and module_binding =
    {
     pmb_name: string loc;
     pmb_expr: module_expr;
     pmb_attributes: attributes;
     pmb_loc: Location.t;
    }
(* X = ME *)

(** {2 Toplevel} *)

(* Toplevel phrases *)

type toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of string * directive_argument
     (* #use, #load ... *)

and directive_argument =
  | Pdir_none
  | Pdir_string of string
  | Pdir_int of string * char option
  | Pdir_ident of Longident.t
  | Pdir_bool of bool

(** conversion starts here *)

let not_supported () = failwith "Not supported with current version of OCaml (4.2.3), please upgrade your ocaml"

let dummy_loc () =
  let open Location in
  {
    loc_start = Lexing.dummy_pos;
    loc_end = Lexing.dummy_pos;
    loc_ghost = false;
  }

let add_nonrec rf attrs =
  let open Parsetree in
  let open Asttypes in
  match rf with
  | Recursive -> attrs
  | Nonrecursive ->
      let name = { txt = "nonrec"; loc = dummy_loc () } in
        (name, PStr []) :: attrs


let map_head f = function
  | [] -> []
  | hd::tl ->
     (f hd) :: tl

let map_option f = function
  | None -> None
  | Some p ->
     Some (f p)

let to_parsetree_label label = label

let to_parsetree_rec_flag = function
  | Nonrecursive -> Asttypes.Nonrecursive
  | Recursive -> Asttypes.Recursive

let to_parsetree_direction_flag = function
  | Upto -> Asttypes.Upto
  | Downto -> Asttypes.Downto

let to_parsetree_override_flag = function
  | Override -> Asttypes.Override
  | Fresh -> Asttypes.Fresh

let to_parsetree_closed_flag = function
  | Closed -> Asttypes.Closed
  | Open -> Asttypes.Open

let to_parsetree_private_flag = function
  | Private -> Asttypes.Private
  | Public -> Asttypes.Public

let to_parsetree_mutable_flag = function
  | Immutable -> Asttypes.Immutable
  | Mutable -> Asttypes.Mutable

let to_parsetree_virtual_flag = function
  | Virtual -> Asttypes.Virtual
  | Concrete -> Asttypes.Concrete

let to_parsetree_variance = function
  | Covariant -> Asttypes.Covariant
  | Contravariant -> Asttypes.Contravariant
  | Invariant -> Asttypes.Invariant

let to_parsetree_arg_label = function
  | Nolabel -> ""
  | Labelled string -> string
  | Optional string -> "?" ^ string


let rec to_parsetree_attribute (loc, payload) = (loc, to_parsetree_payload payload)

and to_parsetree_extension (loc, payload) = (loc, to_parsetree_payload payload)

and to_parsetree_attributes attributes = List.map to_parsetree_attribute attributes

and to_parsetree_payload = function
  | PStr structure ->
     let open Parsetree in
     PStr (to_parsetree_structure structure)
  | PSig signature -> not_supported ()
  | PTyp core_type ->
     let open Parsetree in
     PTyp (to_parsetree_core_type core_type)
  | PPat (pattern, expression_option) ->
     let open Parsetree in
     PPat (to_parsetree_pattern pattern, map_option to_parsetree_expression expression_option)

and to_parsetree_core_type item = {
    Parsetree.ptyp_desc= to_parsetree_core_type_desc item.ptyp_desc;
    Parsetree.ptyp_loc= item.ptyp_loc;
    Parsetree.ptyp_attributes= to_parsetree_attributes item.ptyp_attributes;
  }

and to_parsetree_core_type_desc = function
  | Ptyp_any ->
     let open Parsetree in
     Ptyp_any
  | Ptyp_var string ->
     let open Parsetree in
     Ptyp_var string
  | Ptyp_arrow (arg_label, core_type1, core_type2) ->
     let open Parsetree in
     Ptyp_arrow (
         to_parsetree_arg_label arg_label,
         to_parsetree_core_type core_type1,
         to_parsetree_core_type core_type2
       )

  | Ptyp_tuple core_type_list ->
     let open Parsetree in
     Ptyp_tuple (List.map to_parsetree_core_type core_type_list)

  | Ptyp_constr (longident_loc, core_type_list) ->
     let open Parsetree in
     Ptyp_constr (
         longident_loc,
         List.map to_parsetree_core_type core_type_list
       )

  | Ptyp_object (string_loc_attributes_core_type_list, closed_flag) ->
     let open Parsetree in
     Ptyp_object (
         List.map (
             fun (string_loc, attributes, core_type) ->
             (string_loc.txt,
              to_parsetree_attributes attributes,
              to_parsetree_core_type core_type)
           )
                  string_loc_attributes_core_type_list,
         to_parsetree_closed_flag closed_flag
       )

  | Ptyp_class (longident_loc, core_type_list) ->
     let open Parsetree in
     Ptyp_class (longident_loc, List.map to_parsetree_core_type core_type_list)

  | Ptyp_alias (core_type, string) ->
     let open Parsetree in
     Ptyp_alias (to_parsetree_core_type core_type, string)

  | Ptyp_variant (row_field_list, closed_flag, label_list_option) ->
     let open Parsetree in
     Ptyp_variant (
         List.map to_parsetree_row_field row_field_list,
         to_parsetree_closed_flag closed_flag,
         label_list_option
       )

  | Ptyp_poly (string_loc_list, core_type) ->
     let open Parsetree in
     Ptyp_poly (
         List.map (fun string_loc -> string_loc.txt) string_loc_list,
         to_parsetree_core_type core_type
       )

  | Ptyp_package package_type ->
     let open Parsetree in
     Ptyp_package (to_parsetree_package_type package_type)
  | Ptyp_extension extension ->
     let open Parsetree in
     Ptyp_extension (to_parsetree_extension extension)
        (* [%id] *)

and to_parsetree_expression item = {
    Parsetree.pexp_desc= to_parsetree_expression_desc item.pexp_desc;
    Parsetree.pexp_loc= item.pexp_loc;
    Parsetree.pexp_attributes= to_parsetree_attributes item.pexp_attributes;
  }

and to_parsetree_constant = function
  | Pconst_integer (i,None) ->
     let open Asttypes in
     Const_int (Int_literal_converter.int i)
  | Pconst_integer (i,Some 'l') ->
     let open Asttypes in
     Const_int32 (Int_literal_converter.int32 i)
  | Pconst_integer (i,Some 'L') ->
     let open Asttypes in
     Const_int64 (Int_literal_converter.int64 i)
  | Pconst_integer (i,Some 'n') ->
     let open Asttypes in
     Const_nativeint (Int_literal_converter.nativeint i)
  | Pconst_integer (i,Some c) ->
     raise (Unknown_literal (i, c))
  | Pconst_char c ->
     let open Asttypes in
     Const_char c
  | Pconst_string (s,d) ->
     let open Asttypes in
     Const_string (s,d)
  | Pconst_float (f,None)->
     let open Asttypes in
     Const_float f
  | Pconst_float (f,Some c) ->
     raise (Unknown_literal (f, c))

and to_parsetree_expression_desc = function
  | Pexp_ident (longident_loc) ->
     let open Parsetree in
     Pexp_ident (longident_loc)

  | Pexp_constant constant ->
     let open Parsetree in
     Pexp_constant (to_parsetree_constant constant)

  | Pexp_let (rec_flag, value_binding_list, expression) ->
     let open Parsetree in
     Pexp_let (to_parsetree_rec_flag rec_flag,
               List.map to_parsetree_value_binding value_binding_list,
               to_parsetree_expression expression)

  | Pexp_function case_list ->
     let open Parsetree in
     Pexp_function (List.map to_parsetree_case case_list)

  | Pexp_fun (arg_label, expression_option, pattern, expression) ->
     let open Parsetree in
     Pexp_fun (to_parsetree_arg_label arg_label,
               map_option to_parsetree_expression expression_option,
               to_parsetree_pattern pattern,
               to_parsetree_expression expression)

  | Pexp_apply (expression, arg_label_expression_list) ->
     let open Parsetree in
     Pexp_apply (to_parsetree_expression expression,
                 List.map (fun (arg_label, expression) ->
                     (to_parsetree_arg_label arg_label,
                      to_parsetree_expression expression))
                          arg_label_expression_list)

  | Pexp_match (expression, case_list) ->
     let open Parsetree in
     Pexp_match (to_parsetree_expression expression, List.map to_parsetree_case case_list)

  | Pexp_try (expression, case_list) ->
     let open Parsetree in
     Pexp_try (to_parsetree_expression expression, List.map to_parsetree_case case_list)

  | Pexp_tuple expression_list ->
     let open Parsetree in
     Pexp_tuple (List.map to_parsetree_expression expression_list)

  | Pexp_construct (longident_loc, expression_option) ->
     let open Parsetree in
     Pexp_construct (longident_loc, map_option to_parsetree_expression expression_option)

  | Pexp_variant (label, expression_option) ->
     let open Parsetree in
     Pexp_variant (label, map_option to_parsetree_expression expression_option)

  | Pexp_record (longident_loc_expression_list, expression_option) ->
     let open Parsetree in
     Pexp_record (List.map (fun (longident_loc, expression) ->
                     (longident_loc,
                      to_parsetree_expression expression))
                           longident_loc_expression_list,
                  map_option to_parsetree_expression expression_option)

  | Pexp_field (expression, longident_loc) ->
     let open Parsetree in
     Pexp_field (to_parsetree_expression expression, longident_loc)

  | Pexp_setfield (expression1, longident_loc, expression2) ->
     let open Parsetree in
     Pexp_setfield (to_parsetree_expression expression1, longident_loc, to_parsetree_expression expression2)

  | Pexp_array expression_list ->
     let open Parsetree in
     Pexp_array (List.map to_parsetree_expression expression_list)

  | Pexp_ifthenelse (expression1, expression2, expression_option) ->
     let open Parsetree in
     Pexp_ifthenelse (to_parsetree_expression expression1,
                      to_parsetree_expression expression2,
                      map_option to_parsetree_expression expression_option)

  | Pexp_sequence (expression1, expression2) ->
     let open Parsetree in
     Pexp_sequence (to_parsetree_expression expression1,
                    to_parsetree_expression expression2)

  | Pexp_while (expression1, expression2) ->
     let open Parsetree in
     Pexp_while (to_parsetree_expression expression1,
                 to_parsetree_expression expression2)

  | Pexp_for (pattern, expression1, expression2, direction_flag, expression3) ->
     let open Parsetree in
     Pexp_for (to_parsetree_pattern pattern,
               to_parsetree_expression expression1,
               to_parsetree_expression expression2,
               to_parsetree_direction_flag direction_flag,
               to_parsetree_expression expression3)

  | Pexp_constraint (expression, core_type) ->
     let open Parsetree in
     Pexp_constraint (to_parsetree_expression expression, to_parsetree_core_type core_type)

  | Pexp_coerce (expression, core_type_option, core_type) ->
     let open Parsetree in
     Pexp_coerce (to_parsetree_expression expression,
                  map_option to_parsetree_core_type core_type_option,
                  to_parsetree_core_type core_type)


  | Pexp_send (expression, string_loc) ->
     let open Parsetree in
     Pexp_send (to_parsetree_expression expression, string_loc.txt)

  | Pexp_new (longident_loc) ->
     let open Parsetree in
     Pexp_new longident_loc

  | Pexp_setinstvar (string_loc, expression) ->
     let open Parsetree in
     Pexp_setinstvar (string_loc, to_parsetree_expression expression)

  | Pexp_override string_loc_expression_list ->
     let open Parsetree in
     Pexp_override (List.map (fun (string_loc, expression) ->
                        (string_loc,
                         to_parsetree_expression expression))
                             string_loc_expression_list)


  | Pexp_letmodule (string_loc, module_expr, expression) ->
     let open Parsetree in
     Pexp_letmodule (string_loc,
                     to_parsetree_module_expr module_expr,
                     to_parsetree_expression expression)

  | Pexp_letexception (extension_constructor, expression) ->
     not_supported ()

  | Pexp_assert expression ->
     let open Parsetree in
     Pexp_assert (to_parsetree_expression expression)

  | Pexp_lazy expression ->
     let open Parsetree in
     Pexp_lazy (to_parsetree_expression expression)

  | Pexp_poly (expression, core_type_option) ->
     let open Parsetree in
     Pexp_poly (to_parsetree_expression expression,
                map_option to_parsetree_core_type core_type_option)

  | Pexp_object class_structure ->
     let open Parsetree in
     Pexp_object (to_parsetree_class_structure class_structure)

  | Pexp_newtype (string_loc, expression) ->
     let open Parsetree in
     Pexp_newtype (string_loc.txt, to_parsetree_expression expression)


  | Pexp_pack module_expr ->
     let open Parsetree in
     Pexp_pack (to_parsetree_module_expr module_expr)

  | Pexp_open (override_flag, longident_loc, expression) ->
     let open Parsetree in
     Pexp_open (to_parsetree_override_flag override_flag,
                longident_loc,
                to_parsetree_expression expression)

  | Pexp_extension extension ->
     let open Parsetree in
     Pexp_extension (to_parsetree_extension extension)

  | Pexp_unreachable ->
     not_supported ()

and to_parsetree_case item = {
    Parsetree.pc_lhs= to_parsetree_pattern item.pc_lhs;
    Parsetree.pc_guard= map_option to_parsetree_expression item.pc_guard;
    Parsetree.pc_rhs= to_parsetree_expression item.pc_rhs;
  }

and to_parsetree_value_description item = {
    Parsetree.pval_name= item.pval_name;
    Parsetree.pval_type= to_parsetree_core_type item.pval_type;
    Parsetree.pval_prim= item.pval_prim;
    Parsetree.pval_attributes= to_parsetree_attributes item.pval_attributes;
    Parsetree.pval_loc= item.pval_loc;
  }

and to_parsetree_type_declaration item = {
    Parsetree.ptype_name= item.ptype_name;
    Parsetree.ptype_params=
      List.map (fun (core_type, variance) -> (
                  to_parsetree_core_type core_type,
                  to_parsetree_variance variance)
        ) item.ptype_params;
    Parsetree.ptype_cstrs=
      List.map (fun (core_type1, core_type2, location) -> (
                  to_parsetree_core_type core_type1,
                  to_parsetree_core_type core_type2,
                  location
                )
        ) item.ptype_cstrs;
    Parsetree.ptype_kind= to_parsetree_type_kind item.ptype_kind;
    Parsetree.ptype_private= to_parsetree_private_flag item.ptype_private;
    Parsetree.ptype_manifest= map_option to_parsetree_core_type item.ptype_manifest;
    Parsetree.ptype_attributes= to_parsetree_attributes item.ptype_attributes;
    Parsetree.ptype_loc= item.ptype_loc;
  }

and to_parsetree_type_kind = function
  | Ptype_abstract ->
     let open Parsetree in
     Ptype_abstract
  | Ptype_variant constructor_declaration_list ->
     let open Parsetree in
     Ptype_variant (List.map to_parsetree_constructor_declaration constructor_declaration_list)

  | Ptype_record label_declaration_list ->
     let open Parsetree in
     Ptype_record (List.map to_parsetree_label_declaration label_declaration_list)
        (* Invariant: non-empty list *)
  | Ptype_open ->
     let open Parsetree in
     Ptype_open

and to_parsetree_label_declaration item = {
     Parsetree.pld_name= item.pld_name;
     Parsetree.pld_mutable= to_parsetree_mutable_flag item.pld_mutable;
     Parsetree.pld_type= to_parsetree_core_type item.pld_type;
     Parsetree.pld_loc= item.pld_loc;
     Parsetree.pld_attributes= to_parsetree_attributes item.pld_attributes;
    }

and to_parsetree_constructor_declaration item = {
     Parsetree.pcd_name= item.pcd_name;
     Parsetree.pcd_args= to_parsetree_constructor_arguments item.pcd_args;
     Parsetree.pcd_res= map_option to_parsetree_core_type item.pcd_res;
     Parsetree.pcd_loc= item.pcd_loc;
     Parsetree.pcd_attributes= to_parsetree_attributes item.pcd_attributes;
    }

and to_parsetree_constructor_arguments = function
  | Pcstr_tuple core_type_list ->
     let open Parsetree in
     List.map to_parsetree_core_type core_type_list
  | Pcstr_record label_declaration_list ->
     not_supported ()

and to_parsetree_package_type (location_loc, longident_loc_core_type_list)= (
  location_loc,
  List.map (fun (longident_loc, core_type)-> (
              longident_loc,
              to_parsetree_core_type core_type
            )) longident_loc_core_type_list)

and to_parsetree_row_field = function
  | Rtag (label, attributes, bool, core_type_list) ->
     let open Parsetree in
     Rtag (to_parsetree_label label,
           to_parsetree_attributes attributes,
           bool,
           List.map to_parsetree_core_type core_type_list
          )
  | Rinherit core_type ->
     let open Parsetree in
     Rinherit (to_parsetree_core_type core_type)

and to_parsetree_pattern item = {
    Parsetree.ppat_desc= to_parsetree_pattern_desc item.ppat_desc;
    Parsetree.ppat_loc= item.ppat_loc;
    Parsetree.ppat_attributes= to_parsetree_attributes item.ppat_attributes;
  }

and to_parsetree_pattern_desc = function
  | Ppat_any ->
     let open Parsetree in
     Ppat_any

  | Ppat_var string_loc ->
     let open Parsetree in
     Ppat_var string_loc

  | Ppat_alias (pattern, string_loc) ->
     let open Parsetree in
     Ppat_alias (to_parsetree_pattern pattern,
                 string_loc
                )

  | Ppat_constant constant ->
     let open Parsetree in
     Ppat_constant (to_parsetree_constant constant)

  | Ppat_interval (constant1, constant2) ->
     let open Parsetree in
     Ppat_interval (to_parsetree_constant constant1, to_parsetree_constant constant2)

  | Ppat_tuple pattern_list ->
     let open Parsetree in
     Ppat_tuple (List.map to_parsetree_pattern pattern_list)

  | Ppat_construct (longident_loc, pattern_option) ->
     let open Parsetree in
     Ppat_construct (longident_loc, map_option to_parsetree_pattern pattern_option)

  | Ppat_variant (label, pattern_option) ->
     let open Parsetree in
     Ppat_variant (to_parsetree_label label, map_option to_parsetree_pattern pattern_option)

  | Ppat_record (longident_loc_pattern_list, closed_flag) ->
     let open Parsetree in
     Ppat_record (
         List.map (fun (longident_loc, pattern) ->
             (longident_loc, to_parsetree_pattern pattern)
           ) longident_loc_pattern_list,
         to_parsetree_closed_flag closed_flag
       )

  | Ppat_array pattern_list ->
     let open Parsetree in
     Ppat_array (List.map to_parsetree_pattern pattern_list)

  | Ppat_or (pattern1, pattern2) ->
     let open Parsetree in
     Ppat_or (to_parsetree_pattern pattern1,
              to_parsetree_pattern pattern2
             )

  | Ppat_constraint (pattern, core_type) ->
     let open Parsetree in
     Ppat_constraint (to_parsetree_pattern pattern,
              to_parsetree_core_type core_type)

  | Ppat_type longident_loc ->
     let open Parsetree in
     Ppat_type (longident_loc)

  | Ppat_lazy pattern ->
     let open Parsetree in
     Ppat_lazy (to_parsetree_pattern pattern)

  | Ppat_unpack string_loc ->
     let open Parsetree in
     Ppat_unpack string_loc

  | Ppat_exception pattern ->
     let open Parsetree in
     Ppat_exception (to_parsetree_pattern pattern)

  | Ppat_extension extension ->
     let open Parsetree in
     Ppat_extension (to_parsetree_extension extension)

  | Ppat_open (longident_loc, pattern) ->
     not_supported ()

and to_parsetree_type_extension item = {
    Parsetree.ptyext_path= item.ptyext_path;
    Parsetree.ptyext_params= List.map (fun (core_type, variance) ->
                                 (to_parsetree_core_type core_type,
                                  to_parsetree_variance variance))
                                   item.ptyext_params;

    Parsetree.ptyext_constructors=
      List.map to_parsetree_extension_constructor item.ptyext_constructors;

    Parsetree.ptyext_private= to_parsetree_private_flag item.ptyext_private;
    Parsetree.ptyext_attributes= to_parsetree_attributes item.ptyext_attributes;
  }

and to_parsetree_extension_constructor item =  {
    Parsetree.pext_name= item.pext_name;
    Parsetree.pext_kind= to_parsetree_extension_constructor_kind item.pext_kind;
    Parsetree.pext_loc= item.pext_loc;
    Parsetree.pext_attributes= to_parsetree_attributes item.pext_attributes;
  }

and to_parsetree_extension_constructor_kind = function
  | Pext_decl (constructor_arguments, core_type_option) ->
     let open Parsetree in
     Pext_decl (to_parsetree_constructor_arguments constructor_arguments,
                map_option to_parsetree_core_type core_type_option)

  | Pext_rebind longident_loc ->
     let open Parsetree in
     Pext_rebind longident_loc

and to_parsetree_class_type item = {
     Parsetree.pcty_desc= to_parsetree_class_type_desc item.pcty_desc;
     Parsetree.pcty_loc= item.pcty_loc;
     Parsetree.pcty_attributes= to_parsetree_attributes item.pcty_attributes;
    }

and to_parsetree_class_type_desc = function
  | Pcty_constr (longident_loc, core_type_list) ->
     let open Parsetree in
     Pcty_constr (longident_loc,
                  List.map to_parsetree_core_type core_type_list
                 )

  | Pcty_signature class_signature ->
     let open Parsetree in
     Pcty_signature (to_parsetree_class_signature class_signature)

  | Pcty_arrow (arg_label, core_type, class_type) ->
     let open Parsetree in
     Pcty_arrow (to_parsetree_arg_label arg_label,
                 to_parsetree_core_type core_type,
                 to_parsetree_class_type class_type)

  | Pcty_extension extension ->
     let open Parsetree in
     Pcty_extension (to_parsetree_extension extension)

and to_parsetree_class_signature item = {
    Parsetree.pcsig_self= to_parsetree_core_type item.pcsig_self;
    Parsetree.pcsig_fields= List.map to_parsetree_class_type_field item.pcsig_fields;
}

and to_parsetree_class_type_field item = {
    Parsetree.pctf_desc= to_parsetree_class_type_field_desc item.pctf_desc;
    Parsetree.pctf_loc= item.pctf_loc;
    Parsetree.pctf_attributes= to_parsetree_attributes item.pctf_attributes;
  }

and to_parsetree_class_type_field_desc = function
  | Pctf_inherit class_type ->
     let open Parsetree in
     Pctf_inherit (to_parsetree_class_type class_type)

  | Pctf_val (string_loc, mutable_flag, virtual_flag, core_type) ->
     let open Parsetree in
     Pctf_val (string_loc.txt,
               to_parsetree_mutable_flag mutable_flag,
               to_parsetree_virtual_flag virtual_flag,
               to_parsetree_core_type core_type
              )

  | Pctf_method (string_loc, private_flag, virtual_flag, core_type) ->
     let open Parsetree in
     Pctf_method (string_loc.txt,
                  to_parsetree_private_flag private_flag,
                  to_parsetree_virtual_flag virtual_flag,
                  to_parsetree_core_type core_type
                 )

  | Pctf_constraint (core_type1, core_type2) ->
     let open Parsetree in
     Pctf_constraint (to_parsetree_core_type core_type1,
                      to_parsetree_core_type core_type2
                     )

  | Pctf_attribute attribute ->
     let open Parsetree in
     Pctf_attribute (to_parsetree_attribute attribute)

  | Pctf_extension extension ->
     let open Parsetree in
     Pctf_extension (to_parsetree_extension extension)

and to_parsetree_class_structure item = {
    Parsetree.pcstr_self= to_parsetree_pattern item.pcstr_self;
    Parsetree.pcstr_fields= List.map to_parsetree_class_field item.pcstr_fields;
  }

and to_parsetree_class_field item = {
     Parsetree.pcf_desc= to_parsetree_class_field_desc item.pcf_desc;
     Parsetree.pcf_loc= item.pcf_loc;
     Parsetree.pcf_attributes= to_parsetree_attributes item.pcf_attributes;
  }

and to_parsetree_class_field_kind = function
  | Cfk_virtual core_type ->
     let open Parsetree in
     Cfk_virtual (to_parsetree_core_type core_type)
  | Cfk_concrete (override_flag, expression) ->
     let open Parsetree in
     Cfk_concrete (to_parsetree_override_flag override_flag,
                   to_parsetree_expression expression)

and to_parsetree_class_field_desc = function
  | Pcf_inherit (override_flag, class_expr, string_loc_option) ->
     let open Parsetree in
     Pcf_inherit (to_parsetree_override_flag override_flag,
                  to_parsetree_class_expr class_expr,
                  map_option (fun str_loc -> str_loc.txt) string_loc_option)
  | Pcf_val (string_loc, mutable_flag, class_field_kind) ->
     let open Parsetree in
     Pcf_val (string_loc,
              to_parsetree_mutable_flag mutable_flag,
              to_parsetree_class_field_kind class_field_kind
             )

  | Pcf_method (string_loc, private_flag, class_field_kind) ->
     let open Parsetree in
     Pcf_method (string_loc,
                 to_parsetree_private_flag private_flag,
                 to_parsetree_class_field_kind class_field_kind
                )

  | Pcf_constraint (core_type1, core_type2) ->
     let open Parsetree in
     Pcf_constraint (to_parsetree_core_type core_type1,
                     to_parsetree_core_type core_type2
                    )

  | Pcf_initializer expression ->
     let open Parsetree in
     Pcf_initializer (to_parsetree_expression expression)

  | Pcf_attribute attribute ->
     let open Parsetree in
     Pcf_attribute (to_parsetree_attribute attribute)

  | Pcf_extension extension ->
     let open Parsetree in
     Pcf_extension (to_parsetree_extension extension)
        (* [%%id] *)

and to_parsetree_module_type_declaration item = {
     Parsetree.pmtd_name= item.pmtd_name;
     Parsetree.pmtd_type= map_option to_parsetree_module_type item.pmtd_type;
     Parsetree.pmtd_attributes= to_parsetree_attributes item.pmtd_attributes;
     Parsetree.pmtd_loc= item.pmtd_loc;
    }
and to_parsetree_open_description item = {
    Parsetree.popen_lid= item.popen_lid;
    Parsetree.popen_override= to_parsetree_override_flag item.popen_override;
    Parsetree.popen_loc= item.popen_loc;
    Parsetree.popen_attributes= to_parsetree_attributes item.popen_attributes;
  }

and to_parsetree_class_type_declaration item = {
     Parsetree.pci_virt= to_parsetree_virtual_flag item.pci_virt;
     Parsetree.pci_params= List.map (fun (core_type, variance) ->
                               to_parsetree_core_type core_type,
                               to_parsetree_variance variance
                             ) item.pci_params;
     Parsetree.pci_name= item.pci_name;
     Parsetree.pci_expr= to_parsetree_class_type item.pci_expr;
     Parsetree.pci_loc= item.pci_loc;
     Parsetree.pci_attributes= to_parsetree_attributes item.pci_attributes;
    }

and to_parsetree_class_description item = to_parsetree_class_type_declaration item

and to_parsetree_class_expr item = {
     Parsetree.pcl_desc= to_parsetree_class_expr_desc item.pcl_desc;
     Parsetree.pcl_loc= item.pcl_loc;
     Parsetree.pcl_attributes= to_parsetree_attributes item.pcl_attributes;
  }

and to_parsetree_class_expr_desc = function
  | Pcl_constr (longident_loc, core_type_list) ->
     let open Parsetree in
     Pcl_constr (longident_loc,
                 List.map to_parsetree_core_type core_type_list
                )

  | Pcl_structure class_structure ->
     let open Parsetree in
     Pcl_structure (to_parsetree_class_structure class_structure)

  | Pcl_fun (arg_label, expression_option, pattern, class_expr) ->
     let open Parsetree in
     Pcl_fun (to_parsetree_arg_label arg_label,
              map_option to_parsetree_expression expression_option,
              to_parsetree_pattern pattern,
              to_parsetree_class_expr class_expr
             )

  | Pcl_apply (class_expr, arg_label_expression_list) ->
     let open Parsetree in
     Pcl_apply (to_parsetree_class_expr class_expr,
                List.map (fun (arg_label, expression)->
                    (to_parsetree_arg_label arg_label,
                     to_parsetree_expression expression
                    )
                  )
                arg_label_expression_list)

  | Pcl_let (rec_flag, value_binding_list, class_expr) ->
     let open Parsetree in
     Pcl_let (to_parsetree_rec_flag rec_flag,
              List.map to_parsetree_value_binding value_binding_list,
              to_parsetree_class_expr class_expr
             )

  | Pcl_constraint (class_expr, class_type) ->
     let open Parsetree in
     Pcl_constraint (to_parsetree_class_expr class_expr,
                     to_parsetree_class_type class_type
                    )

  | Pcl_extension extension ->
     let open Parsetree in
     Pcl_extension (to_parsetree_extension extension)


and to_parsetree_class_declaration item = {
     Parsetree.pci_virt= to_parsetree_virtual_flag item.pci_virt;
     Parsetree.pci_params= List.map (fun (core_type, variance) ->
                               to_parsetree_core_type core_type,
                               to_parsetree_variance variance
                             ) item.pci_params;
     Parsetree.pci_name= item.pci_name;
     Parsetree.pci_expr= to_parsetree_class_expr item.pci_expr;
     Parsetree.pci_loc= item.pci_loc;
     Parsetree.pci_attributes= to_parsetree_attributes item.pci_attributes;
    }


and to_parsetree_module_type item = {
    Parsetree.pmty_desc= to_parsetree_module_type_desc item.pmty_desc;
    Parsetree.pmty_loc= item.pmty_loc;
    Parsetree.pmty_attributes= to_parsetree_attributes item.pmty_attributes;
  }

and to_parsetree_module_type_desc = function
  | Pmty_ident longident_loc ->
     let open Parsetree in
     Pmty_ident longident_loc

  | Pmty_signature signature ->
     let open Parsetree in
     Pmty_signature (to_parsetree_signature signature)

  | Pmty_functor (string_loc, module_type_option, module_type) ->
     let open Parsetree in
     Pmty_functor (string_loc,
                   map_option to_parsetree_module_type module_type_option,
                   to_parsetree_module_type module_type)

  | Pmty_with (module_type, with_constraint_list) ->
     let open Parsetree in
     Pmty_with (
         to_parsetree_module_type module_type,
         List.map to_parsetree_with_constraint with_constraint_list
       )

  | Pmty_typeof module_expr ->
     let open Parsetree in
     Pmty_typeof (to_parsetree_module_expr module_expr)

  | Pmty_extension extension ->
     let open Parsetree in
     Pmty_extension (
         to_parsetree_extension extension
       )
  | Pmty_alias longident_loc ->
     let open Parsetree in
     Pmty_alias longident_loc

and to_parsetree_signature items = List.map to_parsetree_signature_item items

and to_parsetree_signature_item item = {
     Parsetree.psig_desc= to_parsetree_signature_item_desc item.psig_desc;
     Parsetree.psig_loc= item.psig_loc;
    }

and to_parsetree_signature_item_desc = function
  | Psig_value value_description ->
     let open Parsetree in
     Psig_value (to_parsetree_value_description value_description)

  | Psig_type (rec_flag, type_declaration_list) ->
     let open Parsetree in
     Psig_type (List.map to_parsetree_type_declaration type_declaration_list)
  | Psig_typext type_extension ->
     let open Parsetree in
     Psig_typext (to_parsetree_type_extension type_extension)

  | Psig_exception extension_constructor ->
     let open Parsetree in
     Psig_exception (to_parsetree_extension_constructor extension_constructor)

  | Psig_module module_declaration ->
     let open Parsetree in
     Psig_module (to_parsetree_module_declaration module_declaration)

  | Psig_recmodule module_declaration_list ->
     let open Parsetree in
     Psig_recmodule (List.map to_parsetree_module_declaration module_declaration_list)

  | Psig_modtype module_type_declaration ->
     let open Parsetree in
     Psig_modtype (to_parsetree_module_type_declaration module_type_declaration)

  | Psig_open open_description ->
     let open Parsetree in
     Psig_open (to_parsetree_open_description open_description)

  | Psig_include include_description ->
     let open Parsetree in
     Psig_include (to_parsetree_include_description include_description)

  | Psig_class class_description_list ->
     let open Parsetree in
     Psig_class (List.map to_parsetree_class_description class_description_list)

  | Psig_class_type class_type_declaration_list ->
     let open Parsetree in
     Psig_class_type (List.map to_parsetree_class_type_declaration class_type_declaration_list)

  | Psig_attribute attribute ->
     let open Parsetree in
     Psig_attribute (to_parsetree_attribute attribute)

  | Psig_extension (extension, attributes) ->
     let open Parsetree in
     Psig_extension (to_parsetree_extension extension,
                     to_parsetree_attributes attributes
                    )

and to_parsetree_module_declaration item = {
     Parsetree.pmd_name= item.pmd_name;
     Parsetree.pmd_type= to_parsetree_module_type item.pmd_type;
     Parsetree.pmd_attributes= to_parsetree_attributes item.pmd_attributes;
     Parsetree.pmd_loc= item.pmd_loc;
}

and to_parsetree_include_description item = {
     Parsetree.pincl_mod= to_parsetree_module_type item.pincl_mod;
     Parsetree.pincl_loc= item.pincl_loc;
     Parsetree.pincl_attributes= to_parsetree_attributes item.pincl_attributes;
    }

and to_parsetree_include_declaration item = {
     Parsetree.pincl_mod= to_parsetree_module_expr item.pincl_mod;
     Parsetree.pincl_loc= item.pincl_loc;
     Parsetree.pincl_attributes= to_parsetree_attributes item.pincl_attributes;
    }

and to_parsetree_with_constraint = function
  | Pwith_type (longident_loc, type_declaration) ->
     let open Parsetree in
     Pwith_type (longident_loc,
                 to_parsetree_type_declaration type_declaration
                )

  | Pwith_module (longident_loc1, longident_loc2) ->
     let open Parsetree in
     Pwith_module (longident_loc1,
                   longident_loc2
                  )

  | Pwith_typesubst type_declaration ->
     let open Parsetree in
     Pwith_typesubst (to_parsetree_type_declaration type_declaration)

  | Pwith_modsubst (string_loc, longident_loc) ->
     let open Parsetree in
     Pwith_modsubst (string_loc, longident_loc)

and to_parsetree_module_expr item = {
    Parsetree.pmod_desc= to_parsetree_module_expr_desc item.pmod_desc;
    Parsetree.pmod_loc= item.pmod_loc;
    Parsetree.pmod_attributes= to_parsetree_attributes item.pmod_attributes;
  }

and to_parsetree_module_expr_desc = function
  | Pmod_ident longident_loc ->
     let open Parsetree in
     Pmod_ident longident_loc

  | Pmod_structure structure ->
     let open Parsetree in
     Pmod_structure (to_parsetree_structure structure)

  | Pmod_functor (string_loc, module_type_option, module_expr) ->
     let open Parsetree in
     Pmod_functor (string_loc,
                   map_option to_parsetree_module_type module_type_option,
                   to_parsetree_module_expr module_expr)

  | Pmod_apply (module_expr1, module_expr2) ->
     let open Parsetree in
     Pmod_apply (to_parsetree_module_expr module_expr1,
                 to_parsetree_module_expr module_expr2)

  | Pmod_constraint (module_expr, module_type) ->
     let open Parsetree in
     Pmod_constraint (to_parsetree_module_expr module_expr,
                      to_parsetree_module_type module_type)

  | Pmod_unpack expression ->
     let open Parsetree in
     Pmod_unpack (to_parsetree_expression expression)

  | Pmod_extension extension ->
     let open Parsetree in
     Pmod_extension (to_parsetree_extension extension)


and to_parsetree_structure structure: Parsetree.structure =
  List.map to_parsetree_structure_item structure

and to_parsetree_structure_item item = {
    Parsetree.pstr_desc= to_parsetree_item_desc item.pstr_desc;
    Parsetree.pstr_loc= item.pstr_loc;
  }

and to_parsetree_item_desc = function
  | Pstr_eval (expression, attributes) ->
     let open Parsetree in
     Pstr_eval (to_parsetree_expression expression, to_parsetree_attributes attributes)

  | Pstr_value (rec_flag, value_binding_list) ->
     let open Parsetree in
     Pstr_value (to_parsetree_rec_flag rec_flag, List.map to_parsetree_value_binding value_binding_list)

  | Pstr_primitive value_description ->
     let open Parsetree in
     Pstr_primitive (to_parsetree_value_description value_description)

  | Pstr_type (rec_flag, type_declaration_list) ->
     let open Parsetree in
     let add_flag type_declaration =
       let flag = to_parsetree_rec_flag rec_flag in
       {type_declaration with ptype_attributes = add_nonrec flag type_declaration.ptype_attributes}
     in
     Pstr_type (List.map to_parsetree_type_declaration type_declaration_list |> map_head add_flag)

  | Pstr_typext type_extension ->
     let open Parsetree in
     Pstr_typext (to_parsetree_type_extension type_extension)

  | Pstr_exception extension_constructor ->
     let open Parsetree in
     Pstr_exception (to_parsetree_extension_constructor extension_constructor)

  | Pstr_module module_binding ->
     let open Parsetree in
     Pstr_module (to_parsetree_module_binding module_binding)

  | Pstr_recmodule module_binding_list ->
     let open Parsetree in
     Pstr_recmodule (List.map to_parsetree_module_binding module_binding_list)

  | Pstr_modtype module_type_declaration ->
     let open Parsetree in
     Pstr_modtype (to_parsetree_module_type_declaration module_type_declaration)

  | Pstr_open open_description ->
     let open Parsetree in
     Pstr_open (to_parsetree_open_description open_description)

  | Pstr_class class_declaration_list ->
     let open Parsetree in
     Pstr_class (List.map to_parsetree_class_declaration class_declaration_list)

  | Pstr_class_type class_type_declaration_list ->
     let open Parsetree in
     Pstr_class_type (List.map to_parsetree_class_type_declaration class_type_declaration_list)

  | Pstr_include include_declaration ->
     let open Parsetree in
     Pstr_include (to_parsetree_include_declaration include_declaration)

  | Pstr_attribute attribute ->
     let open Parsetree in
     Pstr_attribute (to_parsetree_attribute attribute)

  | Pstr_extension (extension, attributes) ->
     let open Parsetree in
     Pstr_extension (to_parsetree_extension extension, to_parsetree_attributes attributes)

and to_parsetree_value_binding item = {
    Parsetree.pvb_pat= to_parsetree_pattern item.pvb_pat;
    Parsetree.pvb_expr= to_parsetree_expression item.pvb_expr;
    Parsetree.pvb_attributes= to_parsetree_attributes item.pvb_attributes;
    Parsetree.pvb_loc= item.pvb_loc;
  }

and to_parsetree_module_binding item = {
     Parsetree.pmb_name= item.pmb_name;
     Parsetree.pmb_expr= to_parsetree_module_expr item.pmb_expr;
     Parsetree.pmb_attributes= to_parsetree_attributes item.pmb_attributes;
     Parsetree.pmb_loc= item.pmb_loc;
    }

and to_parsetree_toplevel_phrase = function
  | Ptop_def structure ->
     let open Parsetree in
     Ptop_def (to_parsetree_structure structure)

  | Ptop_dir (string, directive_argument) ->
     let open Parsetree in
     Ptop_dir (string, to_parsetree_directive_argument directive_argument)

and to_parsetree_directive_argument = function
  | Pdir_none ->
     let open Parsetree in
     Pdir_none

  | Pdir_string string ->
     let open Parsetree in
     Pdir_string string

  | Pdir_int (i, _) ->
     let open Parsetree in
     Pdir_int (Int_literal_converter.int i)

  | Pdir_ident longident_t ->
     let open Parsetree in
     Pdir_ident longident_t

  | Pdir_bool bool ->
     let open Parsetree in
     Pdir_bool bool
(**
 * docstrings_plus.ml
 *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                               Leo White                                *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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
  if Warnings.is_active (Warnings.Bad_docstring true) then begin
    List.iter
      (fun ds ->
         match ds.ds_attached with
         | Info -> ()
         | Unattached ->
           prerr_warning ds.ds_loc (Warnings.Bad_docstring true)
         | Docs ->
             match ds.ds_associated with
             | Zero | One -> ()
             | Many ->
               prerr_warning ds.ds_loc (Warnings.Bad_docstring false))
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
  let exp =
    { pexp_desc = Pexp_constant (Pconst_string(ds.ds_body, None));
      pexp_loc = ds.ds_loc;
      pexp_attributes = []; }
  in
  let item =
    { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
  in
    (doc_loc, PStr [item])

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

(* Docstrings not attached to a specifc item *)

type text = docstring list

let empty_text = []
let empty_text_lazy = lazy []

let text_loc = {txt = "ocaml.text"; loc = Location.none}

let text_attr ds =
  let exp =
    { pexp_desc = Pexp_constant (Pconst_string(ds.ds_body, None));
      pexp_loc = ds.ds_loc;
      pexp_attributes = []; }
  in
  let item =
    { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
  in
    (text_loc, PStr [item])

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


(* (Re)Initialise all comment state *)

let init () =
  docstrings := [];
  Hashtbl.reset pre_table;
  Hashtbl.reset post_table;
  Hashtbl.reset floating_table;
  Hashtbl.reset pre_extra_table;
  Hashtbl.reset post_extra_table


(**
 * ast_helper_plus.ml
 *)

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helpers to produce Parsetree fragments *)

type lid = Longident.t loc
type str = string loc
(* type loc = Location.t *)
type attrs = attribute list

let default_loc = ref Location.none

let with_default_loc l f =
  let old = !default_loc in
  default_loc := l;
  try let r = f () in default_loc := old; r
  with exn -> default_loc := old; raise exn

module Const = struct
  let integer ?suffix i = Pconst_integer (i, suffix)
  let int ?suffix i = integer ?suffix (string_of_int i)
  let int32 ?(suffix='l') i = integer ~suffix (Int32.to_string i)
  let int64 ?(suffix='L') i = integer ~suffix (Int64.to_string i)
  let nativeint ?(suffix='n') i = integer ~suffix (Nativeint.to_string i)
  let float ?suffix f = Pconst_float (f, suffix)
  let char c = Pconst_char c
  let string ?quotation_delimiter s = Pconst_string (s, quotation_delimiter)
end

module Typ = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ptyp_desc = d; ptyp_loc = loc; ptyp_attributes = attrs}
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
end

module Pat = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ppat_desc = d; ppat_loc = loc; ppat_attributes = attrs}
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
    {pexp_desc = d; pexp_loc = loc; pexp_attributes = attrs}
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
  let open_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_open (a, b, c))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)
  let unreachable ?loc ?attrs () = mk ?loc ?attrs Pexp_unreachable

  let case lhs ?guard rhs =
    {
     pc_lhs = lhs;
     pc_guard = guard;
     pc_rhs = rhs;
    }
end

module Mty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
  let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
  let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_alias a)
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
  let functor_ ?loc ?attrs a b c = mk ?loc ?attrs (Pmty_functor (a, b, c))
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
  let functor_ ?loc ?attrs arg arg_ty body =
    mk ?loc ?attrs (Pmod_functor (arg, arg_ty, body))
  let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
  let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
  let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
end

module Sig = struct
  let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

  let value ?loc a = mk ?loc (Psig_value a)
  let type_ ?loc rec_flag a = mk ?loc (Psig_type (rec_flag, a))
  let type_extension ?loc a = mk ?loc (Psig_typext a)
  let exception_ ?loc a = mk ?loc (Psig_exception a)
  let module_ ?loc a = mk ?loc (Psig_module a)
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
        ?(override = Fresh) lid =
    {
     popen_lid = lid;
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
  let mk ?(attrs = []) ?(docs = empty_docs)
        ?(params = []) ?(priv = Public) path constructors =
    {
     ptyext_path = path;
     ptyext_params = params;
     ptyext_constructors = constructors;
     ptyext_private = priv;
     ptyext_attributes = add_docs_attrs docs attrs;
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


(**
 * ast_mapper_plus.ml
 *)
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* A generic Parsetree mapping class *)

(*
[@@@ocaml.warning "+9"]
  (* Ensure that record patterns don't miss any field. *)
*)
open Location

type mapper = {
  attribute: mapper -> attribute -> attribute;
  attributes: mapper -> attribute list -> attribute list;
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
  module_expr: mapper -> module_expr -> module_expr;
  module_type: mapper -> module_type -> module_type;
  module_type_declaration: mapper -> module_type_declaration
                           -> module_type_declaration;
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

module T = struct
  (* Type expressions for the core language *)

  let row_field sub = function
    | Rtag (l, attrs, b, tl) ->
        Rtag (l, sub.attributes sub attrs, b, List.map (sub.typ sub) tl)
    | Rinherit t -> Rinherit (sub.typ sub t)

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
        let f (s, a, t) =
          (map_loc sub s, sub.attributes sub a, sub.typ sub t) in
        object_ ~loc ~attrs (List.map f l) o
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
    Type.mk (map_loc sub ptype_name)
      ~params:(List.map (map_fst (sub.typ sub)) ptype_params)
      ~priv:ptype_private
      ~cstrs:(List.map
                (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
                ptype_cstrs)
      ~kind:(sub.type_kind sub ptype_kind)
      ?manifest:(map_opt (sub.typ sub) ptype_manifest)
      ~loc:(sub.location sub ptype_loc)
      ~attrs:(sub.attributes sub ptype_attributes)

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
       ptyext_attributes} =
    Te.mk
      (map_loc sub ptyext_path)
      (List.map (sub.extension_constructor sub) ptyext_constructors)
      ~params:(List.map (map_fst (sub.typ sub)) ptyext_params)
      ~priv:ptyext_private
      ~attrs:(sub.attributes sub ptyext_attributes)

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
    Te.constructor
      (map_loc sub pext_name)
      (map_extension_constructor_kind sub pext_kind)
      ~loc:(sub.location sub pext_loc)
      ~attrs:(sub.attributes sub pext_attributes)

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
    | Pmty_functor (s, mt1, mt2) ->
        functor_ ~loc ~attrs (map_loc sub s)
          (Misc.may_map (sub.module_type sub) mt1)
          (sub.module_type sub mt2)
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
    | Pwith_typesubst d -> Pwith_typesubst (sub.type_declaration sub d)
    | Pwith_modsubst (s, lid) ->
        Pwith_modsubst (map_loc sub s, map_loc sub lid)

  let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
    let open Sig in
    let loc = sub.location sub loc in
    match desc with
    | Psig_value vd -> value ~loc (sub.value_description sub vd)
    | Psig_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
    | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Psig_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
    | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
    | Psig_recmodule l ->
        rec_module ~loc (List.map (sub.module_declaration sub) l)
    | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Psig_open x -> open_ ~loc (sub.open_description sub x)
    | Psig_include x -> include_ ~loc (sub.include_description sub x)
    | Psig_class l -> class_ ~loc (List.map (sub.class_description sub) l)
    | Psig_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Psig_extension (x, attrs) ->
        extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
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
    | Pmod_functor (arg, arg_ty, body) ->
        functor_ ~loc ~attrs (map_loc sub arg)
          (Misc.may_map (sub.module_type sub) arg_ty)
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
        eval ~loc ~attrs:(sub.attributes sub attrs) (sub.expr sub x)
    | Pstr_value (r, vbs) -> value ~loc r (List.map (sub.value_binding sub) vbs)
    | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
    | Pstr_type (rf, l) -> type_ ~loc rf (List.map (sub.type_declaration sub) l)
    | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Pstr_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
    | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
    | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
    | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Pstr_open x -> open_ ~loc (sub.open_description sub x)
    | Pstr_class l -> class_ ~loc (List.map (sub.class_declaration sub) l)
    | Pstr_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
    | Pstr_extension (x, attrs) ->
        extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
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
    | Pexp_constant x -> constant ~loc ~attrs x
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
    | Pexp_open (ovf, lid, e) ->
        open_ ~loc ~attrs ovf (map_loc sub lid) (sub.expr sub e)
    | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
    | Pexp_unreachable -> unreachable ~loc ~attrs ()
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
    | Ppat_constant c -> constant ~loc ~attrs c
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
    Ci.mk
     ~virt:pci_virt
     ~params:(List.map (map_fst (sub.typ sub)) pl)
      (map_loc sub pci_name)
      (f pci_expr)
      ~loc:(sub.location sub pci_loc)
      ~attrs:(sub.attributes sub pci_attributes)
end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_mapper =
  {
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

    module_declaration =
      (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
         Md.mk
           (map_loc this pmd_name)
           (this.module_type this pmd_type)
           ~attrs:(this.attributes this pmd_attributes)
           ~loc:(this.location this pmd_loc)
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


    open_description =
      (fun this {popen_lid; popen_override; popen_attributes; popen_loc} ->
         Opn.mk (map_loc this popen_lid)
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
    attribute = (fun this (s, e) -> (map_loc this s, this.payload this e));
    attributes = (fun this l -> List.map (this.attribute this) l);
    payload =
      (fun this -> function
         | PStr x -> PStr (this.structure this x)
         | PSig x -> PSig (this.signature this x)
         | PTyp x -> PTyp (this.typ this x)
         | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g)
      );
  }

let rec extension_of_error {loc; msg; if_highlight; sub} =
  { loc; txt = "ocaml.error" },
  PStr ([Str.eval (Exp.constant (Pconst_string (msg, None)));
         Str.eval (Exp.constant (Pconst_string (if_highlight, None)))] @
        (List.map (fun ext -> Str.extension (extension_of_error ext)) sub))

let attribute_of_warning loc s =
  { loc; txt = "ocaml.ppwarning" },
  PStr ([Str.eval ~loc (Exp.constant (Pconst_string (s, None)))])

module StringMap = Map.Make(struct
    type t = string
    let compare = compare
end)

let cookies = ref StringMap.empty

let get_cookie k =
  try Some (StringMap.find k !cookies)
  with Not_found -> None

let set_cookie k v =
  cookies := StringMap.add k v !cookies

let tool_name_ref = ref "_none_"

let tool_name () = !tool_name_ref


module PpxContext = struct
  open Longident

  let lid name = { txt = Lident name; loc = Location.none }

  let make_string x = Exp.constant (Pconst_string (x, None))

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
      (StringMap.bindings !cookies)

  let mk fields =
    { txt = "ocaml.ppx.context"; loc = Location.none },
    PStr [Str.eval (Exp.record fields None)]

  let make ~tool_name () =
    let fields =
      [
        lid "tool_name",    make_string tool_name;
        lid "include_dirs", make_list make_string !Clflags.include_dirs;
        lid "load_path",    make_list make_string !Config.load_path;
        lid "open_modules", make_list make_string !Clflags.open_modules;
        lid "for_package",  make_option make_string !Clflags.for_package;
        lid "debug",        make_bool !Clflags.debug;
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
        | { pexp_desc = Pexp_constant (Pconst_string (str, None)) } -> str
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
          Config.load_path := get_list get_string payload
      | "open_modules" ->
          Clflags.open_modules := get_list get_string payload
      | "for_package" ->
          Clflags.for_package := get_option get_string payload
      | "debug" ->
          Clflags.debug := get_bool payload
      | "cookies" ->
          let l = get_list (get_pair get_string (fun x -> x)) payload in
          cookies :=
            List.fold_left
              (fun s (k, v) -> StringMap.add k v s) StringMap.empty
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

let ext_of_exn exn =
  match error_of_exn exn with
  | Some error -> extension_of_error error
  | None -> raise exn


let apply_lazy ~source ~target mapper =
  let implem ast =
    let fields, ast =
      match ast with
      | {pstr_desc = Pstr_attribute ({txt = "ocaml.ppx.context"}, x)} :: l ->
          PpxContext.get_fields x, l
      | _ -> [], ast
    in
    PpxContext.restore fields;
    let ast =
      try
        let mapper = mapper () in
        mapper.structure mapper ast
      with exn ->
        [{pstr_desc = Pstr_extension (ext_of_exn exn, []);
          pstr_loc  = Location.none}]
    in
    let fields = PpxContext.update_cookies fields in
    Str.attribute (PpxContext.mk fields) :: ast
  in
  let iface ast =
    let fields, ast =
      match ast with
      | {psig_desc = Psig_attribute ({txt = "ocaml.ppx.context"}, x)} :: l ->
          PpxContext.get_fields x, l
      | _ -> [], ast
    in
    PpxContext.restore fields;
    let ast =
      try
        let mapper = mapper () in
        mapper.signature mapper ast
      with exn ->
        [{psig_desc = Psig_extension (ext_of_exn exn, []);
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
  | {pstr_desc = Pstr_attribute({Location.txt = "ocaml.ppx.context"}, a)}
    :: items ->
      if restore then
        PpxContext.restore (PpxContext.get_fields a);
      items
  | items -> items

let drop_ppx_context_sig ~restore = function
  | {psig_desc = Psig_attribute({Location.txt = "ocaml.ppx.context"}, a)}
    :: items ->
      if restore then
        PpxContext.restore (PpxContext.get_fields a);
      items
  | items -> items

let add_ppx_context_str ~tool_name ast =
  Str.attribute (ppx_context ~tool_name ()) :: ast

let add_ppx_context_sig ~tool_name ast =
  Sig.attribute (ppx_context ~tool_name ()) :: ast


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
          (* PR #6463 *)
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

let register_function: (string -> (string list -> mapper) -> unit) ref = ref (fun _name f -> run_main f)
let register name f = !register_function name f
