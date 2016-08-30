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

open Asttypes_plus

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
