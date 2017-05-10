/*
 *  Copyright (c) 2015-present, Facebook, Inc.
 *  All rights reserved.
 *
 *  This source code is licensed under the BSD-style license found in the
 *  LICENSE file in the root directory of this source tree. An additional grant
 *  of patent rights can be found in the PATENTS file in the same directory.
 *
 *  Forked from OCaml, which is provided under the license below:
 *
 *  Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *
 *  Copyright © 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006 Inria
 *
 *  Permission is hereby granted, free of charge, to the Licensee obtaining a
 *  copy of this software and associated documentation files (the "Software"),
 *  to deal in the Software without restriction, including without limitation
 *  the rights to use, copy, modify, merge, publish, distribute, sublicense
 *  under any license of the Licensee's choice, and/or sell copies of the
 *  Software, subject to the following conditions:
 *
 *  1.	Redistributions of source code must retain the above copyright notice
 *  and the following disclaimer.
 *  2.	Redistributions in binary form must reproduce the above copyright
 *  notice, the following disclaimer in the documentation and/or other
 *  materials provided with the distribution.
 *  3.	All advertising materials mentioning features or use of the Software
 *  must display the following acknowledgement: This product includes all or
 *  parts of the Caml system developed by Inria and its contributors.
 *  4.	Other than specified in clause 3, neither the name of Inria nor the
 *  names of its contributors may be used to endorse or promote products
 *  derived from the Software without specific prior written permission.
 *
 *  Disclaimer
 *
 *  This software is provided by Inria and contributors “as is” and any express
 *  or implied warranties, including, but not limited to, the implied
 *  warranties of merchantability and fitness for a particular purpose are
 *  disclaimed. in no event shall Inria or its contributors be liable for any
 *  direct, indirect, incidental, special, exemplary, or consequential damages
 *  (including, but not limited to, procurement of substitute goods or
 *  services; loss of use, data, or profits; or business interruption) however
 *  caused and on any theory of liability, whether in contract, strict
 *  liability, or tort (including negligence or otherwise) arising in any way
 *  out of the use of this software, even if advised of the possibility of such
 *  damage.
 *
 */

/* The parser definition */

%{
open Migrate_parsetree.OCaml_404.Ast
open Syntax_util
open Location
open Asttypes
open Longident
open Parsetree
open Ast_helper
open Ast_mapper

(*
   TODO:
   - Remove all [open]s from the top of this file one by one and fix compilation
   failures that ensue by specifying the appropriate long identifiers. That
   will make the parser much easier to reason about.
   - Go back to trunk, do the same (remove [open]s, and fully specify long
   idents), to perform a clean diff.

*)

(**

   location.ml:
   ------------
   let mkloc txt loc = { txt ; loc }
   let rhs_loc n = {
     loc_start = Parsing.rhs_start_pos n;
     loc_end = Parsing.rhs_end_pos n;
     loc_ghost = false;
   }
   let symbol_rloc () = {
     loc_start = Parsing.symbol_start_pos ();
     loc_end = Parsing.symbol_end_pos ();
     loc_ghost = false;
   }

   let symbol_gloc () = {
     loc_start = Parsing.symbol_start_pos ();
     loc_end = Parsing.symbol_end_pos ();
     loc_ghost = true;
   }

   ast_helper.ml:
   ------------
   module Typ = struct
    val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
    let mk ?(loc = !default_loc) ?(attrs = []) d =
       {ptyp_desc = d; ptyp_loc = loc; ptyp_attributes = attrs}
     ..
   end

   parse_tree.mli
   --------------
   and core_type = {
     ptyp_desc: core_type_desc;
     ptyp_loc: Location.t;
     ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
   }

   and core_type_desc =
     | Ptyp_any
           (*  _ *)
     | Ptyp_var of string
           (* 'a *)
     | Ptyp_arrow of label * core_type * core_type
           (* T1 -> T2       (label = "")
              ~l:T1 -> T2    (label = "l")
              ?l:T1 -> T2    (label = "?l")
            *)
     | Ptyp_tuple of core_type list
           (* T1 * ... * Tn   (n >= 2) *)

   reason_parser.mly
   ---------------
   In general:

                                          syntax variant          {pblah_desc: core_blah_desc
                                                                   pblah_loc: {txt, loc}
                                                                   pblah_attributes: ... }
                                         /              \            /       \
   val mkblah: ~loc -> ~attributes ->     core_blah_desc     ->      core_blah
   let mkblah = Blah.mk

*)


let dummy_loc () = {
  loc_start = Lexing.dummy_pos;
  loc_end = Lexing.dummy_pos;
  loc_ghost = false;
}

let mklocation loc_start loc_end = {
  loc_start = loc_start;
  loc_end = loc_end;
  loc_ghost = false;
}

let with_txt a txt = {
    a with txt=txt;
}

let make_real_loc loc = {
    loc with loc_ghost = false
}

let make_ghost_loc loc = {
    loc with loc_ghost = true
}

let ghloc ?(loc=dummy_loc ()) d = { txt = d; loc = (make_ghost_loc loc) }

(**
  * turn an object into a real
  *)
let make_real_exp exp = {
    exp with pexp_loc = make_real_loc exp.pexp_loc
}
let make_real_pat pat = {
    pat with ppat_loc = make_real_loc pat.ppat_loc
}
let make_real_cf cf = {
    cf with pcf_loc = make_real_loc cf.pcf_loc
}

(**
  * turn a object into ghost
  *)
let make_ghost_cf cf = {
    cf with pcf_loc = make_ghost_loc cf.pcf_loc
}
let make_ghost_exp exp = {
    exp with pexp_loc = make_ghost_loc exp.pexp_loc
}

let make_ghost_pat pat = {
    pat with ppat_loc = make_ghost_loc pat.ppat_loc
}

(**
  * change the location state to be a ghost location or real location
  *)
let set_loc_state is_ghost loc =
    if is_ghost then make_ghost_loc loc else make_real_loc loc

let mktyp ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Typ.mk ~loc d

let mkpat ?(attrs=[]) ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Pat.mk ~loc ~attrs d

let mkexp ?(attrs=[]) ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Exp.mk ~loc ~attrs d

let mkmty ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Mty.mk ~loc d

let mksig ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Sig.mk ~loc d

let mkmod ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Mod.mk ~loc d

let mkstr ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Str.mk ~loc d

let mkclass ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Cl.mk ~loc d

let mkcty ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Cty.mk ~loc d

let mkctf ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Ctf.mk ~loc d

let may_tuple = function
  | []  -> assert false
  | [x] -> x
  | xs  -> mkexp (Pexp_tuple xs)

(**
  Make a core_type from a as_loc(LIDENT).
  Useful for record type punning.
  type props = {width: int, height: int};
  type state = {nbrOfClicks: int};
  type component = {props, state};
*)
let mkct lbl =
  let lident = Lident lbl.txt in
  let ttype = Ptyp_constr({txt = lident; loc = lbl.loc}, []) in
  {ptyp_desc = ttype; ptyp_loc = lbl.loc; ptyp_attributes = []}

let mkcf ?(loc=dummy_loc()) ?(ghost=false) d =
    let loc = set_loc_state ghost loc in
    Cf.mk ~loc d

let simple_ghost_text_attr ?(loc=dummy_loc ()) txt =
  let loc = set_loc_state true loc in
  [({txt; loc}, PStr [])]

let mkExplicitArityTuplePat ?(loc=dummy_loc ()) pat =
  (* Tell OCaml type system that what this tuple construction represents is
     not actually a tuple, and should represent several constructor
     arguments.  This allows the syntax the ability to distinguish between:

     X (10, 20)  -- One argument constructor
     X 10 20     -- Multi argument constructor
  *)
  mkpat
    ~loc
    ~attrs:(simple_ghost_text_attr ~loc "explicit_arity")
    pat

let mkExplicitArityTupleExp ?(loc=dummy_loc ()) exp =
  mkexp
    ~loc
    ~attrs:(simple_ghost_text_attr ~loc "explicit_arity")
    exp

let is_pattern_list_single_any = function
  | [{ppat_desc=Ppat_any; ppat_attributes=[]} as onlyItem] -> Some onlyItem
  | _ -> None

let set_structure_item_location x loc = {x with pstr_loc = loc};;

let mkoperator_loc name loc =
  Exp.mk ~loc (Pexp_ident(mkloc (Lident name) loc))

let mkoperator name pos =
  mkoperator_loc name (rhs_loc pos)

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitly in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -annot option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.

  jordwalke: Noticed that ghost expressions are often used when inserting
   additional AST nodes from a parse rule. Either an extra wrapping one, or an
   additional inner node. This is consistent with the above description, I
   believe.
*)


let ghunit ?(loc=dummy_loc ()) () =
  mkexp ~ghost:true ~loc (Pexp_construct (mknoloc (Lident "()"), None))

let mkinfixop arg1 op arg2 =
  mkexp(Pexp_apply(op, [Nolabel, arg1; Nolabel, arg2]))

let mkinfix arg1 name arg2 =
  mkinfixop arg1 (mkoperator name 2) arg2

let neg_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Pconst_integer (n,m)) ->
      mkexp(Pexp_constant(Pconst_integer(neg_string n,m)))
  | ("-" | "-."), Pexp_constant(Pconst_float (f, m)) ->
      mkexp(Pexp_constant(Pconst_float(neg_string f, m)))
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, [Nolabel, arg]))

let mkFunctorThatReturns functorArgs returns =
  List.fold_right (fun (n, t) acc -> mkmod (Pmod_functor(n, t, acc)))
    functorArgs returns

let mkuplus name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant(Pconst_integer _)
  | ("+" | "+."), Pexp_constant(Pconst_float _) -> mkexp desc
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, [Nolabel, arg]))

let mkexp_cons consloc args loc =
  mkexp ~loc (Pexp_construct(mkloc (Lident "::") consloc, Some args))

let mkexp_constructor_unit consloc loc =
  mkexp ~loc (Pexp_construct(mkloc (Lident "()") consloc, None))

let ghexp_cons consloc args loc =
  mkexp ~ghost:true ~loc (Pexp_construct(mkloc (Lident "::") loc, Some args))

let mkpat_cons consloc args loc =
  mkpat ~loc (Ppat_construct(mkloc (Lident "::") loc, Some args))

let ghpat_cons consloc args loc =
  mkpat ~ghost:true ~loc (Ppat_construct(mkloc (Lident "::") loc, Some args))

let mkpat_constructor_unit consloc loc =
  mkpat ~loc (Ppat_construct(mkloc (Lident "()") consloc, None))

let mkexp_fun pat body =
  mkexp ~loc:(mklocation pat.ppat_loc.loc_start body.pexp_loc.loc_end)
    (Pexp_fun (Nolabel, None, pat, body))

let simple_pattern_list_to_tuple ?(loc=dummy_loc ()) = function
  | [] -> assert false
  | lst -> mkpat ~loc (Ppat_tuple lst)

let mktailexp_extension loc seq ext_opt =
  let rec handle_seq = function
    | [] ->
        let base_case = match ext_opt with
          | Some ext ->
            ext
          | None ->
            let loc = make_ghost_loc loc in
            let nil = { txt = Lident "[]"; loc } in
            Exp.mk ~loc (Pexp_construct (nil, None)) in
        base_case
    | e1 :: el ->
        let exp_el = handle_seq el in
        let loc = mklocation e1.pexp_loc.loc_start exp_el.pexp_loc.loc_end in
        let arg = mkexp ~ghost:true ~loc (Pexp_tuple [e1; exp_el]) in
        ghexp_cons loc arg loc
  in
  handle_seq seq

let mktailpat_extension loc (seq, ext_opt) =
  let rec handle_seq = function
    [] ->
      let base_case = match ext_opt with
        | Some ext ->
          ext
        | None ->
          let loc = make_ghost_loc loc in
          let nil = { txt = Lident "[]"; loc } in
          mkpat ~loc (Ppat_construct (nil, None)) in
      base_case
  | p1 :: pl ->
      let pat_pl = handle_seq pl in
      let loc = mklocation p1.ppat_loc.loc_start pat_pl.ppat_loc.loc_end in
      let arg = mkpat ~ghost:true ~loc (Ppat_tuple [p1; pat_pl]) in
      ghpat_cons loc arg loc in
  handle_seq seq

let makeFrag loc body =
  let attribute = ({txt = "JSX"; loc = loc}, PStr []) in
  { body with pexp_attributes = attribute :: body.pexp_attributes }


(* Applies attributes to the structure item, not the expression itself. Makes
 * structure item have same location as expression. *)
let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = e.pexp_loc }

let ghexp_constraint loc e (t1, t2) =
  match t1, t2 with
  | Some t, None -> mkexp ~ghost:true ~loc (Pexp_constraint(e, t))
  | _, Some t -> mkexp ~ghost:true ~loc (Pexp_coerce(e, t1, t))
  | None, None -> assert false

let array_function ?(loc=dummy_loc()) str name =
  ghloc ~loc (Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name)))

let syntax_error_str loc msg =
  if !Reason_config.recoverable then
    Str.mk ~loc:loc (Pstr_extension (Syntax_util.syntax_error_extension_node loc msg, []))
  else
    raise(Syntaxerr.Error(Syntaxerr.Other loc))

let syntax_error () =
  raise Syntaxerr.Escape_error

let syntax_error_exp loc msg =
  if !Reason_config.recoverable then
    Exp.mk ~loc (Pexp_extension (Syntax_util.syntax_error_extension_node loc msg))
  else
    syntax_error ()

let unclosed opening closing =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(opening.loc, opening.txt,
                                           closing.loc, closing.txt)))

let unclosed_extension closing =
  Syntax_util.syntax_error_extension_node closing.loc ("Expecting \"" ^ closing.txt ^ "\"")

let unclosed_mod opening closing =
  if !Reason_config.recoverable then
    mkmod(Pmod_extension (unclosed_extension closing))
  else
    unclosed opening closing

let unclosed_cl opening closing =
  if !Reason_config.recoverable then
    mkclass(Pcl_extension (unclosed_extension closing))
  else
    unclosed opening closing

let unclosed_mty opening closing =
  if !Reason_config.recoverable then
    mkmty(Pmty_extension (unclosed_extension closing))
  else
    unclosed opening closing

let unclosed_cty opening closing =
  if !Reason_config.recoverable then
    mkcty(Pcty_extension (unclosed_extension closing))
  else
    unclosed opening closing

let unclosed_exp opening closing =
  if !Reason_config.recoverable then
    mkexp(Pexp_extension (unclosed_extension closing))
  else
    unclosed opening closing

let unclosed_pat opening closing =
  if !Reason_config.recoverable then
    mkpat(Ppat_extension (unclosed_extension closing))
  else
    unclosed opening closing

let expecting nonterm =
    raise Syntaxerr.(Error(Expecting(nonterm.loc, nonterm.txt)))

let expecting_pat nonterm =
  if !Reason_config.recoverable then
    mkpat(Ppat_extension (Syntax_util.syntax_error_extension_node nonterm.loc ("Expecting " ^ nonterm.txt)))
  else
    expecting nonterm

let not_expecting start_pos end_pos nonterm =
    raise Syntaxerr.(Error(Not_expecting(mklocation start_pos end_pos, nonterm)))

let bigarray_function ?(loc=dummy_loc()) str name =
  ghloc ~loc (Ldot(Ldot(Lident "Bigarray", str), name))

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist; pexp_loc = _ } -> explist
  | exp -> [exp]

let bigarray_get ?(loc=dummy_loc()) arr arg =
  let get = if !Clflags.fast then "unsafe_get" else "get" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Array1" get)),
                       [Nolabel, arr; Nolabel, c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Array2" get)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Array3" get)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, c3]))
  | coords ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Genarray" "get")),
                       [Nolabel, arr; Nolabel, mkexp ~ghost:true ~loc (Pexp_array coords)]))

let bigarray_set ?(loc=dummy_loc()) arr arg newval =
  let set = if !Clflags.fast then "unsafe_set" else "set" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Array1" set)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Array2" set)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Array3" set)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, c3; Nolabel, newval]))
  | coords ->
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(bigarray_function ~loc "Genarray" "set")),
                       [Nolabel, arr;
                        Nolabel, mkexp ~ghost:true ~loc (Pexp_array coords);
                        Nolabel, newval]))

let lapply p1 p2 start_pos end_pos =
  if !Clflags.applicative_functors
  then Lapply(p1, p2)
  else raise (Syntaxerr.Error(Syntaxerr.Applicative_path (mklocation start_pos end_pos)))

let exp_of_label label =
  mkexp ~loc:label.loc (Pexp_ident {label with txt=Lident(Longident.last label.txt)})

let pat_of_label label =
  mkpat ~loc:label.loc (Ppat_var {label with txt=(Longident.last label.txt)})

let check_variable vl loc v =
  if List.mem v vl then
    raise Syntaxerr.(Error(Variable_in_scope(loc,v)))

let varify_constructors var_names t =
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
      | Ptyp_constr( { txt = Lident s }, []) when List.mem s var_names ->
          Ptyp_var s
      | Ptyp_constr(longident, lst) ->
          Ptyp_constr(longident, List.map loop lst)
      | Ptyp_object (lst, o) ->
          Ptyp_object
            (List.map (fun (s, attrs, t) -> (s, attrs, loop t)) lst, o)
      | Ptyp_class (longident, lst) ->
          Ptyp_class (longident, List.map loop lst)
      | Ptyp_alias(core_type, string) ->
          check_variable var_names t.ptyp_loc string;
          Ptyp_alias(loop core_type, string)
      | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
          Ptyp_variant(List.map loop_row_field row_field_list,
                       flag, lbl_lst_option)
      | Ptyp_poly(string_lst, core_type) ->
          List.iter (check_variable var_names t.ptyp_loc) string_lst;
          Ptyp_poly(string_lst, loop core_type)
      | Ptyp_package(longident,lst) ->
          Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
      | Ptyp_extension (s, arg) ->
          Ptyp_extension (s, arg)
    in
    {t with ptyp_desc = desc}
  and loop_row_field  =
    function
      | Rtag(label,attrs,flag,lst) ->
          Rtag(label,attrs,flag,List.map loop lst)
      | Rinherit t ->
          Rinherit (loop t)
  in
  loop t

let pexp_newtypes ?loc newtypes exp =
  List.fold_right (fun newtype exp -> mkexp ?loc (Pexp_newtype (newtype, exp)))
    newtypes exp

(**
  I believe that wrap_type_annotation will automatically generate the type
  arguments (type a) (type b) based on what was listed before the dot in a
  polymorphic type annotation that uses locally abstract types.
 *)
let wrap_type_annotation newtypes core_type body =
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp = pexp_newtypes newtypes exp in
  let typ = mktyp ~ghost:true (Ptyp_poly(newtypes,varify_constructors newtypes core_type)) in
  (exp, typ)


let struct_item_extension (ext_attrs, ext_id) structure_item =
  mkstr ~ghost:true (Pstr_extension ((ext_id, PStr [structure_item]), ext_attrs))

let extension_expression (ext_attrs, ext_id) item_expr =
  mkexp ~ghost:true ~attrs:ext_attrs (Pexp_extension (ext_id, PStr [mkstrexp item_expr []]))

(* There's no more need for these functions - this was for the following:
 *
 *     fun % ext [@foo] arg => arg;
 *
 *   Becoming
 *
 *     [%ext  (fun arg => arg) [@foo]]
 *
 *   Which we no longer support.
 *)
(* Applies the attributes to the body, then wraps entire thing in an extension
 * expression, whose payload consists of a single structure item that is body
 *)
(* let wrap_exp_attrs body (ext, attrs) = *)
(*   (* todo: keep exact location for the entire attribute *) *)
(*   let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in *)
(*   match ext with *)
(*   | None -> body *)
(*   | Some id -> mkexp ~ghost:true (Pexp_extension (id, PStr [mkstrexp body []])) *)

(* Why not just mkexp with the right attributes in the first place? *)
(* let mkexp_attrs d attrs = *)
(*   wrap_exp_attrs (mkexp d) attrs *)

let mkcf_attrs ?(loc=dummy_loc()) d attrs =
  Cf.mk ~loc ~attrs d

let mkctf_attrs d attrs =
  Ctf.mk ~attrs d

type let_binding =
  { lb_pattern: pattern;
    lb_expression: expression;
    (* The meaning of lb_leading_attributes and lbs_extension are dependent on
     * the context of the let binding (module/expression etc) *)
    lb_attributes: attributes;
    (* lb_docs: docs Lazy.t; *)
    (* lb_text: text Lazy.t; *)
    lb_loc: Location.t; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_rec: rec_flag;
    lbs_extension: string Asttypes.loc option;
    (* In Reason, we use this field to represent
       extension attributes attached to the extension on a series of "let/and"
       bindings As in: let [@extAttrs ] [%id] [@attribute] x = ...; It only
       makes sense to have [lbs_attributes] when there is an [lbs_extension].
     *)
    lbs_attributes: attributes;
    lbs_loc: Location.t }

let mklb (p, e) attrs loc =
  { lb_pattern = p;
    lb_expression = e;
    (* Only some individual let bindings are allowed to have attributes
     * depending on the context *)
    lb_attributes = attrs;
    lb_loc = loc; }

let mklbs (extAttrs, extId) rf lb loc =
  { lbs_bindings = [lb];
    lbs_rec = rf;
    lbs_extension = extId ;
    lbs_attributes = extAttrs;
    lbs_loc = loc; }

let addlbs lbs lbs' =
  { lbs with lbs_bindings = lbs.lbs_bindings @ lbs' }

let val_of_let_bindings lbs =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           (* ~docs:(Lazy.force lb.lb_docs) *)
           (* ~text:(Lazy.force lb.lb_text) *)
           lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
  let str = mkstr(Pstr_value(lbs.lbs_rec, bindings)) in
  (* Note that for value bindings, when there's an extension, the
   * lbs_attributes are attributes on the extension *)
  match (lbs.lbs_extension) with
    | None -> str
    | Some ext_id -> struct_item_extension (lbs.lbs_attributes, ext_id) str

let expr_of_let_bindings lbs body =
  let bindings =
    List.map
      (fun lb ->
         (* Individual let bindings in an *expression* can't have item attributes. *)
         if lb.lb_attributes <> [] then
           raise Syntaxerr.(Error(Not_expecting(lb.lb_loc, "item attribute")));
         Vb.mk ~loc:lb.lb_loc lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
  (* The location of this expression unfortunately includes the entire rule,
   * which will include any preceeding extensions. *)
  let item_expr = mkexp (Pexp_let(lbs.lbs_rec, bindings, body)) in
  (* Note that for let expression bindings, when there's an extension, the
   * lbs_attributes are attributes on the entire [let ..in x] expression. *)
  match lbs.lbs_extension with
    | None -> item_expr
    | Some ext_id -> extension_expression (lbs.lbs_attributes, ext_id) item_expr

let class_of_let_bindings lbs body =
  let bindings =
    List.map
      (fun lb ->
         if lb.lb_attributes <> [] then
           raise Syntaxerr.(Error(Not_expecting(lb.lb_loc, "item attribute")));
         Vb.mk ~loc:lb.lb_loc lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
    if lbs.lbs_extension <> None then
      raise Syntaxerr.(Error(Not_expecting(lbs.lbs_loc, "extension")));
    if lbs.lbs_attributes <> [] then
      raise Syntaxerr.(Error(Not_expecting(lbs.lbs_loc, "attributes")));
    mkclass(Pcl_let (lbs.lbs_rec, bindings, body))

(*
 * arity_conflict_resolving_mapper is triggered when both "implicit_arity" "explicit_arity"
 * are in the attribtues. In that case we have to remove "explicit_arity"
 *
 * However, if we simply remove explicit_arity, we would end up with a
 * wrapping tuple which has only one component (inner tuple).
 * This is against the invariance where tuples must have 2+ components.
 * Therefore, in the case we have to remove explicit_arity, we also need to
 * unwrap the tuple to expose the inner tuple directly.
 *
 *)
let arity_conflict_resolving_mapper super =
{ super with
  expr = begin fun mapper expr ->
    match expr with
      | {pexp_desc=Pexp_construct(lid, args);
         pexp_loc;
         pexp_attributes} when attributes_conflicted "implicit_arity" "explicit_arity" pexp_attributes ->
         let new_args =
           match args with
             | Some {pexp_desc = Pexp_tuple [sp]} -> Some sp
             | _ -> args in
         super.expr mapper
         {pexp_desc=Pexp_construct(lid, new_args); pexp_loc; pexp_attributes=
          normalized_attributes "explicit_arity" pexp_attributes}
      | x -> super.expr mapper x
  end;
  pat = begin fun mapper pattern ->
    match pattern with
      | {ppat_desc=Ppat_construct(lid, args);
         ppat_loc;
         ppat_attributes} when attributes_conflicted "implicit_arity" "explicit_arity" ppat_attributes ->
         let new_args =
           match args with
             | Some {ppat_desc = Ppat_tuple [sp]} -> Some sp
             | _ -> args in
         super.pat mapper
         {ppat_desc=Ppat_construct(lid, new_args); ppat_loc; ppat_attributes=
          normalized_attributes "explicit_arity" ppat_attributes}
      | x -> super.pat mapper x
  end;
}

(* NB: making this a function might have parse-time performance penalties *)
let reason_mapper () =
  begin
    if !Reason_config.add_printers
    then create_auto_printer_mapper default_mapper
    else default_mapper
  end
  |> unescape_stars_slashes_mapper
  |> reason_to_ml_swap_operator_mapper
  |> arity_conflict_resolving_mapper

let rec string_of_longident = function
    | Lident s -> s
    | Ldot(longPrefix, s) ->
        s
    | Lapply (y,s) -> string_of_longident s

let built_in_explicit_arity_constructors = ["Some"; "Assert_failure"; "Match_failure"]

let jsx_component module_name attrs children loc =
  let firstPart = (List.hd (Longident.flatten module_name)) in
  let lident = if String.get firstPart 0 != '_' && firstPart = String.capitalize firstPart then
    (* firstPart will be non-empty so the 0th access is fine. Modules can't start with underscore *)
    Ldot(module_name, "createElement")
  else
    Lident firstPart
  in
  let ident = mkloc lident loc in
  let body = mkexp(Pexp_apply(mkexp(Pexp_ident ident) ~loc, attrs @ children)) ~loc in
  let attribute = ({txt = "JSX"; loc = loc}, PStr []) in
  { body with pexp_attributes = attribute :: body.pexp_attributes }

let ensureTagsAreEqual startTag endTag loc =
  if startTag <> endTag then
     let startTag = (String.concat "" (Longident.flatten startTag)) in
     let endTag = (String.concat "" (Longident.flatten endTag)) in
     let _ = Location.raise_errorf ~loc "Syntax error: Start tag <%s> does not match end tag </%s>" startTag endTag in
     ()

type object_record =
  | Object_open
  | Object_closed
  | Record

type core_type_object =
  | Core_type of core_type
  | Record_type of label_declaration list

let only_core_type t startp endp =
  match t with
  | Core_type ct -> ct
  | Record_type _ ->
    let loc = mklocation startp endp in
    Location.raise_errorf ~loc "Record type is not allowed"

let only_labels l =
  let rec loop label_declarations result =
    match label_declarations with
    | hd :: tail ->
      let (l, a) = hd in
      if (List.length a > 0) then
        syntax_error ()
      else
        loop tail (result @ [l] )
    | [] -> result
  in
  loop l []

%}



/* Tokens */

%token <string> LIDENTCOLONCOLON
%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOTDOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string * char option> FLOAT
%token <string> COLONCOLONLIDENT
%token FOR
%token FUN ES6_FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
/* SLASHGREATER is an INFIXOP3 that is handled specially */
%token SLASHGREATER
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <string * char option> INT
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LBRACKETPERCENT
%token LBRACKETPERCENTPERCENT
%token LESS
%token <string> LESSIDENT
%token LESSGREATER
%token LESSSLASHGREATER
%token LESSDOTDOTGREATER
%token LESSMINUS
%token EQUALGREATER
%token LET
%token <string> LIDENT
%token LPAREN
%token LBRACKETAT
%token LBRACKETATAT
%token LBRACKETATATAT
%token LESSSLASH
%token OF
%token PRI
%token SWITCH
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT
%token NEW
%token NONREC
%token OBJECT
%token OPEN
%token OR
/* %token PARSER */
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token <string> PREFIXOP
%token PUB
%token QUESTION
%token OPTIONAL_NO_DEFAULT
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token <string> LESSSLASHIDENTGREATER
%token SEMI
%token SEMISEMI
%token SHARP
%token <string> SHARPOP
%token SIG
%token STAR
%token <string * string option> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%token <string * Location.t> COMMENT

%token EOL

/* Precedences and associativities.

Tokens and rules have precedences and those precedences are used to
resolve what would otherwise be a conflict in the grammar.

Precedence and associativity/Resolving conflicts:
----------------------------
See [http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual026.html] section
about conflicts.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

*/
/* Question: Where is the SEMI explicit precedence? */
%nonassoc below_SEMI
%nonassoc below_EQUALGREATER
%right    EQUALGREATER                  /* core_type2 (t => t => t) */
%right COLON
%right    EQUAL                         /* below COLONEQUAL (lbl = x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc QUESTION
%nonassoc WITH             /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc AS
%nonassoc below_BAR                     /* Allows "building up" of many bars */
%left     BAR                           /* pattern (p|p|p) */

%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%left     INFIXOP0 LESS GREATER         /* expr (e OP e OP e) */
%left     LESSDOTDOTGREATER /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ /* expr (e OP e OP e) */
%left     PERCENT INFIXOP3 SLASHGREATER STAR          /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */

/**
 * With the way attributes are currently parsed, if we want consistent precedence for
 *
 * The OCaml parser parses the following attributes:
 *
 *    let x = true && (false [@attrOnFalse])
 *    let x = true && false [@attrOnFalse]
 *    let x = 10 + 20 [@attrOn20]
 *    let x = (10 + 20) [@attrEntireAddition]
 *
 * As:
 *
 *    let x = true && ((false)[@attrOnFalse ])
 *    let x = true && ((false)[@attrOnFalse ])
 *    let x = ((10 + 20)[@attrOn20 ])
 *    let x = ((10 + 20)[@attrEntireAddition ])
 *
 * That is because the precedence of tokens is configured as following, which
 * only serves to treat certain infix operators as different than others with
 * respect to attributes *only*.
 *
 *    %right    OR BARBAR
 *    %right    AMPERSAND AMPERAMPER
 *    %nonassoc below_EQUAL
 *    %left     INFIXOP0 EQUAL LESS GREATER
 *    %right    INFIXOP1
 *    %nonassoc LBRACKETAT
 *    %nonassoc LBRACKETATAT
 *    %right    COLONCOLON
 *    %left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ
 *    %left     PERCENT INFIXOP3 SLASHGREATER STAR
 *    %right    INFIXOP4
 *
 * So instead, with Reason, we treat all infix operators identically w.r.t.
 * attributes. In expressions, they have the same precedence as function
 * arguments, as if they are additional arguments to a function application.
 *
 * Note that unary subtractive/plus parses with lower precedence than function
 * application (and attributes) This means that:
 *
 *   let = - something blah blah [@attr];
 *
 * Will have the attribute applied to the entire content to the right of the
 * unary minus, as if the attribute was merely another argument to the function
 * application.
 *
 *
 * To make the attribute apply to the unary -, wrap in parens.
 *
 *   let = (- something blah blah) [@attr];
 *
 * Where arrows occur, it will (as always) obey the rules of function/type
 * application.
 *
 *   type x = int => int [@onlyAppliedToTheInt];
 *   type x = (int => int) [@appliedToTheArrow];
 *
 * However, unary subtractive/plus parses with *higher* precedence than infix
 * application, so that
 *
 *   3 + - funcCall arg arg + 3;
 *
 * Is parsed as:
 *
 *   3 + (- (funcCall arg arg)) + 3;
 *
 * TODO:
 *
 * We would also like to bring this behavior to `!` as well, when ! becomes
 * "not". This is so that you may do !someFunction(arg, arg) and have the
 * entire function application negated. In fact, we may as well just have all
 * of PREFIXOP have the unary precedence parsing behavior for consistency.
 */

%nonassoc attribute_precedence

%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
/* Now that commas require wrapping parens (for tuples), prec_constr_appl no
* longer needs to be above COMMA, but it doesn't hurt */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */

/* PREFIXOP and BANG precedence */
%nonassoc below_DOT_AND_SHARP           /* practically same as below_SHARP but we convey purpose */
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%left     SHARPOP
%nonassoc below_DOT
%nonassoc DOT

/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc below_LPAREN
%nonassoc LBRACE LPAREN UIDENT LBRACKETPERCENT

/* Entry points */

%start implementation                   /* for implementation files */
%type <Ast_404.Parsetree.structure> implementation
%start interface                        /* for interface files */
%type <Ast_404.Parsetree.signature> interface
%start toplevel_phrase                  /* for interactive use */
%type <Ast_404.Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Ast_404.Parsetree.toplevel_phrase list> use_file
%start parse_core_type
%type <Ast_404.Parsetree.core_type> parse_core_type
%start parse_expression
%type <Ast_404.Parsetree.expression> parse_expression
%start parse_pattern
%type <Ast_404.Parsetree.pattern> parse_pattern

/* Instead of reporting an error directly, productions specified
 * below will be reduced first and poped up in the stack to a higher
 * level production.
 *
 * This is essential to error reporting as it is much friendier to provide
 * a higher level error (e.g., "Expecting the parens to be closed" )
 * as opposed to a low-level one (e.g., "Expecting to finish
 * current type definition").
 *
 * See Menhir's manual for more details.
 */
%on_error_reduce curried_binding_return_typed
                 structure_item let_binding_body
                 item_attribute+
                 less_aggressive_simple_expression
                 type_longident
                 attribute*
                 constr_longident
                 pattern
                 nonrec_flag
                 val_ident
                 SEMI?
                 curried_binding
                 expr_optional_constraint
%%

/* Entry points */

implementation:
  structure EOF
  { apply_mapper_to_structure $1 (reason_mapper ()) }
;

interface:
  signature EOF
  { apply_mapper_to_signature $1 (reason_mapper ()) }
;

toplevel_phrase: embedded
  ( EOF                              { raise End_of_file}
  | structure_item     SEMI          { Ptop_def [$1]}
  | toplevel_directive SEMI          { $1 }
  ) {apply_mapper_to_toplevel_phrase $1 (reason_mapper ()) }
;

use_file: embedded
  ( EOF                              { [] }
  | structure_item     SEMI use_file { Ptop_def[$1] :: $3 }
  | toplevel_directive SEMI use_file { $1 :: $3 }
  | structure_item     EOF           { [Ptop_def[$1]] }
  | toplevel_directive EOF           { [$1] }
  ) {apply_mapper_to_use_file $1 (reason_mapper ())}
;

parse_core_type:
  only_core_type(core_type) EOF
  { apply_mapper_to_type $1 (reason_mapper ()) }
;

parse_expression:
  expr EOF
  { apply_mapper_to_expr $1 (reason_mapper ()) }
;

parse_pattern:
  pattern EOF
  { apply_mapper_to_pattern $1 (reason_mapper ()) }
;

/* Module expressions */

functor_arg:
  | LPAREN RPAREN
	  { (mkloc "*" (mklocation $startpos $endpos), None) }
  | LPAREN as_loc(functor_arg_name) COLON module_type RPAREN
    { ($2, Some $4) }
;

functor_arg_name:
  | UIDENT     { $1 }
  | UNDERSCORE { "_" }
;

simple_module_expr:
mark_position_mod
  ( as_loc(mod_longident)
    { mkmod(Pmod_ident $1) }
  | LBRACE structure RBRACE
    { mkmod(Pmod_structure($2)) }
  | LPAREN module_expr COLON module_type RPAREN
    { mkmod(Pmod_constraint($2, $4)) }
  | as_loc(LPAREN) module_expr COLON module_type as_loc(error)
    { unclosed_mod (with_txt $1 "(") (with_txt $5 ")")}
  | LPAREN module_expr RPAREN
    { $2 }
  | LPAREN VAL expr RPAREN
    { mkmod(Pmod_unpack $3) }
  | LPAREN VAL expr COLON package_type RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkmod (Pmod_unpack(
             mkexp ~ghost:true ~loc (Pexp_constraint($3, mktyp ~ghost:true ~loc (Ptyp_package $5))))) }
  | LPAREN VAL expr COLON package_type COLONGREATER package_type RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkmod (Pmod_unpack(
             mkexp ~ghost:true ~loc (Pexp_coerce($3, Some(mktyp ~ghost:true ~loc (Ptyp_package $5)),
                                    mktyp ~ghost:true ~loc (Ptyp_package $7))))) }
  | LPAREN VAL expr COLONGREATER package_type RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkmod (Pmod_unpack(
             mkexp ~ghost:true ~loc (Pexp_coerce($3, None, mktyp ~ghost:true ~loc (Ptyp_package $5))))) }
  | LPAREN RPAREN
    { mkmod (Pmod_structure []) }
  | extension
    { mkmod (Pmod_extension $1) }
  ) {$1};

module_expr:
mark_position_mod
  ( simple_module_expr { $1 }
  /**
   * Although it would be nice (and possible) to support annotated return value
   * here, that wouldn't be consistent with what is possible for functions.
   * Update: In upstream, it *is* possible to annotate return values for
   * lambdas.
   */
  | FUN functor_arg+ EQUALGREATER module_expr
    { mkFunctorThatReturns $2 $4 }
  | module_expr simple_module_expr
    { mkmod (Pmod_apply($1, $2)) }
  | module_expr as_loc(LPAREN) module_expr as_loc(error)
    { unclosed_mod (with_txt $2 "(") (with_txt $4 ")") }
  | as_loc(LPAREN) module_expr as_loc(error)
    { unclosed_mod (with_txt $1 "(") (with_txt $3 ")") }
  | as_loc(LPAREN) VAL expr COLON as_loc(error)
    { unclosed_mod (with_txt $1 "(") (with_txt $5 ")") }
  | as_loc(LPAREN) VAL expr COLONGREATER as_loc(error)
    { unclosed_mod (with_txt $1 "(") (with_txt $5 ")") }
  | as_loc(LPAREN) VAL expr as_loc(error)
    { unclosed_mod (with_txt $1 "(") (with_txt $4 ")") }
  | attribute module_expr %prec attribute_precedence
    { {$2 with pmod_attributes = $1 :: $2.pmod_attributes } }
  ) {$1};

/**
 * Attributes/Extension points TODO:
 * - Faux-ternary to support extension points (printing/parsing).
 * - Audit to ensure every [item attributes] [item extensions] is supported.
 * - wrap_exp_attrs / mkexp_attrs cleanup - no need for the confusing
 * indirection.
 * - Ensure proper parsing as ensured by commit:
 *   4c48d802cb9e8110ab3b57ca0b6a02fdd5655283
 * - Support the Item Extension + Item Attributes pattern/sugar/unification for
 * all items in let sequences (let module etc / let open).
 */

/*
 * In OCaml there are (confusingly) two ways to execute imperitive code at the
 * top level:
 *
 *   doStuff();      (* parsed as let _ = ... *)
 *   doMoreStuff();  (* parsed as let _ = ... *)
 *   ;;              (* SEMISEMI *)
 *   let exportedThing = blah
 *
 *   let _ = doStuff()
 *   let _ = doStuff()
 *   let exportedThing = blah   (* SEMISEMI not needed if no leading seq_expr *)
 *
 *
 * SEMISEMI (and a bunch of other inconsistencies/suprises) are the price you
 * pay for not requiring a delimiter after each binding/imperitive action.
 *
 *    let myFn () =
 *       let x = 0 in
 *       let y = 10 in
 *       x + y
 *
 * Also, in OCaml, there is a different syntax for let bindings in function
 * bodies, where each let bindings group must end with IN.
 *
 * If we just *require* that every module export is terminated by a SEMI, and
 * require that sequence expressions are grouped by some surrounding tokens {},
 * then we can have a consistent, familiar way of executing imperitive code, or
 * let bindings (without introducing SEMISEMI and without forcing you to write
 * [let _ = imperitive()]).
 *
 *   doStuff();
 *   doStuff();
 *   let exportedThing = blah;   (* SEMISEMI not needed *)
 *
 * Also, we can then make function scoped let bindings have a consistent syntax
 * with the top level module syntax.
 *
 *    let myFn () => {
 *      let x = 0;
 *      let y = 10;
 *      x + y;
 *    };
 }
 *
 * There are other practical reasons to require each structure item (or record
 * item etc) to be terminated by a SEMI. It allows IDEs to correct indentation
 * as you type. Otherwise (as in OCaml) your editor has to wait until you type
 * `let` again to determine the indentation of the new line - for many editors,
 * achieving that configuration is not easy.
 *
 *  structure:
 *      seq_expr item_attribute* structure_tail { mkstrexp $1 $2 :: $3 }
 *    | structure_tail { $1 }
 *  ;
 *  structure_tail:
 *                           { [] }
 *    | SEMISEMI structure   { $2 }
 *    | structure_item SEMI structure_tail { $1 :: $3 }
 */

structure:
  | /* Empty */ { [] }
  | as_loc(error) structure
    { let menhirError = Syntax_util.findMenhirErrorMessage $1.loc in
      match menhirError with
      | MenhirMessagesError errMessage -> (syntax_error_str errMessage.loc errMessage.msg) :: $2
      | _ -> (syntax_error_str $1.loc "Invalid statement") :: $2
    }
  | as_loc(error) SEMI structure
    { let menhirError = Syntax_util.findMenhirErrorMessage $1.loc in
      match menhirError with
      | MenhirMessagesError errMessage -> (syntax_error_str errMessage.loc errMessage.msg) :: $3
      | _ -> (syntax_error_str $1.loc "Invalid statement") :: $3
    }
  | structure_item { [$1] }
  | as_loc(structure_item) error structure
    { let menhirError = Syntax_util.findMenhirErrorMessage $1.loc in
      match menhirError with
      | MenhirMessagesError errMessage -> (syntax_error_str errMessage.loc errMessage.msg) :: $3
      | _ -> (syntax_error_str $1.loc "Statement has to end with a semicolon") :: $3
    }
  | structure_item SEMI structure
    { let effective_loc = mklocation $symbolstartpos $endpos($2) in
      set_structure_item_location $1 effective_loc :: $3
    }
;

structure_item:
mark_position_str
  ( item_extension_sugar structure_item_without_item_extension_sugar
    { struct_item_extension $1 $2 }
  | structure_item_without_item_extension_sugar
    { $1 }
    /* Each let binding has its own item_attribute* */
  | let_bindings
    { val_of_let_bindings $1 }
  ) {$1};

structure_item_without_item_extension_sugar:
mark_position_str
  /* We consider a floating expression to be equivalent to a single let binding
     to the "_" (any) pattern.  */
  ( item_attributes expr
    { mkstrexp $2 $1 }
  | item_attributes
    EXTERNAL as_loc(val_ident) COLON only_core_type(core_type) EQUAL primitive_declaration
    { let loc = mklocation $symbolstartpos $endpos in
      mkstr (Pstr_primitive (Val.mk $3 $5 ~prim:$7 ~attrs:$1 ~loc)) }
  | type_declarations
    { let (nonrec_flag, tyl) = $1 in mkstr(Pstr_type (nonrec_flag, tyl)) }
  | str_type_extension
    { mkstr(Pstr_typext $1) }
  | str_exception_declaration
    { mkstr(Pstr_exception $1) }
  | item_attributes opt_LET_MODULE as_loc(UIDENT) module_binding_body
    { let loc = mklocation $symbolstartpos $endpos in
      mkstr(Pstr_module (Mb.mk $3 $4 ~attrs:$1 ~loc)) }
  | item_attributes opt_LET_MODULE REC as_loc(UIDENT) module_binding_body
    and_nonlocal_module_bindings*
    { let loc = mklocation $symbolstartpos $endpos($5) in
      mkstr (Pstr_recmodule ((Mb.mk $4 $5 ~attrs:$1 ~loc) :: $6))
    }
  | item_attributes MODULE TYPE OF? as_loc(ident)
    { let loc = mklocation $symbolstartpos $endpos in
      mkstr(Pstr_modtype (Mtd.mk $5 ~attrs:$1 ~loc)) }
  | item_attributes MODULE TYPE OF? as_loc(ident) EQUAL module_type
    { let loc = mklocation $symbolstartpos $endpos in
      mkstr(Pstr_modtype (Mtd.mk $5 ~typ:$7 ~attrs:$1 ~loc)) }
  | open_statement
    { mkstr(Pstr_open $1) }
  | item_attributes CLASS class_declaration_details and_class_declaration*
    { let (ident, binding, virt, params) = $3 in
      let loc = mklocation $symbolstartpos $endpos($3) in
      let first = Ci.mk ident binding ~virt ~params ~attrs:$1 ~loc in
      mkstr (Pstr_class (first :: $4))
    }
  | class_type_declarations
      (* Each declaration has their own preceeding item_attribute* *)
    { mkstr(Pstr_class_type $1) }
  | item_attributes INCLUDE module_expr
    { let loc = mklocation $symbolstartpos $endpos in
      mkstr(Pstr_include (Incl.mk $3 ~attrs:$1 ~loc))
    }
  | item_attributes item_extension
    (* No sense in having item_extension_sugar for something that's already an
     * item_extension *)
    { mkstr(Pstr_extension ($2, $1)) }
  | floating_attribute
    { mkstr(Pstr_attribute $1) }
  ) {$1};

module_binding_body_expr:
  | EQUAL module_expr
    { $2 }
  | COLON module_type EQUAL module_expr
    { let loc = mklocation $symbolstartpos $endpos in
      mkmod ~loc (Pmod_constraint($4, $2)) }
;

module_binding_body_functor:
mark_position_mod
  ( functor_arg+ EQUALGREATER module_expr
    { mkFunctorThatReturns $1 $3 }
  | functor_arg+ COLON non_arrowed_module_type EQUALGREATER module_expr
    { let loc = mklocation $startpos($5) $endpos($5) in
      mkFunctorThatReturns $1 (mkmod ~loc (Pmod_constraint($5, $3))) }
  ) {$1};

module_binding_body:
  module_binding_body_expr | module_binding_body_functor { $1 };

and_nonlocal_module_bindings:
  item_attributes AND as_loc(UIDENT) module_binding_body
  { Mb.mk $3 $4 ~attrs:$1 ~loc:(mklocation $symbolstartpos $endpos) }
;

/* Module types */

/*
(*
 * For now, WITH constraints on module types shouldn't be considered
 * "non-arrowed" because their WITH constraints themselves might contain arrows.
 * We could ensure that you may only supply *non* arrowed types (or wrap arrowed
 * types in parens) in thier WITH constraints, but that's just too difficult to
 * think about so we will simply not consider WITH constrained module types as
 * non-arrowed.
 *
 * Given this, we can probably take out the below_WITH precedence in the rules
 * down below.
 *)
*/

/* Allowed in curried let bidings */
non_arrowed_module_type:
mark_position_mty
  ( simple_module_type
    {$1}
  | MODULE TYPE OF module_expr
    { mkmty (Pmty_typeof $4) }
  | attribute module_type %prec attribute_precedence
    { {$2 with pmty_attributes = $1 :: $2.pmty_attributes} }
  ) {$1};

simple_module_type:
mark_position_mty
  ( LPAREN module_type RPAREN
    { $2 }
  | as_loc(LPAREN) module_type as_loc(error)
    { unclosed_mty (with_txt $1 "(") (with_txt $3 ")")}
  | as_loc(mty_longident)
    { mkmty (Pmty_ident $1) }
  | LBRACE signature RBRACE
    { mkmty (Pmty_signature $2) }
  | as_loc(LBRACE) signature as_loc(error)
    { unclosed_mty (with_txt $1 "{") (with_txt $3 "}")}
  | extension
    { mkmty (Pmty_extension $1) }
  ) {$1};

/*
(*
 * For module types, we have:
 *
 *  simple_module_type: (Non arrowed implied)
 *  non_arrowed_module_type
 *  module_type ::=
 *    module_type WITH ...
 *    non_arrowed_module_type
 *    (arg : module_type) => module_type
 *    module_type => module_type
 *)
*/

module_type:
mark_position_mty
  ( module_type WITH lseparated_nonempty_list(AND, with_constraint)
    (* See note above about why WITH constraints aren't considered
     * non-arrowed.
     * We might just consider unifying the syntax for record extension with
     * module extension/WITH constraints.
     *
     *    mod MyModule = {
     *       ModuleToInclude...
     *    };
     *
     *    let module CreateFactory
     *               (Spec: ContainerSpec)
     *               :{DescriptorFactoryIntf.S with
     *                  type props = Spec.props and type dependencies = Spec.props} =>
     *
     *)
    { mkmty (Pmty_with($1, $3)) }
  | non_arrowed_module_type
    (* below_EQUALGREATER to prevent following shift reduce conflict:
     *  1158: shift/reduce conflict (shift 1285, reduce 75) on EQUALGREATER
     *  state 1158
     *    module_binding_body_functor : functor_arg+ COLON non_arrowed_module_type . EQUALGREATER module_expr  (59)
     *    module_type : non_arrowed_module_type .  (75)
     *
     *    EQUALGREATER  shift 1285
     *    LBRACKETAT  reduce 75
     *    WITH  reduce 75
     *)
    { $1 }
  | LPAREN as_loc(functor_arg_name) COLON module_type RPAREN EQUALGREATER module_type %prec below_SEMI
    (* Why does this rule cause a conflict with core_type2? It has nothing to do
     * with it.
     *
     * Update: I'm guessing it has something to do with the fact that this isn't
     * parsed correctly because the => is considered part of "type dependenences" :
     *
     *    let module CreateFactory
     *               (Spec: ContainerSpec)
     *               :DescriptorFactoryIntf.S with
     *                  type props = Spec.props and type dependencies = Spec.props =>
     *)
    { mkmty(Pmty_functor($2, Some $4, $7)) }
  | module_type EQUALGREATER module_type %prec below_SEMI
      (**
       * In OCaml, this is invalid:
       * module MyFunctor: functor MT -> (sig end) = functor MT -> (struct end);;
       *
       * Not only must curried functor args have annotations, but functor
       * annotations must include *names* for each argument in a functor type.
       *
       * module MyFunctor: functor (MT:MT) -> (sig end) = functor (MT:MT) -> (struct end)
       *
       * In Reason, we will parse the functor type:
       *
       *    (AB:MT) -> ReturnSig
       *
       * as in:
       *                   /----------------\
       * module MyFunctor: (A:B) => ReturnSig = functor (C:D) => {}
       *
       * But only for the sake of compatibility with existing OCaml code (the
       * ability to "view" OCaml code in Reason's syntax without loss of
       * information.) Do not write identifiers in functor argument type
       * positions - you wouldn't do it with functions, and they are
       * meaningless in functors.
       *
       *  But for sake of consistency (and for sake of a syntax that truly
       *  unifies functor syntax with function syntax, the following "sugars"
       *  will be parsed (and printed):
       *
       *   A => B => C
       *
       * Is parsed into:
       *
       * functor (_:A) -> functor (_:B) -> C
       *
       * And a dummy "_" is inserted into the parse tree where no name has been
       * provided.
       *
       *   {SomeSig} => {} => {}
       *
       * Is parsed into:
       *
       * (_:SomeSig) => (_:{}) => {}
       *
       *
       *)
    { let loc = mklocation $symbolstartpos $endpos in
      mkmty(Pmty_functor(ghloc ~loc "_", Some $1, $3))
    }
  ) {$1};

signature: terminated(signature_item, SEMI)* { $1 };

signature_item:
mark_position_sig
  ( item_attributes
    LET as_loc(val_ident) COLON only_core_type(core_type)
    { let loc = mklocation $symbolstartpos $endpos in
      mksig(Psig_value (Val.mk $3 $5 ~attrs:$1 ~loc))
    }
  | item_attributes
    EXTERNAL as_loc(val_ident) COLON only_core_type(core_type) EQUAL primitive_declaration
    { let loc = mklocation $symbolstartpos $endpos in
      mksig(Psig_value (Val.mk $3 $5 ~prim:$7 ~attrs:$1 ~loc))
    }
  | type_declarations
    { let (nonrec_flag, tyl) = $1 in mksig (Psig_type (nonrec_flag, tyl)) }
  | sig_type_extension
    { mksig(Psig_typext $1) }
  | sig_exception_declaration
    { mksig(Psig_exception $1) }
  | item_attributes opt_LET_MODULE as_loc(UIDENT) module_declaration
    { let loc = mklocation $symbolstartpos $endpos in
      mksig(Psig_module (Md.mk $3 $4 ~attrs:$1 ~loc))
    }
  | item_attributes opt_LET_MODULE as_loc(UIDENT) EQUAL as_loc(mod_longident)
    { let loc = mklocation $symbolstartpos $endpos in
      let loc_mod = mklocation $startpos($5) $endpos($5) in
      mksig(
        Psig_module (
          Md.mk
            $3
            (Mty.alias ~loc:loc_mod $5)
            ~attrs:$1
            ~loc
        )
      )
    }
  | item_attributes opt_LET_MODULE REC as_loc(UIDENT) COLON module_type
      and_module_rec_declaration*
    { let loc = mklocation $symbolstartpos $endpos($6) in
      mksig (Psig_recmodule (Md.mk $4 $6 ~attrs:$1 ~loc :: $7)) }
  | item_attributes MODULE TYPE as_loc(ident)
    { let loc = mklocation $symbolstartpos $endpos in
      mksig(Psig_modtype (Mtd.mk $4 ~attrs:$1 ~loc))
    }
  | item_attributes MODULE TYPE as_loc(ident) EQUAL module_type
    { let loc = mklocation $symbolstartpos $endpos in
      mksig(Psig_modtype (Mtd.mk $4 ~typ:$6 ~loc ~attrs:$1))
    }
  | open_statement
    { mksig(Psig_open $1) }
  | item_attributes INCLUDE module_type
    { let loc = mklocation $symbolstartpos $endpos in
      mksig(Psig_include (Incl.mk $3 ~attrs:$1 ~loc))
    }
  | class_descriptions
    { mksig(Psig_class $1) }
  | class_type_declarations
    { mksig(Psig_class_type $1) }
  | item_attributes item_extension
    { mksig(Psig_extension ($2, $1)) }
  | floating_attribute
    { mksig(Psig_attribute $1) }
  ) {$1};

open_statement:
  item_attributes OPEN override_flag as_loc(mod_longident)
  { Opn.mk $4 ~override:$3 ~attrs:$1 ~loc:(mklocation $symbolstartpos $endpos) }
;

module_declaration:
mark_position_mty
  ( COLON module_type
      { $2 }
  | LPAREN as_loc(UIDENT) COLON module_type RPAREN module_declaration
      { mkmty(Pmty_functor($2, Some $4, $6)) }
  | LPAREN RPAREN module_declaration
      { mkmty(Pmty_functor(mkloc "*" (mklocation $startpos($1) $endpos($1)), None, $3)) }
  ) {$1};

and_module_rec_declaration:
  item_attributes AND as_loc(UIDENT) COLON module_type
  { Md.mk $3 $5 ~attrs:$1 ~loc:(mklocation $symbolstartpos $endpos) }
;

/* Class expressions */

and_class_declaration:
  item_attributes AND class_declaration_details
  { let (ident, binding, virt, params) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    Ci.mk ident binding ~virt ~params ~attrs:$1 ~loc
  }
;

class_declaration_details:
  | virtual_flag as_loc(LIDENT) class_type_parameters class_declaration_expr
    { ($2, $4, $1, $3) }
  | virtual_flag as_loc(LIDENT) class_declaration_expr
    { ($2, $3, $1, []) }
;

class_declaration_expr:
  /* Used in order to parse: class ['a, 'b] myClass argOne argTwo (:retType) => cl_expr */
  | class_fun_binding
  /* We make a special rule here because we only accept colon after the
   * identifier - can't be a part of class_fun_binding. Used in order to parse:
   * class ['a, 'b] myClass: class_constructor_type = class_expr
   */
  | constrained_class_declaration
  /* Used in order to parse:  class ['a, 'b] myClass = class_expr */
  | preceded(EQUAL, class_expr)
    { $1 }
;

constrained_class_declaration:
mark_position_cl
  ( COLON class_constructor_type EQUAL class_expr
    { mkclass(Pcl_constraint($4, $2)) }
  ) {$1};

/**
 * Had to split class_fun_return rom class_fun_binding to prevent arrow from
 * being used when no arguments.
 * Without that, there were difficult parsing conflicts between:
 *
 *   class thisDoesntParse : blah =>
 *   class thisDoesParse   : blah =
 *
 * Now, you can do:
 *
 *   class myClass arg blah : instance_type => {
 *     method blah => ..;
 *   }
 *
 * But you cannot constrain with a function Pcty_arrow
 *
 *   class myClass arg blah : (int => instance_type) =>
 *      fun i => {...};
 *
 * Instead, you must write the previous as:
 * (And the printer must ensure this)
 *
 *   class myClass arg blah =>
 *      (fun i => {...} : int => instance_type);
 */
class_fun_binding:
mark_position_cl
  ( labeled_simple_pattern class_fun_binding
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $2)) }
  | labeled_simple_pattern class_fun_return
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $2)) }
  ) {$1};

class_fun_return:
  | EQUALGREATER class_expr { $2 }
  | COLON mark_position_cty(non_arrowed_class_constructor_type)
      EQUALGREATER class_expr
    { let loc = mklocation $symbolstartpos $endpos in
      mkclass ~loc (Pcl_constraint($4, $2))
    }
;

class_fun_def:
mark_position_cl
  ( labeled_simple_pattern EQUALGREATER class_expr
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $3)) }
  | labeled_simple_pattern class_fun_def
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $2)) }
  ) {$1};

class_expr_lets_and_rest:
mark_position_cl
  ( class_expr { $1 }
  | let_bindings SEMI class_expr_lets_and_rest
    { class_of_let_bindings $1 $3 }
  | object_body { mkclass (Pcl_structure $1) }
  ) {$1};

object_body:
  mark_position_pat (
  /* Empty is by default the pattern identifier [this] */
    { mkpat (Ppat_var (mkloc "this" (mklocation $symbolstartpos $endpos))) }
  /* Whereas in OCaml, specifying nothing means "_", in Reason, you'd
     have to explicity specify "_" (any) pattern. In Reason, writing nothing
     is how you specify the "this" pattern. */
  | AS pattern SEMI
    { $2 }
  )
  lseparated_list(SEMI, class_field) SEMI?
  { Cstr.mk $1 $2 }
;

class_expr:
mark_position_cl
  ( class_simple_expr
    { $1 }
  | FUN class_fun_def
    { $2 }
  | class_simple_expr labeled_argument_list
      /**
       * This is an interesting way to "partially apply" class construction:
       *
       * let inst = new oldclass 20;
       * class newclass = oldclass withInitArg;
       * let inst = new newclass;
       */
    { mkclass(Pcl_apply($1, $2)) }
  | attribute class_expr
    { {$2 with pcl_attributes = $1 :: $2.pcl_attributes} }
  /*
    When referring to class expressions (not regular types that happen to be
    classes), you must refer to it as a class. This gives syntactic real estate
    to place type parameters which are distinguished from constructor application
    of arguments.

       class myClass 'x = (class yourClass 'x int) y z;
       inherit (class yourClass float int) initArg initArg;
       ...
       let myVal: myClass int = new myClass 10;

   */
  | CLASS as_loc(class_longident) loption(type_parameters)
    { mkclass(Pcl_constr($2, $3)) }
  | extension
    { mkclass(Pcl_extension $1) }
  ) {$1};

class_simple_expr:
mark_position_cl
  ( as_loc(class_longident)
    { mkclass(Pcl_constr($1, [])) }
  | LBRACE class_expr_lets_and_rest RBRACE
    { $2 }
  | as_loc(LBRACE) class_expr_lets_and_rest as_loc(error)
    { unclosed_cl (with_txt $1 "{") (with_txt $3 "}") }
  | LPAREN class_expr COLON class_constructor_type RPAREN
    { mkclass(Pcl_constraint($2, $4)) }
  | as_loc(LPAREN) class_expr COLON class_constructor_type as_loc(error)
    { unclosed_cl (with_txt $1 "(") (with_txt $5 ")") }
  | LPAREN class_expr RPAREN
    { $2 }
  | as_loc(LPAREN) class_expr as_loc(error)
    { unclosed_cl (with_txt $1 "(") (with_txt $3 ")") }
  ) {$1};

class_field:
mark_position_cf
  ( item_attributes INHERIT override_flag class_expr preceded(AS,LIDENT)?
    { mkcf_attrs (Pcf_inherit ($3, $4, $5)) $1 }
  | item_attributes VAL value
    { mkcf_attrs (Pcf_val $3) $1 }
  | item_attributes PUB method_
    { let (a, b) = $3 in mkcf_attrs (Pcf_method (a, Public, b)) $1 }
  | item_attributes PRI method_
    { let (a, b) = $3 in mkcf_attrs (Pcf_method (a, Private, b)) $1 }
  | item_attributes CONSTRAINT constrain_field
    { mkcf_attrs (Pcf_constraint $3) $1 }
  | item_attributes INITIALIZER EQUALGREATER expr
    { mkcf_attrs (Pcf_initializer $4) $1 }
  | item_attributes item_extension
    { mkcf_attrs (Pcf_extension $2) $1 }
  | floating_attribute
    { mkcf (Pcf_attribute $1) }
  ) {$1};

value:
/* TODO: factorize these rules (also with method): */
  | override_flag MUTABLE VIRTUAL as_loc(label) COLON only_core_type(core_type)
    { if $1 = Override
      then not_expecting $symbolstartpos $endpos "members marked virtual may not also be marked overridden"
      else ($4, Mutable, Cfk_virtual $6)
    }
  | override_flag MUTABLE VIRTUAL label COLON only_core_type(core_type) EQUAL
    { not_expecting $startpos($7) $endpos($7) "not expecting equal - cannot specify value for virtual val" }
  | VIRTUAL mutable_flag as_loc(label) COLON only_core_type(core_type)
    { ($3, $2, Cfk_virtual $5) }
  | VIRTUAL mutable_flag label COLON core_type EQUAL
    { not_expecting $startpos($6) $endpos($6) "not expecting equal - cannot specify value for virtual val" }
  | override_flag mutable_flag as_loc(label) EQUAL expr
    { ($3, $2, Cfk_concrete ($1, $5)) }
  | override_flag mutable_flag as_loc(label) type_constraint EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      let e = ghexp_constraint loc $6 $4 in
      ($3, $2, Cfk_concrete ($1, e)) }
;

method_:
/* TODO: factorize those rules... */
  | override_flag VIRTUAL as_loc(label) COLON poly_type
      { if $1 = Override then syntax_error ();
        $3, Cfk_virtual $5 }
  | override_flag as_loc(label) curried_binding
      { $2,
        Cfk_concrete ($1, let loc = mklocation $symbolstartpos $endpos in mkexp ~ghost:true ~loc (Pexp_poly ($3, None))) }
  | override_flag as_loc(label) COLON poly_type EQUAL expr
      /* Without locally abstract types, you'll see a Ptyp_poly in the Pexp_poly */
      { $2,
        Cfk_concrete ($1, let loc = mklocation $symbolstartpos $endpos in mkexp ~ghost:true ~loc (Pexp_poly($6, Some $4))) }
  | override_flag as_loc(label) EQUAL expr
      { $2,
        Cfk_concrete ($1, let loc = mklocation $symbolstartpos $endpos in mkexp ~ghost:true ~loc (Pexp_poly($4, None))) }
  | override_flag as_loc(label) COLON TYPE LIDENT+ DOT only_core_type(core_type) EQUAL expr
    /* WITH locally abstract types, you'll see a Ptyp_poly in the Pexp_poly,
       but the expression will be a Pexp_newtype and type vars will be
       "varified". */
    {
      (* For non, methods we'd create a pattern binding:
         ((Ppat_constraint(mkpatvar ..., Ptyp_poly (typeVars, poly_type_varified))),
          exp_with_newtypes_constrained_by_non_varified)

         For methods, we create:
         Pexp_poly (Pexp_constraint (methodFunWithNewtypes, non_varified), Some (Ptyp_poly newTypes varified))
       *)
      let (exp_non_varified, poly_vars) = wrap_type_annotation $5 $7 $9 in
      let exp = Pexp_poly(exp_non_varified, Some poly_vars) in
      let loc = mklocation $symbolstartpos $endpos in
      ($2, Cfk_concrete ($1, mkexp ~ghost:true ~loc exp))
    }
;

/* A parsing of class_constructor_type is the type of the class's constructor - and if the
   constructor takes no arguments, then simply the class_instance_type of what is
   returned.

   The class_instance_type is type of the thing returned from a class constructor
   which can either be:
    - The identifier of another class.
    - Type construction of a class type. [v1, v2] classIdentifier
    - The type definition of the object returned (which is much like the type
      of anonymous object but with different syntax, and ability to specify
      inheritance):
      - The class signature body may contain the type of methods (just like object types do)
      - Inheritance of another class_instance_type.
      - The instance variable "val"s.
      - Constraints (same syntax that is used at class *definition* time)


     When In Modules:
     ================
     There are two primary language "class" constructs that are included in modules:
     Class Definitions, and Class Type Definitions.

     A Class Definition:
     -------------------
     Brings three things into the environment. For a class defined with name
     [myClass], the environment would then contain the following:
       1. A type called [myClass] which represents instances that have the same
       structure as what is returned from the constructor - and do not have
       any *more* members than what is returned from the constructor.
       2. A type called [#myClass] which loosely speaking represents objects
       that have the same structure as what is returned from the constructor,
       but can also have *additional* fields as well. This ability to have
       additional fields is accomplished via the same row-polymorphism of
       objects without classes. Any reference to [#myClass] implicitly includes
       a type variable, whose name we may not mention.
       3. The class constructor for [myClass] - a function that "new" can be
       invoked on. If the constructor is a function that takes arguments, then
       the type of the constructor is the type of that function (including
       the type of object returned). If it doesn't take arguments, then the
       type of the constructor is the type of the object returned. You can
       explicitly annotate/constrain the type of the constructor, and can
       annotate them in module signatures as well.

     The type constraint on a constructor has one syntactic nuance. The final
     item separated by arrow must be prefixed with "new". This is merely to
     resolve parsing conflicts. This can be fixed by making a parse rule for
     types that parses *either* non-arowed class_instance_type or core_types,
     but that means deferring the interpretation of patterns like "lowercase
     identifier" and "type application", until it is known if that item was the
     *final* arrow segment.

     A Class Type Definition:
     -----------------------
     Brings two things into the environment. For a class type definition with
     name [myClass], the environment would then contain the following:
       1. A type called [myClass] which describes any object having *exactly*
       the fields listed in the definition.
       2. A type called [#myClass] which loosely speaking represents objects
       that have the same structure as the definition, but can also have
       *additional* fields as well.


     When In Signatures:
     ================

     A Class Definition (in signature becomes a class "description"):
     -------------------
     A Class Definition merely includes the type of that classes constructor.
     [class myClass: args => new classType;] This merely admits that the module
     includes a class whose constructor has that type.

     A Class Type Definition is specified exactly as it is in a module (of
     course, it may be abstract).


     A Class Type Definition:
     -----------------------
     A Class Type Definition is specified exactly as it is in a module (of
     course, it may be abstract).
     Here are the parsing rules for everything discussed above:



    Overview of Parsing Rules:
    ==========================
     (Note: I have renamed class_type from the upstream compiler to
     class_constructor_type and class_signature to class_instance_type for
     clarity)

     constructor_type:
        | class_instance_type
        | constructorFunction => typeWithArrows => class_instance_type

     Modules
       Class Definitions
       class_declarations
         | class LIDENT = class_expr
         | class LIDENT = (class_expr : constructor_type)
         "Pstr_class"

       Class Type Definition
       class_type_declarations
         CLASS TYPE ident = class_instance_type
         "Pstr_class_type"

     Signatures
       Class Descriptions (describes Class Definitions in Module)
       class_descriptions
         CLASS ident : constructor_type
         "Psig_class"

       Class Type Declarations (subset of Class Type Definitions in Module)
       class_type_declarations
         CLASS TYPE ident = class_instance_type
        "Psig_class_type"

 */
class_constructor_type:
mark_position_cty
  ( NEW class_instance_type
    { $2 }
  | LIDENTCOLONCOLON boption(QUESTION)
      only_core_type(non_arrowed_core_type) EQUALGREATER class_constructor_type
    { let label = if $2 then Optional $1 else Labelled $1 in
      mkcty (Pcty_arrow(label, $3, $5))
    }
  | only_core_type(non_arrowed_core_type) EQUALGREATER class_constructor_type
    { mkcty(Pcty_arrow(Nolabel, $1, $3)) }
  ) {$1};

non_arrowed_class_constructor_type:
  | class_instance_type                  { $1 }
  | LPAREN class_constructor_type RPAREN { $2 }
;

class_instance_type:
mark_position_cty
  ( as_loc(clty_longident)
      { mkcty (Pcty_constr ($1, [])) }
  | as_loc(clty_longident) only_core_type(non_arrowed_simple_core_type)+
      { mkcty (Pcty_constr ($1, $2)) }
  | LBRACE class_sig_body RBRACE
      { mkcty (Pcty_signature $2) }
  | as_loc(LBRACE) class_sig_body as_loc(error)
      { unclosed_cty (with_txt $1 "{") (with_txt $3 "}") }
  | attribute class_instance_type
      /* Note that this will compound attributes - so they will become
         attached to whatever */
      { {$2 with pcty_attributes = $1 :: $2.pcty_attributes} }
  | extension
      { mkcty (Pcty_extension $1) }
  ) {$1};

class_sig_body:
  class_self_type lseparated_list(SEMI, class_sig_field) SEMI?
  { Csig.mk $1 $2 }
;

class_self_type:
  | AS only_core_type(core_type) SEMI { $2 }
  | /* empty */ { Typ.mk ~loc:(mklocation $symbolstartpos $endpos) Ptyp_any }
;

class_sig_field:
mark_position_ctf
  ( item_attributes INHERIT class_instance_type
    { mkctf_attrs (Pctf_inherit $3) $1 }
  | item_attributes VAL value_type
    { mkctf_attrs (Pctf_val $3) $1 }
  | item_attributes PRI virtual_flag label COLON poly_type
    { mkctf_attrs (Pctf_method ($4, Private, $3, $6)) $1 }
  | item_attributes PUB virtual_flag label COLON poly_type
    { mkctf_attrs (Pctf_method ($4, Public, $3, $6)) $1 }
  | item_attributes CONSTRAINT constrain_field
    { mkctf_attrs (Pctf_constraint $3) $1 }
  | item_attributes item_extension
    { mkctf_attrs (Pctf_extension $2) $1 }
  | floating_attribute
    { mkctf(Pctf_attribute $1) }
  ) {$1};

value_type:
  mutable_or_virtual_flags label COLON only_core_type(core_type)
  { let (mut, virt) = $1 in ($2, mut, virt, $4) }
;

constrain:
  only_core_type(core_type) EQUAL only_core_type(core_type)
  { ($1, $3, mklocation $symbolstartpos $endpos) }
;

constrain_field:
  only_core_type(core_type) EQUAL only_core_type(core_type)
  { ($1, $3) }
;

class_descriptions:
  item_attributes CLASS class_description_details and_class_description*
  { let (ident, binding, virt, params) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    (Ci.mk ident binding ~virt ~params ~attrs:$1 ~loc :: $4)
  }
;

and_class_description:
  item_attributes AND class_description_details
  { let (ident, binding, virt, params) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    Ci.mk ident binding ~virt ~params ~attrs:$1 ~loc
  }
;

%inline class_type_parameters:
  parenthesized(lseparated_nonempty_list(COMMA, type_parameter))
  { $1 }
;

class_description_details:
  virtual_flag as_loc(LIDENT) loption(class_type_parameters) COLON class_constructor_type
  { ($2, $5, $1, $3) }
;

class_type_declarations:
  item_attributes CLASS TYPE class_type_declaration_details
  and_class_type_declaration*
  { let (ident, instance_type, virt, params) = $4 in
    let loc = mklocation $symbolstartpos $endpos in
    (Ci.mk ident instance_type ~virt ~params ~attrs:$1 ~loc :: $5)
  }
;

and_class_type_declaration:
  item_attributes AND class_type_declaration_details
  { let (ident, instance_type, virt, params) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    Ci.mk ident instance_type ~virt ~params ~attrs:$1 ~loc
  }
;

class_type_declaration_details:
  virtual_flag as_loc(LIDENT) loption(class_type_parameters) EQUAL class_instance_type
  { ($2, $5, $1, $3) }
;

/* Core expressions */

/* Note: If we will parse this as Pexp_apply, and it will
 * not be printed with the braces, except in a couple of cases such as if/while
 * loops.
 *
 *  let add a b = {
 *    a + b;
 *  };
 *  TODO: Rename to [semi_delimited_block_sequence]
 *
 * Since semi_terminated_seq_expr doesn't require a final SEMI, then without
 * a final SEMI, a braced sequence with a single identifier is
 * indistinguishable from a punned record.
 *
 *   let myThing = {x};
 *
 * Is it {x:x} the record or x the identifier? We simply decided to break
 * the tie and say that it should be parsed as a single identifier because
 * single field records are incredibly rare. Apart from this one
 * disadvantage, there's no disadvantage to not requiring the final brace.
 *
 * For each valid sequence item, we must list three forms:
 *
 *   [item_extension_sugar] [nonempty_item_attributes] ITEM
 *   [nonempty_item_attributes] ITEM
 *   ITEM
 */
semi_terminated_seq_expr:
mark_position_exp
  ( item_extension_sugar semi_terminated_seq_expr_row
    { extension_expression $1 $2 }
  | semi_terminated_seq_expr_row
    { $1 }
  /* Let bindings already have their potential item_extension_sugar. */
  | let_bindings SEMI semi_terminated_seq_expr
    { expr_of_let_bindings $1 $3 }
  | let_bindings SEMI?
    { let loc = mklocation $symbolstartpos $endpos in
      expr_of_let_bindings $1 @@ ghunit ~loc ()
    }
  ) {$1};

semi_terminated_seq_expr_row:
mark_position_exp
  /**
   * Expression SEMI
   */
  ( item_attributes expr SEMI?  {
      (* Final item in the sequence *)
      {$2 with pexp_attributes = $1 @ $2.pexp_attributes}
    }
  | item_attributes opt_LET_MODULE as_loc(UIDENT) module_binding_body SEMI semi_terminated_seq_expr
    { mkexp ~attrs:$1 (Pexp_letmodule($3, $4, $6)) }
  | item_attributes LET? OPEN override_flag as_loc(mod_longident) SEMI semi_terminated_seq_expr
    { mkexp ~attrs:$1 (Pexp_open($4, $5, $7)) }
  | item_attributes expr SEMI semi_terminated_seq_expr
    { mkexp ~attrs:$1 (Pexp_sequence($2, $4)) }
  ) {$1};

/*


A:
let named                 a::a      b::b             => a + b;
type named =              a::int => b::int => int;
B:
let namedAlias            a::aa     b::bb  => aa + bb;
let namedAlias            a::aa     b::bb  => aa + bb;
type namedAlias =         a::int => b::int => int;
C:
let namedAnnot            a::(a:int) b::(b:int)   => 20;
D:
let namedAliasAnnot       a::(aa:int) b::(bb:int) => 20;
E:
let myOptional            a::a=?    b::b=?      ()  => 10;
type named =              a::int? => b::int? => unit => int;
F:
let optionalAlias         a::aa=?   b::bb=?   ()  => 10;
G:
let optionalAnnot         a::(a:int)=? b::(b:int)=? ()  => 10;
H:
let optionalAliasAnnot    a::(aa:int)=? b::(bb:int)=? ()  => 10;
I: :
let defOptional           a::a=10    b::b=10  ()  => 10;
type named =              a::int?  => b::int? => unit => int;
J:
let defOptionalAlias      a::aa=10      b::bb=10  ()  => 10;
K:
let defOptionalAnnot      a::(a:int)=10 b::(b:int)=10 ()  => 10;
L:
let defOptionalAliasAnnot a::(aa:int)=10 b::(bb:int)=10 ()  => 10;

M: Invoking them - Punned :
let resNotAnnotated = named a::a b::b;
N::
let resAnnotated    = (named a::a b::b :int);
O: Invoking them :
let resNotAnnotated = named a::a b::b;
P: Invoking them :
let resAnnotated    = (named a::a b::b :int);

Q: Here's why "punning" doesn't work!  :
 Is b:: punned with a final non-named arg, or is b:: supplied b as one named arg? :
let b = 20;
let resAnnotated    = (named a::a b:: b);

R: Proof that there are no ambiguities with return values being annotated :
let resAnnotated    = (named a::a b :ty);


S: Explicitly passed optionals are a nice way to say "use the default value":
let explictlyPassed =          myOptional a::?None b::?None;
T: Annotating the return value of the entire function call :
let explictlyPassedAnnotated = (myOptional a::?None b::?None :int);
U: Explicitly passing optional with identifier expression :
let a = None;
let explictlyPassed =           myOptional a::?a b::?None;
let explictlyPassedAnnotated = (myOptional a::?a b::?None :int);







*/

labeled_simple_pattern:
  | COLONCOLONLIDENT
    { let loc = mklocation $startpos($1) $endpos($1) in
        (Labelled $1, None, mkpat(Ppat_var (mkloc $1 loc)) ~loc)
    }
  | COLONCOLONLIDENT OPTIONAL_NO_DEFAULT
    { let loc = mklocation $symbolstartpos $endpos in
        (Optional $1, None, mkpat(Ppat_var (mkloc $1 loc)) ~loc)
    }
  | COLONCOLONLIDENT EQUAL simple_expr
    { let loc = mklocation $symbolstartpos $endpos in
      (Optional $1, Some $3, mkpat(Ppat_var (mkloc $1 loc)) ~loc)
    }
   /* Case A, B, C, D */
  | LIDENTCOLONCOLON simple_pattern
    { (Labelled $1, None, $2) }
   /* Case E, F, G, H */
  | LIDENTCOLONCOLON simple_pattern OPTIONAL_NO_DEFAULT
    { (Optional $1, None, $2) }
   /* Case I, J, K, L */
  | LIDENTCOLONCOLON simple_pattern EQUAL simple_expr
    { (Optional $1, Some $4, $2) }
  | simple_pattern
    { (Nolabel, None, $1) }
;
// TODO: properly fix JSX labelled/optional stuff
jsx_arguments:
  /* empty */ { [] }
  | LIDENT OPTIONAL_NO_DEFAULT simple_expr jsx_arguments
    { (* a=?b *)
      [(Optional $1, $3)] @ $4
    }
  | LIDENT EQUAL simple_expr jsx_arguments
    { (* a=b *)
      [(Labelled $1, $3)] @ $4
    }
  | LIDENT jsx_arguments
    { (* a (punning) *)
      let loc_lident = mklocation $startpos($1) $endpos($1) in
      [(Labelled $1, mkexp (Pexp_ident {txt = Lident $1; loc = loc_lident}) ~loc:loc_lident)] @ $2
    }
;

jsx_start_tag_and_args:
  LESSIDENT jsx_arguments
  { let name = Longident.parse $1 in
    (jsx_component name $2, name)
  }
;

jsx_start_tag_and_args_without_leading_less:
  mod_longident jsx_arguments
  { (jsx_component $1 $2, $1) }
;

simple_expr_no_call: simple_expr %prec below_LPAREN { $1 };

jsx:
  | LESSGREATER simple_expr_no_call* LESSSLASHGREATER
    { let loc = mklocation $symbolstartpos $endpos in
      let body = mktailexp_extension loc $2 None in
      makeFrag loc body
    }
  | jsx_start_tag_and_args SLASHGREATER
    { let (component, _) = $1 in
      let loc = mklocation $symbolstartpos $endpos in
      component [
        (Labelled "children", mktailexp_extension loc [] None);
        (Nolabel, mkexp_constructor_unit loc loc)
      ] loc
    }
  | jsx_start_tag_and_args GREATER simple_expr_no_call* LESSSLASHIDENTGREATER
    { let (component, start) = $1 in
      let loc = mklocation $symbolstartpos $endpos in
      (* TODO: Make this tag check simply a warning *)
      let endName = Longident.parse $4 in
      let _ = ensureTagsAreEqual start endName loc in
      let siblings = if List.length $3 > 0 then $3 else [] in
      component [
        (Labelled "children", mktailexp_extension loc siblings None);
        (Nolabel, mkexp_constructor_unit loc loc)
      ] loc
    }
;

jsx_without_leading_less:
  | GREATER simple_expr_no_call* LESSSLASHGREATER {
    let loc = mklocation $symbolstartpos $endpos in
    let body = mktailexp_extension loc $2 None in
    makeFrag loc body
  }
  | jsx_start_tag_and_args_without_leading_less SLASHGREATER {
    let (component, _) = $1 in
    let loc = mklocation $symbolstartpos $endpos in
    component [
      (Labelled "children", mktailexp_extension loc [] None);
      (Nolabel, mkexp_constructor_unit loc loc)
    ] loc
  }
  | jsx_start_tag_and_args_without_leading_less GREATER simple_expr_no_call* LESSSLASHIDENTGREATER {
    let (component, start) = $1 in
    let loc = mklocation $symbolstartpos $endpos in
    (* TODO: Make this tag check simply a warning *)
    let endName = Longident.parse $4 in
    let _ = ensureTagsAreEqual start endName loc in
    let siblings = if List.length $3 > 0 then $3 else [] in
    component [
      (Labelled "children", mktailexp_extension loc siblings None);
      (Nolabel, mkexp_constructor_unit loc loc)
    ] loc
  }
;

/*
 * Much like how patterns are partitioned into pattern/simple_pattern,
 * expressions are divided into expr/simple_expr.
 * expr: contains function application, but simple_expr doesn't (unless it's
 * wrapped in parens).
 */
expr:
mark_position_exp
  /** One reason for below_SHARP (in [less_aggressive_simple_expression] and
    * others) is so that Module.Long.ident does not cause `Module` to be parsed
    * as a simple expression. This precedence causes the parser to "wait" and
    * build up the full long identifier before reducing it to a simple_expr (in
    * the case of constr_longident and potentially others).  This precedence quirk
    * has been consolidated into less_aggressive_simple_expression
   */
  ( less_aggressive_simple_expression
      { $1 }
  | FUN labeled_simple_pattern fun_def
    { let (l,o,p) = $2 in
      mkexp (Pexp_fun(l, o, p, $3))
    }
  | ES6_FUN parenthesized(separated_list(COMMA, pattern_optional_constraint))
      EQUALGREATER simple_expr
    { match $2 with
      | [] ->
        let loc = mklocation $startpos($2) $endpos($2) in
        mkexp (Pexp_fun (Nolabel, None, mkpat_constructor_unit loc loc, $4))
      | pats -> List.fold_left (fun body pat -> mkexp_fun pat body) $4 pats
    }
  | FUN LPAREN TYPE LIDENT+ RPAREN fun_def
    { pexp_newtypes $4 $6 }
  /* List style rules like this often need a special precendence
     such as below_BAR in order to let the entire list "build up"
   */
  | FUN match_cases(expr) %prec below_BAR
    { mkexp (Pexp_function $2) }
  | SWITCH simple_expr LBRACE match_cases(semi_terminated_seq_expr) RBRACE
    { mkexp (Pexp_match ($2, $4)) }
  | TRY simple_expr LBRACE match_cases(semi_terminated_seq_expr) RBRACE
    { mkexp (Pexp_try ($2, $4)) }
  | TRY simple_expr WITH error
    { syntax_error_exp (mklocation $startpos($4) $endpos($4)) "Invalid try with"}
  | name_tag simple_expr
    { mkexp(Pexp_variant($1, Some $2)) }
  | IF simple_expr_no_call simple_expr ioption(preceded(ELSE,expr))
    { mkexp(Pexp_ifthenelse($2, $3, $4)) }
  | WHILE simple_expr_no_call simple_expr
    { mkexp (Pexp_while($2, $3)) }
  | FOR pattern IN simple_expr direction_flag simple_expr_no_call simple_expr
    { mkexp(Pexp_for($2, $4, $6, $5, $7)) }
  | LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
    { let loc_colon = mklocation $startpos($2) $endpos($2) in
      let loc = mklocation $symbolstartpos $endpos in
      mkexp_cons loc_colon (mkexp ~ghost:true ~loc (Pexp_tuple[$5;$7])) loc
    }
  | expr infix_operator expr
    { mkinfix $1 $2 $3 }
  | subtractive expr %prec prec_unary_minus
    { mkuminus $1 $2 }
  | additive expr %prec prec_unary_plus
    { mkuplus $1 $2 }
  | simple_expr DOT as_loc(label_longident) EQUAL expr
    { mkexp(Pexp_setfield($1, $3, $5)) }
  | simple_expr DOT LPAREN expr RPAREN EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(array_function ~loc "Array" "set")),
                       [Nolabel,$1; Nolabel,$4; Nolabel,$7]))
    }
  | simple_expr DOT LBRACKET expr RBRACKET EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(array_function ~loc "String" "set")),
                       [Nolabel,$1; Nolabel,$4; Nolabel,$7]))
    }
  | simple_expr DOT LBRACE expr RBRACE EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      bigarray_set ~loc $1 $4 $7
    }
  | as_loc(label) EQUAL expr
    { mkexp(Pexp_setinstvar($1, $3)) }
  | ASSERT simple_expr
    { mkexp (Pexp_assert $2) }
  | LAZY simple_expr
    { mkexp (Pexp_lazy $2) }
  /*
   * Ternary is just a shortcut for:
   *
   *     switch expression { | true => expr1 | false => expr2 }
   *
   * The COLON token priority is below QUESTION so that the following parses:
   *
   *    x ? y :
   *    z ? q : r
   *
   *  As
   *
   *    x ? y :
   *    (z ? q : r)
   *
   *  Instead of:
   *
   *    (x ? y :
   *    z) ? q : r
   *
   * When a question mark is seen, *this* parsing rule has lower priority so
   * that we, instead, shift the qusetion mark so that the *latter* ternary is
   * recognized first on the top of the stack. (z ? q : r).
   */
  | expr QUESTION expr COLON expr
    { (* Should use ghost expressions, but not sure how that would work with source maps *)
      (* So ? will become true and : becomes false for now*)
      let loc_question = mklocation $startpos($2) $endpos($2) in
      let loc_colon = mklocation $startpos($4) $endpos($4) in
      let fauxTruePat =
        Pat.mk ~loc:loc_question (Ppat_construct({txt = Lident "true"; loc = loc_question}, None)) in
      let fauxFalsePat =
        Pat.mk ~loc:loc_colon (Ppat_construct({txt = Lident "false"; loc = loc_colon}, None)) in
      let fauxMatchCaseTrue = Exp.case fauxTruePat $3 in
      let fauxMatchCaseFalse = Exp.case fauxFalsePat $5 in
      mkexp (Pexp_match ($1, [fauxMatchCaseTrue; fauxMatchCaseFalse]))
    }
  | attribute expr %prec attribute_precedence
    { {$2 with pexp_attributes = $1 :: $2.pexp_attributes} }
  ) {$1};

simple_expr:
mark_position_exp
  ( as_loc(val_longident) { mkexp (Pexp_ident $1) }
  | constant              { mkexp (Pexp_constant $1) }
  | jsx                   { $1 }
  /* Not sure why this couldn't have just been below_SHARP (Answer: Being
   * explicit about needing to wait for "as") */
  | simple_expr labeled_argument_list
      { mkexp(Pexp_apply($1, $2)) }
  | as_loc(constr_longident) %prec prec_constant_constructor
    { mkexp (Pexp_construct ($1, None)) }
  | as_loc(constr_longident) non_labeled_argument_list
    { if List.mem (string_of_longident $1.txt)
         built_in_explicit_arity_constructors then
        (* unboxing the inner tupple *)
        match $2 with
          | [inner] -> mkexp (Pexp_construct($1, Some inner))
          | _ -> assert false
      else
        let args = mkexp (Pexp_tuple($2)) in
        mkExplicitArityTupleExp (Pexp_construct($1, Some args))
    }
  | name_tag %prec prec_constant_constructor
    { mkexp (Pexp_variant ($1, None)) }
  /*
     Because [< is a special token, the <ident and <> won't be picked up as separate
     tokens, when a list begins witha JSX tag. So we special case it.
     (todo: pick totally different syntax for polymorphic variance types to avoid
     the issue alltogether.

     first token
     /\
     [<ident    args />  , remainingitems ]
     [<>                 , remainingitems ]
   */
  | LBRACKETLESS jsx_without_leading_less COMMA expr_comma_seq_extension RBRACKET
    { let entireLoc = mklocation $startpos($1) $endpos($4) in
      let (seq, ext_opt) = $4 in
      mktailexp_extension entireLoc ($2::seq) ext_opt
    }
  | LBRACKETLESS jsx_without_leading_less RBRACKET
    { let entireLoc = mklocation $startpos($1) $endpos($3) in
      mktailexp_extension entireLoc ($2::[]) None
    }
  | LPAREN expr_list RPAREN
    { may_tuple $2 }
  | as_loc(LPAREN) expr_list as_loc(error)
    { unclosed_exp (with_txt $1 "(") (with_txt $3 ")") }
  | as_loc(mod_longident) DOT LPAREN expr_list RPAREN
    { mkexp(Pexp_open(Fresh, $1, may_tuple $4)) }
  | mod_longident DOT as_loc(LPAREN) expr_list as_loc(error)
    { unclosed_exp (with_txt $3 "(") (with_txt $5 ")") }
  /**
   * A single expression can be type constrained.
   */
  | LBRACE semi_terminated_seq_expr RBRACE
    { $2 }
  | LBRACE as_loc(semi_terminated_seq_expr) error
    { syntax_error_exp $2.loc "SyntaxError in block" }
  | simple_expr DOT as_loc(label_longident)
    { mkexp(Pexp_field($1, $3)) }
  | as_loc(mod_longident) DOT LBRACE RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      let pat = mkpat (Ppat_var (mkloc "this" loc)) in
      mkexp(Pexp_open (Fresh, $1,
                       mkexp(Pexp_object(Cstr.mk pat []))))
    }
  | simple_expr DOT LPAREN expr RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(array_function ~loc "Array" "get")),
                       [Nolabel,$1; Nolabel,$4]))
    }
  | simple_expr DOT as_loc(LPAREN) expr as_loc(error)
    { unclosed_exp (with_txt $3 "(") (with_txt $5 ")") }
  | simple_expr DOT LBRACKET expr RBRACKET
    { let loc = mklocation $symbolstartpos $endpos in
      mkexp(Pexp_apply(mkexp ~ghost:true ~loc (Pexp_ident(array_function ~loc "String" "get")),
                       [Nolabel,$1; Nolabel,$4]))
    }
  | simple_expr DOT as_loc(LBRACKET) expr as_loc(error)
    { unclosed_exp (with_txt $3 "[") (with_txt $5 "]") }
  | simple_expr DOT LBRACE expr RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      bigarray_get ~loc $1 $4
    }

  /* This might not be needed anymore
  | simple_expr DOT as_loc(LBRACE) expr_comma_list as_loc(error)
      { unclosed_exp (with_txt $3 "[") (with_txt $5 "]") }
  */

  /* TODO: Let Sequence? */

  | LBRACE DOTDOTDOT expr_optional_constraint COMMA? RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      let msg = "Record construction must have at least one field explicitly set" in
      syntax_error_exp loc msg
    }
  | LBRACE record_expr RBRACE
    { let (exten, fields) = $2 in mkexp (Pexp_record(fields, exten)) }
  | as_loc(LBRACE) record_expr as_loc(error)
    { unclosed_exp (with_txt $1 "{") (with_txt $3 "}")}
  | LBRACE record_expr_with_string_keys RBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      let (exten, fields) = $2 in
      mkexp ~loc (Pexp_extension (mkloc ("bs.obj") loc,
             PStr [mkstrexp (mkexp ~loc (Pexp_record(fields, exten))) []]))
    }
  | as_loc(LBRACE) record_expr_with_string_keys as_loc(error)
    { unclosed_exp (with_txt $1 "{") (with_txt $3 "}")}
  /* Todo: Why is this not a simple_expr? */
  | LBRACE object_body RBRACE
    { mkexp (Pexp_object $2) }
  | as_loc(LBRACE) object_body as_loc(error)
    { unclosed_exp (with_txt $1 "{") (with_txt $3 "}") }
  | as_loc(mod_longident) DOT LBRACE record_expr RBRACE
    { let (exten, fields) = $4 in
      let loc = mklocation $symbolstartpos $endpos in
      let rec_exp = mkexp ~loc (Pexp_record (fields, exten)) in
      mkexp(Pexp_open(Fresh, $1, rec_exp))
    }
  | mod_longident DOT as_loc(LBRACE) record_expr as_loc(error)
    { unclosed_exp (with_txt $3 "{") (with_txt $5 "}") }
  | LBRACKETBAR expr_list BARRBRACKET
    { mkexp (Pexp_array $2) }
  | as_loc(LBRACKETBAR) expr_list as_loc(error)
    { unclosed_exp (with_txt $1 "[|") (with_txt $3 "|]") }
  | LBRACKETBAR BARRBRACKET
    { mkexp (Pexp_array []) }
  | as_loc(mod_longident) DOT LBRACKETBAR expr_list BARRBRACKET
    { let loc = mklocation $symbolstartpos $endpos in
      let rec_exp = Exp.mk ~loc ~attrs:[] (Pexp_array $4) in
      mkexp(Pexp_open(Fresh, $1, rec_exp))
    }
  | mod_longident DOT as_loc(LBRACKETBAR) expr_list as_loc(error)
    { unclosed_exp (with_txt $3 "[|") (with_txt $5 "|]") }
  | LBRACKET expr_comma_seq_extension RBRACKET
    { let seq, ext_opt = $2 in
      let loc = mklocation $startpos($2) $endpos($2) in
      make_real_exp (mktailexp_extension loc seq ext_opt)
    }
  | as_loc(mod_longident) DOT LBRACKET expr_comma_seq_extension RBRACKET
    { let seq, ext_opt = $4 in
      let loc = mklocation $startpos($4) $endpos($4) in
      let list_exp = make_real_exp (mktailexp_extension loc seq ext_opt) in
      let list_exp = { list_exp with pexp_loc = loc } in
      mkexp (Pexp_open (Fresh, $1, list_exp))
    }
  | PREFIXOP simple_expr %prec below_DOT_AND_SHARP
    { mkexp(Pexp_apply(mkoperator $1 1, [Nolabel, $2])) }
  /**
   * Must be below_DOT_AND_SHARP so that the parser waits for several dots for
   * nested record access that the bang should apply to.
   *
   * !x.y.z should be parsed as !(((x).y).z)
   */
  | as_loc(BANG) simple_expr %prec below_DOT_AND_SHARP
    { mkexp (Pexp_apply(mkoperator "!" 1, [Nolabel,$2])) }
  | NEW as_loc(class_longident)
    { mkexp (Pexp_new $2) }
  | LBRACELESS field_expr_list COMMA? GREATERRBRACE
    { mkexp (Pexp_override $2) }
  | as_loc(LBRACELESS) field_expr_list COMMA? as_loc(error)
    { unclosed_exp (with_txt $1 "{<") (with_txt $4 ">}" ) }
  | LBRACELESS GREATERRBRACE
    { mkexp (Pexp_override [])}
  | as_loc(mod_longident) DOT LBRACELESS field_expr_list COMMA? GREATERRBRACE
    { let loc = mklocation $symbolstartpos $endpos in
      let exp = Exp.mk ~loc ~attrs:[] (Pexp_override $4) in
      mkexp (Pexp_open(Fresh, $1, exp))
    }
  | mod_longident DOT as_loc(LBRACELESS) field_expr_list COMMA? as_loc(error)
    { unclosed_exp (with_txt $3 "{<") (with_txt $6 ">}") }
  | simple_expr SHARP label
    { mkexp (Pexp_send($1, $3)) }
  | simple_expr as_loc(SHARPOP) simple_expr
    { mkinfixop $1 (mkoperator_loc $2.txt $2.loc) $3 }
  | LPAREN MODULE module_expr RPAREN
    { mkexp (Pexp_pack $3) }
  | LPAREN MODULE module_expr COLON package_type RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkexp (Pexp_constraint (mkexp ~ghost:true ~loc (Pexp_pack $3),
                              mktyp ~ghost:true ~loc (Ptyp_package $5)))
    }
  | as_loc(LPAREN) MODULE module_expr COLON as_loc(error)
    { unclosed_exp (with_txt $1 "(") (with_txt $5 ")") }
  | as_loc(mod_longident) DOT LPAREN MODULE module_expr COLON package_type RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkexp (Pexp_open(Fresh, $1,
        mkexp ~loc (Pexp_constraint (mkexp ~ghost:true ~loc (Pexp_pack $5),
                                     mktyp ~ghost:true ~loc (Ptyp_package $7)))))
    }
  | mod_longident DOT as_loc(LPAREN) MODULE module_expr COLON as_loc(error)
    { unclosed_exp (with_txt $3 "(") (with_txt $7 ")")}
  | extension
    { mkexp (Pexp_extension $1) }
  ) {$1};


/**
 * Nice reusable version of simple_expr that waits to be reduced until enough
 * of the input has been observed, in order to consider Module.X.Y a single
 * simple_expr. It's terribly nuanced that this would make a difference. (No
 * longer necessary)?
 */
less_aggressive_simple_expression:
  simple_expr {$1}
;

non_labeled_argument_list:
  | LPAREN RPAREN
    { let loc = mklocation $startpos $endpos in
      [mkexp_constructor_unit loc loc] }
  | LPAREN separated_nonempty_list(COMMA, simple_expr) RPAREN
    { $2 }
;

labeled_argument_list:
  | LPAREN RPAREN
    { let loc = mklocation $startpos $endpos in
      [(Nolabel, mkexp_constructor_unit loc loc)] }
  | LPAREN separated_nonempty_list(COMMA, labeled_simple_expr) RPAREN
    { $2 }
;

labeled_simple_expr:
  | simple_expr { (Nolabel, $1) }
  | label_expr  { $1 }
;

label_expr:
  | COLONCOLONLIDENT
    { let loc = mklocation $symbolstartpos $endpos in
      (Labelled $1, mkexp (Pexp_ident(mkloc (Lident $1) loc)) ~loc)
    }
  | LIDENTCOLONCOLON less_aggressive_simple_expression
    { (Labelled $1, $2) }
  /* Expliclitly provided default optional:
   * let res = someFunc optionalArg:?None;
   */
  | COLONCOLON QUESTION val_longident
    { let loc = mklocation $symbolstartpos $endpos in
      (Optional (String.concat "" (Longident.flatten $3)),
       mkexp (Pexp_ident(mkloc $3 loc)) ~loc)
    }
  | LIDENTCOLONCOLON QUESTION less_aggressive_simple_expression
    { (Optional $1, $3) }
;

%inline and_let_binding:
  /* AND bindings don't accept a preceeding extension ID, but do accept
   * preceeding item_attribute*. These preceeding item_attribute* will cause an
   * error if this is an *expression * let binding. Otherwise, they become
   * item_attribute* on the structure item for the "and" binding.
   */
  item_attributes AND let_binding_body
  { mklb $3 $1 (mklocation $symbolstartpos $endpos) }
;

let_bindings: let_binding and_let_binding* { addlbs $1 $2 };

let_binding:
  /* Form with item extension sugar */
  ioption(item_extension_sugar) item_attributes LET rec_flag let_binding_body
  { let (ext_attrs, ext_id) = match $1 with
      | Some (ext_attrs, ext_id) -> (ext_attrs, Some ext_id)
      | None -> ([], None)
    in
    let loc = mklocation $symbolstartpos $endpos in
    mklbs (ext_attrs, ext_id) $4 (mklb $5 $2 loc) loc
  }
;

let_binding_body:
  | with_patvar(val_ident) type_constraint EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      ($1, ghexp_constraint loc $4 $2) }
  | with_patvar(val_ident) curried_binding_return_typed
    { ($1, $2) }
  | with_patvar(val_ident) COLON preceded(QUOTE,ident)+ DOT only_core_type(core_type)
      EQUAL mark_position_exp(expr)
    { let typ = mktyp ~ghost:true (Ptyp_poly($3, $5)) in
      let loc = mklocation $symbolstartpos $endpos in
      (mkpat ~ghost:true ~loc (Ppat_constraint($1, typ)), $7)
    }
  | with_patvar(val_ident) COLON TYPE LIDENT+ DOT only_core_type(core_type)
      EQUAL mark_position_exp(expr)
  /* Because core_type will appear to contain "type constructors" since the
   * type variables listed in LIDENT+ don't have leading single quotes, we
   * have to call [varify_constructors] (which is what [wrap_type_annotation]
   * does among other things) to turn those "type constructors" that correspond
   * to LIDENT+ into regular type variables. I don't think this should be
   * done in the parser!
   */
  /* In general, this is a very strange transformation that occurs in the
   * standard OCaml parser, but producing the same strange transformation, and
   * being able to recover original code from the result has the benefit that
   * this syntax will integrate seamlessly with ASTs produced by the standard
   * OCaml parser.
   *
   *  let s : type a . core_type = body
   *
   *  LET_BINDING:
   *  Ppat_constraint(Ppat_var s, Ptyp_poly(newtypes, varified_core_type))
   *  Pexp_newtype..(a, Pexp_constraint(body, NOT_varified_core_type))
   *
   *  When [a] is in the first arg to [PTyp_poly] the second arg [core_type]
   *  expects to see [a] and ['a] is not allowed anywhere.
   *  When pretty printing, we must reverse this entire process!
   *
   *  All of this is consistent with the Manual which states:
   *
   *    let rec f : type t1 t2. t1 * t2 list -> t1 = ...
   *
   *  is automatically expanded into
   *
   *    let rec f : 't1 't2. 't1 * 't2 list -> 't1 =
   *      fun (type t1) (type t2) -> (... : t1 * t2 list -> t1)
   *
   * So therefore we end up generating the following two forms of parse trees
   * for the two primary forms of explicitly polymorphic type annotations:
   *
   *
   *           /-----------Ppat_constraint--------\ /-PExp_constraint--\
   *       let x: locallyAbstractTypes . typeVars       =      exp
   *
   *     or
   *           /-------------Ppat_constraint-----------\     /----PExp_constraint------\
   *       let x: type abstractTypes . varified_core_type = (exp : core_type_non_varified)
   *       where carified_core_type must equal (varify_constructors core_type_non_varified)
   *
   *     And in the later case
   *
   */
   { let exp, poly = wrap_type_annotation $4 $6 $8 in
     let loc = mklocation $symbolstartpos $endpos in
     (mkpat ~ghost:true ~loc (Ppat_constraint($1, poly)), exp)
   }

  /* The combination of the following two rules encompass every type of
   * pattern *except* val_identifier. The fact that we want handle the
   * val_ident separately as a separate rule in let_binding_body alone justifies the
   * need for the strange structuring of
   * [pattern]/[simple_pattern_not_ident]/[simple_pattern] - it allows us
   * to isolate [val_ident] in the special case of [let_binding_body].
   *
   * TODO:
   * Unfortunately, it means we cannot do: let (myCurriedFunc: int -> int) a -> a;
   */
  | pattern EQUAL expr
    { ($1, $3) }
  | simple_pattern_not_ident COLON only_core_type(core_type) EQUAL expr
    { let loc = mklocation $symbolstartpos $endpos in
      (mkpat ~loc (Ppat_constraint($1, $3)), $5)
    }
;

/*
 * TODO:
 * In OCaml, the following function binding would be parsed by the function
 * parsers but would return a non function. Coincidentally, everything just worked.
 *   let x: int = 10;
 * Since in Reason, function bindings always use arrows, it's wrong to rely
 * on function parsers to return non function bindings:
 *   let x: int -> 10;
 *   let y (:returnType) -> 20;  (* wat *)
 */

curried_binding_return_typed:
mark_position_exp
  ( labeled_simple_pattern curried_binding_return_typed_
    { let loc = mklocation $symbolstartpos $endpos in
      let (l, o, p) = $1 in mkexp ~loc (Pexp_fun(l, o, p, $2))
    }
  | LPAREN TYPE LIDENT+ RPAREN curried_binding_return_typed_
    { pexp_newtypes $3 $5 }
  ) {$1};

curried_binding_return_typed_:
  | curried_binding {$1}
  /* Parens are required around function return value annotations to allow
   * unifying of arrow syntax for all functions:
   *
   *   let add (x:int) (:int->int) -> fun a -> a + 1;
   *
   * Would have conflicts if return value annotation was not grouped in parens.
   * No one uses this annotation style anyways.
   *
   * But why not just require that the type be simple?
   *
   *   let add (x:int) :(int->int) -> fun a -> a + 1;
   *
   * Answer: Because we want to allow *non* simple types as well, as long as
   * they don't have arrows.
   *
   */
  | COLON only_core_type(non_arrowed_core_type) EQUALGREATER expr
    { let loc = mklocation $symbolstartpos $endpos in
      ghexp_constraint loc $4 (Some $2, None)
    }
;

/*
 * Arguments aren't required to object method!
 */
curried_binding:
  | EQUALGREATER expr
    { $2 }
  | labeled_simple_pattern curried_binding_return_typed_
    { let loc = mklocation $symbolstartpos $endpos in
      let (l, o, p) = $1 in
      mkexp ~loc (Pexp_fun(l, o, p, $2))
    }
  | LPAREN TYPE LIDENT+ RPAREN curried_binding_return_typed_
    { let loc = mklocation $symbolstartpos $endpos in
      pexp_newtypes ~loc $3 $5
    }
;

%inline match_cases(EXPR): lnonempty_list(match_case(EXPR)) { $1 };

match_case(EXPR):
  BAR pattern preceded(WHEN,expr)? EQUALGREATER EXPR
  { Exp.case $2 ?guard:$3 $5 }
;

fun_def:
  | EQUALGREATER expr { $2 }
  | COLON only_core_type(non_arrowed_core_type) EQUALGREATER expr
    { mkexp ~loc:(mklocation $startpos $endpos) (Pexp_constraint($4, $2)) }
  | labeled_simple_pattern fun_def
    { let (l,o,p) = $1 in
      let loc = mklocation $symbolstartpos $endpos in
      mkexp ~ghost:true ~loc (Pexp_fun (l, o, p, $2))
    }
  | LPAREN TYPE LIDENT+ RPAREN fun_def
    { pexp_newtypes ~loc:(mklocation $startpos $endpos) $3 $5 }
;

/* At least one comma delimited: Each item optionally annotated. */
expr_list:
  lseparated_nonempty_list(COMMA, expr_optional_constraint) COMMA?
  { $1 }
;

/* [x, y, z, ...n] --> ([x,y,z], Some n) */
expr_comma_seq_extension:
  | DOTDOTDOT expr_optional_constraint
    { ([], Some $2) }
  | expr_optional_constraint COMMA?
    { ([$1], None) }
  | expr_optional_constraint COMMA expr_comma_seq_extension
    { let seq, ext = $3 in ($1::seq, ext) }
;

/* Same as expr_comma_seq_extension but occuring after an item.

  Used when parsing `[<> </>, remaining]`. We know that there is at
  least one item, so we either should have a comma + more, or nothing.

expr_comma_seq_extension_second_item:
  | DOTDOTDOT expr_optional_constraint RBRACKET
    { ([], Some $2) }
  | expr_optional_constraint COMMA? RBRACKET
    { ([$1], None) }
  | expr_optional_constraint COMMA expr_comma_seq_extension
    { let seq, ext = $3 in ($1::seq, ext) }
;

/**
 * See note about tuple patterns. There are few cases where expressions may be
 * type constrained without requiring additional parens, and inside of tuples
 * are one exception.
 */
expr_optional_constraint:
  | expr { $1 }
  | expr type_constraint
    { ghexp_constraint (mklocation $symbolstartpos $endpos) $1 $2 }
;

%inline reject_attributes:
  item_attributes
  { if $1 <> [] then
      not_expecting $symbolstartpos $endpos "attribute before record field" }

record_expr:
  | DOTDOTDOT expr_optional_constraint lnonempty_list(preceded(COMMA,lbl_expr))
    { (Some $2, $3) }
  | reject_attributes as_loc(label_longident) COLON expr
    { (None, [($2, $4)]) }
  | reject_attributes lseparated_two_or_more(COMMA, lbl_expr) COMMA?
    { (None, $2) }
;

lbl_expr:
  | as_loc(label_longident) COLON expr { ($1, $3) }
  | as_loc(label_longident)            { ($1, exp_of_label $1) }
;

record_expr_with_string_keys:
  | DOTDOTDOT expr_optional_constraint string_literal_exprs
    { (Some $2, $3) }
  | reject_attributes STRING COLON expr
    { let loc = mklocation $symbolstartpos $endpos in
      let (s, d) = $2 in
      let lident_lident_loc = mkloc (Lident s) loc in
      (None, [(lident_lident_loc, $4)])
    }
  | reject_attributes string_literal_expr string_literal_exprs
    { (None, $2 :: $3) }
;

%inline string_literal_exprs:
  lnonempty_list(preceded(COMMA, string_literal_expr)) { $1 };

string_literal_expr:
  STRING preceded(COLON, expr)?
  { let loc = mklocation $startpos $endpos in
    let (s, d) = $1 in
    let lident_lident_loc = mkloc (Lident s) loc in
    let exp = match $2 with
      | Some x -> x
      | None -> mkexp (Pexp_ident lident_lident_loc)
    in
    (lident_lident_loc, exp)
  }
;

/**
 * field_expr is distinct from record_expr because labels cannot/shouldn't be scoped.
 */
field_expr:
  /* Using LIDENT instead of label here, because a reduce/reduce conflict occurs on:
   *   {blah:x}
   *
   * After `blah`, the parser couldn't tell whether to reduce `label` or
   * `val_ident`. So inlining the terminal here to avoid the whole decision.
   * Another approach would have been to place the `label` rule at at a precedence
   * of below_COLON or something.
   */
 | as_loc(LIDENT) COLON expr
   { ($1, $3) }
 | LIDENT
   { let loc = mklocation $symbolstartpos $endpos in
     let lident_loc = mkloc $1 loc in
     let lident_lident_loc = mkloc (Lident $1) loc in
     (lident_loc, mkexp (Pexp_ident lident_lident_loc))
   }
;

%inline field_expr_list: lseparated_nonempty_list(COMMA, field_expr) { $1 };

type_constraint:
  | COLON only_core_type(core_type)
      preceded(COLONGREATER,only_core_type(core_type))?
    { (Some $2, $3) }
  | COLONGREATER only_core_type(core_type)
    { (None, Some $2) }
;

/* Patterns */

pattern:
  | pattern_without_or { $1 }
  | mark_position_pat(pattern BAR pattern { mkpat(Ppat_or($1, $3)) }) { $1 }
;

pattern_without_or:
mark_position_pat
  ( simple_pattern { $1 }

  | pattern_without_or AS as_loc(val_ident)
    { mkpat(Ppat_alias($1, $3)) }

  | pattern_without_or AS as_loc(error)
    { expecting_pat (with_txt $3 "identifier") }

  /**
    * Parses a (comma-less) list of patterns into a tuple, or a single pattern
    * (if there is only one item in the list). This is kind of sloppy as there
    * should probably be a different AST construct for the syntax construct this
    * is used in (multiple constructor arguments). The things passed to
    * constructors are not actually tuples either in underlying representation or
    * semantics (they are not first class).
    */
  | as_loc(constr_longident) simple_pattern+
    { match is_pattern_list_single_any $2 with
      | Some singleAnyPat ->
        mkpat (Ppat_construct($1, Some singleAnyPat))
      | None ->
        let loc = mklocation $symbolstartpos $endpos in
        let argPattern = simple_pattern_list_to_tuple ~loc $2 in
        mkExplicitArityTuplePat (Ppat_construct($1, Some argPattern))
    }

  | name_tag simple_pattern { mkpat (Ppat_variant($1, Some $2)) }

  | pattern_without_or as_loc(COLONCOLON) pattern_without_or
    { Location.raise_errorf ~loc:$2.loc
        ":: is not supported in Reason, please use [hd, ...tl] instead" }

  | pattern_without_or COLONCOLON as_loc(error)
    { expecting_pat (with_txt $3 "pattern") }

  | LPAREN COLONCOLON RPAREN LPAREN pattern_without_or COMMA pattern_without_or RPAREN
    { let loc_coloncolon = mklocation $startpos($2) $endpos($2) in
      let loc = mklocation $symbolstartpos $endpos in
      mkpat_cons loc_coloncolon (mkpat ~ghost:true ~loc (Ppat_tuple[$5;$7])) loc
    }

  | as_loc(LPAREN) COLONCOLON RPAREN LPAREN
      pattern_without_or COMMA pattern_without_or as_loc(error)
    { unclosed_pat (with_txt $1 "(") (with_txt $8 ")") }

  | EXCEPTION pattern_without_or %prec prec_constr_appl
    { mkpat(Ppat_exception $2) }

  | LAZY simple_pattern { mkpat(Ppat_lazy $2) }

  /**
   * Attribute "attribute" everything to the left of the attribute,
   * up until the point of to the start of an expression, left paren, left
   * bracket, comma, bar - whichever comes first.
   */
  | attribute pattern_without_or %prec attribute_precedence
    { {$2 with ppat_attributes = $1 :: $2.ppat_attributes} }

  ) {$1};

/* A "simple pattern" is either a value identifier, or it is a
 * simple *non*value identifier.
 *
 * A "pattern" (the more general) is the set of all simple patterns,
 * but also with:
 * - more information such as `as X`, `lazy` etc.
 *
 * "labeled_simple_pattern"s contain the set of all simple_patterns, but also
 * include patterns suitable in function bindings where labeled arguments are
 * accepted.
 *
 * But, in all patterns, it seems type constraints must be grouped within some
 * parens or something.
 */
simple_pattern:
mark_position_pat
  ( as_loc(val_ident)        { mkpat(Ppat_var $1) }
  | simple_pattern_not_ident { $1 }
  ) {$1};

simple_pattern_not_ident:
mark_position_pat
  ( UNDERSCORE
    { mkpat (Ppat_any) }
  | signed_constant
    { mkpat (Ppat_constant $1) }
  | signed_constant DOTDOT signed_constant
    { mkpat (Ppat_interval ($1, $3)) }
  | as_loc(constr_longident)
    { mkpat (Ppat_construct ($1, None)) }
  | name_tag
    { mkpat (Ppat_variant ($1, None)) }
  | SHARP as_loc(type_longident)
    { mkpat (Ppat_type ($2)) }
  | LBRACE lbl_pattern_list RBRACE
    { let (fields, closed) = $2 in mkpat (Ppat_record (fields, closed)) }
  | as_loc(LBRACE) lbl_pattern_list as_loc(error)
    { unclosed_pat (with_txt $1 "{") (with_txt $3 "}") }
  | LBRACKET pattern_comma_list_extension RBRACKET
    { make_real_pat (mktailpat_extension (mklocation $startpos($2) $endpos($2)) $2) }
  | as_loc(LBRACKET) pattern_comma_list_extension as_loc(error)
    { unclosed_pat (with_txt $1 "[") (with_txt $3 "]") }
  | LBRACKETBAR pattern_comma_list SEMI? BARRBRACKET
    { mkpat (Ppat_array $2) }
  | LBRACKETBAR BARRBRACKET
    { mkpat (Ppat_array []) }
  | as_loc(LBRACKETBAR) pattern_comma_list SEMI? as_loc(error)
    { unclosed_pat (with_txt $1 "[|") (with_txt $4 "|]") }
  | LPAREN pattern RPAREN
    { $2 }
  | LPAREN lseparated_two_or_more(COMMA, pattern_optional_constraint) RPAREN
    { mkpat (Ppat_tuple $2) }
  | as_loc(LPAREN) pattern as_loc(error)
    { unclosed_pat (with_txt $1 "(") (with_txt $3 ")") }
  | LPAREN pattern COLON only_core_type(core_type) RPAREN
    { mkpat(Ppat_constraint($2, $4)) }
  | as_loc(LPAREN) pattern COLON core_type as_loc(error)
    { unclosed_pat (with_txt $1 "(") (with_txt $5 ")") }
  | LPAREN pattern COLON as_loc(error)
    { expecting_pat (with_txt $4 "type") }
  | LPAREN MODULE as_loc(UIDENT) RPAREN
    { mkpat(Ppat_unpack $3) }
  | LPAREN MODULE as_loc(UIDENT) COLON package_type RPAREN
    { let loc = mklocation $symbolstartpos $endpos in
      mkpat(Ppat_constraint(mkpat ~ghost:true ~loc (Ppat_unpack $3),
                            mktyp ~ghost:true ~loc (Ptyp_package $5)))
    }
  | as_loc(LPAREN) MODULE UIDENT COLON package_type as_loc(error)
    { unclosed_pat (with_txt $1 "(") (with_txt $6 ")") }
  | extension
    { mkpat(Ppat_extension $1) }
  ) {$1};

pattern_optional_constraint:
mark_position_pat
  ( pattern                                 { $1 }
  | pattern COLON only_core_type(core_type) { mkpat(Ppat_constraint($1, $3)) }
  ) {$1};
;

%inline pattern_comma_list:
  lseparated_nonempty_list(COMMA, pattern) { $1 };

/* [x, y, z, ...n] --> ([x,y,z], Some n) */
pattern_comma_list_extension:
  | pattern_comma_list COMMA DOTDOTDOT pattern { ($1, Some $4) }
  | pattern_comma_list COMMA?                  { ($1, None)    }
;

lbl_pattern_list:
  | lbl_pattern                            { ([$1], Closed) }
  | lbl_pattern COMMA                      { ([$1], Closed) }
  | lbl_pattern COMMA UNDERSCORE COMMA?    { ([$1], Open)   }
  | lbl_pattern COMMA lbl_pattern_list
    { let (fields, closed) = $3 in $1 :: fields, closed }
;

lbl_pattern:
  | as_loc(label_longident) COLON pattern { ($1,$3) }
  | as_loc(label_longident)               { ($1, pat_of_label $1) }
;

/* Primitive declarations */

primitive_declaration: nonempty_list(STRING { fst $1 }) { $1 };

/* Type declarations */

type_declarations:
  item_attributes TYPE nonrec_flag type_declaration_details
    and_type_declaration*
  { let (ident, params, constraints, kind, priv, manifest) = $4 in
    let loc = mklocation $symbolstartpos $endpos($4) in
    let ty = Type.mk ident ~params:params ~cstrs:constraints
             ~kind ~priv ?manifest ~attrs:$1 ~loc in
    ($3, ty :: $5)
  }
;

%inline and_type_declaration:
  item_attributes AND type_declaration_details
  { let (ident, params, cstrs, kind, priv, manifest) = $3 in
    let loc = mklocation $symbolstartpos $endpos in
    Type.mk ident ~params ~cstrs ~kind ~priv ?manifest ~attrs:$1 ~loc
  }
;

type_declaration_details:
  | as_loc(UIDENT) type_variables_with_variance type_kind constraints
    { Location.raise_errorf ~loc:$1.loc
        "A type's name need to begin with a lower-case letter or _"
    }
  | as_loc(LIDENT) type_variables_with_variance type_kind constraints
    { let (kind, priv, manifest) = $3 in
      ($1, $2, $4, kind, priv, manifest)
    }
;

constraints: preceded(CONSTRAINT, constrain)* { $1 };

type_kind:
  | /*empty*/
    { (Ptype_abstract, Public, None) }
  | EQUAL private_flag core_type
    { match $3 with
      | Core_type ct -> (Ptype_abstract, $2, Some ct)
      | Record_type rt -> (Ptype_record rt, $2, None)
    }
  | EQUAL private_flag constructor_declarations
    { (Ptype_variant ($3), $2, None) }
  | EQUAL DOTDOT
    { (Ptype_open, Public, None) }
  | EQUAL only_core_type(core_type) EQUAL private_flag constructor_declarations
    { (Ptype_variant ($5), $4, Some $2) }
  | EQUAL only_core_type(core_type) EQUAL DOTDOT
    { (Ptype_open, Public, Some $2) }
  | EQUAL only_core_type(core_type) EQUAL private_flag LBRACE label_declarations RBRACE
    { (Ptype_record (only_labels $6), $4, Some $2) }
;

%inline type_variables_with_variance:
  loption(parenthesized(separated_nonempty_list(COMMA, type_variable_with_variance)))
  { $1 }
;

type_variable_with_variance:
  embedded
  ( QUOTE ident       { (mktyp (Ptyp_var $2) , Invariant    ) }
  | UNDERSCORE        { (mktyp (Ptyp_any)    , Invariant    ) }
  | PLUS QUOTE ident  { (mktyp (Ptyp_var $3) , Covariant    ) }
  | PLUS UNDERSCORE   { (mktyp (Ptyp_any)    , Covariant    ) }
  | MINUS QUOTE ident { (mktyp (Ptyp_var $3) , Contravariant) }
  | MINUS UNDERSCORE  { (mktyp Ptyp_any      , Contravariant) }
  )
  { let first, second = $1 in
    let ptyp_loc =
        {first.ptyp_loc with loc_start = $symbolstartpos; loc_end = $endpos}
    in
    ({first with ptyp_loc}, second)
  }
;

type_parameter: type_variance type_variable { ($2, $1) };

type_variance:
  | /* empty */ { Invariant }
  | PLUS        { Covariant }
  | MINUS       { Contravariant }
;

type_variable:
mark_position_typ
  (QUOTE ident { mktyp (Ptyp_var $2) })
  { $1 };

constructor_declarations:
  | constructor_declaration attributed_constructor_declaration* { $1 :: $2 }
  | attributed_constructor_declaration+ { $1 }
;

%inline attributed_constructor_declaration:
  attribute* BAR constructor_declaration
  { {$3 with pcd_attributes = $1 @ $3.pcd_attributes} }
;

constructor_declaration:
  as_loc(constr_ident) generalized_constructor_arguments
  { let args, res = $2 in
    let loc = mklocation $symbolstartpos $endpos in
    Type.constructor $1 ~args ?res ~loc }
;

/* Why are there already item_attribute* on the extension_constructor_declaration? */
str_exception_declaration:
  item_attributes EXCEPTION
    either(extension_constructor_declaration, extension_constructor_rebind)
  { {$3 with pext_attributes = $3.pext_attributes @ $1} }
;

sig_exception_declaration:
  item_attributes EXCEPTION
    extension_constructor_declaration
  { {$3 with pext_attributes = $3.pext_attributes @ $1} }
;

generalized_constructor_arguments:
  | preceded(COLON, only_core_type(core_type))?
    { (Pcstr_tuple [], $1) }
  | ioption(OF) constructor_arguments
    preceded(COLON,only_core_type(core_type))?
    { ($2,$3) }
;

constructor_arguments:
  | non_arrowed_simple_core_type
    { match $1 with
      | Core_type ct -> Pcstr_tuple [ct]
      | Record_type rt -> Pcstr_record rt
    }
  | only_core_type(non_arrowed_simple_core_type)
    only_core_type(non_arrowed_simple_core_type)+
    { Pcstr_tuple ($1 :: $2) }
;

label_declaration:
  | attribute* mutable_flag as_loc(LIDENT)
    { let loc = mklocation $symbolstartpos $endpos in
      (Type.field $3 (mkct $3) ~mut:$2 ~loc, $1)
    }
  | attribute* mutable_flag as_loc(LIDENT) COLON poly_type
    { let loc = mklocation $symbolstartpos $endpos in
      (Type.field $3 $5 ~mut:$2 ~loc, $1)
    }
;

/* Type Extensions */

potentially_long_ident_and_optional_type_parameters:
  | LIDENT type_variables_with_variance
    { let loc = mklocation $startpos($1) $endpos($1) in
      let lident_lident_loc = mkloc (Lident $1) loc in
      (lident_lident_loc, $2)
    }
  | as_loc(type_strictly_longident) type_variables_with_variance
    {($1, $2)}
;

str_type_extension:
  item_attributes
  TYPE nonrec_flag potentially_long_ident_and_optional_type_parameters
  PLUSEQ embedded(private_flag)
  attributed_ext_constructors(either(extension_constructor_declaration, extension_constructor_rebind))
  { if $3 <> Recursive then
      not_expecting $startpos($3) $endpos($3) "nonrec flag";
    let (potentially_long_ident, params) = $4 in
    Te.mk potentially_long_ident $7 ~params ~priv:$6 ~attrs:$1
  }
;

sig_type_extension:
  item_attributes
  TYPE nonrec_flag potentially_long_ident_and_optional_type_parameters
  PLUSEQ embedded(private_flag)
  attributed_ext_constructors(extension_constructor_declaration)
  { if $3 <> Recursive then
      not_expecting $startpos($3) $endpos($3) "nonrec flag";
    let (potentially_long_ident, params) = $4 in
    Te.mk potentially_long_ident $7 ~params ~priv:$6 ~attrs:$1
  }
;

%inline attributed_ext_constructor(X):
  attribute* BAR X { {$3 with pext_attributes = $1 @ $3.pext_attributes} }
;

attributed_ext_constructors(X):
  | X attributed_ext_constructor(X)* { $1 :: $2 }
  | attributed_ext_constructor(X)+ { $1 }
;

extension_constructor_declaration:
  as_loc(constr_ident) generalized_constructor_arguments
  { let args, res = $2 in
    let loc = mklocation $symbolstartpos $endpos in
    Te.decl $1 ~args ?res ~loc
  }
;

extension_constructor_rebind:
  as_loc(constr_ident) EQUAL as_loc(constr_longident)
  { let loc = mklocation $symbolstartpos $endpos in
    Te.rebind $1 $3 ~loc
  }
;

/* "with" constraints (additional type equations over signature components) */

with_constraint:
  | TYPE as_loc(label_longident) type_variables_with_variance
      EQUAL embedded(private_flag) only_core_type(core_type) constraints
    { let loc = mklocation $symbolstartpos $endpos in
      let typ = Type.mk {$2 with txt=Longident.last $2.txt}
                  ~params:$3 ~cstrs:$7 ~manifest:$6 ~priv:$5 ~loc in
      Pwith_type ($2, typ)
    }
    /* used label_longident instead of type_longident to disallow
       functor applications in type path */
  | TYPE as_loc(label_longident) type_variables_with_variance
      COLONEQUAL only_core_type(core_type)
    { let last = match $2.txt with
        | Lident s -> s
        | _ -> not_expecting $startpos($2) $endpos($2) "Long type identifier"
      in
      let loc = mklocation $symbolstartpos $endpos in
      Pwith_typesubst (Type.mk {$2 with txt=last} ~params:$3 ~manifest:$5 ~loc)
    }
  | MODULE as_loc(mod_longident) EQUAL as_loc(mod_ext_longident)
      { Pwith_module ($2, $4) }
  | MODULE as_loc(UIDENT) COLONEQUAL as_loc(mod_ext_longident)
      { Pwith_modsubst ($2, $4) }
;

/* Polymorphic types */

poly_type:
mark_position_typ
  ( only_core_type(core_type)
    { $1 }
  | preceded(QUOTE,ident)+ DOT only_core_type(core_type)
    { mktyp(Ptyp_poly($1, $3)) }
  ) {$1};

/**
  * OCaml types before:
  * -------------------
  * core_type ::=
  *   core_type2
  *   core_type2 as ident
  *
  * core_type2 ::=
  *   simple_core_type_or_tuple
  *   ?ident: core_type2 -> core_type2
  *   ?ident: core_type2 -> core_type2
  *   ident: core_type2 -> core_type2
  *   core_type2 -> core_type2
  *
  * simple_core_type_or_tuple ::=
  *   simple_core_type
  *   simple_core_type * simple_core_type_list   (*Reason deprecates this*)
  *
  * simple_core_type_list ::=                    (*Reason might be able to deprecate this*)
  *   simple_core_type
  *   simple_core_type_list STAR simple_core_type
  *
  * simple_core_type ::=
  *   simple_core_type
  *   simple_core_type2
  *   ( core_type)      (*core_type_comma_list of len 1*)
  *
  * simple_core_type2 ::=
  *  'ident
  *  Long.ident
  *  simple_core_type2 Long.ident
  *  (core_type, core_type) Long.ident
  *  <method1: ... method_2: ...>
  *  <>
  *  {method1: .. method2: ..}
  *  # class_longident #
  *  simple_core_type2 # class_longident
  *  (core_type, core_type) # class_longident
  *  (module package_type)
  *  [%]
  *  bunch of other stuff
  *
  * Reason types where tuples require grouping:
  * -------------------
  * Simple types are implicitly non-arrowed.
  * Non arrowed types may be shown in the trailing positino of the curried sugar
  * function/functor bindings, but it doesn't have to be simple.
  *
  * let x ... :non_arrowed_type => ..
  *
  * - A better name for core_type2 would be arrowed_core_type
  * - A better name for core_type would be aliased_arrowed_core_type
  * core_type ::=
  *   core_type2
  *   core_type2 as ident
  *
  * core_type2 ::=
  *   non_arrowed_core_type
  *   ident::? non_arrowed_core_type => non_arrowed_core_type
  *   ident:: non_arrowed_core_type => non_arrowed_core_type
  *   core_type2 => core_type2
  *
  * non_arrowed_core_type ::=
  *    non_arrowed_non_simple_core_type
  *    non_arrowed_simple_core_type
  *
  * non_arrowed_non_simple_core_type ::=
  *   type_longident non_arrowed_simple_core_type_list
  *   # class_longident
  *   non_arrowed_simple_core_type # class_longident
  *   [core_type_comma_list] # class_longident
  *
  * non_arrowed_simple_core_type ::=
  *   <>
  *   ()
  *   {}
  *
  *
  *  'ident
  *  Long.ident
  *  non_arrowed_simple_core_type Long.ident
  *  Long.ident non_arrowed_simple_core_type_list
  *  <method1: ... method_2: ...>
  *  <>
  *  {method1: .. method2: ..}
  *  # class_longident #
  *  non_arrowed_simple_core_type # class_longident
  *  (core_type, core_type) # class_longident
  *  (module package_type)
  *  [%]
  *  bunch of other stuff
  */

/** Potentially includes:
 * - arrows
 * - space separated type applications
 * - "as" aliases
 */
core_type:
mark_position_typ2
  /* For some reason, when unifying Functor type syntax (using EQUALGREATER),
   * there was a shift reduce conflict likely caused by
   * type module MyFunctor = {type x = blah => foo } => SomeSig
   * That should *not* be a shift reduce conflict (on =>), and it's not clear
   * why the shift reduce conflict showed up in core_type2. Either way, this
   * seems to resolve the issue. If removing the below_EQUALGREATER, the shift
   * reduce conflict is actually caused by the new Functor type annotations
   * even though *nothing* will point towards that.
   * If switching to Menhir, this may not be needed.
   */
  ( core_type2 %prec below_EQUALGREATER
    { $1 }
  | only_core_type(core_type2) AS QUOTE ident
    { Core_type (mktyp(Ptyp_alias($1, $4))) }
  ) {$1};

/**
 *
 * core_type is basically just core_type2 but with a single AS x potentially
 * appended.
 * core_type2 Potentially includes:
 * - arrows
 * - space separated type applications
 * - Polymorphic type variable application
 */
core_type2:
mark_position_typ2
  ( non_arrowed_core_type
    { $1 }
  | LIDENTCOLONCOLON only_core_type(non_arrowed_core_type)
      QUESTION EQUALGREATER only_core_type(core_type2)
    { Core_type (mktyp(Ptyp_arrow(Optional $1, $2, $5))) }
  | LIDENTCOLONCOLON only_core_type(non_arrowed_core_type)
      EQUALGREATER only_core_type(core_type2)
    { Core_type (mktyp(Ptyp_arrow(Labelled $1, $2, $4))) }
  | only_core_type(core_type2)
      EQUALGREATER only_core_type(core_type2)
    { Core_type (mktyp(Ptyp_arrow(Nolabel, $1, $3))) }
  ) {$1};

/* Among other distinctions, "simple" core types can be used in Variant types:
 * type myType = Count of anySimpleCoreType. Core types (and simple core types)
 * don't include variant declarations (`constructor_declarations`) and don't
 * include the "faux curried" variant Constructor arguments list.
 *
 * In general, "simple" syntax constructs, don't need to be wrapped in
 * parens/braces when embedded in lists of those very constructs.
 *
 * A [non_arrowed_simple_core_type] *can* be wrapped in parens, but
 * it doesn't have to be.
 */

/* The name [core_type] was taken. [non_arrowed_core_type] is the same as
 * [non_arrowed_simple_core_type] but used in cases
 * where application needn't be wrapped in additional parens */
/* Typically, other syntax constructs choose to allow either
 * [non_arrowed_simple_core_type] or
 * [non_arrowed_non_simple_core_type] depending on whether or not
 * they are in a context that expects space separated lists of types to carry
 * particular meaning outside of type constructor application.
 *
 * type x = SomeConstructor x y;
 */
non_arrowed_core_type:
  non_arrowed_non_simple_core_type | non_arrowed_simple_core_type
  { $1 }
;

type_parameters:
  parenthesized(separated_nonempty_list(COMMA, only_core_type(non_arrowed_simple_core_type)))
  { $1 }
;

non_arrowed_non_simple_core_type:
mark_position_typ2
  ( as_loc(type_longident) type_parameters
    { Core_type (mktyp(Ptyp_constr($1, $2))) }
  | SHARP as_loc(class_longident) type_parameters
    { Core_type (mktyp(Ptyp_class($2, $3))) }
  | attribute only_core_type(non_arrowed_core_type)
    { Core_type {$2 with ptyp_attributes = $1 :: $2.ptyp_attributes} }
  ) {$1};

non_arrowed_simple_core_type:
mark_position_typ2
  ( LPAREN separated_nonempty_list(COMMA, only_core_type(core_type)) RPAREN
    { match $2 with
      | []    -> assert false
      | [one] -> Core_type one
      | many  -> Core_type (mktyp (Ptyp_tuple many))
    }
  | QUOTE ident
    { Core_type (mktyp(Ptyp_var $2)) }
  | SHARP as_loc(class_longident)
    { Core_type (mktyp(Ptyp_class($2, []))) }
  | UNDERSCORE
    { Core_type (mktyp(Ptyp_any)) }
  | as_loc(type_longident)
    { Core_type (mktyp(Ptyp_constr($1, []))) }
  | object_record_type
    { let (labels, object_record) = $1 in
      match object_record with
      | Record -> Record_type(only_labels labels)
      | Object_open -> (
        Core_type(mktyp(Ptyp_object (List.map (fun l -> let (label, attr) = l in if label.pld_mutable == Mutable then syntax_error(); label.pld_name.txt, attr, label.pld_type ) labels, Open)))
        )
      | Object_closed -> Core_type(mktyp(Ptyp_object (List.map (fun l -> let (label, attr) = l in if label.pld_mutable == Mutable then syntax_error(); label.pld_name.txt, attr, label.pld_type ) labels, Closed)))
    }
  | LBRACKET tag_field RBRACKET
    { Core_type(mktyp(Ptyp_variant([$2], Closed, None))) }
/* PR#3835: this is not LR(1), would need lookahead=2
  | LBRACKET non_arrowed_simple_core_type RBRACKET
      { mktyp(Ptyp_variant([$2], Closed, None)) }
*/
  | LBRACKET BAR row_field_list RBRACKET
    { Core_type(mktyp(Ptyp_variant ($3, Closed, None))) }
  | LBRACKET row_field BAR row_field_list RBRACKET
    { Core_type(mktyp(Ptyp_variant ($2 :: $4, Closed, None))) }
  | LBRACKETGREATER BAR? row_field_list RBRACKET
    { Core_type(mktyp(Ptyp_variant ($3, Open, None))) }
  | LBRACKETGREATER RBRACKET
    { Core_type(mktyp(Ptyp_variant ([], Open, None))) }
  | LBRACKETLESS BAR? row_field_list RBRACKET
    { Core_type(mktyp(Ptyp_variant ($3, Closed, Some []))) }
  | LBRACKETLESS BAR? row_field_list GREATER name_tag+ RBRACKET
    { Core_type(mktyp(Ptyp_variant ($3, Closed, Some $5))) }
  | LPAREN MODULE package_type RPAREN
    { Core_type(mktyp(Ptyp_package $3)) }
  | extension
    { Core_type(mktyp(Ptyp_extension $1)) }
  ) {$1};

object_record_type:
  | LBRACE RBRACE                                    { syntax_error () }
  | LBRACE label_declarations RBRACE                 { $2, Record }
  | LBRACE DOT loption(label_declarations) RBRACE    { $3, Object_closed }
  | LBRACE DOTDOT loption(label_declarations) RBRACE { $3, Object_open }
;

%inline label_declarations:
  lseparated_nonempty_list(COMMA, label_declaration) COMMA? { $1 };

package_type:
  as_loc(mty_longident)
    loption(preceded(WITH, separated_nonempty_list(AND, package_type_cstr)))
  { ($1, $2) }
;

package_type_cstr:
  TYPE as_loc(label_longident) EQUAL only_core_type(core_type)
  { ($2, $4) }
;

row_field_list: separated_nonempty_list(BAR, row_field) { $1 };

row_field:
  | tag_field                                    { $1 }
  | only_core_type(non_arrowed_simple_core_type) { Rinherit $1 }
;

tag_field:
  | attribute* name_tag OF? boption(AMPERSAND)
      separated_nonempty_list(AMPERSAND, only_core_type(core_type))
    { Rtag ($2, $1, $4, $5) }
  |  attribute* name_tag
    { Rtag ($2, $1, true, []) }
;

/* Constants */

constant:
  | INT          { let (n, m) = $1 in Pconst_integer (n, m) }
  | CHAR         { Pconst_char $1 }
  | STRING       { let (s, d) = $1 in Pconst_string (s, d) }
  | FLOAT        { let (f, m) = $1 in Pconst_float (f, m) }
;

signed_constant:
  | constant     { $1 }
  | MINUS INT    { let (n, m) = $2 in Pconst_integer("-" ^ n, m) }
  | MINUS FLOAT  { let (f, m) = $2 in Pconst_float("-" ^ f, m) }
  | PLUS INT     { let (n, m) = $2 in Pconst_integer (n, m) }
  | PLUS FLOAT   { let (f, m) = $2 in Pconst_float(f, m) }
;

/* Identifiers and long identifiers */

ident: UIDENT | LIDENT { $1 };

val_ident:
  | LIDENT                 { $1 }
  | LPAREN operator RPAREN { $2 }
;

%inline infix_operator:
  | INFIXOP0          { $1 }
  | INFIXOP1          { $1 }
  | INFIXOP2          { $1 }
  | INFIXOP3          { $1 }
  /* SLASHGREATER is INFIXOP3 but we needed to call it out specially */
  | SLASHGREATER      { "/>" }
  | INFIXOP4          { $1 }
  | PLUS              { "+" }
  | PLUSDOT           { "+." }
  | MINUS             { "-" }
  | MINUSDOT          { "-." }
  | STAR              { "*" }
  | LESS              { "<" }
  | GREATER           { ">" }
  | OR                { "or" }
  | BARBAR            { "||" }
  | AMPERSAND         { "&" }
  | AMPERAMPER        { "&&" }
  | COLONEQUAL        { ":=" }
  | PLUSEQ            { "+=" }
  | PERCENT           { "%" }
  /* We don't need to (and don't want to) consider </> an infix operator for now
     because our lexer requires that "</>" be expressed as "<\/>" because
     every star and slash after the first character must be escaped.
  */
  /* Also, we don't want to consider <> an infix operator because the Reason
     operator swapping requires that we express that as != */
  | LESSDOTDOTGREATER { "<..>" }
  | GREATER GREATER   { ">>" }
;

operator:
  | PREFIXOP          { $1 }
  | BANG              { "!" }
  | infix_operator    { $1 }
;
%inline constr_ident:
  | UIDENT            { $1 }
  | LBRACKET RBRACKET { "[]" }
  | LPAREN RPAREN     { "()" }
  | COLONCOLON        { "::" }
/*  | LPAREN COLONCOLON RPAREN { "::" } */
  | FALSE             { "false" }
  | TRUE              { "true" }
;

val_longident:
  | val_ident                     { Lident $1 }
  | mod_longident DOT val_ident   { Ldot($1, $3) }
;

constr_longident:
  | mod_longident %prec below_DOT { $1 }
  | LBRACKET RBRACKET             { Lident "[]" }
  | LPAREN RPAREN                 { Lident "()" }
  | FALSE                         { Lident "false" }
  | TRUE                          { Lident "true" }
;

label_longident:
  | LIDENT                        { Lident $1 }
  | mod_longident DOT LIDENT      { Ldot($1, $3) }
;

type_longident:
  | LIDENT                        { Lident $1 }
  | mod_ext_longident DOT LIDENT  { Ldot($1, $3) }
;

/* Type long identifiers known to be "long". Only needed to resolve shift
 * reduce conflicts between `type myType 'a 'b = ..` and `type
 * Potentially.Long.myType 'a 'b += ..` - we needed to break the parsing of
 * Potentially.Long.myType into two cases, one that is long, and one that is
 * not.
 */
type_strictly_longident:
  mod_ext_longident DOT LIDENT
  { Ldot($1, $3) }
;

mod_longident:
  | UIDENT                        { Lident $1 }
  | mod_longident DOT UIDENT      { Ldot($1, $3) }
;

mod_ext_longident:
  | UIDENT                        { Lident $1 }
  | mod_ext_longident DOT UIDENT  { Ldot($1, $3) }
  | mod_ext2                      { $1 }
;

mod_ext2:
  | mod_ext_longident DOT UIDENT LPAREN mod_ext_longident RPAREN
    { lapply ( Ldot($1, $3)) $5 $symbolstartpos $endpos }
  | mod_ext2 LPAREN mod_ext_longident RPAREN
    { lapply $1 $3 $symbolstartpos $endpos }
  | UIDENT LPAREN mod_ext_longident RPAREN
    { lapply (Lident($1)) $3 $symbolstartpos $endpos }
;

mty_longident:
  | ident                        { Lident $1 }
  | mod_ext_longident DOT ident  { Ldot($1, $3) }
;

clty_longident:
  | LIDENT                       { Lident $1 }
  | mod_ext_longident DOT LIDENT { Ldot($1, $3) }
;

class_longident:
  | LIDENT                       { Lident $1 }
  | mod_longident DOT LIDENT     { Ldot($1, $3) }
;

/* Toplevel directives */

toplevel_directive:
  SHARP ident embedded( /* empty */   { Pdir_none }
          | STRING        { Pdir_string (fst $1) }
          | INT           { let (n, m) = $1 in Pdir_int (n, m) }
          | val_longident { Pdir_ident $1 }
          | mod_longident { Pdir_ident $1 }
          | FALSE         { Pdir_bool false }
          | TRUE          { Pdir_bool true }
          )
  { Ptop_dir($2, $3) }
;

/* Miscellaneous */

opt_LET_MODULE: MODULE { () } | LET MODULE { () };

%inline name_tag: BACKQUOTE ident { $2 };

%inline label: LIDENT { $1 };

rec_flag:
  | /* empty */   { Nonrecursive }
  | REC           { Recursive }
;

nonrec_flag:
  | /* empty */   { Recursive }
  | NONREC        { Nonrecursive }
;

direction_flag:
  | TO            { Upto }
  | DOWNTO        { Downto }
;

%inline private_flag:
  | /* empty */   { Public }
  | PRI           { Private }
;

mutable_flag:
  | /* empty */   { Immutable }
  | MUTABLE       { Mutable }
;

virtual_flag:
  | /* empty */   { Concrete }
  | VIRTUAL       { Virtual }
;

mutable_or_virtual_flags:
  | /* empty */   { Immutable, Concrete }
  | VIRTUAL mutable_flag { $2, Virtual }
  | MUTABLE virtual_flag { Mutable, $2 }
;

override_flag:
  | /* empty */   { Fresh }
  | BANG          { Override }
;

subtractive:
  | MINUS         { "-"  }
  | MINUSDOT      { "-." }
;

additive:
  | PLUS          { "+"  }
  | PLUSDOT       { "+." }
;

single_attr_id:
  | LIDENT      { $1 }
  | UIDENT      { $1 }
  | AND         { "and" }
  | AS          { "as" }
  | ASSERT      { "assert" }
  | BEGIN       { "begin" }
  | CLASS       { "class" }
  | CONSTRAINT  { "constraint" }
  | DO          { "do" }
  | DONE        { "done" }
  | DOWNTO      { "downto" }
  | ELSE        { "else" }
  | END         { "end" }
  | EXCEPTION   { "exception" }
  | EXTERNAL    { "external" }
  | FALSE       { "false" }
  | FOR         { "for" }
  | FUN         { "fun" }
  | FUNCTION    { "function" }
  | FUNCTOR     { "functor" }
  | IF          { "if" }
  | IN          { "in" }
  | INCLUDE     { "include" }
  | INHERIT     { "inherit" }
  | INITIALIZER { "initializer" }
  | LAZY        { "lazy" }
  | LET         { "let" }
  | SWITCH      { "switch" }
  | MODULE      { "module" }
  | MUTABLE     { "mutable" }
  | NEW         { "new" }
  | NONREC      { "nonrec" }
  | OBJECT      { "object" }
  | OF          { "of" }
  | OPEN        { "open" }
  | OR          { "or" }
  | PRI         { "private" }
  | REC         { "rec" }
  | SIG         { "sig" }
  | STRUCT      { "struct" }
  | THEN        { "then" }
  | TO          { "to" }
  | TRUE        { "true" }
  | TRY         { "try" }
  | TYPE        { "type" }
  | VAL         { "val" }
  | VIRTUAL     { "virtual" }
  | WHEN        { "when" }
  | WHILE       { "while" }
  | WITH        { "with" }
/* mod/land/lor/lxor/lsl/lsr/asr are not supported for now */
;

attr_id:
  | as_loc(single_attr_id) { $1 }
  | single_attr_id DOT attr_id { mkloc ($1 ^ "." ^ $3.txt) (mklocation $symbolstartpos $endpos) }
;

attribute: LBRACKETAT attr_id payload RBRACKET { ($2, $3) };

item_attribute: LBRACKETATAT attr_id payload RBRACKET { ($2, $3) };

(* Inlined to avoid having to deal with buggy $symbolstartpos *)
%inline item_attributes:
  | { [] }
  | item_attribute+ { $1 }

floating_attribute: LBRACKETATATAT attr_id payload RBRACKET {($2, $3)};

item_extension_sugar:
  /**
   * Note, this form isn't really super useful, but wouldn't cause any parser
   * conflicts. Not supporting it though just to avoid having to write the
   * pretty printing logic.
   *
   *   [@attrsOnExtension] %extSugarId [@attrOnLet] LET ..
   *
   * We won't document it though, and probably won't format it as such.
   *  | PERCENT attr_id item_attribute item_attribute* {
   *     ($3::$4, $2)
   *    }
   */
  PERCENT attr_id { ([], $2) }
;

extension:
  LBRACKETPERCENT attr_id payload RBRACKET { ($2, $3) }
;

item_extension:
  LBRACKETPERCENTPERCENT attr_id payload RBRACKET { ($2, $3) }
;

payload:
  | structure                       { PStr $1 }
  | COLON only_core_type(core_type) { PTyp $2 }
  | QUESTION pattern                { PPat ($2, None) }
  | QUESTION pattern WHEN expr      { PPat ($2, Some $4) }
;

%inline only_core_type(X): X { only_core_type $1 $symbolstartpos $endpos }

%inline mark_position_mod(X): x = X
	{ {x with pmod_loc = {x.pmod_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_cty(X): x = X
	{ {x with pcty_loc = {x.pcty_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_ctf(X): x = X
	{ {x with pctf_loc = {x.pctf_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_exp(X): x = X
	{ {x with pexp_loc = {x.pexp_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_typ(X): x = X
	{ {x with ptyp_loc = {x.ptyp_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_typ2(X): x = X
	{ match x with
	  | Core_type ct ->
      let loc_start = $symbolstartpos and loc_end = $endpos in
      Core_type ({ct with ptyp_loc = {ct.ptyp_loc with loc_start; loc_end}})
	  | Record_type _ -> x
	}
;

%inline mark_position_mty(X): x = X
	{ {x with pmty_loc = {x.pmty_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_sig(X): x = X
	{ {x with psig_loc = {x.psig_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_str(X): x = X
	{ {x with pstr_loc = {x.pstr_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_cl(X): x = X
	{ {x with pcl_loc = {x.pcl_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_cf(X): x = X
	{ {x with pcf_loc = {x.pcf_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline mark_position_pat(X): x = X
	{ {x with ppat_loc = {x.ppat_loc with loc_start = $symbolstartpos; loc_end = $endpos}} }
;

%inline as_loc(X): x = X
	{ mkloc x (mklocation $symbolstartpos $endpos) }
;

%inline with_patvar(X): x = X
  { let loc = mklocation $symbolstartpos $endpos in
    mkpat ~loc (Ppat_var (mkloc x loc)) }

either(X,Y):
  | X { $1 }
  | Y { $1 }
;

%inline lnonempty_list(X): X llist_aux(X) { $1 :: List.rev $2 };

%inline llist(X): llist_aux(X) { List.rev $1 };

llist_aux(X):
  | /* empty */ { [] }
  | llist_aux(X) X { $2 :: $1 }
;

%inline lseparated_list(sep, X):
  | /* empty */ { [] }
  | lseparated_nonempty_list(sep, X) { $1 };

%inline lseparated_nonempty_list(sep, X):
  lseparated_nonempty_list_aux(sep, X) { List.rev $1 };

lseparated_nonempty_list_aux(sep, X):
  | X { [$1] }
  | lseparated_nonempty_list_aux(sep, X) sep X { $3 :: $1 }
;

%inline lseparated_two_or_more(sep, X):
  X sep lseparated_nonempty_list(sep, X) { $1 :: $3 };

%inline parenthesized(X): delimited(LPAREN, X, RPAREN) { $1 };

%%
