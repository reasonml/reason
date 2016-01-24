/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* The parser definition */

%{
open Location
open Asttypes
open Longident
open Parsetree
open Ast_helper

(*
   TODO:
   - Document all of the helper utility functions here such as mkloc, reloc_pat.
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

   let rhs_loc n = {
     loc_start = Parsing.rhs_start_pos n;
     loc_end = Parsing.rhs_end_pos n;
     loc_ghost = false;
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

(* I believe rloc doesn't stand for rhs loc, it stands for "real" loc *)
(* The "real location" is the location (start/end) of the left hand side of the
   rule *)

let mktyp d = Typ.mk ~loc:(symbol_rloc()) d
let mkpat d = Pat.mk ~loc:(symbol_rloc()) d
let mkexp ?(attrs=[]) d = Exp.mk ~attrs ~loc:(symbol_rloc()) d
let mkmty d = Mty.mk ~loc:(symbol_rloc()) d
let mksig d = Sig.mk ~loc:(symbol_rloc()) d
let mkmod d = Mod.mk ~loc:(symbol_rloc()) d
let mkmodRhs d pos = Mod.mk ~loc:(rhs_loc pos) d
let mkstr d = Str.mk ~loc:(symbol_rloc()) d
let mkclass d = Cl.mk ~loc:(symbol_rloc()) d
let mkcty d = Cty.mk ~loc:(symbol_rloc()) d
let mkctf d = Ctf.mk ~loc:(symbol_rloc()) d
let mkcf d = Cf.mk ~loc:(symbol_rloc()) d

let mkrhs rhs pos = mkloc rhs (rhs_loc pos)
let mkoption d =
  let loc = {d.ptyp_loc with loc_ghost = true} in
  Typ.mk ~loc (Ptyp_constr(mkloc (Ldot (Lident "*predef*", "option")) loc,[d]))


let simple_ghost_text_attr txt = [({txt; loc=symbol_gloc ()}, PStr [])]

let mkExplicitArityTuplePat pat =
  (* Tell OCaml type system that what this tuple construction represents is
     not actually a tuple, and should represent several constructor
     arguments.  This allows the syntax the ability to distinguish between:

     X (10, 20)  -- One argument constructor
     X 10 20     -- Multi argument constructor
  *)
  Pat.mk
    ~loc:(symbol_rloc())
    ~attrs:(simple_ghost_text_attr "explicit_arity")
    pat

let mkExplicitArityTupleExp exp =
  Exp.mk
    ~loc:(symbol_rloc())
    ~attrs:(simple_ghost_text_attr "explicit_arity")
    exp

let is_pattern_list_single_any = function
  | [{ppat_desc=Ppat_any; ppat_attributes=[]} as onlyItem] -> Some onlyItem
  | _ -> None

let reloc_pat x = { x with ppat_loc = symbol_rloc () };;

(* I believe this relocates an expression. This is useful for when
   precedence-only tokens are used to group expressions, and you want the
   tokens to be counted as part of the expression. You simply "relocate" the
   expression that was grouped so that it includes the parens. If the right
   hand side of a rule includes parens, then symbol_rloc () (which returns the
   location of the left side of a rule) will naturally include the location of
   the parens [or whatever surrounds the expression]. *)
let reloc_exp x = { x with pexp_loc = symbol_rloc () };;
let reloc_class x = { x with pcl_loc = symbol_rloc () };;
let reloc_class_field x = { x with pcf_loc = symbol_rloc () };;
let reloc_class_type x = { x with pcty_loc = symbol_rloc () };;

let mkoperator name pos =
  let loc = rhs_loc pos in
  Exp.mk ~loc (Pexp_ident(mkloc (Lident name) loc))

let mkpatvar name pos =
  Pat.mk ~loc:(rhs_loc pos) (Ppat_var (mkrhs name pos))

let mk_ghost_patvar name pos =
  Pat.mk ~loc:(symbol_gloc ()) (Ppat_var (mkrhs name pos))


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
let ghexp ?(attrs=[]) d = Exp.mk ~attrs ~loc:(symbol_gloc ()) d
let ghpat d = Pat.mk ~loc:(symbol_gloc ()) d
let ghtyp d = Typ.mk ~loc:(symbol_gloc ()) d
let ghloc d = { txt = d; loc = symbol_gloc () }
let ghstr d = Str.mk ~loc:(symbol_gloc()) d
let ghsig d = Sig.mk ~loc:(symbol_gloc()) d

let ghunit () =
  ghexp (Pexp_construct (mknoloc (Lident "()"), None))

let mkinfix arg1 name arg2 =
  mkexp(Pexp_apply(mkoperator name 2, ["", arg1; "", arg2]))

let neg_float_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Const_int n) ->
      mkexp(Pexp_constant(Const_int(-n)))
  | "-", Pexp_constant(Const_int32 n) ->
      mkexp(Pexp_constant(Const_int32(Int32.neg n)))
  | "-", Pexp_constant(Const_int64 n) ->
      mkexp(Pexp_constant(Const_int64(Int64.neg n)))
  | "-", Pexp_constant(Const_nativeint n) ->
      mkexp(Pexp_constant(Const_nativeint(Nativeint.neg n)))
  | ("-" | "-."), Pexp_constant(Const_float f) ->
      mkexp(Pexp_constant(Const_float(neg_float_string f)))
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, ["", arg]))

let mkFunctorThatReturns functorArgs returns =
  List.fold_left (
    fun acc (n, t) -> mkmod(Pmod_functor(n, t, acc))
  ) returns functorArgs

let mkuplus name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant(Const_int _)
  | "+", Pexp_constant(Const_int32 _)
  | "+", Pexp_constant(Const_int64 _)
  | "+", Pexp_constant(Const_nativeint _)
  | ("+" | "+."), Pexp_constant(Const_float _) -> mkexp desc
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, ["", arg]))

let mkexp_cons consloc args loc =
  Exp.mk ~loc (Pexp_construct(mkloc (Lident "::") consloc, Some args))

let mkpat_cons consloc args loc =
  Pat.mk ~loc (Ppat_construct(mkloc (Lident "::") consloc, Some args))


(* TODO: the location is likely incorrect. *)
let simple_pattern_list_to_tuple lst =
  match lst with
    | [] -> assert false
    | _ -> mkpat (Ppat_tuple (List.rev lst))

(* TODO: the location is likely incorrect. *)
let simple_pattern_list_to_tuple_or_single_pattern lst =
  match lst with
    | [] -> assert false
    | hd::[] -> hd
    | _ -> mkpat (Ppat_tuple (List.rev lst))

  (* match lst with *)
  (*   | hd::[] -> hd *)
  (*   | _ -> mkpat (Ppat_tuple (List.rev lst)) *)

let mktailexp_extension nilloc seq ext_opt =
  let rec handle_seq = function
    [] ->
      let base_case = match ext_opt with
        | Some ext ->
          ext
        | None ->
          let loc = { nilloc with loc_ghost = true } in
          let nil = { txt = Lident "[]"; loc = loc } in
          Exp.mk ~loc (Pexp_construct (nil, None)) in
      base_case
  | e1 :: el ->
      let exp_el = handle_seq el in
      let loc = {loc_start = e1.pexp_loc.loc_start;
               loc_end = exp_el.pexp_loc.loc_end;
               loc_ghost = true}
      in
      let arg = Exp.mk ~loc (Pexp_tuple [e1; exp_el]) in
      mkexp_cons {loc with loc_ghost = true} arg loc in
  handle_seq seq

let mktailpat_extension nilloc seq ext_opt =
  let rec handle_seq = function
    [] ->
      let base_case = match ext_opt with
        | Some ext ->
          ext
        | None ->
          let loc = { nilloc with loc_ghost = true } in
          let nil = { txt = Lident "[]"; loc = loc } in
          Pat.mk ~loc (Ppat_construct (nil, None)) in
      base_case
  | p1 :: pl ->
      let pat_pl = handle_seq pl in
      let loc = {loc_start = p1.ppat_loc.loc_start;
               loc_end = pat_pl.ppat_loc.loc_end;
               loc_ghost = true}
      in
      let arg = Pat.mk ~loc (Ppat_tuple [p1; pat_pl]) in
      mkpat_cons {loc with loc_ghost = true} arg loc in
  handle_seq seq

let mktailexp nilloc seq =
  mktailexp_extension nilloc seq None

let mktailpat nilloc seq =
  mktailpat_extension nilloc seq None

(* Applies attributes to the structure item, not the expression itself. Makes
 * structure item have same location as expression. *)
let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = e.pexp_loc }

let mkexp_constraint e (t1, t2) =
  match t1, t2 with
  | Some t, None -> ghexp(Pexp_constraint(e, t))
  | _, Some t -> ghexp(Pexp_coerce(e, t1, t))
  | None, None -> assert false

let array_function str name =
  ghloc (Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name)))

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_num closing_name closing_num =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(rhs_loc opening_num, opening_name,
                                           rhs_loc closing_num, closing_name)))

let expecting pos nonterm =
    raise Syntaxerr.(Error(Expecting(rhs_loc pos, nonterm)))

let not_expecting pos nonterm =
    raise Syntaxerr.(Error(Not_expecting(rhs_loc pos, nonterm)))

let bigarray_function str name =
  ghloc (Ldot(Ldot(Lident "Bigarray", str), name))

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist; pexp_loc = _ } -> explist
  | exp -> [exp]

let bigarray_get arr arg =
  let get = if !Clflags.fast then "unsafe_get" else "get" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" get)),
                       ["", arr; "", c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" get)),
                       ["", arr; "", c1; "", c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" get)),
                       ["", arr; "", c1; "", c2; "", c3]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "get")),
                       ["", arr; "", ghexp(Pexp_array coords)]))

let bigarray_set arr arg newval =
  let set = if !Clflags.fast then "unsafe_set" else "set" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" set)),
                       ["", arr; "", c1; "", newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" set)),
                       ["", arr; "", c1; "", c2; "", newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" set)),
                       ["", arr; "", c1; "", c2; "", c3; "", newval]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "set")),
                       ["", arr;
                        "", ghexp(Pexp_array coords);
                        "", newval]))

let lapply p1 p2 =
  if !Clflags.applicative_functors
  then Lapply(p1, p2)
  else raise (Syntaxerr.Error(Syntaxerr.Applicative_path (symbol_rloc())))

let exp_of_label lbl pos =
  mkexp (Pexp_ident(mkrhs (Lident(Longident.last lbl)) pos))

let pat_of_label lbl pos =
  mkpat (Ppat_var (mkrhs (Longident.last lbl) pos))

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

(**
  I believe that wrap_type_annotation will automatically generate the type
  arguments (type a) (type b) based on what was listed before the dot in a
  polymorphic type annotation that uses locally abstract types.
 *)
let wrap_type_annotation newtypes core_type body =
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp =
    List.fold_right (fun newtype exp -> mkexp (Pexp_newtype (newtype, exp)))
      newtypes exp
  in
  (exp, ghtyp(Ptyp_poly(newtypes,varify_constructors newtypes core_type)))


let struct_item_extension (ext_attrs, ext_id) structure_item =
  ghstr (Pstr_extension ((ext_id, PStr [structure_item]), ext_attrs))

let extension_expression (ext_attrs, ext_id) item_expr =
  ghexp ~attrs:ext_attrs (Pexp_extension (ext_id, PStr [mkstrexp item_expr []]))

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
(*   | Some id -> ghexp(Pexp_extension (id, PStr [mkstrexp body []])) *)

(* Why not just mkexp with the right attributes in the first place? *)
(* let mkexp_attrs d attrs = *)
(*   wrap_exp_attrs (mkexp d) attrs *)

let mkcf_attrs d attrs =
  Cf.mk ~loc:(symbol_rloc()) ~attrs d

let mkctf_attrs d attrs =
  Ctf.mk ~loc:(symbol_rloc()) ~attrs d


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

let mklb (p, e) attrs =
  { lb_pattern = p;
    lb_expression = e;
    (* Only some individual let bindings are allowed to have attributes
     * depending on the context *)
    lb_attributes = attrs;
    (* lb_docs = symbol_docs_lazy (); *)
    (* lb_text = symbol_text_lazy (); *)
    lb_loc = symbol_rloc (); }

let mklbs (extAttrs, extId) rf lb =
  { lbs_bindings = [lb];
    lbs_rec = rf;
    lbs_extension = extId ;
    lbs_attributes = extAttrs;
    lbs_loc = symbol_rloc (); }

let addlb lbs lb =
  { lbs with lbs_bindings = lb :: lbs.lbs_bindings }

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
  let str = mkstr(Pstr_value(lbs.lbs_rec, List.rev bindings)) in
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
  let item_expr = mkexp (Pexp_let(lbs.lbs_rec, List.rev bindings, body)) in
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
    mkclass(Pcl_let (lbs.lbs_rec, List.rev bindings, body))


%}

/* Tokens */

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
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <int> INT
%token <int32> INT32
%token <int64> INT64
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
%token LESSMINUS
%token EQUALGREATER
%token LET
%token <string> LIDENT
%token LPAREN
%token LBRACKETAT
%token LBRACKETATAT
%token LBRACKETATATAT
%token SWITCH
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT
%token NEW
%token OBJECT
%token OF
%token OPEN
%token OR
/* %token PARSER */
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token OPTIONAL_NO_DEFAULT
%token EXPLICITLY_PASSED_OPTIONAL
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
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

Precedence and associativity:
----------------------------
- The list that follows this docblock determines precedence in order
  form low to high along with an optional associativity.
- Within each row in the list, the precedences are equivalent.
- Each entry's position in the list either determines the relative precedence of the
  mentioned token, or it creates a new "name" for a reusable precednece/associativity
  (such as below_SEMI).
  - By default, a rule has the precedence of its rightmost terminal (if any).
  - But a rule can use one of the reusable "names" for precedence/associativity
    from the list below.

Resolving conflicts in grammar:
-------------------------------
Different types of conflicts are resolved in different ways.
- Reduce/reduce conflict:
  - (Are not resolved using precedence?)
  - (Instead?) Resolved in favor of the first rule (in source file order).
- Shift/reduce conflict between rule and token being shifted:
  - Resolved by comparing the "precedence" and "associativity" of the
    token to be shifted with those of the rule to be reduced.
  - When the rule and the token have the same precedence, it is resolved
    using the associativity:
    - If the token is left-associative, the parser will reduce.
    - If the token is right-associative, the parser will shift.
    - If non-associative, the parser will declare a syntax error.
  - Question: What about when the rule to reduce has no precedence because it
  has no rightmost terminal?

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

*/

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc below_EQUALGREATER
%right    EQUALGREATER                  /* core_type2 (t => t => t) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%nonassoc below_QUESTION
%nonassoc QUESTION
%nonassoc COLON
%nonassoc FUN FUNCTION WITH              /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc below_AS
%nonassoc AS
%nonassoc below_BAR                     /* Allows "building up" of many bars */
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ /* expr (e OP e OP e) */
%left     PERCENT INFIXOP3 STAR                 /* expr (e OP e OP e) */
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
 *    %nonassoc below_LBRACKETAT
 *    %nonassoc LBRACKETAT
 *    %nonassoc LBRACKETATAT
 *    %right    COLONCOLON
 *    %left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ
 *    %left     PERCENT INFIXOP3 STAR
 *    %right    INFIXOP4
 *
 * So instead, with Reason, we treat all infix operators identically w.r.t.
 * attributes. In expressions, they have the same precedence as function
 * arguments, as if they are additional arguments to a function application.
 *
 * Note that prefix unary subtractive/plus parses with lower precedence than
 * function application (and attributes) This means that:
 *
 *   let = - something blah blah [@attr];
 *
 * Will have the attribute applied to the entire content to the right of the
 * unary minus, as if the attribute was merely another argument to the function
 * application.
 *
 * Where arrows occur, it will (as always) obey the rules of function/type
 * application.
 *
 *   type x = int => int [@onlyAppliedToTheInt];
 *   type x = (int => int) [@appliedToTheArrow];
 *
 */

%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
/* Now that commas require wrapping parens (for tuples), prec_constr_appl no
* longer needs to be above COMMA, but it doesn't hurt */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_NEWDOT
%nonassoc below_DOT_AND_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT

%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%nonassoc LBRACKETATAT

/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT
          LBRACKETPERCENT LBRACKETPERCENTPERCENT


/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file
%start parse_core_type
%type <Parsetree.core_type> parse_core_type
%start parse_expression
%type <Parsetree.expression> parse_expression
%start parse_pattern
%type <Parsetree.pattern> parse_pattern
%%

/* Entry points */

implementation:
    structure EOF                        { $1 }
;
interface:
    signature EOF                        { $1 }
;
toplevel_phrase:
    top_structure                        { Ptop_def $1 }
  | toplevel_directive SEMISEMI          { $1 }
  | EOF                                  { raise End_of_file }
;
/* See note about deprecating SEMISEMI */
top_structure:
    /* empty */                            { [] }
  | structure_item SEMI top_structure      { $1 :: $3 }
;
use_file:
  SEMISEMI SEMISEMI {[]}
  /*
    use_file_tail                        { $1 }
  | expr post_item_attributes use_file_tail
                                         { Ptop_def[mkstrexp $1 $2] :: $3 }
  */
;
/*
use_file_tail:
    EOF                                       { [] }
  | SEMISEMI EOF                              { [] }
  | SEMISEMI expr post_item_attributes use_file_tail
                                              { Ptop_def[mkstrexp $2 $3] :: $4 }
  | SEMISEMI structure_item use_file_tail     { Ptop_def[$2] :: $3 }
  | SEMISEMI toplevel_directive use_file_tail { $2 :: $3 }
  | structure_item use_file_tail              { Ptop_def[$1] :: $2 }
  | toplevel_directive use_file_tail          { $1 :: $2 }
;
*/

parse_core_type:
    core_type EOF { $1 }
;
parse_expression:
    expr EOF { $1 }
;
parse_pattern:
    pattern EOF { $1 }
;

/* Module expressions */

functor_arg:
    LPAREN RPAREN
      { mkrhs "*" 2, None }
  | LPAREN functor_arg_name COLON module_type RPAREN
      { mkrhs $2 2, Some $4 }
;

functor_arg_name:
    UIDENT     { $1 }
  | UNDERSCORE { "_" }
;

functor_args:
    functor_args functor_arg
      { $2 :: $1 }
  | functor_arg
      { [ $1 ] }
;

simple_module_expr:
    mod_longident
      { mkmod(Pmod_ident (mkrhs $1 1)) }
  | LBRACE structure RBRACE
      { mkmod(Pmod_structure($2)) }
  | LPAREN module_expr COLON module_type RPAREN
      { mkmod(Pmod_constraint($2, $4)) }
  | LPAREN module_expr COLON module_type error
      { unclosed "(" 1 ")" 5 }
  | LPAREN module_expr RPAREN
      { $2 }
  | LPAREN VAL expr RPAREN
      { mkmod(Pmod_unpack $3) }
  | LPAREN VAL expr COLON package_type RPAREN
      { mkmod(Pmod_unpack(
              ghexp(Pexp_constraint($3, ghtyp(Ptyp_package $5))))) }
  | LPAREN VAL expr COLON package_type COLONGREATER package_type RPAREN
      { mkmod(Pmod_unpack(
              ghexp(Pexp_coerce($3, Some(ghtyp(Ptyp_package $5)),
                                    ghtyp(Ptyp_package $7))))) }
  | LPAREN VAL expr COLONGREATER package_type RPAREN
      { mkmod(Pmod_unpack(
              ghexp(Pexp_coerce($3, None, ghtyp(Ptyp_package $5))))) }
  | LPAREN RPAREN
      { mkmod (Pmod_structure []) }
  | extension
      { mkmod(Pmod_extension $1) }
;

module_expr:
    simple_module_expr
      { $1 }
  | LBRACE structure error
      { unclosed "struct" 1 "end" 3 }
  /**
   * Although it would be nice (and possible) to support annotated return value
   * here, that wouldn't be consistent with what is possible for functions.
   * Update: In upstream, it *is* possible to annotate return values for
   * lambdas.
   */
  | FUNCTOR functor_args EQUALGREATER module_expr
      { mkFunctorThatReturns $2 $4 }
  | module_expr simple_module_expr
      { mkmod(Pmod_apply($1, $2)) }
  | module_expr LPAREN module_expr error
      { unclosed "(" 2 ")" 4 }
  | LPAREN module_expr error
      { unclosed "(" 1 ")" 3 }
  | LPAREN VAL expr COLON error
      { unclosed "(" 1 ")" 5 }
  | LPAREN VAL expr COLONGREATER error
      { unclosed "(" 1 ")" 5 }
  | LPAREN VAL expr error
      { unclosed "(" 1 ")" 4 }
  | module_expr attribute
      { Mod.attr $1 $2 }
;

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
 *      seq_expr post_item_attributes structure_tail { mkstrexp $1 $2 :: $3 }
 *    | structure_tail { $1 }
 *  ;
 *  structure_tail:
 *                           { [] }
 *    | SEMISEMI structure   { $2 }
 *    | structure_item SEMI structure_tail { $1 :: $3 }
 */

structure:

  | /* Empty */           {[]}
  | structure_item { [$1] }
  | structure_item SEMI structure { $1 :: $3 }
;

structure_item:
  | item_extension_sugar structure_item_without_item_extension_sugar {
    struct_item_extension $1 $2
  }
  | structure_item_without_item_extension_sugar {
    $1
  }
  /* Each let binding has its own post_item_attributes */
  | let_bindings { val_of_let_bindings $1 }
;
structure_item_without_item_extension_sugar:
  /* We consider a floating expression to be equivalent to a single let binding
     to the "_" (any) pattern.  */
  | expr post_item_attributes {
      {pstr_desc = Pstr_eval($1, $2); pstr_loc=symbol_rloc()}
    }
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration post_item_attributes {
      mkstr
        (Pstr_primitive (Val.mk (mkrhs $2 2) $4 ~prim:$6 ~attrs:$7 ~loc:(symbol_rloc ())))
    }
  | many_type_declarations {
      mkstr(Pstr_type (List.rev $1))
    }
  | str_type_extension {
      mkstr(Pstr_typext $1)
    }
  | str_exception_declaration {
      mkstr(Pstr_exception $1)
    }
  | LET MODULE nonlocal_module_binding_details post_item_attributes {
      let (ident, body) = $3 in
      mkstr(Pstr_module (Mb.mk ident body ~attrs:$4 ~loc:(symbol_rloc ())))
    }
  | many_nonlocal_module_bindings {
      mkstr(Pstr_recmodule(List.rev $1))
    }
  | MODULE TYPE ident post_item_attributes {
      let item_attrs = $4 in
      let ident = (mkrhs $3 3) in
      mkstr(Pstr_modtype (Mtd.mk ident ~attrs:item_attrs ~loc:(symbol_rloc())))
    }
  | MODULE TYPE ident EQUAL module_type post_item_attributes {
      let ident = (mkrhs $3 3) in
      mkstr(Pstr_modtype (Mtd.mk ident ~typ:$5 ~attrs:$6 ~loc:(symbol_rloc())))
    }
  | open_statement {
      mkstr(Pstr_open $1)
    }
  | many_class_declarations {
      (* Each declaration has their own preceeding post_item_attributes *)
      mkstr(Pstr_class (List.rev $1))
    }
  | many_class_type_declarations {
      (* Each declaration has their own preceeding post_item_attributes *)
      mkstr(Pstr_class_type (List.rev $1))
    }
  | INCLUDE module_expr post_item_attributes {
      mkstr(Pstr_include (Incl.mk $2 ~attrs:$3 ~loc:(symbol_rloc())))
    }
  | item_extension post_item_attributes {
    (* No sense in having item_extension_sugar for something that's already an
     * item_extension *)
    mkstr(Pstr_extension ($1, $2))
  }
  | floating_attribute
      { mkstr(Pstr_attribute $1) }
;

module_binding_body_expr:
    EQUAL module_expr
      { $2 }
  | COLON module_type EQUAL module_expr
      { mkmod(Pmod_constraint($4, $2)) }
;

module_binding_body_functor:
  | functor_args EQUALGREATER module_expr {
      mkFunctorThatReturns $1 $3
    }
  | functor_args COLON non_arrowed_module_type EQUALGREATER module_expr {
      mkFunctorThatReturns $1 (mkmodRhs (Pmod_constraint($5, $3)) 5)
    }
;

module_binding_body:
    module_binding_body_expr { $1 }
  | module_binding_body_functor { $1 }

many_nonlocal_module_bindings:
  | LET MODULE REC nonlocal_module_binding_details post_item_attributes {
    let (ident, body) = $4 in
    [Mb.mk ident body ~attrs:$5 ~loc:(symbol_rloc ())]
  }
  | many_nonlocal_module_bindings and_nonlocal_module_bindings {
    $2::$1
  }
;

and_nonlocal_module_bindings:
  | AND nonlocal_module_binding_details post_item_attributes {
    let (ident, body) = $2 in
    Mb.mk ident body ~attrs:$3 ~loc:(symbol_rloc ())
  }
;

nonlocal_module_binding_details:
    UIDENT module_binding_body{
      ((mkrhs $1 1), $2)
    }
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
  | simple_module_type {$1}
  | MODULE TYPE OF module_expr %prec below_LBRACKETAT
      { mkmty(Pmty_typeof $4) }
  | module_type attribute
      { Mty.attr $1 $2 }

simple_module_type:
  | LPAREN module_type RPAREN
      { $2 }
  | LPAREN module_type error
      { unclosed "(" 1 ")" 3 }
  | mty_longident
    { mkmty(Pmty_ident (mkrhs $1 1)) }
  | LBRACE signature RBRACE
      { mkmty(Pmty_signature $2) }
  | LBRACE signature error
      { unclosed "sig" 1 "}" 3 }
/*  | LPAREN MODULE mod_longident RPAREN
      { mkmty (Pmty_alias (mkrhs $3 3)) } */
  | extension
      { mkmty(Pmty_extension $1) }

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
  | module_type WITH with_constraints {
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
    mkmty(Pmty_with($1, List.rev $3))
  }
  | non_arrowed_module_type %prec below_EQUALGREATER {
    (* below_EQUALGREATER to prevent following shift reduce conflict:
     *  1158: shift/reduce conflict (shift 1285, reduce 75) on EQUALGREATER
     *  state 1158
     *    module_binding_body_functor : functor_args COLON non_arrowed_module_type . EQUALGREATER module_expr  (59)
     *    module_type : non_arrowed_module_type .  (75)
     *
     *    EQUALGREATER  shift 1285
     *    LBRACKETAT  reduce 75
     *    WITH  reduce 75
     *)
    $1
  }
  | LPAREN functor_arg_name COLON module_type RPAREN EQUALGREATER module_type %prec below_WITH {
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
      mkmty(Pmty_functor(mkrhs $2 2, Some $4, $7))
     }
  | module_type EQUALGREATER module_type %prec below_WITH {
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
      mkmty(Pmty_functor(ghloc "_", Some $1, $3))
    }
;
signature:
    /* empty */          { [] }
  | signature_item SEMI signature { $1 :: $3 }
;

signature_item:
    LET val_ident COLON core_type post_item_attributes {
        mksig(Psig_value (Val.mk (mkrhs $2 2) $4 ~attrs:$5 ~loc:(symbol_rloc())))
    }
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration post_item_attributes {
      mksig(Psig_value (Val.mk (mkrhs $2 2) $4 ~prim:$6 ~attrs:$7 ~loc:(symbol_rloc())))
    }
  | many_type_declarations {
      mksig(Psig_type (List.rev $1))
    }
  | sig_type_extension {
      mksig(Psig_typext $1)
    }
  | sig_exception_declaration {
      mksig(Psig_exception $1)
    }
  | LET MODULE UIDENT module_declaration post_item_attributes {
      mksig(Psig_module (Md.mk (mkrhs $3 3) $4 ~attrs:$5 ~loc:(symbol_rloc())))
    }
  | LET MODULE UIDENT EQUAL mod_longident post_item_attributes {
      mksig(
        Psig_module (
          Md.mk
            (mkrhs $3 3)
            (Mty.alias ~loc:(rhs_loc 5) (mkrhs $5 5))
            ~attrs:$6
            ~loc:(symbol_rloc())
        )
      )
    }
  | many_module_rec_declarations {
      mksig(Psig_recmodule (List.rev $1))
    }
  | MODULE TYPE ident post_item_attributes {
      mksig(Psig_modtype (Mtd.mk (mkrhs $3 3) ~attrs:$4 ~loc:(symbol_rloc())))
    }
  | MODULE TYPE ident EQUAL module_type post_item_attributes {
      mksig(Psig_modtype (Mtd.mk (mkrhs $3 3) ~typ:$5 ~loc:(symbol_rloc()) ~attrs:$6))
    }
  | open_statement {
      mksig(Psig_open $1)
    }
  | INCLUDE module_type post_item_attributes %prec below_WITH {
      mksig(Psig_include (Incl.mk $2 ~attrs:$3 ~loc:(symbol_rloc())))
    }
  | many_class_descriptions {
      mksig(Psig_class (List.rev $1))
    }
  | many_class_type_declarations {
      mksig(Psig_class_type (List.rev $1))
    }
  | item_extension post_item_attributes {
      mksig(Psig_extension ($1, $2))
    }
  | floating_attribute {
      mksig(Psig_attribute $1)
    }
;
open_statement:
  | OPEN override_flag mod_longident post_item_attributes
      { Opn.mk (mkrhs $3 3) ~override:$2 ~attrs:$4 ~loc:(symbol_rloc()) }
;
module_declaration:
    COLON module_type
      { $2 }
  | LPAREN UIDENT COLON module_type RPAREN module_declaration
      { mkmty(Pmty_functor(mkrhs $2 2, Some $4, $6)) }
  | LPAREN RPAREN module_declaration
      { mkmty(Pmty_functor(mkrhs "*" 1, None, $3)) }
;

module_rec_declaration_details:
  | UIDENT COLON module_type { ((mkrhs $1 1), $3) }
;

many_module_rec_declarations:
  | LET MODULE REC module_rec_declaration_details post_item_attributes {
      let (ident, body) = $4 in
      [Md.mk ident body ~attrs:$5 ~loc:(symbol_rloc())]
    }
  | many_module_rec_declarations and_module_rec_declaration  {
      $2::$1
    }
;

and_module_rec_declaration:
  | AND module_rec_declaration_details post_item_attributes {
      let (ident, body) = $2 in
      Md.mk ident body ~attrs:$3 ~loc:(symbol_rloc())
    }
;


/* Class expressions */

many_class_declarations:
  | CLASS class_declaration_details post_item_attributes {
    let (ident, binding, virt, params) = $2 in
    [Ci.mk ident binding ~virt ~params ~attrs:$3 ~loc:(symbol_rloc ())]
  }
  | many_class_declarations and_class_declaration {
    $2::$1
  }
;

and_class_declaration:
  AND class_declaration_details post_item_attributes {
    let (ident, binding, virt, params) = $2 in
    Ci.mk ident binding ~virt ~params ~attrs:$3 ~loc:(symbol_rloc ())
  }
;

/* Having a rule makes it easier to correctly apply the location */
constrained_class_declaration:
   | COLON class_constructor_type EQUAL class_expr
      { mkclass(Pcl_constraint($4, $2)) }

class_declaration_details:
  /*
     Used in order to parse: class ['a, 'b] myClass argOne argTwo (:retType) => cl_expr
   */
  | virtual_flag LIDENT class_type_parameters class_fun_binding
      {
       ((mkrhs $2 2), $4, $1, List.rev $3)
      }
  /*
     We make a special rule here because we only accept colon after the
     identifier - can't be a part of class_fun_binding. Used in order to parse:
     class ['a, 'b] myClass: class_constructor_type = class_expr
   */
  | virtual_flag LIDENT class_type_parameters constrained_class_declaration
    {
       ((mkrhs $2 2), $4, $1, List.rev $3)
    }

  /*
     Used in order to parse:  class ['a, 'b] myClass = class_expr
   */
  | virtual_flag LIDENT class_type_parameters EQUAL class_expr
    {
       ((mkrhs $2 2), $5, $1, List.rev $3)
    }
;

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
  | labeled_simple_pattern class_fun_binding
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $2)) }
  | labeled_simple_pattern class_fun_return
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $2)) }
;

class_fun_return:
    EQUALGREATER class_expr
      { $2 }
  | COLON non_arrowed_class_constructor_type EQUALGREATER class_expr
      { mkclass(Pcl_constraint($4, $2)) }
;

class_type_parameters:
  /* Empty */ { [] }
  | class_type_parameters type_parameter {
      $2::$1
    }
;
class_fun_def:
  | labeled_simple_pattern EQUALGREATER class_expr
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $3)) }
  | labeled_simple_pattern class_fun_def
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $2)) }
;

class_expr_lets_and_rest:
  /* expr will present a conflict */
  /* | expr opt_semi  { reloc_exp $1 } */
  | class_self_pattern_and_structure { mkclass(Pcl_structure $1)}
  | class_expr {$1}
  | let_bindings SEMI class_expr_lets_and_rest {
    class_of_let_bindings $1 $3
  }
;

class_self_pattern:
  /* Empty is by default the pattern identifier [this] */
  | { mk_ghost_patvar "this" 0 }
  /* Whereas in OCaml, specifying nothing means "_", in Reason, you'd
     have to explicity specify "_" (any) pattern. In Reason, writing nothing
     is how you specify the "this" pattern. */
  | AS pattern SEMI
      { reloc_pat $2 }
;

class_self_pattern_and_structure:
  | class_self_pattern semi_terminated_class_fields
    { Cstr.mk $1 $2 }
;

class_expr:
    class_simple_expr
      { $1 }
  | FUN class_fun_def
      { $2 }
  | class_simple_expr simple_labeled_expr_list
      /**
       * This is an interesting way to "partially apply" class construction:
       *
       * let inst = new oldclass 20;
       * class newclass = oldclass withInitArg;
       * let inst = new newclass;
       */
      { mkclass(Pcl_apply($1, List.rev $2)) }
  | class_expr attribute
      { Cl.attr $1 $2 }

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
  | CLASS class_longident {
      mkclass(Pcl_constr(mkloc $2 (rhs_loc 2), []))
    }

  | CLASS class_longident non_arrowed_simple_core_type_list {
      mkclass(Pcl_constr(mkloc $2 (rhs_loc 2), List.rev $3))
    }

  | extension
      { mkclass(Pcl_extension $1) }
;
class_simple_expr:
  | class_longident
    { mkclass(Pcl_constr(mkrhs $1 1, [])) }
  | LBRACE class_expr_lets_and_rest RBRACE { reloc_class $2 }
  | LBRACE class_expr_lets_and_rest error
    { unclosed "{" 1 "}" 3 }
  | LPAREN class_expr COLON class_constructor_type RPAREN
      { mkclass(Pcl_constraint($2, $4)) }
  | LPAREN class_expr COLON class_constructor_type error
      { unclosed "(" 1 ")" 5 }
  | LPAREN class_expr RPAREN
      { reloc_class $2 }
  | LPAREN class_expr error
      { unclosed "(" 1 ")" 3 }
;

class_field:
  | INHERIT override_flag class_expr parent_binder post_item_attributes
      { mkcf_attrs (Pcf_inherit ($2, $3, $4)) $5 }
  | VAL value post_item_attributes
      { mkcf_attrs (Pcf_val $2) $3 }
  | METHOD method_ post_item_attributes
      { mkcf_attrs (Pcf_method $2) $3 }
  | CONSTRAINT constrain_field post_item_attributes
      { mkcf_attrs (Pcf_constraint $2) $3 }
  | INITIALIZER EQUALGREATER expr post_item_attributes
      { mkcf_attrs (Pcf_initializer $3) $4 }
  | item_extension post_item_attributes
      { mkcf_attrs (Pcf_extension $1) $2 }
  | floating_attribute
      { mkcf (Pcf_attribute $1) }
;

/* Don't need opt_semi here because of the empty rule (which normally doesn't
happen for things like records */
semi_terminated_class_fields:
  | /* Empty */           {[]}
  | class_field           { [reloc_class_field $1] }
  | class_field SEMI semi_terminated_class_fields   { $1::$3 }
;

parent_binder:
    AS LIDENT
          { Some $2 }
  | /* empty */
          { None }
;
value:
/* TODO: factorize these rules (also with method): */
    override_flag MUTABLE VIRTUAL label COLON core_type {
      if $1 = Override then not_expecting 7 "not expecting equal - cannot specify value for virtual val"
      else mkloc $4 (rhs_loc 4), Mutable, Cfk_virtual $6
    }
  | override_flag MUTABLE VIRTUAL label COLON core_type EQUAL
      { not_expecting 7 "not expecting equal - cannot specify value for virtual val" }
  | VIRTUAL mutable_flag label COLON core_type
      { mkrhs $3 3, $2, Cfk_virtual $5 }
  | VIRTUAL mutable_flag label COLON core_type EQUAL
      { not_expecting 6 "not expecting equal - cannot specify value for virtual val" }
  | override_flag mutable_flag label EQUAL expr
      { mkrhs $3 3, $2, Cfk_concrete ($1, $5) }
  | override_flag mutable_flag label type_constraint EQUAL expr
      {
       let e = mkexp_constraint $6 $4 in
       mkrhs $3 3, $2, Cfk_concrete ($1, e)
      }
;
method_:
/* TODO: factorize those rules... */
    override_flag PRIVATE VIRTUAL label COLON poly_type
      { if $1 = Override then syntax_error ();
        mkloc $4 (rhs_loc 4), Private, Cfk_virtual $6 }
  | override_flag VIRTUAL private_flag label COLON poly_type
      { if $1 = Override then syntax_error ();
        mkloc $4 (rhs_loc 4), $3, Cfk_virtual $6 }
  | override_flag private_flag label curried_binding
      { mkloc $3 (rhs_loc 3), $2,
        Cfk_concrete ($1, ghexp(Pexp_poly ($4, None))) }
  | override_flag private_flag label COLON poly_type EQUAL expr
      /* Without locally abstract types, you'll see a Ptyp_poly in the Pexp_poly */
      { mkloc $3 (rhs_loc 3), $2,
        Cfk_concrete ($1, ghexp(Pexp_poly($7, Some $5))) }
  | override_flag private_flag label EQUAL expr
      { mkloc $3 (rhs_loc 3), $2,
        Cfk_concrete ($1, ghexp(Pexp_poly($5, None))) }
  | override_flag private_flag label COLON TYPE lident_list DOT core_type EQUAL expr
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
        let (exp_with_newtypes_constrained_by_non_varified, poly_type_varified) =
          wrap_type_annotation $6 $8 $10 in
        (
          mkloc $3 (rhs_loc 3),
          $2,
          Cfk_concrete (
            $1,
            ghexp(Pexp_poly(exp_with_newtypes_constrained_by_non_varified, Some poly_type_varified))
          )
        )
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


    When in signatures:
    A Class Definition merely includes the type of that classes constructor.
    [class myClass: args => new classType;]

    A Class Type Definition is specified exactly as it is in a module (of
    course, it may be abstract).


    Here are the parsing rules for everything discussed above:

     (Note: I have renamed class_type from the upstream compiler to
     class_constructor_type and class_signature to class_instance_type for
     clarity)

     constructor_type:
        | class_instance_type
        | constructorFunction => typeWithArrows => class_instance_type

     Modules
       Class Definitions
       many_class_declarations
         | class LIDENT = class_expr
         | class LIDENT = (class_expr : constructor_type)
         "Pstr_class"

       Class Type Definition
       many_class_type_declarations
         CLASS TYPE ident = class_instance_type
         "Pstr_class_type"

     Signatures
       Class Descriptions (describes Class Definitions in Module)
       many_class_descriptions
         CLASS ident : constructor_type
         "Psig_class"

       Class Type Declarations (subset of Class Type Definitions in Module)
       many_class_type_declarations
         CLASS TYPE ident = class_instance_type
        "Psig_class_type"

 */
class_constructor_type:
  | NEW class_instance_type
      { $2 }
  | LIDENT EXPLICITLY_PASSED_OPTIONAL non_arrowed_core_type EQUALGREATER class_constructor_type
      { mkcty(Pcty_arrow("?" ^ $1, mkoption $3, $5)) }
  | LIDENT COLONCOLON non_arrowed_core_type EQUALGREATER class_constructor_type
      { mkcty(Pcty_arrow($1, $3, $5)) }
  | non_arrowed_core_type EQUALGREATER class_constructor_type
      { mkcty(Pcty_arrow("", $1, $3)) }
;

non_arrowed_class_constructor_type:
  | class_instance_type
      { $1 }
  | LPAREN class_constructor_type RPAREN { reloc_class_type $2}
;

class_instance_type:
  | clty_longident
      { mkcty(Pcty_constr (mkrhs $1 1, [])) }
  | clty_longident non_arrowed_simple_core_type_list
      { mkcty(Pcty_constr (mkloc $1 (rhs_loc 1), List.rev $2)) }
  | LBRACE class_sig_body RBRACE
      { mkcty(Pcty_signature $2) }
  | LBRACE class_sig_body error
      { unclosed "{" 1 "}" 3 }
  | class_instance_type attribute
      /* Note that this will compound attributes - so they will become
         attached to whatever */
      { Cty.attr $1 $2 }
  | extension
      { mkcty(Pcty_extension $1) }
;


class_sig_body:
    class_self_type class_sig_fields opt_semi
    { Csig.mk $1 (List.rev $2 )}
;
class_self_type:
    AS core_type SEMI
      { $2 }
  | /* empty */
      { mktyp(Ptyp_any) }
;
class_sig_fields:
    /* empty */                         { [] }
| class_sig_field { [$1] }
| class_sig_fields SEMI class_sig_field { $3 :: $1 }
;
class_sig_field:
  /* The below_LBRACKETAT and two forms below are needed (but not in upstream
     for some reason) */
  | INHERIT class_instance_type %prec below_LBRACKETAT
      { mkctf_attrs (Pctf_inherit $2) [] }
  | INHERIT class_instance_type item_attribute post_item_attributes
      { mkctf_attrs (Pctf_inherit $2) ($3::$4) }
  | VAL value_type post_item_attributes {
      mkctf_attrs (Pctf_val $2) $3
    }
  | METHOD private_virtual_flags label COLON poly_type post_item_attributes
       {
        let (p, v) = $2 in
        mkctf_attrs (Pctf_method ($3, p, v, $5)) $6
       }
  | CONSTRAINT constrain_field post_item_attributes
       { mkctf_attrs (Pctf_constraint $2) $3 }
  | item_extension post_item_attributes
       { mkctf_attrs (Pctf_extension $1) $2 }
  | floating_attribute
      { mkctf(Pctf_attribute $1) }
;
value_type:
    VIRTUAL mutable_flag label COLON core_type
      { $3, $2, Virtual, $5 }
  | MUTABLE virtual_flag label COLON core_type
      { $3, Mutable, $2, $5 }
  | label COLON core_type
      { $1, Immutable, Concrete, $3 }
;
constrain:
        core_type EQUAL core_type          { $1, $3, symbol_rloc() }
;
constrain_field:
        core_type EQUAL core_type          { $1, $3 }
;
many_class_descriptions:
  | CLASS class_description_details post_item_attributes {
    let (ident, binding, virt, params) = $2 in
    [Ci.mk ident binding ~virt ~params ~attrs:$3 ~loc:(symbol_rloc ())]
  }
  | many_class_descriptions and_class_description {
    $2 :: $1
  }
;
and_class_description:
  | AND class_description_details post_item_attributes {
    let (ident, binding, virt, params) = $2 in
    Ci.mk ident binding ~virt ~params ~attrs:$3 ~loc:(symbol_rloc ())
  }
;
class_description_details:
  | virtual_flag LIDENT class_type_parameters COLON class_constructor_type {
    ((mkrhs $2 2), $5, $1, List.rev $3)
  }
;

many_class_type_declarations:
  | CLASS TYPE class_type_declaration_details post_item_attributes {
    let (ident, instance_type, virt, class_type_params) = $3 in
    [Ci.mk ident instance_type ~virt:virt ~params:class_type_params ~attrs:$4 ~loc:(symbol_rloc ())]
  }
  | many_class_type_declarations and_class_type_declaration {
    $2::$1
  }
;

and_class_type_declaration:
  AND class_type_declaration_details post_item_attributes {
    let (ident, instance_type, virt, class_type_params) = $2 in
    Ci.mk ident instance_type ~virt:virt ~params:class_type_params ~attrs:$3 ~loc:(symbol_rloc ())
  }
;

class_type_declaration_details:
    virtual_flag LIDENT class_type_parameters EQUAL class_instance_type {
      ((mkrhs $2 2), $5, $1, List.rev $3)
    }
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
  | item_extension_sugar semi_terminated_seq_expr_row {
      extension_expression $1 $2
    }
  | semi_terminated_seq_expr_row {
      $1
    }
  /**
   * Let bindings already have their potential item_extension_sugar.
   */
  | let_bindings SEMI semi_terminated_seq_expr {
      expr_of_let_bindings $1 $3
    }
;
semi_terminated_seq_expr_row:
  /**
   * Expression SEMI
   */
  | expr post_item_attributes opt_semi  {
      let expr = reloc_exp $1 in
      let item_attrs = $2 in
      (* Final item in the sequence - just append item attributes to the
       * expression attributes *)
      {expr with pexp_attributes = item_attrs @ expr.pexp_attributes}
    }
  | LET MODULE UIDENT module_binding_body post_item_attributes SEMI semi_terminated_seq_expr {
      let item_attrs = $5 in
      mkexp ~attrs:item_attrs (Pexp_letmodule(mkrhs $3 3, $4, $7))
    }
  | LET OPEN override_flag mod_longident post_item_attributes SEMI semi_terminated_seq_expr {
      let item_attrs = $5 in
      mkexp ~attrs:item_attrs (Pexp_open($3, mkrhs $4 4, $7))
    }
  | expr post_item_attributes SEMI semi_terminated_seq_expr  {
      let item_attrs = $2 in
      mkexp ~attrs:item_attrs (Pexp_sequence($1, $4))
    }

;

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
   /* Case A, B, C, D */
  | LIDENT COLONCOLON simple_pattern
      { ($1, None, $3) }
   /* Case E, F, G, H */
  | LIDENT COLONCOLON simple_pattern OPTIONAL_NO_DEFAULT
      { ("?" ^ $1, None, $3) }
   /* Case I, J, K, L */
  | LIDENT COLONCOLON simple_pattern EQUAL simple_expr
      { ("?" ^ $1, Some $5, $3) }
  | simple_pattern
      { ("", None, $1) }
;

/*
 * Much like how patterns are partitioned into pattern/simple_pattern,
 * expressions are divided into expr/simple_expr.
 * expr: contains function application, but simple_expr doesn't (unless it's
 * wrapped in parens).
 */
expr:
  /** One reason for below_NEWDOT (in [less_aggressive_simple_expression] and
    * others) is so that Module.Long.ident does not cause `Module` to be parsed
    * as a simple expression. This precedence causes the parser to "wait" and
    * build up the full long identifier before reducing it to a simple_expr (in
    * the case of constr_longident and potentially others).  This precedence quirk
    * has been consolidated into less_aggressive_simple_expression
   */
    less_aggressive_simple_expression
      { $1 }
  | less_aggressive_simple_expression simple_labeled_expr_list
      { mkexp(Pexp_apply($1, List.rev $2)) }
  | FUN labeled_simple_pattern fun_def {
      let (l,o,p) = $2 in
      mkexp (Pexp_fun(l, o, p, $3))
    }
  | FUN LPAREN TYPE LIDENT RPAREN fun_def
      { mkexp (Pexp_newtype($4, $6)) }
  /* List style rules like this often need a special precendence
     such as below_BAR in order to let the entire list "build up"
   */
  | FUN BAR no_leading_bar_match_cases %prec below_BAR
      { mkexp (Pexp_function(List.rev $3)) }
  | SWITCH simple_expr LBRACE BAR no_leading_bar_match_cases RBRACE
      { mkexp (Pexp_match($2, List.rev $5)) }
  | TRY simple_expr LBRACE BAR no_leading_bar_match_cases RBRACE
      { mkexp (Pexp_try($2, List.rev $5)) }
  | TRY simple_expr WITH error
      { syntax_error() }
  | constr_longident simple_non_labeled_expr_list_as_tuple
    {
      mkExplicitArityTupleExp (Pexp_construct(mkrhs $1 1, Some $2))
    }
  | name_tag simple_expr
    {
      mkexp(Pexp_variant($1, Some $2))
    }
  | IF simple_expr simple_expr ELSE expr
      { mkexp(Pexp_ifthenelse($2, $3, Some $5)) }
  | IF simple_expr simple_expr
      { mkexp (Pexp_ifthenelse($2, $3, None)) }
  | WHILE simple_expr simple_expr
      { mkexp (Pexp_while($2, $3)) }
  | FOR pattern IN simple_expr direction_flag simple_expr simple_expr
      { mkexp(Pexp_for($2, $4, $6, Upto, $7)) }

  | LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
      { mkexp_cons (rhs_loc 2) (ghexp(Pexp_tuple[$5;$7])) (symbol_rloc()) }
  | expr INFIXOP0 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP1 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP2 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP3 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP4 expr
      { mkinfix $1 $2 $3 }
  | expr PLUS expr
      { mkinfix $1 "+" $3 }
  | expr PLUSDOT expr
      { mkinfix $1 "+." $3 }
  | expr PLUSEQ expr
      { mkinfix $1 "+=" $3 }
  | expr MINUS expr
      { mkinfix $1 "-" $3 }
  | expr MINUSDOT expr
      { mkinfix $1 "-." $3 }
  | expr STAR expr
      { mkinfix $1 "*" $3 }
  | expr PERCENT expr
      { mkinfix $1 "%" $3 }
  | expr EQUAL expr
      { mkinfix $1 "=" $3 }
  | expr LESS expr
      { mkinfix $1 "<" $3 }
  | expr GREATER expr
      { mkinfix $1 ">" $3 }
  | expr OR expr
      { mkinfix $1 "or" $3 }
  | expr BARBAR expr
      { mkinfix $1 "||" $3 }
  | expr AMPERSAND expr
      { mkinfix $1 "&" $3 }
  | expr AMPERAMPER expr
      { mkinfix $1 "&&" $3 }
  | expr COLONEQUAL expr
      { mkinfix $1 ":=" $3 }
  | subtractive expr %prec prec_unary_minus
      { mkuminus $1 $2 }
  | additive expr %prec prec_unary_plus
      { mkuplus $1 $2 }
  | simple_expr DOT label_longident LESSMINUS expr
      { mkexp(Pexp_setfield($1, mkrhs $3 3, $5)) }
  | simple_expr DOT LPAREN expr RPAREN LESSMINUS expr
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "set")),
                         ["",$1; "",$4; "",$7])) }
  | simple_expr DOT LBRACKET expr RBRACKET LESSMINUS expr
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "String" "set")),
                         ["",$1; "",$4; "",$7])) }
  | simple_expr DOT LBRACE expr RBRACE LESSMINUS expr
      { bigarray_set $1 $4 $7 }
  | label LESSMINUS expr
      { mkexp(Pexp_setinstvar(mkrhs $1 1, $3)) }
  | ASSERT simple_expr %prec below_NEWDOT
      { mkexp (Pexp_assert $2) }
  | LAZY simple_expr %prec below_NEWDOT
      { mkexp (Pexp_lazy $2) }
  /*
   * Ternary is just a shortcut for:
   *
   *     switch expression { | true => expr1 | false => expr2 }
   *
   * The prec below_QUESTION is so that the following parses:
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
  | expr QUESTION expr COLON expr %prec below_QUESTION {
      (* Should use ghost expressions, but not sure how that would work with source maps *)
      (* So ? will become true and : becomes false for now*)
      let fauxTruePat =
        Pat.mk ~loc:(rhs_loc 2) (Ppat_construct({txt = Lident "true"; loc = rhs_loc 2}, None)) in
      let fauxFalsePat =
        Pat.mk ~loc:(rhs_loc 4) (Ppat_construct({txt = Lident "false"; loc = rhs_loc 4}, None)) in
      let fauxMatchCaseTrue = Exp.case fauxTruePat $3 in
      let fauxMatchCaseFalse = Exp.case fauxFalsePat $5 in
      let fauxSwitch = mkexp (Pexp_match ($1, [fauxMatchCaseTrue; fauxMatchCaseFalse])) in
      fauxSwitch
    }
  | expr attribute
      { Exp.attr $1 $2 }
;
simple_expr:
    val_longident
      { mkexp(Pexp_ident (mkrhs $1 1)) }
  | constant
      { mkexp(Pexp_constant $1) }
  /* Not sure why this couldn't have just been below_NEWDOT (Answer: Being
   * explicit about needing to wait for "as") */
  | constr_longident %prec prec_constant_constructor
    {
      mkexp (Pexp_construct(mkrhs $1 1, None))
    }

  | name_tag %prec prec_constant_constructor
      { mkexp(Pexp_variant($1, None)) }
  | LPAREN expr RPAREN
      { reloc_exp $2 }
  | LPAREN expr error
      { unclosed "(" 1 ")" 3 }
  /**
   * A single expression can be type constrained.
   */
  | LPAREN expr type_constraint RPAREN
      { mkexp_constraint $2 $3 }
  | LBRACE semi_terminated_seq_expr RBRACE
      { reloc_exp $2 }
  | LPAREN expr_comma_list RPAREN
      { mkexp(Pexp_tuple(List.rev $2)) }
  | LPAREN expr_comma_list error
      { unclosed "(" 1 ")" 3 }
  | simple_expr DOT label_longident
      { mkexp(Pexp_field($1, mkrhs $3 3)) }
  | mod_longident DOT LPAREN expr RPAREN
      { mkexp(Pexp_open(Fresh, mkrhs $1 1, $4)) }
  | mod_longident DOT LPAREN expr error
      { unclosed "(" 3 ")" 5 }
  | simple_expr DOT LPAREN expr RPAREN
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "get")),
                         ["",$1; "",$4])) }
  | simple_expr DOT LPAREN expr error
      { unclosed "(" 3 ")" 5 }
  | simple_expr DOT LBRACKET expr RBRACKET
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "String" "get")),
                         ["",$1; "",$4])) }
  | simple_expr DOT LBRACKET expr error
      { unclosed "[" 3 "]" 5 }
  | simple_expr DOT LBRACE expr RBRACE
      { bigarray_get $1 $4 }

  /* This might not be needed anymore
  | simple_expr DOT LBRACE expr_comma_list error
      { unclosed "{" 3 "}" 5 }
  */

  /* TODO: Let Sequence? */

  | LBRACE record_expr RBRACE
      { let (exten, fields) = $2 in mkexp (Pexp_record(fields, exten)) }
  | LBRACE record_expr error
      { unclosed "{" 1 "}" 3 }
  /* Todo: Why is this not a simple_expr? */
  | LBRACE class_self_pattern_and_structure RBRACE
      { mkexp (Pexp_object $2) }
  | LBRACE class_self_pattern_and_structure error
      { unclosed "{" 1 "}" 4 }

  | mod_longident DOT LBRACE record_expr RBRACE
      { let (exten, fields) = $4 in
        let rec_exp = mkexp(Pexp_record(fields, exten)) in
        mkexp(Pexp_open(Fresh, mkrhs $1 1, rec_exp)) }
  | mod_longident DOT LBRACE record_expr error
      { unclosed "{" 3 "}" 5 }
  | LBRACKETBAR expr_comma_seq opt_comma BARRBRACKET
      { mkexp (Pexp_array(List.rev $2)) }
  | LBRACKETBAR expr_comma_seq opt_comma error
      { unclosed "[|" 1 "|]" 4 }
  | LBRACKETBAR BARRBRACKET
      { mkexp (Pexp_array []) }
  | mod_longident DOT LBRACKETBAR expr_comma_seq opt_comma BARRBRACKET
      { mkexp(Pexp_open(Fresh, mkrhs $1 1, mkexp(Pexp_array(List.rev $4)))) }
  | mod_longident DOT LBRACKETBAR expr_comma_seq opt_comma error
      { unclosed "[|" 3 "|]" 6 }
  | LBRACKET expr_comma_seq_extension
      { let seq, ext_opt = $2 in
        reloc_exp (mktailexp_extension (rhs_loc 4) seq ext_opt) }
  | mod_longident DOT LBRACKET expr_comma_seq_extension
      { let seq, ext_opt = $4 in
        let list_exp = reloc_exp (mktailexp_extension (rhs_loc 6) seq ext_opt) in
        mkexp (Pexp_open (Fresh, mkrhs $1 1, list_exp)) }
  | PREFIXOP simple_expr %prec below_DOT_AND_SHARP
      { mkexp(Pexp_apply(mkoperator $1 1, ["",$2])) }
  /**
   * Must be below_DOT_AND_SHARP so that the parser waits for several dots for
   * nested record access that the bang should apply to.
   *
   * !x.y.z should be parsed as !(((x).y).z)
   */
  | BANG simple_expr %prec below_DOT_AND_SHARP
      { mkexp(Pexp_apply(mkoperator "!" 1, ["",$2])) }
  | NEW class_longident
      { mkexp (Pexp_new(mkrhs $2 2)) }
  | LBRACELESS field_expr_list opt_comma GREATERRBRACE
      { mkexp (Pexp_override(List.rev $2)) }
  | LBRACELESS field_expr_list opt_comma error
      { unclosed "{<" 1 ">}" 4 }
  | LBRACELESS GREATERRBRACE
      { mkexp (Pexp_override [])}
  | mod_longident DOT LBRACELESS field_expr_list opt_comma GREATERRBRACE
      { mkexp(Pexp_open(Fresh, mkrhs $1 1, mkexp (Pexp_override(List.rev $4))))}
  | mod_longident DOT LBRACELESS field_expr_list opt_comma error
      { unclosed "{<" 3 ">}" 6 }
  | simple_expr SHARP label
      { mkexp(Pexp_send($1, $3)) }
  | LPAREN MODULE module_expr RPAREN
      { mkexp (Pexp_pack $3) }
  | LPAREN MODULE module_expr COLON package_type RPAREN
      { mkexp (Pexp_constraint (ghexp (Pexp_pack $3),
                                ghtyp (Ptyp_package $5))) }
  | LPAREN MODULE module_expr COLON error
      { unclosed "(" 1 ")" 5 }
  | mod_longident DOT LPAREN MODULE module_expr COLON package_type RPAREN
      { mkexp(Pexp_open(Fresh, mkrhs $1 1,
        mkexp (Pexp_constraint (ghexp (Pexp_pack $5),
                                ghtyp (Ptyp_package $7))))) }
  | mod_longident DOT LPAREN MODULE module_expr COLON error
      { unclosed "(" 3 ")" 7 }
  | extension
      { mkexp (Pexp_extension $1) }
;


/**
 * Nice reusable version of simple_expr that waits to be reduced until enough
 * of the input has been observed, in order to consider Module.X.Y a single simple_expr
 */
less_aggressive_simple_expression:
  simple_expr %prec below_NEWDOT {$1}
;

simple_non_labeled_expr_list:
    less_aggressive_simple_expression
      { [$1] }
  | simple_non_labeled_expr_list less_aggressive_simple_expression
      { $2 :: $1 }
;

simple_non_labeled_expr_list_as_tuple:
  simple_non_labeled_expr_list {
    mkexp (Pexp_tuple(List.rev $1))
    (* match $1 with *)
    (*   | hd::[] -> hd *)
    (*   | _ -> mkexp (Pexp_tuple(List.rev $1)) *)
  }
;

simple_non_labeled_expr_list_as_tuple_or_single_expr:
  simple_non_labeled_expr_list {
    match $1 with
      | hd::[] -> hd
      | _ -> mkexp (Pexp_tuple(List.rev $1))
  }
;

simple_labeled_expr_list:
    labeled_simple_expr
      { [$1] }
  | simple_labeled_expr_list labeled_simple_expr
      { $2 :: $1 }
;

labeled_simple_expr:
    less_aggressive_simple_expression
      { ("", $1) }
  | label_expr
      { $1 }
;

/* No punning! */
label_expr:
    LIDENT COLONCOLON less_aggressive_simple_expression
      { ($1, $3) }
  /* Expliclitly provided default optional:
   * let res = someFunc optionalArg:?None;
   */
  | LIDENT EXPLICITLY_PASSED_OPTIONAL less_aggressive_simple_expression
      { ("?" ^ $1, $3) }
;

and_let_binding:
  /* AND bindings don't accept a preceeding extension ID, but do accept
   * preceeding post_item_attributes. These preceeding post_item_attributes will cause an
   * error if this is an *expression * let binding. Otherwise, they become
   * post_item_attributes on the structure item for the "and" binding.
   */
  AND let_binding_body post_item_attributes
      { mklb $2 $3 }
;
let_bindings:
    let_binding                                 { $1 }
  | let_bindings and_let_binding                { addlb $1 $2 }
;
lident_list:
    LIDENT                            { [$1] }
  | LIDENT lident_list                { $1 :: $2 }
;
let_binding_impl:
  /* Form with item extension sugar */
  | LET rec_flag let_binding_body post_item_attributes {
      ($2, $3, $4)
    }
;

let_binding:
  /* Form with item extension sugar */
  | let_binding_impl {
    let (rec_flag, body, item_attrs) = $1 in
    mklbs ([], None) rec_flag (mklb body item_attrs)
  }
  /**
   * This is frustrating. We had to manually inline the sugar rule here, to
   * avoid a parsing conflict. Why is that the case? How can we treat rules as
   * "exactly the same as being inlined?".
   */
  | item_extension_sugar let_binding_impl {
      let (rec_flag, body, item_attrs) = $2 in
      let (ext_attrs, ext_id) = $1 in
      mklbs (ext_attrs, Some ext_id) rec_flag (mklb body item_attrs)
    }
;

let_binding_body:
  | val_ident EQUAL expr
      { (mkpatvar $1 1, $3) }

  | val_ident type_constraint EQUAL expr
      { (mkpatvar $1 1,  mkexp_constraint $4 $2) }

  | val_ident curried_binding_return_typed
      { (mkpatvar $1 1, $2) }

  | val_ident COLON typevar_list DOT core_type EQUAL expr
      { (ghpat(Ppat_constraint(mkpatvar $1 1, ghtyp(Ptyp_poly(List.rev $3,$5)))), $7) }
  | val_ident COLON TYPE lident_list DOT core_type EQUAL expr
  /* Because core_type will appear to contain "type constructors" since the
   * type variables listed in lident_list don't have leading single quotes, we
   * have to call [varify_constructors] (which is what [wrap_type_annotation]
   * does among other things) to turn those "type constructors" that correspond
   * to lident_list into regular type variables. I don't think this should be
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
     {
       let exp, poly = wrap_type_annotation $4 $6 $8 in
       (ghpat(Ppat_constraint(mkpatvar $1 1, poly)), exp)
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
  | simple_pattern_not_ident COLON core_type EQUAL expr
      { (ghpat(Ppat_constraint($1, $3)), $5) }
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
    EQUALGREATER expr
      {
          let nil = { txt = Lident "()"; loc = symbol_gloc () } in
          ghexp(Pexp_fun("", None, ghpat(Ppat_construct (nil, None)), $2))
      }
  | labeled_simple_pattern curried_binding_return_typed_
      { let (l, o, p) = $1 in ghexp(Pexp_fun(l, o, p, $2)) }
  | LPAREN TYPE LIDENT RPAREN curried_binding_return_typed_
      { mkexp(Pexp_newtype($3, $5)) }
  | COLON non_arrowed_core_type EQUALGREATER expr
      {
          let nil = { txt = Lident "()"; loc = symbol_gloc () } in
          let exp = ghexp(Pexp_fun("", None, ghpat(Ppat_construct (nil, None)), $4)) in
          mkexp_constraint exp (Some $2, None)
      }
;

curried_binding_return_typed_:
  curried_binding
    {$1}

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
  | COLON non_arrowed_core_type EQUALGREATER expr
      { mkexp_constraint $4 (Some $2, None) }
;

/*
 * Arguments aren't required to object method!
 */
curried_binding:
    EQUALGREATER expr
      { $2 }
  | labeled_simple_pattern curried_binding_return_typed_
      { let (l, o, p) = $1 in ghexp(Pexp_fun(l, o, p, $2)) }
  | LPAREN TYPE LIDENT RPAREN curried_binding_return_typed_
      { mkexp(Pexp_newtype($3, $5)) }
;

no_leading_bar_match_cases:
  | match_case { [$1] }
  | no_leading_bar_match_cases BAR match_case { $3 :: $1 }
;

or_pattern:
  /* The reloc_pat likely isn't helping anything here */
  | pattern BAR pattern
    { reloc_pat (mkpat(Ppat_or($1, $3))) }


match_case:
  | pattern EQUALGREATER expr
      { Exp.case $1 $3 }
  | pattern WHEN expr EQUALGREATER expr
      { Exp.case $1 ~guard:$3 $5 }
;

fun_def:
    EQUALGREATER expr                              { $2 }
/* Cf #5939: we used to accept (fun p when e0 -> e) */
  | labeled_simple_pattern fun_def
      {
       let (l,o,p) = $1 in
       ghexp(Pexp_fun(l, o, p, $2))
      }
  | LPAREN TYPE LIDENT RPAREN fun_def
      { mkexp(Pexp_newtype($3, $5)) }
;

/* At least two comma delimited: Each item optionally annotated. */
/* Along with other sequences we make one special case for a label that could
 * not possibly be a named argument function call.
 */
expr_comma_list:
    expr_comma_list COMMA expr_optional_constraint
      { $3 :: $1 }
    | expr_optional_constraint COMMA expr_optional_constraint
      { [$3; $1] }
;


/* At least one comma delimited: Each item optionally annotated. */
expr_comma_seq:
    expr_comma_seq COMMA expr_optional_constraint
      { $3::$1 }
    | expr_optional_constraint
      { [$1] }
;

/* [x, y, z, ...n] --> ([x,y,z], Some n) */
expr_comma_seq_extension:
  | DOTDOTDOT expr_optional_constraint RBRACKET
    { ([], Some $2) }
  | expr_optional_constraint opt_comma RBRACKET
    { ([$1], None) }
  | expr_optional_constraint opt_comma error
    { unclosed "[" 1 "]" 3 }
  | expr_optional_constraint COMMA expr_comma_seq_extension
    { let seq, ext = $3 in ($1::seq, ext) }
;

/**
 * See note about tuple patterns. There are few cases where expressions may be
 * type constrained without requiring additional parens, and inside of tuples
 * are one exception.
 */
expr_optional_constraint:
    expr                 { $1 }
  | expr type_constraint { mkexp_constraint $1 $2 }
;

record_expr:
    DOTDOTDOT expr_optional_constraint COMMA lbl_expr_list   { (Some $2, $4) }
  | lbl_expr_list_that_is_not_a_single_punned_field         { (None, $1) }
;
lbl_expr_list:
     lbl_expr { [$1] }
  |  lbl_expr COMMA lbl_expr_list { $1 :: $3 }
  |  lbl_expr COMMA { [$1] }
;
lbl_expr:
  label_longident COLON expr
      { (mkrhs $1 1, $3) }
  | label_longident
      { (mkrhs $1 1, exp_of_label $1 1) }
;

non_punned_lbl_expression:
  label_longident COLON expr
      { (mkrhs $1 1, $3) }
;

/**
 * To allow sequence expressions to not *require* a final semicolon, we make a small tradeoff:
 * {label} would normally be an ambiguity: Is it a punned record, or a sequence expression
 * with only one item? It makes more sense to break the tie by interpreting it as a sequence
 * expression with a single item, as opposed to a punned record with one field. Justification:
 *
 * - Constructing single field records is very rare.
 * - Block sequences should not require a semicolon after their final item:
 *   - let something = {print "hi"; foo};
 *   - We often want to be able to print *single* values in block form, without a trailing semicolon.
 *   - You should be able to delete `print "hi"` in the above statement and still parse intuitively.
 *   - An equivalent scenario when you delete all but a single field from a punned record is more rare:
 *     -  let myRecord = {deleteThisField, butLeaveThisOne};
 * - For whatever tiny remaining confusion would occur, virtually all of them would be caught by the type system.
 */
lbl_expr_list_that_is_not_a_single_punned_field:
  | non_punned_lbl_expression
     { [$1] }
  | lbl_expr COMMA lbl_expr_list
      { $1::$3 }
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
  LIDENT COLON expr
    { (mkrhs $1 1, $3) }
 | LIDENT
    { (mkrhs $1 1, mkexp (Pexp_ident(mkrhs (Lident $1) 1))) }
;

field_expr_list:
  field_expr
     { [$1] }
  | field_expr_list COMMA field_expr
      { $3::$1 }
;


type_constraint_right_of_colon:
    core_type                             { (Some $1, None) }
  | core_type COLONGREATER core_type      { (Some $1, Some $3) }
;

type_constraint:
    COLON type_constraint_right_of_colon        { $2 }
  | COLONGREATER core_type                      { (None, Some $2) }
  | COLON error                                 { syntax_error() }
  | COLONGREATER error                          { syntax_error() }
;

/* Patterns */

pattern:
  | pattern_without_or {$1}
  | or_pattern {$1}  %prec below_AS

pattern_without_or:
    simple_pattern
      { $1 }
  | pattern_without_or AS val_ident
      { mkpat(Ppat_alias($1, mkrhs $3 3)) }
  | pattern_without_or AS error
      { expecting 3 "identifier" }
  /**
    * Parses a (comma-less) list of patterns into a tuple, or a single pattern
    * (if there is only one item in the list). This is kind of sloppy as there
    * should probably be a different AST construct for the syntax construct this
    * is used in (multiple constructor arguments). The things passed to
    * constructors are not actually tuples either in underlying representation or
    * semantics (they are not first class).
    */
  | constr_longident simple_pattern_list %prec prec_constr_appl
    {
      match is_pattern_list_single_any $2 with
        | Some singleAnyPat ->
            Pat.mk
              ~loc:(symbol_rloc())
              (Ppat_construct(mkrhs $1 1, Some singleAnyPat))
        | None ->
          let argPattern = simple_pattern_list_to_tuple $2 in
          mkExplicitArityTuplePat (Ppat_construct(mkrhs $1 1, Some argPattern))
    }
  | name_tag simple_pattern %prec prec_constr_appl
    {
      mkpat (Ppat_variant($1, Some $2))
    }
  | pattern_without_or COLONCOLON pattern_without_or
      { mkpat_cons (rhs_loc 2) (ghpat(Ppat_tuple[$1;$3])) (symbol_rloc()) }
  | pattern_without_or COLONCOLON error
      { expecting 3 "pattern" }
  | LPAREN COLONCOLON RPAREN LPAREN pattern_without_or COMMA pattern_without_or RPAREN
      { mkpat_cons (rhs_loc 2) (ghpat(Ppat_tuple[$5;$7])) (symbol_rloc()) }
  | LPAREN COLONCOLON RPAREN LPAREN pattern_without_or COMMA pattern_without_or error
      { unclosed "(" 4 ")" 8 }
  | LAZY simple_pattern
      { mkpat(Ppat_lazy $2) }
  | EXCEPTION pattern_without_or %prec prec_constr_appl
      { mkpat(Ppat_exception $2) }
  /**
   * Attribute "attribute" everything to the left of the attribute,
   * up until the point of to the start of an expression, left paren, left
   * bracket, comma, bar - whichever comes first.
   */
  | pattern_without_or attribute
      { Pat.attr $1 $2 }
;

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
    val_ident %prec below_EQUAL
      { mkpat(Ppat_var (mkrhs $1 1)) }
  | simple_pattern_not_ident { $1 }
;
simple_pattern_not_ident:
  | UNDERSCORE
      { mkpat(Ppat_any) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
  | signed_constant DOTDOT signed_constant
      { mkpat(Ppat_interval ($1, $3)) }
  | constr_longident
      { mkpat(Ppat_construct(mkrhs $1 1, None)) }
  | name_tag
      { mkpat(Ppat_variant($1, None)) }
  | SHARP type_longident
      { mkpat(Ppat_type (mkrhs $2 2)) }
  | LBRACE lbl_pattern_list RBRACE
      { let (fields, closed) = $2 in mkpat(Ppat_record(fields, closed)) }
  | LBRACE lbl_pattern_list error
      { unclosed "{" 1 "}" 3 }

  | LBRACKET pattern_comma_list_extension opt_semi RBRACKET
      { let seq, ext_opt = $2 in
        reloc_pat (mktailpat_extension (rhs_loc 4) (List.rev seq) ext_opt) }
  | LBRACKET pattern_comma_list_extension opt_semi error
      { unclosed "[" 1 "]" 4 }

  | LBRACKETBAR pattern_comma_list opt_semi BARRBRACKET
      { mkpat(Ppat_array(List.rev $2)) }
  | LBRACKETBAR BARRBRACKET
      { mkpat(Ppat_array []) }
  | LBRACKETBAR pattern_comma_list opt_semi error
      { unclosed "[|" 1 "|]" 4 }
  | LPAREN pattern RPAREN
      { reloc_pat $2 }
  | LPAREN pattern_two_or_more_comma_list RPAREN
      { mkpat(Ppat_tuple(List.rev $2)) }
  | LPAREN pattern error
      { unclosed "(" 1 ")" 3 }

  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" 1 ")" 5 }
  | LPAREN pattern COLON error
      { expecting 4 "type" }
  | LPAREN MODULE UIDENT RPAREN
      { mkpat(Ppat_unpack (mkrhs $3 3)) }
  | LPAREN MODULE UIDENT COLON package_type RPAREN
      { mkpat(Ppat_constraint(mkpat(Ppat_unpack (mkrhs $3 3)),
                              ghtyp(Ptyp_package $5))) }
  | LPAREN MODULE UIDENT COLON package_type error
      { unclosed "(" 1 ")" 6 }
  | extension
      { mkpat(Ppat_extension $1) }
;

/**
 * Tuple patterns/expressions are one of the rare cases where type constraints
 * are accepted without having to group the constraint in parenthesis.  Notice
 * how these rules require that the `pattern_two_or_more_comma_list` include at
 * *least* two items.
 */
pattern_two_or_more_comma_list:
    pattern_two_or_more_comma_list COMMA pattern_optional_constraint
    { $3::$1 }
  | pattern_optional_constraint COMMA pattern_optional_constraint
    { [$3; $1] }
  | pattern_optional_constraint COMMA error
    { expecting 3 "pattern" }
;

simple_pattern_list:
    simple_pattern_list simple_pattern
    { $2::$1 }
  | simple_pattern
    { [$1] }
;

pattern_optional_constraint:
    pattern                     { $1 }
  | pattern COLON core_type     { mkpat(Ppat_constraint($1, $3)) }
;


pattern_comma_list:
    pattern                                     { [$1] }
  | pattern_comma_list COMMA pattern              { $3 :: $1 }
;

/* [x, y, z, ...n] --> ([x,y,z], Some n) */
pattern_comma_list_extension:
  | pattern                                     { [$1], None }
  | pattern_comma_list COMMA DOTDOTDOT pattern  { ($1, Some $4) }
  | pattern_comma_list COMMA pattern            { ($3 :: $1, None) }
;

lbl_pattern_list:
    lbl_pattern { [$1], Closed }
  | lbl_pattern COMMA { [$1], Closed }
  | lbl_pattern COMMA UNDERSCORE opt_comma { [$1], Open }
  | lbl_pattern COMMA lbl_pattern_list
      { let (fields, closed) = $3 in $1 :: fields, closed }
;
lbl_pattern:
  label_longident COLON pattern
      { (mkrhs $1 1,$3) }
  | label_longident
      { (mkrhs $1 1, pat_of_label $1 1) }
;

/* Primitive declarations */

primitive_declaration:
    STRING                                      { [fst $1] }
  | STRING primitive_declaration                { fst $1 :: $2 }
;

/* Type declarations */

many_type_declarations:
  | TYPE type_declaration_details post_item_attributes {
      let (ident, params, constraints, kind, priv, manifest) = $2 in
      [Type.mk ident
        ~params:params ~cstrs:constraints
        ~kind ~priv ?manifest ~attrs:$3 ~loc:(symbol_rloc())]
  }
  | many_type_declarations and_type_declaration      { $2 :: $1 }
;

and_type_declaration:
  | AND type_declaration_details post_item_attributes {
    let (ident, params, constraints, kind, priv, manifest) = $2 in
    Type.mk ident
      ~params:params ~cstrs:constraints
      ~kind ~priv ?manifest ~attrs:$3 ~loc:(symbol_rloc())
  }
;

type_declaration_details:
    LIDENT optional_type_parameters type_kind constraints
      { let (kind, priv, manifest) = $3 in
        ((mkrhs $1 1), $2, List.rev $4, kind, priv, manifest)
       }
;
constraints:
        constraints CONSTRAINT constrain        { $3 :: $1 }
      | /* empty */                             { [] }
;
type_kind:
    /*empty*/
      { (Ptype_abstract, Public, None) }
  | EQUAL core_type
      { (Ptype_abstract, Public, Some $2) }
  | EQUAL PRIVATE core_type
      { (Ptype_abstract, Private, Some $3) }
  | EQUAL constructor_declarations
      { (Ptype_variant(List.rev $2), Public, None) }
  | EQUAL PRIVATE constructor_declarations
      { (Ptype_variant(List.rev $3), Private, None) }
  | EQUAL private_flag BAR constructor_declarations
      { (Ptype_variant(List.rev $4), $2, None) }
  | EQUAL DOTDOT
      { (Ptype_open, Public, None) }
  | EQUAL private_flag LBRACE label_declarations opt_comma RBRACE
      { (Ptype_record(List.rev $4), $2, None) }
  | EQUAL core_type EQUAL private_flag opt_bar constructor_declarations
      { (Ptype_variant(List.rev $6), $4, Some $2) }
  | EQUAL core_type EQUAL DOTDOT
      { (Ptype_open, Public, Some $2) }
  | EQUAL core_type EQUAL private_flag LBRACE label_declarations opt_comma RBRACE
      { (Ptype_record(List.rev $6), $4, Some $2) }
;
optional_type_parameters:
    /*empty*/                      { [] }
  | optional_type_parameter_list   { List.rev $1 }
;

optional_type_parameter_list:
    optional_type_variable_with_variance                                 { [$1] }
  | optional_type_parameter_list optional_type_variable_with_variance    { $2 :: $1 }
;
optional_type_variable_with_variance:
  | QUOTE ident                                 { mktyp(Ptyp_var $2), Invariant }
  | UNDERSCORE                                  { mktyp(Ptyp_any), Invariant }
  | PLUS QUOTE ident                            { mktyp(Ptyp_var $3), Covariant}
  | PLUS UNDERSCORE                             { mktyp(Ptyp_any), Covariant }
  | MINUS QUOTE ident                           { mktyp(Ptyp_var $3), Contravariant }
  | MINUS UNDERSCORE                            { mktyp(Ptyp_any), Contravariant }
;

type_parameter:
    type_variance type_variable                   { $2, $1 }
;
type_variance:
    /* empty */                                 { Invariant }
  | PLUS                                        { Covariant }
  | MINUS                                       { Contravariant }
;
type_variable:
    QUOTE ident                                 { mktyp(Ptyp_var $2) }
;

constructor_declarations:
    constructor_declaration                     { [$1] }
  | constructor_declarations BAR constructor_declaration { $3 :: $1 }
;
constructor_declaration:
  | constr_ident generalized_constructor_arguments attributes
      {
       let args,res = $2 in
       Type.constructor (mkrhs $1 1) ~args ?res ~loc:(symbol_rloc()) ~attrs:$3
      }
;
/* Why are there already post_item_attributes on the extension_constructor_declaration? */
str_exception_declaration:
  | EXCEPTION extension_constructor_declaration post_item_attributes
      {
        let ext = $2 in
        {ext with pext_attributes = ext.pext_attributes @ $3}
      }
  | EXCEPTION extension_constructor_rebind post_item_attributes
      {
        let ext = $2 in
        {ext with pext_attributes = ext.pext_attributes @ $3}
      }
;
sig_exception_declaration:
  | EXCEPTION extension_constructor_declaration post_item_attributes
      {
        let ext = $2 in
        {ext with pext_attributes = ext.pext_attributes @ $3}
      }
;
generalized_constructor_arguments:
    /*empty*/                                   { ([],None) }
  | OF non_arrowed_simple_core_type_list                    { (List.rev $2, None) }
  | OF non_arrowed_simple_core_type_list COLON core_type
                                                { (List.rev $2,Some $4) }
  | COLON core_type
                                                { ([],Some $2) }
;



label_declarations:
    label_declaration                           { [$1] }
  | label_declarations COMMA label_declaration   { $3 :: $1 }
;

label_declaration:
    mutable_flag LIDENT COLON poly_type attributes
      {
       Type.field (mkrhs $2 2) $4 ~mut:$1 ~attrs:$5 ~loc:(symbol_rloc())
      }
;

/* Type Extensions */

potentially_long_ident_and_optional_type_parameters:
  LIDENT optional_type_parameters                    {(mkrhs (Lident $1) 1, $2)}
  | type_strictly_longident optional_type_parameters {(mkrhs $1 1, $2)}
;

str_type_extension:
  TYPE
  potentially_long_ident_and_optional_type_parameters
  PLUSEQ private_flag opt_bar str_extension_constructors
  post_item_attributes
  {
    let (potentially_long_ident, optional_type_parameters) = $2 in
    Te.mk potentially_long_ident (List.rev $6)
          ~params:optional_type_parameters ~priv:$4 ~attrs:$7 }
;
sig_type_extension:
  TYPE
  potentially_long_ident_and_optional_type_parameters
  PLUSEQ private_flag opt_bar sig_extension_constructors
  post_item_attributes
  {
    let (potentially_long_ident, optional_type_parameters) = $2 in
    Te.mk potentially_long_ident (List.rev $6)
          ~params:optional_type_parameters ~priv:$4 ~attrs:$7
  }
;
str_extension_constructors:
    extension_constructor_declaration                     { [$1] }
  | extension_constructor_rebind                          { [$1] }
  | str_extension_constructors BAR extension_constructor_declaration
      { $3 :: $1 }
  | str_extension_constructors BAR extension_constructor_rebind
      { $3 :: $1 }
;
sig_extension_constructors:
    extension_constructor_declaration                     { [$1] }
  | sig_extension_constructors BAR extension_constructor_declaration
      { $3 :: $1 }
;
extension_constructor_declaration:
  | constr_ident generalized_constructor_arguments attributes
      { let args, res = $2 in
        Te.decl (mkrhs $1 1) ~args ?res ~loc:(symbol_rloc()) ~attrs:$3 }
;
extension_constructor_rebind:
  | constr_ident EQUAL constr_longident attributes
      { Te.rebind (mkrhs $1 1) (mkrhs $3 3) ~loc:(symbol_rloc()) ~attrs:$4 }
;

/* "with" constraints (additional type equations over signature components) */

with_constraints:
    with_constraint                             { [$1] }
  | with_constraints AND with_constraint        { $3 :: $1 }
;
with_constraint:
    TYPE label_longident optional_type_parameters with_type_binder core_type constraints
      { Pwith_type
          (mkrhs $2 2,
           (Type.mk (mkrhs (Longident.last $2) 2)
              ~params:$3
              ~cstrs:(List.rev $6)
              ~manifest:$5
              ~priv:$4
              ~loc:(symbol_rloc()))) }
    /* used label_longident instead of type_longident to disallow
       functor applications in type path */
  | TYPE label_longident optional_type_parameters COLONEQUAL core_type
    {
      let last = (
        match $2 with
            | Lident s -> s
            | _ -> not_expecting 2 "Long type identifier"
      ) in
      Pwith_typesubst
          (Type.mk (mkrhs last 2)
             ~params:$3
             ~manifest:$5
             ~loc:(symbol_rloc()))
    }
  | MODULE mod_longident EQUAL mod_ext_longident
      { Pwith_module (mkrhs $2 2, mkrhs $4 4) }
  | MODULE UIDENT COLONEQUAL mod_ext_longident
      { Pwith_modsubst (mkrhs $2 2, mkrhs $4 4) }
;
with_type_binder:
    EQUAL          { Public }
  | EQUAL PRIVATE  { Private }
;

/* Polymorphic types */

typevar_list:
        QUOTE ident                             { [$2] }
      | typevar_list QUOTE ident                { $3 :: $1 }
;
poly_type:
        core_type
          { $1 }
      | typevar_list DOT core_type
          { mktyp(Ptyp_poly(List.rev $1, $3)) }
;

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
  *    non_arrowed_non_simple_core_type  %prec below_NEWDOT
  *    non_arrowed_simple_core_type %prec below_NEWDOT
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
    core_type2 %prec below_EQUALGREATER
      { $1 }
  | core_type2 AS QUOTE ident
      { mktyp(Ptyp_alias($1, $4)) }
;

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
  /* The %prec below_LBRACKETAT was inherited from the previous rule
   * simple_core_type_or_tuple */
    non_arrowed_core_type %prec below_LBRACKETAT
      { $1 }
  | LIDENT COLONCOLON non_arrowed_core_type QUESTION EQUALGREATER core_type2
      { mktyp(Ptyp_arrow("?" ^ $1 , mkoption $3, $6)) }
  | LIDENT COLONCOLON non_arrowed_core_type EQUALGREATER core_type2
      { mktyp(Ptyp_arrow($1, $3, $5)) }
  | core_type2 EQUALGREATER core_type2
      { mktyp(Ptyp_arrow("", $1, $3)) }
;


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
   non_arrowed_non_simple_core_type  %prec below_NEWDOT
    { $1 }
 | non_arrowed_simple_core_type %prec below_NEWDOT
    { $1 }
;

non_arrowed_non_simple_core_type:
  | type_longident non_arrowed_simple_core_type_list
      { mktyp(Ptyp_constr(mkrhs $1 1, List.rev $2)) }
  | SHARP class_longident non_arrowed_simple_core_type_list
      { mktyp(Ptyp_class(mkrhs $2 2, List.rev $3)) }
  | non_arrowed_core_type attribute
      { Typ.attr $1 $2 }
;

non_arrowed_simple_core_type:
  | LPAREN core_type_comma_list RPAREN %prec below_NEWDOT
      { match $2 with
        | [] -> raise Parse_error
        | one::[] -> one
        | moreThanOne -> mktyp(Ptyp_tuple(List.rev moreThanOne)) }
  | QUOTE ident
      { mktyp(Ptyp_var $2) }
  | SHARP class_longident
      { mktyp(Ptyp_class(mkrhs $2 2, [])) }
  | UNDERSCORE
      { mktyp(Ptyp_any) }
  | type_longident
      { mktyp(Ptyp_constr(mkrhs $1 1, [])) }
  | LESS meth_list GREATER
      { let (f, c) = $2 in mktyp(Ptyp_object (f, c)) }
  | LESS GREATER
      { mktyp(Ptyp_object ([], Closed)) }
  | LBRACKET tag_field RBRACKET
      { mktyp(Ptyp_variant([$2], Closed, None)) }
/* PR#3835: this is not LR(1), would need lookahead=2
  | LBRACKET non_arrowed_simple_core_type RBRACKET
      { mktyp(Ptyp_variant([$2], Closed, None)) }
*/
  | LBRACKET BAR row_field_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, Closed, None)) }
  | LBRACKET row_field BAR row_field_list RBRACKET
      { mktyp(Ptyp_variant($2 :: List.rev $4, Closed, None)) }
  | LBRACKETGREATER opt_bar row_field_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, Open, None)) }
  | LBRACKETGREATER RBRACKET
      { mktyp(Ptyp_variant([], Open, None)) }
  | LBRACKETLESS opt_bar row_field_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, Closed, Some [])) }
  | LBRACKETLESS opt_bar row_field_list GREATER name_tag_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, Closed, Some (List.rev $5))) }
  | LPAREN MODULE package_type RPAREN
      { mktyp(Ptyp_package $3) }
  | extension
      { mktyp (Ptyp_extension $1) }
;
package_type:
    mty_longident { (mkrhs $1 1, []) }
  | mty_longident WITH package_type_cstrs { (mkrhs $1 1, $3) }
;
package_type_cstr:
    TYPE label_longident EQUAL core_type { (mkrhs $2 2, $4) }
;
package_type_cstrs:
    package_type_cstr { [$1] }
  | package_type_cstr AND package_type_cstrs { $1::$3 }
;
row_field_list:
    row_field                                   { [$1] }
  | row_field_list BAR row_field                { $3 :: $1 }
;
row_field:
    tag_field                                   { $1 }
  | non_arrowed_simple_core_type                            { Rinherit $1 }
;
tag_field:
    name_tag OF opt_ampersand amper_type_list attributes
      { Rtag ($1, $5, $3, List.rev $4) }
  | name_tag attributes
      { Rtag ($1, $2, true, []) }
;
opt_ampersand:
    AMPERSAND                                   { true }
  | /* empty */                                 { false }
;
amper_type_list:
    core_type                                   { [$1] }
  | amper_type_list AMPERSAND core_type         { $3 :: $1 }
;
name_tag_list:
    name_tag                                    { [$1] }
  | name_tag_list name_tag                      { $2 :: $1 }
;

core_type_comma_list:
    core_type                                   { [$1] }
  | core_type_comma_list COMMA core_type        { $3 :: $1 }
;

non_arrowed_simple_core_type_list:
    non_arrowed_simple_core_type %prec below_LBRACKETAT     { [$1] }
  | non_arrowed_simple_core_type_list non_arrowed_simple_core_type  %prec below_LBRACKETAT     { $2 :: $1 }
;
meth_list:
    field COMMA meth_list                     { let (f, c) = $3 in ($1 :: f, c) }
  | field opt_comma                              { [$1], Closed }
  | DOTDOT                                      { [], Open }
;
field:
    label COLON poly_type attributes           { ($1, $4, $3) }
;
label:
    LIDENT                                      { $1 }
;

/* Constants */

constant:
    INT                               { Const_int $1 }
  | CHAR                              { Const_char $1 }
  | STRING                            { let (s, d) = $1 in Const_string (s, d) }
  | FLOAT                             { Const_float $1 }
  | INT32                             { Const_int32 $1 }
  | INT64                             { Const_int64 $1 }
  | NATIVEINT                         { Const_nativeint $1 }
;
signed_constant:
    constant                               { $1 }
  | MINUS INT                              { Const_int(- $2) }
  | MINUS FLOAT                            { Const_float("-" ^ $2) }
  | MINUS INT32                            { Const_int32(Int32.neg $2) }
  | MINUS INT64                            { Const_int64(Int64.neg $2) }
  | MINUS NATIVEINT                        { Const_nativeint(Nativeint.neg $2) }
  | PLUS INT                               { Const_int $2 }
  | PLUS FLOAT                             { Const_float $2 }
  | PLUS INT32                             { Const_int32 $2 }
  | PLUS INT64                             { Const_int64 $2 }
  | PLUS NATIVEINT                         { Const_nativeint $2 }
;

/* Identifiers and long identifiers */

ident:
    UIDENT                                      { $1 }
  | LIDENT                                      { $1 }
;
val_ident:
    LIDENT                                      { $1 }
  | LPAREN operator RPAREN                      { $2 }
  | LPAREN operator error                       { unclosed "(" 1 ")" 3 }
  | LPAREN error                                { expecting 2 "operator" }
  | LPAREN MODULE error                         { expecting 3 "module-expr" }
;
operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | BANG                                        { "!" }
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
  | PLUSEQ                                      { "+=" }
  | PERCENT                                     { "%" }
;
constr_ident:
    UIDENT                                      { $1 }
/*  | LBRACKET RBRACKET                           { "[]" } */
  | LPAREN RPAREN                               { "()" }
  | COLONCOLON                                  { "::" }
/*  | LPAREN COLONCOLON RPAREN                    { "::" } */
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;

val_longident:
    val_ident                                   { Lident $1 }
  | mod_longident DOT val_ident                 { Ldot($1, $3) }
;
constr_longident:
    mod_longident       %prec below_DOT         { $1 }
  | LBRACKET RBRACKET                           { Lident "[]" }
  | LPAREN RPAREN                               { Lident "()" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
;
label_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;
type_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;

/* Type long identifiers known to be "long". Only needed to resolve shift
 * reduce conflicts between `type myType 'a 'b = ..` and `type
 * Potentially.Long.myType 'a 'b += ..` - we needed to break the parsing of
 * Potentially.Long.myType into two cases, one that is long, and one that is
 * not.
 */
type_strictly_longident:
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
mod_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;
mod_ext_longident:
    UIDENT                                      { Lident $1 }
  | mod_ext_longident DOT UIDENT                { Ldot($1, $3) }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN { lapply $1 $3 }
;
mty_longident:
    ident                                       { Lident $1 }
  | mod_ext_longident DOT ident                 { Ldot($1, $3) }
;
clty_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
class_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;

/* Toplevel directives */

toplevel_directive:
    SHARP ident                 { Ptop_dir($2, Pdir_none) }
  | SHARP ident STRING          { Ptop_dir($2, Pdir_string (fst $3)) }
  | SHARP ident INT             { Ptop_dir($2, Pdir_int $3) }
  | SHARP ident val_longident   { Ptop_dir($2, Pdir_ident $3) }
  | SHARP ident mod_longident   { Ptop_dir($2, Pdir_ident $3) }
  | SHARP ident FALSE           { Ptop_dir($2, Pdir_bool false) }
  | SHARP ident TRUE            { Ptop_dir($2, Pdir_bool true) }
;

/* Miscellaneous */

name_tag:
    BACKQUOTE ident                             { $2 }
;
rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
private_flag:
    /* empty */                                 { Public }
  | PRIVATE                                     { Private }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;
virtual_flag:
    /* empty */                                 { Concrete }
  | VIRTUAL                                     { Virtual }
;
private_virtual_flags:
    /* empty */  { Public, Concrete }
  | PRIVATE { Private, Concrete }
  | VIRTUAL { Public, Virtual }
  | PRIVATE VIRTUAL { Private, Virtual }
  | VIRTUAL PRIVATE { Private, Virtual }
;
override_flag:
    /* empty */                                 { Fresh }
  | BANG                                        { Override }
;
opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
opt_comma:
  | /* empty */                                 { () }
  | COMMA                                       { () }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;
subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;
additive:
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
;


single_attr_id:
    LIDENT { $1 }
  | UIDENT { $1 }
  | AND { "and" }
  | AS { "as" }
  | ASSERT { "assert" }
  | BEGIN { "begin" }
  | CLASS { "class" }
  | CONSTRAINT { "constraint" }
  | DO { "do" }
  | DONE { "done" }
  | DOWNTO { "downto" }
  | ELSE { "else" }
  | END { "end" }
  | EXCEPTION { "exception" }
  | EXTERNAL { "external" }
  | FALSE { "false" }
  | FOR { "for" }
  | FUN { "fun" }
  | FUNCTION { "function" }
  | FUNCTOR { "functor" }
  | IF { "if" }
  | IN { "in" }
  | INCLUDE { "include" }
  | INHERIT { "inherit" }
  | INITIALIZER { "initializer" }
  | LAZY { "lazy" }
  | LET { "let" }
  | SWITCH { "match" }
  | METHOD { "method" }
  | MODULE { "module" }
  | MUTABLE { "mutable" }
  | NEW { "new" }
  | OBJECT { "object" }
  | OF { "of" }
  | OPEN { "open" }
  | OR { "or" }
  | PRIVATE { "private" }
  | REC { "rec" }
  | SIG { "sig" }
  | STRUCT { "struct" }
  | THEN { "then" }
  | TO { "to" }
  | TRUE { "true" }
  | TRY { "try" }
  | TYPE { "type" }
  | VAL { "val" }
  | VIRTUAL { "virtual" }
  | WHEN { "when" }
  | WHILE { "while" }
  | WITH { "with" }
/* mod/land/lor/lxor/lsl/lsr/asr are not supported for now */
;

attr_id:
    single_attr_id { mkloc $1 (symbol_rloc()) }
  | single_attr_id DOT attr_id { mkloc ($1 ^ "." ^ $3.txt) (symbol_rloc())}
;
attribute:
  LBRACKETAT attr_id payload RBRACKET { ($2, $3) }
;
item_attribute:
  LBRACKETATAT attr_id payload RBRACKET { ($2, $3) }
;

floating_attribute:
  | LBRACKETATATAT attr_id payload RBRACKET {($2, $3)}
;

attributes:
    /* empty */{ [] }
  | attribute attributes { $1 :: $2 }
;

post_item_attributes:
    /* empty */{ [] }
  | item_attribute post_item_attributes { $1 :: $2 }
;

item_extension_sugar:
  /**
   * Note, this form isn't really super useful, but wouldn't cause any parser
   * conflicts. Not supporting it though just to avoid having to write the
   * pretty printing logic.
   *
   *   [@attrsOnExtension] %extSugarId [@attrOnLet] LET ..
   *
   * We won't document it though, and probably won't format it as such.
   *  | PERCENT attr_id item_attribute post_item_attributes {
   *     ($3::$4, $2)
   *    }
   */
  | PERCENT attr_id {
      ([], $2)
    }
;

extension:
  LBRACKETPERCENT attr_id payload RBRACKET { ($2, $3) }
;


item_extension:
  LBRACKETPERCENTPERCENT attr_id payload RBRACKET { ($2, $3) }
;
payload:
    structure { PStr $1 }
  | COLON core_type { PTyp $2 }
  | QUESTION pattern { PPat ($2, None) }
  | QUESTION pattern WHEN expr { PPat ($2, Some $4) }
;




%%
