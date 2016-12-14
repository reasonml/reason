(*
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
 *)

(* TODO more fine-grained precedence pretty-printing *)

open Asttypes
open Format
open Location
open Lexing
open Longident
open Parsetree
open Easy_format
open Syntax_util
open Ast_mapper


type commentCategory =
  | EndOfLine
  | SingleLine
  | Regular

(* (comment text, attachment_location, physical location) *)
type commentWithCategory = (String.t * commentCategory * Location.t) list

let print_easy easyFormatted =
  match easyFormatted with
  | Atom (s,_) -> s
  | List (_,_) -> "list"
  | Label (_,_) -> "label"
  | Custom _ -> "custom"

let (|>) = fun x f -> f x
let (<|) = fun f x -> f x

exception NotPossible of string

let useSingleColonForNamedArgs = false

let case_not_implemented msg loc (file, line, column) =
  Format.fprintf Format.err_formatter
    "Not Implemented Yet %s %a (from: %s:%s:%s)@."
    msg
    Location.print_loc loc
    file
    (string_of_int line)
    (string_of_int column)

let exprDescrString x =
  x.pexp_loc.loc_start.Lexing.pos_fname ^
    "[" ^
    (string_of_int x.pexp_loc.loc_start.Lexing.pos_lnum) ^
    ", " ^
    (string_of_int x.pexp_loc.loc_end.Lexing.pos_lnum) ^
    "]"

type ruleInfoData = {
  reducePrecedence: precedence;
  shiftPrecedence: precedence;
}

and ruleCategory =
  (* Printing will be parsed with very high precedence, so not much need to
     worry about ensuring it will reduce correctly. In short, you can put
     `FunctionApplication` content anywhere around an infix identifier without
     wrapping in parens. For example `myFunc x y z` or `if x {y} else {z}`
     The layout is kept in list form only to allow for elegant wrapping rules
     to take into consideration the *number* of high precedence parsed items. *)
  | FunctionApplication of layoutNode list
  (* Care should be taken to ensure the rule that caused it to be parsed will
     reduce again on the printed output - context should carefully consider
     wrapping in parens according to the ruleInfoData. *)
  | SpecificInfixPrecedence of ruleInfoData * layoutNode
  (* Not safe to include anywhere between infix operators without wrapping in
     parens. This describes expressions like `fun x => x` which doesn't fit into
     our simplistic algorithm for printing function applications separated by infix.

     It might be possible to include these in between infix, but there are
     tricky rules to determining when these must be guarded by parens (it
     depends highly on context that is hard to reason about). It's so nuanced
     that it's easier just to always wrap them in parens.  *)
  | PotentiallyLowPrecedence of layoutNode
  | Simple of layoutNode

and associativity =
  | Right
  | Nonassoc
  | Left

and precedenceEntryType =
  | TokenPrecedence
  | CustomPrecedence

and precedence =
  | Token of string
  | Custom of string

(* Make a standard list *)
and whenToDoSomething =
  | Never
  | IfNeed
  | Always
  (* Always_rec not only will break, it will break recursively up to the root *)
  | Always_rec

(* Describes the "fixity" of a token, and stores its *printed* representation
   should it be rendered as infix/prefix (This rendering may be different than
   how it is stored in the AST). *)
and tokenFixity =
  (* Such as !simple_expr and ~!simple_expr. These function applications are
     considered *almost* "simple" because they may be allowed anywhere a simple
     expression is accepted, except for when on the left hand side of a
     dot/send. *)
  | AlmostSimplePrefix of string
  | UnaryPlusPrefix of string
  | UnaryMinusPrefix of string
  | Infix of string
  | Normal

and easyFormatLabelFormatter = Easy_format.t -> Easy_format.t -> Easy_format.t
and listConfig = {
  (* Newlines above items that do not have any comments immediately above it.
     Only really useful when used with break:Always/Always_rec *)
  newlinesAboveItems: int;
  (* Newlines above regular comments *)
  newlinesAboveComments: int;
  (* Newlines above doc comments *)
  newlinesAboveDocComments: int;
  (* If you are only grouping something for the sake of visual appearance, and
   * not forming an actual conceptual sequence of items, then this is often
   * useful. For example, if you're appending a semicolon etc. *)
  interleaveComments: bool;
  (* Whether or not to render the final separator *)
  renderFinalSep: bool;
  break: whenToDoSomething;
  (* Break setting that becomes activated if a comment becomes interleaved into
   * this list. Typically, if not specified, the behavior from [break] will be
   * used.
   *)
  wrap: string * string;
  inline: bool * bool;
  sep: string;
  indent: int;
  sepLeft: bool;
  preSpace: bool;
  postSpace: bool;
  pad: bool * bool;
  (* A function, because the system might rearrange your previous settings, and
   * a function allows you to not be locked into some configuration that is made
   * out of date by the formatting system (suppose it removes the separator
   * token etc.) Having a function allows you to instruct our formatter how to
   * extend the "freshest" notion of the list config when comments are
   * interleaved. *)
  listConfigIfCommentsInterleaved: (listConfig -> listConfig) option;

  (* Formatting to use if an item in a list had an end-of-line comment appended *)
  listConfigIfEolCommentsInterleaved: (listConfig -> listConfig) option;
}

(**
 * These represent "intent to format" the AST, with some parts being annotated
 * with original source location. The benefit of tracking this in an
 * intermediate structure, is that we can then interleave comments throughout
 * the tree before generating the final representation. That prevents the
 * formatting code from having to thread comments everywhere.
 *
 * The final representation is rendered using Easy_format.
 *)
and layoutNode =
  | SourceMap of Location.t * layoutNode (* a layout with location info *)
  | WithEOLComment of string * layoutNode (* a layout with comment attached *)
  | Sequence of listConfig * (layoutNode list)
  | Label of easyFormatLabelFormatter * layoutNode * layoutNode
  | Easy of Easy_format.t

let print_comment_type = function
  | Regular -> "Regular"
  | EndOfLine -> "End of Line"
  | SingleLine -> "SingleLine"

let rec print_comments = function
  | [] -> ()
  | ((s, t, loc)::tl) ->
       printf "%d (%d:%d)-%d (%d:%d) -- %s:||%s||\n"
              loc.loc_start.Lexing.pos_cnum
              loc.loc_start.Lexing.pos_lnum
              (loc.loc_start.Lexing.pos_cnum - loc.loc_start.Lexing.pos_bol)
              loc.loc_end.Lexing.pos_cnum
              loc.loc_end.Lexing.pos_lnum
              (loc.loc_end.Lexing.pos_cnum - loc.loc_end.Lexing.pos_bol)
              (print_comment_type t)
              s;
       print_comments tl;
       ()

let rec print_easy_rec ?(indent=0) easyFormatted =
  let space = Array.fold_left (^) "" (Array.make indent " ") in
  match easyFormatted with
  | Atom (s,_) ->
     printf "%s Atom:'%s'\n" space s
  | List ((opening, sep, closing, config), items) ->
     let break = (match config.wrap_body with
                  | `No_breaks -> "No_breaks"
                  | `Wrap_atoms -> "Wrap_atoms"
                  | `Never_wrap -> "Never_wrap"
                  | `Force_breaks -> "Force_breaks"
                  | `Force_breaks_rec -> "Force_breaks_rec"
                  | `Always_wrap -> "Always_wrap") in
     printf "%s List: open %s close %s sep %s break %s \n" space opening closing sep break;
     let _ = List.map (print_easy_rec ~indent:(indent+2)) items in
     ()
  | Label ((left, config), right) ->
     let break = match config.label_break with
     | `Never -> "Never"
     | `Always_rec -> "Always_rec"
     | `Auto -> "Auto"
     | `Always -> "Always" in
     printf "%s Label (break = %s): \n" space break;
     printf "  %s left \n" space;
     print_easy_rec ~indent:(indent+2) left;
     printf "  %s right \n" space;
     print_easy_rec ~indent:(indent+2) right;
  | Custom _ -> printf "custom \n"

let rec print_layout ?(indent=0) layout =
  let space = Array.fold_left (^) "" (Array.make indent " ") in
  match layout with
  | SourceMap (loc, layout) ->
     printf "%s [%d (%d:%d)-%d (%d:%d)]\n" space
            loc.loc_start.Lexing.pos_cnum
            loc.loc_start.Lexing.pos_lnum
            (loc.loc_start.Lexing.pos_cnum - loc.loc_start.Lexing.pos_bol)
            loc.loc_end.Lexing.pos_cnum
            loc.loc_end.Lexing.pos_lnum
            (loc.loc_end.Lexing.pos_cnum - loc.loc_end.Lexing.pos_bol);
     print_layout ~indent:(indent+2) layout
  | Sequence (config, layout_list) ->
     let break = match config.break with
       | Never  -> "Never"
       | IfNeed  -> "if need"
       | Always  -> "Always"
       | Always_rec  -> "Always_rec" in
     printf "%s Sequence of %d, sep: %s stick_to_left: %s break: %s\n" space (List.length layout_list) config.sep (string_of_bool config.sepLeft) break;
     let _ = List.map (print_layout ~indent:(indent+2)) layout_list in
     ()
  | WithEOLComment (comment, layout) ->
     printf "%s WithEOLComment: \n" space;
     printf "  %s node \n" space;
     print_layout ~indent:(indent+2) layout;
     printf "  %s comments : \n" space;
     printf "  %s %s\n" space comment;
     printf "\n";
  | Label (_, left, right) ->
     printf "%s Label: \n" space;
     printf "  %s left \n" space;
     print_layout ~indent:(indent+2) left;
     printf "  %s right \n" space;
     print_layout ~indent:(indent+2) right;
  | Easy e ->
     printf "%s Easy: %s \n" space (print_easy e)

let rec longIdentSame = function
  | (Lident l1, Lident l2) -> String.compare l1 l2 == 0
  | (Ldot (path1, l1), Ldot (path2, l2)) ->
    longIdentSame (path1, path2) && String.compare l1 l2 == 0
  | (Lapply (l11, l12), Lapply (l21, l22)) ->
    longIdentSame (l11, l21) && longIdentSame (l12, l22)
  | _ -> false

let rec trueForEachPair l1 l2 tester = match (l1, l2) with
  | ([], []) -> true
  | ([], _::_) -> false
  | (_::_, []) -> false
  | (hd1::tl1, hd2::tl2) -> (tester hd1 hd2 && trueForEachPair tl1 tl2 tester)

(*
   Checks to see if two types are the same modulo the process of varification
   which turns abstract types into type variables of the same name.
   For example, [same_ast_modulo_varification] would consider (a => b) and ('a
   => 'b) to have the same ast. This is useful in recovering syntactic sugar
   for explicit polymorphic types with locally abstract types.

   Does not compare attributes, or extensions intentionally.

   TODO: This has one more issue: We need to compare only accepting t1's type
   variables, to be considered compatible with t2's type constructors - not the
   other way around.
 *)
let same_ast_modulo_varification_and_extensions t1 t2 =
  let rec loop t1 t2 = match (t1.ptyp_desc, t2.ptyp_desc) with
    (* Importantly, cover the case where type constructors (of the form [a])
       are converted to type vars of the form ['a].
     *)
    | (Ptyp_constr({txt=Lident s1}, []), Ptyp_var s2) -> String.compare s1 s2 == 0
    (* Now cover the case where type variables (of the form ['a]) are
       converted to type constructors of the form [a].
     *)
    | (Ptyp_var s1, Ptyp_constr({txt=Lident s2}, [])) -> String.compare s1 s2 == 0
    (* Now cover the typical case *)
    | (Ptyp_constr(longident1, lst1), Ptyp_constr(longident2, lst2))  ->
      longIdentSame (longident1.txt, longident2.txt) &&
      trueForEachPair lst1 lst2 loop
    | (Ptyp_any, Ptyp_any) -> true
    | (Ptyp_var x1, Ptyp_var x2) -> String.compare x1 x2 == 0
    | (Ptyp_arrow (label1, core_type1, core_type1'), Ptyp_arrow (label2, core_type2, core_type2')) ->
      String.compare label1 label2 == 0 &&
      loop core_type1 core_type2 &&
      loop core_type1' core_type2'
    | (Ptyp_tuple lst1, Ptyp_tuple lst2) -> trueForEachPair lst1 lst2 loop
    | (Ptyp_object (lst1, o1), Ptyp_object (lst2, o2)) ->
      let tester = fun (s1, attrs1, t1) (s2, attrs2, t2) ->
        String.compare s1 s2 == 0 &&
        loop t1 t2
      in
      trueForEachPair lst1 lst2 tester &&
      o1 == o2
    | (Ptyp_class (longident1, lst1), Ptyp_class (longident2, lst2)) ->
      longIdentSame (longident1.txt, longident2.txt) &&
      trueForEachPair lst1 lst2 loop
    | (Ptyp_alias(core_type1, string1), Ptyp_alias(core_type2, string2)) ->
      loop core_type1 core_type2 &&
      String.compare string1 string2 == 0
    | (Ptyp_variant(row_field_list1, flag1, lbl_lst_option1), Ptyp_variant(row_field_list2, flag2, lbl_lst_option2)) ->
      trueForEachPair row_field_list1 row_field_list2 rowFieldEqual &&
      flag1 == flag2 &&
      lbl_lst_option1 == lbl_lst_option2
    | (Ptyp_poly (string_lst1, core_type1), Ptyp_poly (string_lst2, core_type2))->
      trueForEachPair string_lst1 string_lst2 (fun s1 s2 -> String.compare s1 s2 == 0) &&
      loop core_type1 core_type2
    | (Ptyp_package(longident1, lst1), Ptyp_package (longident2, lst2)) ->
      longIdentSame (longident1.txt, longident2.txt) &&
      trueForEachPair lst1 lst2 testPackageType
    | (Ptyp_extension (s1, arg1), Ptyp_extension (s2, arg2)) ->
      String.compare s1.txt s2.txt == 0
    | _ -> false
  and testPackageType (lblLongIdent1, ct1) (lblLongIdent2, ct2) =
    longIdentSame (lblLongIdent1.txt, lblLongIdent2.txt) &&
    loop ct1 ct2
  and rowFieldEqual f1 f2 = match (f1, f2) with
    | ((Rtag(label1, attrs1, flag1, lst1)), (Rtag (label2, attrs2, flag2, lst2))) ->
      String.compare label1 label2 == 0 &&
      flag1 == flag2 &&
      trueForEachPair lst1 lst2 loop
    | (Rinherit t1, Rinherit t2) -> loop t1 t2
    | _ -> false
  in
  loop t1 t2

let wrapLayoutWithLoc loc layout =
  match loc with
  | None -> layout
  | Some loc -> SourceMap (loc, layout)

let expandLocation pos ~expand:(startPos, endPos) =
  { pos with
    loc_start = {
      pos.loc_start with
        Lexing.pos_cnum = pos.loc_start.Lexing.pos_cnum + startPos
    };
    loc_end = {
      pos.loc_end with
        Lexing.pos_cnum = pos.loc_end.Lexing.pos_cnum + endPos
    }
  }

(*
 * Returns (arityAttrs, docAttrs, standard_attrs)
 *)
let rec partitionAttributes attrs =
  match attrs with
    | [] -> ([], [], [], [])
    | (({txt="JSX"; loc}, _) as jsx)::atTl ->
        let (tlArity, tlDoc, tlStandard, tlJsx) = partitionAttributes atTl in
        (tlArity, tlDoc, tlStandard, jsx :: tlJsx)
    | (({txt="explicit_arity"; loc}, _) as arity_attr)::atTl
    | (({txt="implicit_arity"; loc}, _) as arity_attr)::atTl ->
        let (tlArity, tlDoc, tlStandard, tlJsx) = partitionAttributes atTl in
        (arity_attr::tlArity, tlDoc, tlStandard, tlJsx)
    | (({txt="ocaml.text"; loc}, _) as doc)::atTl
    | (({txt="ocaml.doc"; loc}, _) as doc)::atTl ->
        let (tlArity, tlDoc, tlStandard, tlJsx) = partitionAttributes atTl in
        (tlArity, doc::tlDoc, tlStandard, tlJsx)
    | atHd::atTl ->
        let (tlArity, tlDoc, tlStandard, tlJsx) = partitionAttributes atTl in
        (tlArity, tlDoc, atHd::tlStandard, tlJsx)

let partitionNonrecAttr attrs = List.partition (fun attr ->
    match attr with
    | ({txt="nonrec"; _}, _) -> true
    | _ -> false) attrs

let extractStdAttrs attrs =
  let (arityAttrs, docAttrs, standard_attrs, jsxAttrs) = partitionAttributes attrs in
  standard_attrs

let rec sequentialIfBlocks x =
  match x with
    | Some ({pexp_desc=Pexp_ifthenelse (e1, e2, els)}) -> (
       let (nestedIfs, finalExpression) = (sequentialIfBlocks els) in
       ((e1, e2)::nestedIfs, finalExpression)
      )
    | Some e -> ([], Some e)
    | None -> ([], None)


(*
  TODO: IDE integration beginning with Vim:

  - Create recovering version of parser that creates regions of "unknown"
    content in between let sequence bindings (anything between semicolons,
    really).
  - Use Easy_format's "style" features to tag each known node.
  - Turn those style annotations into editor highlight commands.
  - Editors have a set of keys that retrigger the parsing/rehighlighting
    process (typically newline/semi/close-brace).
  - On every parsing/rehighlighting, this pretty printer can be used to
    determine the highlighting of recovered regions, and the editor plugin can
    relegate highlighting of malformed regions to the editor which mostly does
    so based on token patterns.

*)

(*
     @avoidSingleTokenWrapping

  +-----------------------------+
  |+------+                     |     Another label
  || let ( \                    |
  ||    a  | Label              |
  ||    o  |                    |     The thing to the right of any label must be a
  ||    p _+ label RHS          |     list in order for it to wrap correctly. Lists
  ||  ): /   v                  |     will wrap if they need to/can. NON-lists will
  |+--+ sixteenTuple = echoTuple|(    wrap (indented) even though they're no lists!
  +---/ 0,\---------------------+     To prevent a single item from wrapping, make
        0,                            an unbreakable list via ensureSingleTokenSticksToLabel.
        0
      );                              In general, the best approach for indenting
                                      let bindings is to keep building up labels from
                                      the "let", always ensuring things that you want
                                      to wrap will either be lists or guarded in
                                      [ensureSingleTokenSticksToLabel].
                                      If you must join several lists together (via =)
                                      (or colon), ensure that joining is done via
                                      [makeList] (which won't break), and that new
                                      list is always appended to the left
                                      hand side of the label. (So that the right hand
                                      side may always be the untouched list that you want
                                      to wrap with aligned closing).
                                      Always make sure rhs of the label are the

                                      Creating nested labels will preserve the original
                                      indent location ("let" in this
                                      case) as long as that nesting is
                                      done on the left hand side of the labels.

*)

(*
    Table 2.1. Precedence and associativity.
    Precedence from highest to lowest: From RWOC, modified to include !=
    ---------------------------------------

    Operator prefix	Associativity
    !..., ?..., ~...	                              Prefix
    ., .(, .[	-
    function application, constructor, assert, lazy	Left associative
    -, -.                                           Prefix
    **..., lsl, lsr, asr                            Right associative
    *..., /..., %..., mod, land, lor, lxor          Left associative
    +..., -...                                      Left associative
    ::                                              Right associative
    @..., ^...                                      Right associative
---
    !=                                              Left associative (INFIXOP0 listed first in lexer)
    =..., <..., >..., |..., &..., $...              Left associative (INFIXOP0)
    =, <, >                                         Left associative (IN SAME row as INFIXOP0 listed after)
---
    &, &&                                           Right associative
    or, ||                                          Right associative
    ,                                               -
    :=, =                                         	Right associative
    if                                              -
    ;                                               Right associative


   Note: It would be much better if &... and |... were in separate precedence
   groups just as & and | are. This way, we could encourage custom infix
   operators to use one of the two precedences and no one would be confused as
   to precedence (leading &, | are intuitive). Two precedence classes for the
   majority of infix operators is totally sufficient.

   TODO: Free up the (&) operator from pervasives so it can be reused for
   something very common such as string concatenation or list appending.

   let x = tail & head;
 *)

(* "Almost Simple Prefix" function applications parse with the rule:

   `PREFIXOP simple_expr %prec below_DOT_AND_SHARP`, which in turn is almost
   considered a "simple expression" (it's acceptable anywhere a simple
   expression is except in a couple of edge cases.

   "Unary Prefix" function applications parse with the rule:

   `MINUS epxr %prec prec_unary_minus`, which in turn is considered an
   "expression" (not simple). All unary operators are mapped into an identifier
   beginning with "~".

   TODO: Migrate all "almost simple prefix" to "unsary prefix". When `!`
   becomes "not", then it will make more sense that !myFunc (arg) is parsed as
   !(myFunc arg) instead of (!myFunc) arg.

 *)
let almost_simple_prefix_symbols  = [ '!'; '?'; '~'] ;;
(* Subset of prefix symbols that have special "unary precedence" *)
let unary_minus_prefix_symbols  = [ "~-"; "~-."] ;;
let unary_plus_prefix_symbols  = ["~+"; "~+." ] ;;
let infix_symbols = [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/';
                      '$'; '%'; '\\'; '#' ]
let operator_chars = [ '!'; '$'; '%'; '&'; '*'; '+'; '-'; '.'; '/';
                       ':'; '<'; '='; '>'; '?'; '@'; '^'; '|'; '~' ]
let numeric_chars  = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]

let special_infix_strings =
  ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "!=="]

let updateToken = "="
let requireIndentFor = [updateToken; ":="]

let infixTokenRequiresIndent printedIdent =
  if List.exists (fun i -> i = printedIdent) requireIndentFor then None else Some 0

let getPrintableUnaryIdent s =
  if List.mem s unary_minus_prefix_symbols || List.mem s unary_plus_prefix_symbols then
    String.sub s 1 (String.length s -1)
  else s


(* determines if the string is an infix string.
   checks backwards, first allowing a renaming postfix ("_102") which
   may have resulted from Pexp -> Texp -> Pexp translation, then checking
   if all the characters in the beginning of the string are valid infix
   characters. *)
let printedStringAndFixity  = function
  | s when List.mem s special_infix_strings -> Infix s
  | s when List.mem s.[0] infix_symbols -> Infix s
  (* Correctness under assumption that unary operators are stored in AST with
     leading "~" *)
  | s when List.mem s.[0] almost_simple_prefix_symbols &&
           not (List.mem s special_infix_strings) &&
           not (s = "?")-> (
      (* What *kind* of prefix fixity? *)
      if List.mem s unary_plus_prefix_symbols then
        UnaryPlusPrefix (getPrintableUnaryIdent s)
      else if List.mem s unary_minus_prefix_symbols then
        UnaryMinusPrefix (getPrintableUnaryIdent s)
      else
        AlmostSimplePrefix s
  )
  | _ -> Normal


(* Also, this doesn't account for != and !== being infixop!!! *)
let isSimplePrefixToken s = match printedStringAndFixity s with
  | AlmostSimplePrefix _ -> true
  | _ -> false


(* Convenient bank of information that represents the parser's precedence
   rankings.  Each instance describes a precedence table entry. The function
   tests either a token string encountered by the parser, or (in the case of
   `CustomPrecedence`) the string name of a custom rule precedence declared
   using %prec *)
let rules = [
  [
    (TokenPrecedence, (fun s -> (Nonassoc, isSimplePrefixToken s)));
  ];
  [
    (CustomPrecedence, (fun s -> (Nonassoc, s = "prec_unary_minus")));
    (CustomPrecedence, (fun s -> (Nonassoc, s = "prec_unary_plus")));
  ];
  (* Note the special case for "*\*", BARBAR, and LESSMINUS, AMPERSAND(s) *)
  [
    (TokenPrecedence, (fun s -> (Right, String.length s > 1 && s.[0] == '*' && s.[1] == '\\' && s.[2] == '*')));
    (TokenPrecedence, (fun s -> (Right, s = "lsl")));
    (TokenPrecedence, (fun s -> (Right, s = "lsr")));
    (TokenPrecedence, (fun s -> (Right, s = "asr")));
  ];
  [
    (TokenPrecedence, (fun s -> (Left, s.[0] == '*' && (String.length s == 1 || s != "*\\*"))));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '/')));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '%' )));
    (TokenPrecedence, (fun s -> (Left, s = "mod" )));
    (TokenPrecedence, (fun s -> (Left, s = "land" )));
    (TokenPrecedence, (fun s -> (Left, s = "lor" )));
    (TokenPrecedence, (fun s -> (Left, s = "lxor" )));
  ];
  [
    (* Even though these use the same *tokens* as unary plus/minus at parse
       time, when unparsing infix -/+, the CustomPrecedence rule would be
       incorrect to use, and instead we need a rule that models what infix
       parsing would use - just the regular token precedence without a custom
       precedence. *)
    (TokenPrecedence, (fun s -> (Left, s.[0] == '+' )));
    (TokenPrecedence ,(fun s -> (Left, s.[0] == '-' )));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, s = "::")));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, s.[0] == '@')));
    (TokenPrecedence, (fun s -> (Right, s.[0] == '^')));
  ];
  [
    (TokenPrecedence, (fun s -> (Left, s.[0] == '=' && not (s = "=") && not (s = "=>"))));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '<' && not (s = "<"))));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '>' && not (s = ">"))));
    (TokenPrecedence, (fun s -> (Left, s = "!=")));  (* Not preset in the RWO table! *)
    (TokenPrecedence, (fun s -> (Left, s = "!==")));  (* Not preset in the RWO table! *)
    (TokenPrecedence, (fun s -> (Left, s = "==")));
    (TokenPrecedence, (fun s -> (Left, s = "===")));
    (TokenPrecedence, (fun s -> (Left, s = "<")));
    (TokenPrecedence, (fun s -> (Left, s = ">")));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '|' && not (s = "||"))));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '&' && not (s = "&") && not (s = "&&"))));
    (TokenPrecedence, (fun s -> (Left, s.[0] == '$')));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, s = "&")));
    (TokenPrecedence, (fun s -> (Right, s = "&&")));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, s = "or")));
    (TokenPrecedence, (fun s -> (Right, s = "||")));
  ];
  [
    (* The Left shouldn't ever matter in practice. Should never get in a
       situation with two consecutive infix ? - the colon saves us. *)
    (TokenPrecedence, (fun s -> (Left, s = "?")));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, s = ":=")));
  ];
  [
    (TokenPrecedence, (fun s -> (Right, s = updateToken)));
  ];
  (* It's important to account for ternary ":" being lower precedence than "?" *)
  [
    (TokenPrecedence, (fun s -> (Right, s = ":")))
  ];
  [
    (TokenPrecedence, (fun s -> (Nonassoc, s = "=>")));
  ];
]

(* without_prefixed_backslashes removes any prefixing backslashes *)
let without_prefixed_backslashes str =
  Re_str.replace_first (Re_str.regexp "\\\\*\\(.*\\)") ("\\1") str

let indexOfFirstMatch ~prec lst =
  let rec indexOfFirstMatchN ~prec lst n = match lst with
    | [] -> None
    | []::tl -> indexOfFirstMatchN ~prec tl (n + 1)
    | (hdHd::hdTl)::tl -> (
      let (kind, tester) = hdHd in
      match (prec, kind) with
      | (Token str, TokenPrecedence)
      | (Custom str, CustomPrecedence) ->
        let (associativity, foundMatch) = tester str in
        if foundMatch then
          Some (associativity, n)
        else
          indexOfFirstMatchN ~prec (hdTl::tl) (n)
      | _ -> indexOfFirstMatchN ~prec (hdTl::tl) (n)
    )
  in
  indexOfFirstMatchN ~prec lst 0

(* Assuming it's an infix function application. *)
let precedenceInfo ~prec =
  (* Removes prefixed backslashes in order to do proper conversion *)
  let normalizedCheck =
    match prec with
      | Token str -> Token (without_prefixed_backslashes str)
      | Custom str -> prec
  in
  indexOfFirstMatch ~prec:normalizedCheck rules

let isLeftAssociative ~prec = match precedenceInfo ~prec with
  | None -> false
  | Some (Left, _) -> true
  | Some (Right, _) -> false
  | Some (Nonassoc, _) -> false

let isRightAssociative ~prec = match precedenceInfo ~prec with
  | None -> false
  | Some (Right, _) -> true
  | Some (Left, _) -> false
  | Some (Nonassoc, _) -> false

let higherPrecedenceThan c1 c2 = match ((precedenceInfo c1), (precedenceInfo c2)) with
  | (_, None)
  | (None, _) ->
    let (str1, str2) = match (c1, c2) with
      | (Token s1, Token s2) -> ("Token " ^ s1, "Token " ^ s2)
      | (Token s1, Custom s2) -> ("Token " ^ s1, "Custom " ^ s2)
      | (Custom s1, Token s2) -> ("Custom " ^ s1, "Token " ^ s2)
      | (Custom s1, Custom s2) -> ("Custom " ^ s1, "Custom " ^ s2)
    in
    raise (NotPossible ("Cannot determine precedence of two checks " ^ str1 ^ " vs. " ^ str2))
  | (Some (_, p1), Some (_, p2)) -> p1 < p2


let printedStringAndFixityExpr = function
  | {pexp_desc = Pexp_ident {txt=Lident l}} -> printedStringAndFixity l
  | _ -> Normal

let is_predef_option = function
  | (Ldot (Lident "*predef*","option")) -> true
  | _ -> false

(* which identifiers are in fact operators needing parentheses *)
let needs_parens txt =
  match printedStringAndFixity txt with
    | Infix _ -> true
    | UnaryPlusPrefix _ -> true
    | UnaryMinusPrefix _ -> true
    | AlmostSimplePrefix _ -> true
    | Normal -> false

(* some infixes need spaces around parens to avoid clashes with comment
   syntax. This isn't needed for comment syntax /* */ *)
let needs_spaces txt =
  txt.[0]='*' || txt.[String.length txt - 1] = '*'

(* add parentheses to binders when they are in fact infix or prefix operators *)
let protect_ident ppf txt =
  let format : (_, _, _) format =
    if not (needs_parens txt) then "%s"
    else if needs_spaces txt then "(@;%s@;)"
    else "(%s)"
  in fprintf ppf format txt

let protect_longident ppf print_longident longprefix txt =
  let format : (_, _, _) format =
    if not (needs_parens txt) then "%a.%s"
    else if needs_spaces txt then  "(@;%a.%s@;)"
    else "(%a.%s)" in
  fprintf ppf format print_longident longprefix txt

let rec longident f = function
  | Lident s -> protect_ident f s
  | Ldot(y,s) -> protect_longident f longident y s
  | Lapply (y,s) ->
      fprintf f "%a(%a)" longident y longident s

let rec orList = function (* only consider ((A|B)|C)*)
  | {ppat_desc = Ppat_or (p1, p2)} -> (orList p1) @ (orList p2)
  | x -> [x]

type space_formatter = (unit, Format.formatter, unit) format

let override = function
  | Override -> "!"
  | Fresh -> ""

(* variance encoding: need to sync up with the [parser.mly] *)
let type_variance = function
  | Invariant -> ""
  | Covariant -> "+"
  | Contravariant -> "-"

type construct =
  [ `cons of expression list
  | `list of expression list
  | `nil
  | `normal
  | `simple of Longident.t
  | `tuple ]

let view_expr x =
  match x.pexp_desc with
    | Pexp_construct ( {txt= Lident "()"; _},_) -> `tuple
    | Pexp_construct ( {txt= Lident "[]"},_) -> `nil
    | Pexp_construct ( {txt= Lident"::"},Some _) ->
        let rec loop exp acc = match exp with
          | {pexp_desc=Pexp_construct ({txt=Lident "[]"},_)} ->
              (List.rev acc,true)
          | {pexp_desc=
               Pexp_construct ({txt=Lident "::"},
                 Some ({pexp_desc= Pexp_tuple([e1;e2])}))} ->
              loop e2 (e1::acc)
          | e -> (List.rev (e::acc),false) in
        let (ls,b) = loop x []  in
        if b then
          `list ls
        else `cons ls
    | Pexp_construct (x,None) -> `simple (x.txt)
    | _ -> `normal

let is_simple_construct :construct -> bool = function
  | `nil | `tuple | `list _ | `simple _ | `cons _  -> true
  | `normal -> false

let pp = fprintf

let default = new Pprintast.printer ()

type funcReturnStyle =
  | ReturnValOnSameLine

let rec detectJSXComponent e attributes l =
  match (e, attributes) with
    | (Pexp_ident loc, ({txt = "JSX"; _}, PStr []) :: tail) ->
      let rec checkChildren arguments nrOfChildren =
        match arguments with
        | ("", {pexp_desc = Pexp_construct ({txt = Lident "::"}, _)}) :: tail
        | ("", {pexp_desc = Pexp_construct ({txt = Lident "[]"}, _)}) :: tail ->
            checkChildren tail (nrOfChildren + 1)
        | ("", _) :: tail -> false
        | (lbl, _)::tail -> checkChildren tail nrOfChildren
        | [] -> nrOfChildren = 1
      in
      let moduleNameList = List.rev (List.tl (List.rev (Longident.flatten loc.txt))) in
      if List.length moduleNameList > 0 then
        if Longident.last loc.txt = "createElement" && checkChildren l 0 then
          Some (String.concat "." moduleNameList)
        else
          None
      else if checkChildren l 0 then
        Some (Longident.last loc.txt)
      else
        None
    | (Pexp_ident loc,  hd :: tail) -> detectJSXComponent e tail l
    | _ -> None

let detectTernary l = match l with
  | [{
      pc_lhs={ppat_desc=Ppat_construct ({txt=Lident "true"}, _)};
      pc_guard=None;
      pc_rhs=ifTrue
    };
    {
      pc_lhs={ppat_desc=Ppat_construct ({txt=Lident "false"}, _)};
      pc_guard=None;
      pc_rhs=ifFalse
    }] -> Some (ifTrue, ifFalse)
  | _ -> None
type funcApplicationLabelStyle =
  (* No attaching to the label, but if the entire application fits on one line,
     the entire application will appear next to the label as you 'd expect. *)
  | NeverWrapFinalItem
  (* Attach the first term if there are exactly two terms involved in the
     application.

     let x = firstTerm (secondTerm_1 secondTerm_2) thirdTerm;

     Ideally, we'd be able to attach all but the last argument into the label any
     time all but the last term will fit - and *not* when (attaching all but
     the last term isn't enough to prevent a wrap) - But there's no way to tell
     ahead of time if it would prevent a wrap.

     However, the number two is somewhat convenient. This models the
     indentation that you'd prefer in non-curried syntax languages like
     JavaScript, where application only ever has two terms.
  *)
  | WrapFinalListyItemIfFewerThan of int

(*
    space=2, indentWrappedPatternArgs=1, funcReturnStyle=ReturnValOnSameLine
    ------------------------------------------------------------------------
    (* When [ReturnValOnSameLine], [indentWrappedPatternArgs] has no effect! *)
    let myFunc
        (wrappedArgOne:int)
        (wrappedArgTwo:int) => {
      valOne: 10,
      valTwo: 20
    };

    space=2, indentWrappedPatternArgs=2, funcReturnStyle=ReturnValOnSameLine
    ------------------------------------------------------------------------
    (* When [ReturnValOnSameLine], [indentWrappedPatternArgs] has no effect! *)
    let myFunc
        (wrappedArgOne:int)
        (wrappedArgTwo:int) => {
      valOne: 10,
      valTwo: 20
    };

*)

type formatSettings = {
  (* Whether or not to expect that the original parser that generated the AST
     would have annotated constructor argument tuples with explicit arity to
     indicate that they are multiple arguments. (True if parsed in original
     OCaml AST, false if using Reason parser).
  *)
  constructorTupleImplicitArity: bool;
  space: int;
  (* Whether or not to begin a curried function's return expression immediately
     after the [=>] without a newline.
  *)
  returnStyle: funcReturnStyle;

  (* For curried arguments in function *definitions* only: Number of [space]s
     to offset beyond the [let] keyword. Default 1.
  *)
  listsRecordsIndent: int;

  (* When [funcReturnStyle] = [ReturnValOnSameLine],
     [indentWrappedPatternArgs] is not adjustable - wrapped arguments will
     always be aligned with the function name. *)
  indentWrappedPatternArgs: int;

  indentMatchCases: int;

  (* Amount to indent in label-like constructs such as wrapped function
     applications, etc - or even record fields. This is not the same concept as an
     indented curried argument list. *)
  indentAfterLabels: int;

  (* Amount to indent after the opening brace of switch/try.
     Here's an example of what it would look like w/ [trySwitchIndent = 2]:
     Sticks the expression to the last item in a sequence in several [X | Y | Z
     => expr], and forces X, Y, Z to be split onto several lines. (Otherwise,
     sticking to Z would result in hanging expressions).  TODO: In the first case,
     it's clear that we want patterns to have an "extra" indentation with matching
     in a "match". Create extra config param to pass to [self#pattern] for extra
     indentation in this one case.

      switch x {
      | TwoCombos
          (HeresTwoConstructorArguments x y)
          (HeresTwoConstructorArguments a b) =>
          ((a + b) + x) + y;
      | Short
      | AlsoHasARecord a b {x, y} => (
          retOne,
          retTwo
        )
      | AlsoHasARecord a b {x, y} =>
        callMyFunction
          withArg
          withArg
          withArg
          withArg;
      }
  *)
  trySwitchIndent: int;


  (* In the case of two term function application (when flattened), the first
     term should become part of the label, and the second term should be able to wrap
     This doesn't effect n != 2.

       [true]
       let x = reallyShort allFitsOnOneLine;
       let x = someFunction {
         reallyLongObject: true,
         thatWouldntFitOnThe: true,
         firstLine: true
       };

       [false]
       let x = reallyShort allFitsOnOneLine;
       let x =
        someFunction
          {
            reallyLongObject: true,
            thatWouldntFitOnThe: true,
            firstLine: true
          };
  *)
  funcApplicationLabelStyle: funcApplicationLabelStyle;

  funcCurriedPatternStyle: funcApplicationLabelStyle;

  width: int;

  assumeExplicitArity: bool;

  constructorLists: string list;
}

let defaultSettings = {
  constructorTupleImplicitArity = false;
  space = 1;
  returnStyle = ReturnValOnSameLine;
  listsRecordsIndent = 2;
  indentWrappedPatternArgs = 2;
  indentMatchCases = 2;
  indentAfterLabels = 2;
  trySwitchIndent = 0;
  funcApplicationLabelStyle = WrapFinalListyItemIfFewerThan 3;
  (* WrapFinalListyItemIfFewerThan is currently a bad idea for curried
     arguments: It looks great in some cases:

        let myFun (a:int) :(
          int,
          string
        ) => (a, "this is a");

     But horrible in others:

        let myFun
            {
              myField,
              yourField
            } :someReturnType => myField + yourField;

        let myFun
            {            // Curried arg wraps
              myField,
              yourField
            } : (       // But the last is "listy" so it docks
          int,          // To the [let].
          int,
          int
        ) => myField + yourField;

     We probably want some special listy label docking/wrapping mode for
     curried function bindings.

  *)
  funcCurriedPatternStyle = NeverWrapFinalItem;
  width = 90;
  assumeExplicitArity = false;
  constructorLists = [];
}
let configuredSettings = ref defaultSettings

let configure ~width ~assumeExplicitArity ~constructorLists = (
  configuredSettings := {defaultSettings with width; assumeExplicitArity; constructorLists}
)

let string_of_formatter f x =
  Format.asprintf "%a" f x

let createFormatter () =
let module Formatter = struct

let settings = !configuredSettings


(* How do we make
   this a label?

   /---------------------\
   let myVal = (oneThing, {
   field: [],
   anotherField: blah
   });

   But in this case, this wider region a label?
   /------------------------------------------------------\
   let myVal = callSomeFunc (oneThing, {field: [], anotherField: blah}, {
   boo: 'hi'
   });

   This is difficult. You must form a label from the preorder traversal of every
   node - except the last encountered in the traversal. An easier heuristic is:

   - The last argument to a functor application is expanded.

   React.CreateClass SomeThing {
   let render {props} => {
   };
   }

   - The last argument to a function application is expanded on the same line.
   - Only if it's not curried with another invocation.
   -- Optionally: "only if everything else is an atom"
   -- Optionally: "only if there are no other args"

   React.createClass someThing {
   render: fn x => y,
   }

   !!! NOT THIS
   React.createClass someThing {
   render: fn x => y,
   }
   somethingElse
*)

let isArityClear attrs =
  (!configuredSettings).assumeExplicitArity ||
  List.exists
    (function
      | ({txt="explicit_arity"; loc}, _) -> true
      | _ -> false
    )
    attrs


let list_settings = {
  Easy_format.space_after_opening = false;
  space_after_separator = false;
  space_before_separator = false;
  separators_stick_left = true;
  space_before_closing = false;
  stick_to_label = true;
  align_closing = true;
  wrap_body = `No_breaks;
  indent_body = settings.listsRecordsIndent * settings.space;
  list_style = Some "list";
  opening_style = None;
  body_style = None;
  separator_style = None;
  closing_style = None;
}

let nullStyle = { Easy_format.atom_style = Some "null" }
let boolStyle = { Easy_format.atom_style = Some "bool" }
let intStyle = { Easy_format.atom_style = Some "int" }
let stringStyle = { Easy_format.atom_style = Some "string" }
let labelStringStyle = { Easy_format.atom_style = Some "atomClss" }
let colonStyle = { Easy_format.atom_style = Some "punct" }

let simplifiedApplicationSettings = {
  list_settings with
    align_closing = true; (* So the semicolon sticks to end of application *)
    (* This must be true to support this case:

        let oneNestedInvocationThatWraps = outerFunc (
          nestedFuncToInvokeThatCausesWrapping
          []
        );

       Otherwise, we would get:
        let oneNestedInvocationThatWraps = outerFunc
          (nestedFuncToInvokeThatCausesWrapping []);
    *)
    stick_to_label = true; (* I don't believe this has a purpose *)
    space_after_separator = true;
    wrap_body = `Never_wrap
}

let easyListSettingsFromListConfig listConfig =
  let {
    break;
    wrap;
    inline;
    indent;
    sepLeft;
    preSpace;
    postSpace;
    pad;
    sep;
  } = listConfig in
  let (opn, cls) = wrap in
  let (padOpn, padCls) = pad in
  let (inlineStart, inlineEnd) = inline in
  (opn, sep, cls, {
    list_settings with
      wrap_body = (
        match break with
          | Never -> `No_breaks
          (* Yes, `Never_wrap is a horrible name - really means "if needed". *)
          | IfNeed -> `Never_wrap
          | Always -> `Force_breaks
          | Always_rec -> `Force_breaks_rec
      );
      indent_body = indent;
      space_after_separator = postSpace;
      space_before_separator = preSpace;
      space_after_opening = padOpn;
      space_before_closing = padCls;
      stick_to_label = inlineStart;
      align_closing = not inlineEnd;
  })

let makeListConfig
    ?(newlinesAboveItems=0)
    ?(newlinesAboveComments=0)
    ?(newlinesAboveDocComments=0)
    ?(interleaveComments=true)
    ?listConfigIfCommentsInterleaved
    ?(listConfigIfEolCommentsInterleaved)
    ?(renderFinalSep=false)
    ?(break=Never)
    ?(wrap=("", ""))
    ?(inline=(true, false))
    ?(sep="")
    ?(indent=list_settings.indent_body)
    ?(sepLeft=true)
    ?(preSpace=false)
    ?(postSpace=false)
    ?(pad=(false,false))
    () =
  {
    newlinesAboveItems;
    newlinesAboveComments;
    newlinesAboveDocComments;
    interleaveComments;
    listConfigIfCommentsInterleaved;
    listConfigIfEolCommentsInterleaved;
    renderFinalSep;
    break;
    wrap;
    inline;
    sep;
    indent;
    sepLeft;
    preSpace;
    postSpace;
    pad;
  }

let easyListWithConfig listConfig easyListItems =
  let (opn, sep, cls, settings) =
    easyListSettingsFromListConfig listConfig in
  Easy_format.List ((opn, sep, cls, settings), easyListItems)

let makeEasyList
    ?(newlinesAboveItems=0)
    ?(newlinesAboveComments=0)
    ?(newlinesAboveDocComments=0)
    ?(interleaveComments=true)
    ?(renderFinalSep=false)
    ?(break=Never)
    ?(wrap=("", ""))
    ?(inline=(true, false))
    ?(sep="")
    ?(indent=list_settings.indent_body)
    ?(sepLeft=true)
    ?(preSpace=false)
    ?(postSpace=false)
    ?(pad=(false,false)) easyListItems =
  let listConfig =
    makeListConfig
      ~newlinesAboveItems
      ~newlinesAboveComments
      ~newlinesAboveDocComments
      ~interleaveComments
      (* This is unused at this point - separators are handled by our pretty printer,
         not Easy_format (so that we can interleave comments intelligently) *)
      ~renderFinalSep
      ~break
      ~wrap
      ~inline
      ~sep
      ~indent
      ~sepLeft
      ~preSpace
      ~postSpace
      ~pad
      ()
  in
  let (opn, sep, cls, listSettings) = easyListSettingsFromListConfig listConfig in
  Easy_format.List ((opn, sep, cls, listSettings), easyListItems)

let makeList
    (* Allows a fallback in the event that comments were interleaved with the
     * list *)
    ?(newlinesAboveItems=0)
    ?(newlinesAboveComments=0)
    ?(newlinesAboveDocComments=0)
    ?(interleaveComments=true)
    ?listConfigIfCommentsInterleaved
    ?listConfigIfEolCommentsInterleaved
    ?(renderFinalSep=false)
    ?(break=Never)
    ?(wrap=("", ""))
    ?(inline=(true, false))
    ?(sep="")
    ?(indent=list_settings.indent_body)
    ?(sepLeft=true)
    ?(preSpace=false)
    ?(postSpace=false)
    ?(pad=(false,false)) lst =
  let config =
    makeListConfig
      ~newlinesAboveItems
      ~newlinesAboveComments
      ~newlinesAboveDocComments
      ~interleaveComments
      ?listConfigIfCommentsInterleaved
      ?listConfigIfEolCommentsInterleaved
      ~renderFinalSep
      ~break
      ~wrap
      ~inline
      ~sep
      ~indent
      ~sepLeft
      ~preSpace
      ~postSpace
      ~pad
      ()
  in
  Sequence (config, lst)

let makeAppList l =
  match l with
  | hd::[] -> hd
  | _ -> makeList ~inline:(true, true) ~postSpace:true ~break:IfNeed l

let ensureSingleTokenSticksToLabel x =
  makeList
    ~interleaveComments:true
    ~listConfigIfCommentsInterleaved: (
      fun currentConfig -> {currentConfig with break=Always_rec; postSpace=true; indent=0; inline=(true, true)}
    )
    [x]

let unbreakLabelFormatter formatter =
  let newFormatter labelTerm term =
    match formatter labelTerm term with
    | Easy_format.Label ((labelTerm, settings), term) ->
       Easy_format.Label ((labelTerm,
                           {settings with label_break = `Never}),
                          term)
    | _ -> failwith "not a label"
  in newFormatter

let inlineLabel labelTerm term =
  let settings = {
    label_break = `Never;
    space_after_label = true;
    indent_after_label = 0;
    label_style = Some "inlineLabel";
  } in
  Easy_format.Label ((labelTerm, settings), term)

(* Just for debugging: Set debugWithHtml = true *)
let debugWithHtml = ref false

let html_escape_string s =
  let buf = Buffer.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let html_escape = `Escape_string html_escape_string
let html_style = [
  "atom", { Easy_format.tag_open = "<a>"; tag_close = "</a>" };
  "body", { tag_open = "<lb>"; tag_close = "</lb>" };
  "list", { tag_open = "<l>"; tag_close = "</l>" };
  "op", { tag_open = "<op>"; tag_close = "</op>" };
  "cl", { tag_open = "<cl>"; tag_close = "</cl>" };
  "sep", { tag_open = "<sep>"; tag_close = "</sep>" };
  "label", { tag_open = "<la>"; tag_close = "</la>" };
]


let easyLabel ?(break=`Auto) ?(space=false) ?(indent=settings.indentAfterLabels) labelTerm term =
  let settings = {
    label_break = break;
    space_after_label = space;
    indent_after_label = indent;
    label_style = Some "label";
  } in
  Easy_format.Label ((labelTerm, settings), term)

let label ?(break=`Auto) ?(space=false) ?(indent=settings.indentAfterLabels) (labelTerm:layoutNode) (term:layoutNode) =
  Label (
    (fun x y -> easyLabel ~break ~indent ~space x y),
    labelTerm,
    term
  )

let labelSpace l r = label ~space:true l r

let atom ?loc str =
  let layout = Easy (Easy_format.Atom(str, labelStringStyle)) in
  wrapLayoutWithLoc loc layout

let easyAtom str = Easy_format.Atom(str, labelStringStyle)

(** Take x,y,z and n and generate [x, y, z, ...n] *)
let makeES6List lst last =
  let last_dots = makeList [atom "..."; last] in
  makeList ~wrap:("[", "]") ~break:IfNeed ~postSpace:true ~sep:"," (lst @ [last_dots])

let makeNonIndentedBreakingList lst =
    (* No align closing: So that semis stick to the ends of every break *)
  makeList ~break:Always_rec ~indent:0 ~inline:(true, true) lst

let break =
    (* No align closing: So that semis stick to the ends of every break *)
  makeListConfig ~break:Always_rec ~indent:0 ~inline:(true, true) ()

let makeBreakableList lst = makeList ~break:IfNeed ~inline:(true, true) lst

let makeNonIndentedBreakableEasyList lst = makeEasyList ~break:IfNeed ~inline:(true, true) ~indent:0 lst

(* Like a <span> could place with other breakableInline lists without upsetting final semicolons *)
let makeSpacedBreakableInlineList lst =
  makeList ~break:IfNeed ~inline:(true, true) ~postSpace:true lst

let makeCommaBreakableList lst = makeList ~break:IfNeed ~postSpace:true lst

let makeCommaBreakableListSurround opn cls lst =
  makeList ~break:IfNeed ~postSpace:true ~sep:"," ~wrap:(opn, cls) lst

(* TODO: Allow configuration of spacing around colon symbol *)

let formatPrecedence ?loc formattedTerm =
  let withParens = makeList ~wrap:("(", ")") ~break:IfNeed [formattedTerm] in
  match loc with
    | None -> withParens
    | Some l -> SourceMap (l, withParens)

(* What to do when a comment wasn't interleaved in a list - default is to attach and break. *)
let fallbackCommentListConfig = break


let eolCommentListConfig = makeListConfig ~break:Never ~postSpace:true ~inline:(true, true) ()

let isListy = function
  | Easy_format.List _ -> true
  | _ -> false


let strip_trailing_whitespace str =
   Re_str.global_replace (Re_str.regexp " +$") "" str

let easyFormatToFormatter f x =
  let buf = Buffer.create 1000 in
  let fauxmatter = Format.formatter_of_buffer buf in
  let _ = Format.pp_set_margin fauxmatter settings.width in
  if debugWithHtml.contents then
    Easy_format.Pretty.define_styles fauxmatter html_escape html_style;
  let _ = Easy_format.Pretty.to_formatter fauxmatter x in
  let trimmed = strip_trailing_whitespace (Buffer.contents buf) |> String.trim in
  Format.fprintf f "%s\n" trimmed;
  pp_print_flush f ()

let wrap fn = fun term ->
  ignore (flush_str_formatter ());
  let f = str_formatter in
  (fn f term; atom (flush_str_formatter ()))


(** Either an ItemComment  (not eol) designates if it's a doc comment (which
    have extra leading stars).  Or an Item which might include its eol
    comments. *)
type commentOrItem =
  | ItemComment of Easy_format.t * bool
  (* The item, and a list of "end of line" comments to render *)
  | Item of (Easy_format.t * Easy_format.t list)


(**
 * Invokes the supplied partitioning function with normalized location
 * positions. AST nodes and comments' locations have endpoints that are not one
 * beyond the actual end. [extractComments] normalizes this and provides
 * the exact first/last character position. The function should return true iff
 * an item with that exact location is to be included in the left partition.
 *
 * The callback is invoked with both normalized physical location, as well as
 * the "attachment" location. The attachment location makes note of where
 * the comment was relative to indentation or the beginning of a line.
 *
 * Attachment location: What portion of text is the comment annotating
 * (including the comment text itself)?
 * Physical location: Where in the file was the comment? Usually a subset of
 * attachment location.
 *)
let rec extractComments comments tester =
  let open Lexing in
  (* There might be an issue here - we shouldn't have to split "up to and including".
     Up to should be sufficient. Comments' end position might be off by one (too large) *)
  comments |> List.partition (fun (str, attLoc, physLoc) ->
    let oneGreaterThanAttachmentLocEnd = attLoc.loc_end.pos_cnum in
    let attachmentLocLastChar = oneGreaterThanAttachmentLocEnd - 1 in
    let oneGreaterThanPhysLocEnd = physLoc.loc_end.pos_cnum in
    let physLastChar = oneGreaterThanPhysLocEnd - 1 in
    tester attLoc.loc_start.pos_cnum attachmentLocLastChar physLoc.loc_start.pos_cnum physLastChar
  )

let space = " "
(* Can't you tell the difference? *)
let tab = "	"
let lineZeroHasMeaningfulContent str =
  not (Re_str.string_match (Re_str.regexp ("^/[\\*" ^ space ^ tab ^ "]*$")) str 0)

let beginsWithStar str =
  Re_str.string_match (Re_str.regexp ("^[" ^ space ^ tab ^ "]*\\*")) str 0

let numLeadingSpace str =
  (* Actually, always true *)
  if Re_str.string_match (Re_str.regexp ("^[" ^ space ^ tab ^ "]*")) str 0 then
    String.length (Re_str.matched_string str)
  else 0

let spaceBeforeMeaningfulContent str =
  if Re_str.string_match (Re_str.regexp ("^/[\\*" ^ space ^ tab ^ "]*")) str 0 then
    String.length (Re_str.matched_string str)
  else 0

(* Computes the smallest leading spaces for non-empty lines *)
let smallestLeadingSpaces strs =
  let rec smallestLeadingSpaces curMin strs = match strs with
    | [] -> curMin
    | hd::tl ->
      if hd = "" then
        smallestLeadingSpaces curMin tl
      else
        let leadingSpace = numLeadingSpace hd in
        let nextMin = min curMin leadingSpace in
        smallestLeadingSpaces nextMin tl
  in
  smallestLeadingSpaces 99999 strs

let convertIsListyToIsSequencey isListyImpl =
  let rec isSequencey layoutNode = match layoutNode with
    | SourceMap (_, subLayoutNode) -> isSequencey subLayoutNode
    | Sequence _ -> true
    | WithEOLComment (_, sub) -> isSequencey sub
    | Label (_, _, _) -> false
    | Easy easy -> isListyImpl easy
  in
  isSequencey

let isSequencey = convertIsListyToIsSequencey isListy

let inline ?(preSpace=false) ?(postSpace=false) labelTerm term =
  makeList ~inline:(true, true) ~postSpace ~preSpace ~indent:0 ~break:Never [labelTerm; term]

let breakline labelTerm term =
  makeList ~inline:(true, true) ~indent:0 ~break:Always_rec [labelTerm; term]

let insertBlankLines n term =
  if n = 0 then
    term
  else
    makeList ~inline:(true, true) ~indent:0 ~break:Always_rec (Array.to_list (Array.make n (atom "")) @ [term])

let string_after s n = String.sub s n (String.length s - n)

let wrapComment txt =
  ("/*" ^ txt ^ "*/")

let formatComment_ txt =
  let commLines = Re_str.split_delim (Re_str.regexp "\n") (wrapComment txt) in
  match commLines with
  | [] -> atom ""
  | [hd] ->
    makeList ~inline:(true, true) ~postSpace:true ~preSpace:true ~indent:0 ~break:IfNeed [atom hd]
  | zero::one::tl ->
     let hasMeaningfulContentOnLineZero = lineZeroHasMeaningfulContent zero in
     let attemptRemoveCount = (smallestLeadingSpaces (one::tl)) in
     let leftPad =
       if beginsWithStar one then 1
       else (if hasMeaningfulContentOnLineZero then spaceBeforeMeaningfulContent zero else 1)
     in
     let padNonOpeningLine s =
       let numLeadingSpaceForThisLine = numLeadingSpace s in
       if String.length s == 0 then ""
       else (String.make leftPad ' ') ^
              (string_after s (min attemptRemoveCount numLeadingSpaceForThisLine)) in
     let lines = zero :: List.map padNonOpeningLine (one::tl) in
     makeList ~inline:(true, true) ~indent:0 ~break:Always_rec (List.map atom lines)

let formatComment ?locOpt txt =
  let list = formatComment_ txt in
  match locOpt with
  | None ->
     list
  | Some loc ->
     SourceMap (loc, list)

(** [hasComment layout] checks if a layout has comment attached to it *)
let rec hasComment = function
  | WithEOLComment (_, _) -> true
  | SourceMap (_, sub) -> hasComment sub
  | _ -> false

let rec append ?(space=false) txt = function
  | SourceMap (loc, sub) -> SourceMap (loc, append ~space txt sub)
  | Sequence (config, l) when snd config.wrap <> "" ->
     let sep = if space then " " else "" in
     Sequence ({config with wrap=(fst config.wrap, snd config.wrap ^ sep ^ txt)}, l)
  | Sequence (config, l) when config.sep = "" ->
     let sub = List.mapi (fun i layout ->
                   if i + 1 = List.length l then
                     append ~space txt layout
                   else
                     layout
                 ) l in
     Sequence (config,  sub)
  | Label (formatter, left, right) ->
     Label (formatter, left, append ~space txt right)
  | layout ->
     inline ~postSpace:space layout (atom txt)

let appendSep spaceBeforeSep sep layout =
  let sep = if spaceBeforeSep then
              " " ^ sep
            else
              sep in
  append sep layout

let rec flattenCommentAndSep ?spaceBeforeSep:(spaceBeforeSep=false) ?sep = function
  | WithEOLComment (txt, sub) ->
     begin
       match sep with
       | None -> append ~space:true (wrapComment txt) sub
       | Some sep -> append ~space:true (wrapComment txt)
                                   (appendSep spaceBeforeSep sep sub)
     end
  | Sequence (listConfig, [hd]) when hasComment hd ->
    Sequence (listConfig, [flattenCommentAndSep ~spaceBeforeSep ?sep hd])
  | SourceMap (loc, sub) ->
     SourceMap (loc, flattenCommentAndSep ~spaceBeforeSep ?sep sub)
  | layout ->
     begin
       match sep with
       | None -> layout
       | Some sep -> appendSep spaceBeforeSep sep layout
     end

let rec preOrderWalk f layout =
  match f layout with
  | Sequence (listConfig, sublayouts) ->
     let newSublayouts = List.map (preOrderWalk f) sublayouts in
     (Sequence (listConfig, newSublayouts))
  | Label (formatter, left, right) ->
     let newLeftLayout = preOrderWalk f left in
     let newRightLayout = preOrderWalk f right in
     Label (formatter, newLeftLayout, newRightLayout)
  | SourceMap (loc, sub) ->
     let newSub = preOrderWalk f sub in
     SourceMap (loc, newSub)
  | WithEOLComment (c, sub) ->
     let newSub = preOrderWalk f sub in
     WithEOLComment (c, newSub)
  | _ -> layout

(** Recursively unbreaks a layout to make sure they stay within the same line *)
let unbreaklayout = preOrderWalk (function
  | Sequence (listConfig, sublayouts) ->
    Sequence ({listConfig with break=Never}, sublayouts)
  | Label (formatter, left, right) ->
    Label (unbreakLabelFormatter formatter, left, right)
  | layout -> layout
)

(** [consolidateSeparator layout] walks the [layout], extract separators out of each
 *  list and insert them into PrintTree as separated items
 *)
let consolidateSeparator = preOrderWalk (function
  | Sequence (listConfig, sublayouts)
       when listConfig.sep <> ""
         && listConfig.sepLeft
    ->
     let layoutsWithSepAndComment =
       List.mapi (fun i layout ->
           (* Do not render the final separator *)
           if not listConfig.renderFinalSep && i + 1 = List.length sublayouts then
             flattenCommentAndSep ~spaceBeforeSep:listConfig.preSpace layout
           else
             flattenCommentAndSep ~spaceBeforeSep:listConfig.preSpace ~sep:listConfig.sep layout) sublayouts in
     let break = if List.exists hasComment sublayouts then
                   Always_rec
                 else
                   listConfig.break in
     let sep = "" in
     let preSpace = false in
     Sequence ({listConfig with sep; break; preSpace}, layoutsWithSepAndComment)
  | WithEOLComment _ as layout ->
     makeList ~inline:(true, true) ~postSpace:false ~preSpace:true ~indent:0
              ~break:Always_rec [flattenCommentAndSep layout]
  | layout -> layout
)

(** [insertLinesAboveItems layout] walkts the [layout] and insert empty lines
 *  based on the configuration of newlinesAboveItems
 *)
let insertLinesAboveItems = preOrderWalk (function
  | Sequence (listConfig, sublayouts)
       when listConfig.newlinesAboveItems <> 0
    ->
     let layoutsWithLinesInjected =
       List.map (insertBlankLines listConfig.newlinesAboveItems) sublayouts in
     Sequence ({listConfig with newlinesAboveItems=0}, layoutsWithLinesInjected)
  | layout -> layout
)

(** Union of two locations *)
let unionLoc loc1 loc2 =
  match (loc1, loc2) with
  | None, _ -> loc2
  | _, None -> loc1
  | Some loc1, Some loc2  -> Some {loc1 with loc_end = loc2.loc_end}

(** [getLocFromLayout] recursively takes the unioned location of its children,
 *  and returns the max one
 *)
let rec getLocFromLayout = function
  | Sequence (listConfig, subLayouts) ->
     let locs = List.map getLocFromLayout subLayouts in
     List.fold_left unionLoc None locs
  | Label (formatter, left, right) ->
     let leftLoc = getLocFromLayout left in
     let rightLoc = getLocFromLayout right in
     unionLoc leftLoc rightLoc
  | SourceMap (loc, _) ->
     Some loc
  | WithEOLComment (_, sub) ->
     getLocFromLayout sub
  | _ -> None

(**
 * Returns true if loc1 contains loc2
 *)
let containLoc loc1 loc2 =
  loc1.loc_start.Lexing.pos_cnum <= loc2.loc_start.Lexing.pos_cnum &&
  loc1.loc_end.Lexing.pos_cnum >= loc2.loc_end.Lexing.pos_cnum

(**
 * Returns true if loc1 is before loc2
 *)
let beforeLoc loc1 loc2 =
  loc1.loc_end.Lexing.pos_cnum <= loc2.loc_start.Lexing.pos_cnum

let attachEOLComment layout txt =
  WithEOLComment (txt, layout)


(**
 * Returns true if the layout's location contains loc
 *)
let layoutContainsLoc loc layout =
  match getLocFromLayout layout with
  | None -> false
  | Some subLoc -> containLoc subLoc loc


(**
 * Returns true if any of the subLayout's location contains loc
 *)
let anySublayoutContainLocation loc =
  List.exists (layoutContainsLoc loc)

let isDocComment (c, _, _) = String.length c > 0 && c.[0] == '*'

(**
 * prependSingleLineComment inserts a single line comment right above layout
 *)
let rec prependSingleLineComment ?newlinesAboveDocComments:(newlinesAboveDocComments=0) comment layout =
  let (txt, _, loc) = comment in
  match layout with
  | WithEOLComment (c, sub) ->
     WithEOLComment (c, prependSingleLineComment ~newlinesAboveDocComments comment sub)
  | SourceMap (loc, sub) ->
     SourceMap (loc, prependSingleLineComment ~newlinesAboveDocComments comment sub)
  | Sequence (config, hd::tl) when config.break = Always_rec->
     Sequence(config, (prependSingleLineComment ~newlinesAboveDocComments comment hd)::tl)
  | layout ->
     let withComment = breakline (formatComment ~locOpt:loc txt) layout in
     if isDocComment comment then
       insertBlankLines newlinesAboveDocComments withComment
     else
       withComment

(**
 * [looselyAttachComment layout comment] preorderly walks the layout and
 * find a place where the comment can be loosely attached to
 *)
let rec looselyAttachComment layout ((txt, _, commentLoc) as comment) =
  match layout with
  | SourceMap (loc, sub) ->
     SourceMap (loc, looselyAttachComment sub comment)
  | WithEOLComment (c, sub) ->
     WithEOLComment (c, looselyAttachComment sub comment)
  | Easy e ->
     inline ~postSpace:true layout (formatComment txt)
  | Sequence (listConfig, subLayouts) when anySublayoutContainLocation commentLoc subLayouts ->
     (* If any of the subLayout strictly contains this comment, recurse into to it *)
     let subLayouts = List.map (fun layout ->
                          if layoutContainsLoc commentLoc layout then
                            looselyAttachComment layout comment
                          else
                            layout
                        ) subLayouts in
     Sequence (listConfig, subLayouts)
  | Sequence (listConfig, subLayouts) when subLayouts == [] ->
    (* If there are no subLayouts (empty body), create a Sequence of just the comment *)
    Sequence (listConfig, [formatComment txt])
  | Sequence (listConfig, subLayouts) ->
     let (beforeComment, afterComment) = Syntax_util.pick_while (fun layout ->
                                             match getLocFromLayout layout with
                                             | None -> true
                                             | Some loc -> beforeLoc loc commentLoc
                                           ) subLayouts in
     let newSubLayout = match List.rev beforeComment with
       | [] ->
          prependSingleLineComment comment (List.hd afterComment) :: (List.tl afterComment)
       | hd::tl -> (attachEOLComment hd txt :: tl |> List.rev) @ afterComment
     in
     Sequence (listConfig, newSubLayout)
  | Label (formatter, left, right) ->
     let leftLoc = getLocFromLayout left in
     let rightLoc = getLocFromLayout right in
     let newLeft, newRight = match (leftLoc, rightLoc) with
       | (None, None) ->
          (left, looselyAttachComment right comment)
       | (_, Some loc2) when containLoc loc2 commentLoc ->
          (left, looselyAttachComment right comment)
       | (Some loc1, _) when containLoc loc1 commentLoc ->
          (looselyAttachComment left comment, right)
       | (Some loc1, Some loc2) when beforeLoc commentLoc loc1 ->
          (prependSingleLineComment comment left, right)
       | (Some loc1, Some loc2) when beforeLoc commentLoc loc2 ->
          (left, prependSingleLineComment comment right)
       | _ -> (left, attachEOLComment right txt)
     in
     Label (formatter, newLeft, newRight)

(**
 * [insertSingleLineComment layout comment] preorderly walks the layout and
 * find a place where the SingleLineComment can be fit into
 *)
let rec insertSingleLineComment layout comment =
  let (txt, _, commentLoc) = comment in
      match layout with
      | SourceMap (loc, sub) ->
         SourceMap (loc, insertSingleLineComment sub comment)
      | WithEOLComment (c, sub) ->
         WithEOLComment (c, insertSingleLineComment sub comment)
      | Easy e ->
         prependSingleLineComment comment layout
      | Sequence (listConfig, subLayouts) when subLayouts == [] ->
        (* If there are no subLayouts (empty body), create a Sequence of just the comment *)
        Sequence (listConfig, [formatComment txt])
      | Sequence (listConfig, subLayouts) ->
         let newlinesAboveDocComments = listConfig.newlinesAboveDocComments in
         let (beforeComment, afterComment) = Syntax_util.pick_while (fun layout ->
                                                 match getLocFromLayout layout with
                                                 | None -> true
                                                 | Some loc -> beforeLoc loc commentLoc
                                               ) subLayouts in
         begin
           match afterComment with
           | (* Nothing in the list is after comment, attach comment to the statement before the comment *)
           [] -> let revBeforeComment = List.rev beforeComment in
                 let lastItemBeforeComment = List.hd revBeforeComment in
                 Sequence (listConfig,  (List.rev
                                           (breakline lastItemBeforeComment (formatComment ~locOpt:commentLoc txt) :: (List.tl revBeforeComment))))
           | hd::tl ->
              let afterComment =
                match getLocFromLayout hd with
                | Some loc when containLoc loc commentLoc ->
                   insertSingleLineComment hd comment :: tl
                | Some loc ->
                   SourceMap (loc, (prependSingleLineComment ~newlinesAboveDocComments comment hd)) :: tl
                | _ ->
                   prependSingleLineComment ~newlinesAboveDocComments comment hd :: tl
              in
              Sequence (listConfig, beforeComment @ afterComment)
         end
      | Label (formatter, left, right) ->
         let leftLoc = getLocFromLayout left in
         let rightLoc = getLocFromLayout right in
         let newLeft, newRight = match (leftLoc, rightLoc) with
           | (None, None) ->
              (left, insertSingleLineComment right comment)
           | (_, Some loc2) when containLoc loc2 commentLoc ->
              (left, insertSingleLineComment right comment)
           | (Some loc1, _) when containLoc loc1 commentLoc ->
              (insertSingleLineComment left comment, right)
           | (Some loc1, Some loc2) when beforeLoc commentLoc loc1 ->
              (prependSingleLineComment comment left, right)
           | (Some loc1, Some loc2) when beforeLoc commentLoc loc2 ->
              (left, prependSingleLineComment comment right)
           | _ -> (left, breakline right (formatComment ~locOpt:commentLoc txt))
         in
         Label (formatter, newLeft, newRight)

let rec attachCommentToNodeRight layout ((txt, t, loc) as comment) =
  match layout with
  | Sequence (config, sub) when snd config.wrap <> "" ->
     Sequence ({config with wrap=(fst config.wrap, snd config.wrap ^ " " ^ (wrapComment txt))}, sub)
  | SourceMap (loc, sub) ->
     SourceMap (loc, attachCommentToNodeRight sub comment)
  | layout ->
     begin
       match t with
       | EndOfLine ->
          WithEOLComment (txt, layout)
       | _ ->
          inline ~postSpace:true layout (formatComment txt)
     end

let rec attachCommentToNodeLeft ((txt, _, loc) as comment) layout =
  match layout with
  | Sequence (config, sub) when snd config.wrap <> "" ->
     Sequence ({config with wrap=(wrapComment txt ^ " " ^ (fst config.wrap), snd config.wrap)}, sub)
  | SourceMap (loc, sub) ->
     SourceMap (loc, attachCommentToNodeLeft comment sub )
  | layout ->
     Label (inlineLabel, (formatComment txt), layout)

let isNone opt =
  match opt with
  | None -> true | _ -> false


(** [tryPerfectlyAttachComment layout comment] postorderly walk the [layout] and tries
 *  to perfectly attach a comment with a layout node.
 *
 *  Perfectly attach here means a comment's start location is equal to the node's end location
 *  and vice versa.
 *
 *  If the comment can be perfectly attached to any layout node, returns (newLayout, None),
 *  meaning the comment is consumed. Otherwise returns the (unchangedLayout, Some comment),
 *  meaning the comment is not consumed.
 *)
let rec tryPerfectlyAttachComment layout comment =
  match comment with
  | None -> layout, comment
  | Some ((s, t, commLoc) as c)  -> begin
     match layout with
     | Sequence (listConfig, subLayouts) ->
        let distributeCommentIntoSubLayouts (i, processed, newComment) layout =
          let (layout, newComment) = tryPerfectlyAttachComment layout newComment in
          i + 1, layout::processed, newComment
        in
        let (_, processed, consumed) = List.fold_left distributeCommentIntoSubLayouts
                                                      (0, [], comment) (List.rev subLayouts) in
        Sequence (listConfig, processed), consumed
     | Label (labelFormatter, left, right) ->
        let (newRight, comment) = tryPerfectlyAttachComment right comment in
        let (newLeft, comment) = tryPerfectlyAttachComment left comment in
        Label (labelFormatter, newLeft, newRight), comment
     | SourceMap (loc, subLayout) ->
        if loc.loc_end.Lexing.pos_lnum = loc.loc_start.Lexing.pos_lnum &&
             commLoc.loc_start.Lexing.pos_cnum = loc.loc_end.Lexing.pos_cnum then
          SourceMap (loc, makeList ~inline:(true, true) ~break:Always
                                   [unbreaklayout (attachCommentToNodeRight subLayout c)]), None
        else
          let (layout, comment) = tryPerfectlyAttachComment subLayout comment in
          begin
            match comment with
            | None -> (SourceMap (loc, layout), None)
            | Some ((s, t, commLoc) as comment)->
               if commLoc.loc_end.Lexing.pos_cnum = loc.loc_start.Lexing.pos_cnum  then
                 SourceMap (loc, attachCommentToNodeLeft comment layout), None
               else if commLoc.loc_start.Lexing.pos_cnum = loc.loc_end.Lexing.pos_cnum then
                 SourceMap (loc, attachCommentToNodeRight layout comment), None
               else
                 SourceMap (loc, layout), Some comment
          end
     | WithEOLComment (c, sub) ->
        let (processed, consumed) = tryPerfectlyAttachComment sub comment in
        WithEOLComment (c, processed), consumed
     | _ -> layout, comment
     end

(** [insertComment layout comment] inserts comment into layout*)
let insertComment layout comment =
  (* print_layout layout; *)
  let (txt, t, loc) = comment in
  let layout = match t with
  | Regular
  | EndOfLine ->
     let (layout, c) = tryPerfectlyAttachComment layout (Some comment) in
     begin
     match c with
     | None -> layout
     | Some _ -> looselyAttachComment layout comment
     end
  | SingleLine -> insertSingleLineComment layout comment
  in
  (* print_comments [comment]; *)
  (* print_layout layout; *)
  layout

(** [insertComments layout comments] inserts comments into layout*)
let insertComments = List.fold_left insertComment

(** [isSingleLineComment comment] checks if a comment is singleline comment*)
let isSingleLineComment (_, t, _) =
  match t with
  | SingleLine -> true | _ -> false

let rec layoutToEasyFormat_ = function
  | Sequence (listConfig, subLayouts) ->
     easyListWithConfig listConfig (List.map layoutToEasyFormat_ subLayouts)
  | Label (labelFormatter, left, right) ->
     labelFormatter (layoutToEasyFormat_ left) (layoutToEasyFormat_ right)
  | SourceMap (_, subLayout) ->
     layoutToEasyFormat_ subLayout
  | WithEOLComment (_, sub) ->
     layoutToEasyFormat_ sub
  | Easy e -> e

let layoutToEasyFormatNoComments layoutNode =
  layoutToEasyFormat_ layoutNode


let layoutToEasyFormat layoutNode comments =
  (* print_layout layoutNode; *)
  let layout = layoutNode in
  let revComments = List.rev comments in
  let (singleLineComments, nonSingleLineComments) = (List.partition isSingleLineComment revComments) in
  let layout = insertComments layout nonSingleLineComments in
  let layout = consolidateSeparator layout in
  let layout = insertComments layout singleLineComments in
  let layout = insertLinesAboveItems layout in
  let easyFormat= layoutToEasyFormat_ layout in
  (* print_easy_rec easyFormat; *)
  makeEasyList ~break:Always_rec ~indent:0 ~inline:(true, true) [easyFormat]

let partitionFinalWrapping listTester wrapFinalItemSetting x =
  let rev = List.rev x in
  match (rev, wrapFinalItemSetting) with
    | ([], _) -> raise (NotPossible "shouldnt be partitioning 0 label attachments")
    | (_, NeverWrapFinalItem) -> None
    | (last::revEverythingButLast, WrapFinalListyItemIfFewerThan max) ->
        if not (listTester last) || (List.length x) >= max then
          None
        else
          Some (List.rev revEverythingButLast, last)

let semiTerminated term = makeList ~interleaveComments:false [term; atom ";"]


(* postSpace is so that when comments are interleaved, we still use spacing rules. *)
let makeLetSequence letItems =
  makeList
    ~break:Always_rec
    ~inline:(true, false)
    ~wrap:("{", "}")
    ~newlinesAboveComments:0
    ~newlinesAboveItems:0
    ~newlinesAboveDocComments:1
    ~renderFinalSep:false
    ~postSpace:true
    ~sep:";"
    letItems

let makeLetSequenceSingleLine letItems =
  makeList
    ~break:IfNeed
    ~inline:(true, false)
    ~wrap:("{", "}")
    ~newlinesAboveComments:0
    ~newlinesAboveItems:0
    ~newlinesAboveDocComments:1
    ~renderFinalSep:false
    ~preSpace:true
    ~postSpace:true
    ~sep:";"
    letItems

(* postSpace is so that when comments are interleaved, we still use spacing rules. *)
let makeUngaurdedLetSequence letItems =
  makeList
    ~break:Always_rec
    ~inline:(true, true)
    ~wrap:("", "")
    ~newlinesAboveComments:0
    ~indent:0
    ~newlinesAboveItems:0
    ~newlinesAboveDocComments:1
    ~renderFinalSep:false
    ~postSpace:true
    ~sep:";"
    letItems

let formatSimpleAttributed x y =
  makeList
    ~wrap:("(", ")")
    ~break:IfNeed
    ~indent:0
    ~postSpace:true
    [x; y;]

let formatAttributed x y =
  makeList
    ~break:IfNeed
    ~inline:(true, true)
    ~indent:0
    ~postSpace:true
    [x; y]

(* For when the type constraint should be treated as a separate breakable line item itself
   not docked to some value/pattern label.
   fun x
       y
       : retType => blah;
 *)
let formatJustTheTypeConstraint =
  if useSingleColonForNamedArgs then
    (fun typ ->
       (makeList ~postSpace:true [atom ":"; typ]))
  else
    (fun typ ->
       (makeList ~postSpace:false [atom ":"; typ]))

let formatTypeConstraint =
  if useSingleColonForNamedArgs then
    (fun one two ->
      label ~space:true (makeList ~postSpace:true [one; atom ":"]) two)
  else
    (fun one two ->
      label ~space:true (makeList ~postSpace:false [one; atom ":"]) two)

let formatLabeledArgument =
  if useSingleColonForNamedArgs then
    (fun lbl lblSuffix term ->
      label ~space:false (makeList [lbl; atom (":" ^ lblSuffix)]) term)
  else
    (fun lbl lblSuffix term ->
      label ~space:false (makeList [lbl; atom ("::" ^ lblSuffix)]) term)

let formatCoerce expr optType coerced =
  match optType with
    | None ->
      label ~space:true (makeList ~postSpace:true [expr; atom ":>"]) coerced
    | Some typ ->
      label ~space:true (makeList ~postSpace:true [formatTypeConstraint expr typ; atom ":>"]) coerced


(* Standard function application style indentation - no special wrapping
 * behavior.
 *
 * Formats like this:
 *
 *   let result =
 *     someFunc
 *       (10, 20);
 *
 *
 * Instead of this:
 *
 *   let result =
 *     someFunc (
 *       10,
 *       20
 *     );
 *
 *)
let formatIndentedApplication headApplicationItem argApplicationItems =
  label
    ~space:true
    headApplicationItem
    (makeAppList argApplicationItems)


(* The loc, is an optional location or the returned app terms *)
let formatAttachmentApplication finalWrapping (attachTo: (bool * layoutNode) option) (appTermItems, loc) =
  let partitioning = finalWrapping appTermItems in
  let maybeSourceMap maybeLoc x =
    match maybeLoc with
      | None -> x
      | Some loc -> SourceMap (loc, x)
  in
  match partitioning with
    | None -> (
        match (appTermItems, attachTo) with
          | ([], _) -> raise (NotPossible "No app terms")
          | ([hd], None) -> maybeSourceMap loc hd
          | ([hd], (Some (useSpace, toThis))) -> label ~space:useSpace toThis (maybeSourceMap loc hd)
          | (hd::tl, None) ->
            maybeSourceMap loc (formatIndentedApplication hd tl)
          | (hd::tl, (Some (useSpace, toThis))) ->
            label
              ~space:useSpace
              toThis
              (maybeSourceMap loc (formatIndentedApplication hd tl))
      )
    | Some (attachedList, wrappedListy) -> (
        match (attachedList, attachTo) with
          | ([], Some (useSpace, toThis)) -> label ~space:useSpace toThis (maybeSourceMap loc wrappedListy)
          | ([], None) ->
            (* Not Sure when this would happen *)
            maybeSourceMap loc wrappedListy
          | (hd::tl, Some (useSpace, toThis)) ->
            (* TODO: Can't attach location to this - maybe rewrite anyways *)
            let attachedArgs = makeAppList attachedList in
              (label ~space:useSpace toThis (label
              ~space:true attachedArgs wrappedListy))

          | (hd::tl, None) ->
            (* Args that are "attached to nothing" *)
            let appList = makeAppList attachedList in
            maybeSourceMap loc (label ~space:true appList wrappedListy)
      )

(*
  Preprocesses an expression term for the sake of label attachments ([letx =
  expr]or record [field: expr]). Function application should have special
  treatment when placed next to a label. (The invoked function term should
  "stick" to the label in some cases). In others, the invoked function term
  should become a new label for the remaining items to be indented under.
 *)
let applicationFinalWrapping x =
  partitionFinalWrapping isSequencey settings.funcApplicationLabelStyle x

let curriedFunctionFinalWrapping x =
  partitionFinalWrapping isSequencey settings.funcCurriedPatternStyle x

let typeApplicationFinalWrapping typeApplicationItems =
  partitionFinalWrapping isSequencey settings.funcApplicationLabelStyle typeApplicationItems


(* add parentheses to binders when they are in fact infix or prefix operators *)
let protectIdentifier txt =
  if not (needs_parens txt) then atom txt
  else if needs_spaces txt then makeList ~interleaveComments:false ~wrap:("(", ")") ~pad:(true, true) [atom txt]
  else atom ("(" ^ txt ^ ")")

let protectLongIdentifier longPrefix txt =
  makeList ~interleaveComments:false [longPrefix; atom "."; protectIdentifier txt]

let pun_labelled_expression e lbl =
  (match e with
    | { pexp_desc = (Pexp_ident { txt; _ }); _ } when txt = (Longident.parse lbl) -> ""
    | _ -> lbl )

let pun_labelled_pattern e lbl =
  (match e with
    | { ppat_desc = (Ppat_var { txt; _ }) } when txt = lbl -> ""
    | _ -> lbl )

class printer  ()= object(self:'self)
  val pipe = false
  val semi = false
  (* The test and first branch of ternaries must be guarded *)
  method under_pipe = {<pipe=true>}
  method under_semi = {<semi=true>}
  method reset_semi = {<semi=false>}
  method reset_pipe = {<pipe=false>}
  method reset = {<pipe=false;semi=false>}
  method list : 'a . ?sep:space_formatter -> ?first:space_formatter ->
    ?last:space_formatter -> (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a list -> unit
    = default#list
  method option : 'a. ?first:space_formatter -> ?last:space_formatter ->
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit =
    default#option

  method longident = function
    | Lident s -> (protectIdentifier s)
    | Ldot(longPrefix, s) ->
        (protectLongIdentifier (self#longident longPrefix) s)
    | Lapply (y,s) -> makeList ~interleaveComments:false [self#longident y; atom "("; self#longident s; atom ")";]

  (* This form allows applicative functors. *)
  method longident_class_or_type_loc x = self#longident x.txt
  (* TODO: Fail if observing applicative functors for this form. *)
  method longident_loc (x:Longident.t Location.loc) = SourceMap (x.loc, self#longident (x.txt))
  method constant = wrap default#constant

  method constant_string = wrap default#constant_string
  method tyvar = wrap default#tyvar

  (* c ['a,'b] *)
  method class_params_def = function
    | [] -> atom ""
    | l ->
      makeList ~postSpace:true (List.map self#type_param l)

  (* This will fall through to the simple version. *)
  method non_arrowed_core_type x = self#non_arrowed_non_simple_core_type x

  method core_type2 x =
    let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.ptyp_attributes in
    if stdAttrs <> [] then
      formatAttributed
        (self#non_arrowed_simple_core_type {x with ptyp_attributes=[]})
        (self#attributes stdAttrs)
    else
      let rec allArrowSegments xx = match xx.ptyp_desc with
        | Ptyp_arrow (l, ct1, ct2) ->
            (self#type_with_label (l,ct1))::(allArrowSegments ct2)
        | _ -> [self#core_type2 xx]
      in
      match (x.ptyp_desc) with
        | (Ptyp_arrow (l, ct1, ct2)) ->
            let normalized =
              makeList ~break:IfNeed ~sep:"=>" ~preSpace:true ~postSpace:true ~inline:(true, true) (allArrowSegments x)
            in
            SourceMap (x.ptyp_loc, normalized)
        | Ptyp_poly (sl, ct) ->
            let poly =
              makeList ~break:IfNeed [
                makeList ~postSpace:true [
                  makeList ~postSpace:true (List.map (fun x -> self#tyvar x) sl);
                  atom ".";
                ];
                self#core_type ct;
              ]
            in SourceMap (x.ptyp_loc, poly)
        | _ -> self#non_arrowed_core_type x

  (* Same as core_type2 but can be aliased *)
  method core_type x =
    let (arityAttrs, docAtrs, stdAttrs, jsxAttrss) = partitionAttributes x.ptyp_attributes in
    if stdAttrs <> [] then
      formatAttributed
        (self#non_arrowed_simple_core_type {x with ptyp_attributes=[]})
        (self#attributes stdAttrs)
    else match (x.ptyp_desc) with
      | (Ptyp_alias (ct, s)) ->
        SourceMap (
          x.ptyp_loc,
          (label
            ~space:true
            (self#core_type ct)
            (makeList ~postSpace:true [atom "as"; atom ("'" ^ s)])
          )
        )
      | _ -> self#core_type2 x

  method type_with_label (label, ({ptyp_desc} as c)) =
    match label with
      | "" ->  self#non_arrowed_non_simple_core_type c (* otherwise parenthesize *)
      | s  ->
          if s.[0]='?' then
            let len = String.length s - 1 in
            let lbl = String.sub s 1 len in
            match ptyp_desc with
              | Ptyp_constr ({txt}, l) ->
                  assert (is_predef_option txt);
                  let everythingButQuestion =
                    formatLabeledArgument
                      (atom lbl)
                      ""
                      (makeList
                         ~postSpace:true
                         ~break:IfNeed
                         ~inline:(true, true)
                         (* Why not support aliasing here? *)
                         (* I don't think you'll have more than one l here. *)
                         (List.map (self#non_arrowed_non_simple_core_type) l)
                      ) in
                    makeList [everythingButQuestion; atom "?"]
              | _ -> failwith "invalid input in print_type_with_label"
          else formatLabeledArgument (atom s) "" (self#non_arrowed_non_simple_core_type c)

  method type_param (ct, a) =
    makeList [atom (type_variance a); self#core_type ct]

  (* According to the parse rule [type_declaration], the "type declaration"'s
   * physical location (as indicated by [td.ptype_loc]) begins with the
   * identifier and includes the constraints. *)
  method formatOneTypeDef prepend name assignToken ({ptype_params; ptype_kind; ptype_manifest; ptype_loc} as td) =
    let (equalInitiatedSegments, constraints) = (self#type_declaration_binding_segments td) in
    let formattedTypeParams = List.map self#type_param ptype_params in
    let binding = makeList ~postSpace:true (prepend::name::[]) in
    (*
        /-----------everythingButConstraints--------------  | -constraints--\
       /-innerL---| ------innerR--------------------------\
      /binding\     /typeparams\ /--equalInitiatedSegments-\
      type name      'v1    'v1  =  foo = private bar        constraint a = b
    *)

    let labelWithParams = match formattedTypeParams with
        [] -> binding
      | phd::ptl -> label ~space:true binding (makeList ~postSpace:true ~break:IfNeed ~inline:(true, true) (phd::ptl)) in
    let everythingButConstraints =
      let nameParamsEquals = makeList ~postSpace:true [labelWithParams; assignToken] in
      match equalInitiatedSegments with
        | [] -> labelWithParams
        | hd::hd2::hd3::tl -> raise (NotPossible "More than two type segments.")
        | hd::[] ->
            formatAttachmentApplication
              typeApplicationFinalWrapping
              (Some (true, nameParamsEquals))
              (hd, None)
        | hd::hd2::[] ->
            let first = makeList ~postSpace:true ~break:IfNeed ~inline:(true, true) hd in
            let second = makeList ~postSpace:true ~break:IfNeed ~inline:(true, true) hd2 in
            label ~space:true nameParamsEquals (
              label ~space:true
                (makeList ~postSpace:true [first; atom "="])
                (second)
            )
    in
    let everything =
      match constraints with
        | [] -> everythingButConstraints
        | hd::tl -> makeList ~break:IfNeed ~postSpace:true ~indent:0 ~inline:(true, true) (everythingButConstraints::hd::tl)
    in
    (SourceMap (ptype_loc, everything))

  (* shared by [Pstr_type,Psig_type]*)
  method type_def_list l =
    (* As oposed to used in type substitution. *)
    let formatOneTypeDefStandard prepend td =
      let itm =
        self#formatOneTypeDef
          prepend
          (SourceMap (td.ptype_name.loc, (atom td.ptype_name.txt)))
          (atom "=")
          td
      in
      self#attach_std_item_attrs td.ptype_attributes itm
    in

    match l with
      | [] -> raise (NotPossible "asking for type list of nothing")
      | hd::tl ->
          let first =
            match partitionNonrecAttr hd.ptype_attributes with
            | ([], _) -> formatOneTypeDefStandard (atom "type") hd
            | (_, attrs) ->
                let newHd = { hd with ptype_attributes = attrs } in
                formatOneTypeDefStandard (atom "type nonrec") newHd
          in
          match tl with
            (* Exactly one type *)
            | [] -> first
            | tlhd::tltl -> makeList ~indent:0 ~inline:(true, true) ~break:Always_rec (
                first::(List.map (formatOneTypeDefStandard (atom "and")) (tlhd::tltl))
              )

  method type_variant_leaf ?opt_ampersand:(a=false) ?polymorphic:(p=false) = self#type_variant_leaf1 a p true
  method type_variant_leaf_nobar ?opt_ampersand:(a=false) ?polymorphic:(p=false) = self#type_variant_leaf1 a p false

  (* TODOATTRIBUTES: Attributes on the entire variant leaf are likely
   * not parsed or printed correctly. *)
  method type_variant_leaf1 opt_ampersand polymorphic print_bar x =
    let {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} = x in
    let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes pcd_attributes in
    let prefix = if polymorphic then "`" else "" in
    let sourceMappedName = SourceMap (pcd_name.loc, atom (prefix ^ pcd_name.txt)) in
    let nameOf = makeList ~postSpace:true [sourceMappedName] in
    let barName =
      let lst = if print_bar then [atom "|"; sourceMappedName] else [sourceMappedName] in
      makeList ~postSpace:true lst in
    let ampersand_helper i arg =
      let ct = self#non_arrowed_simple_core_type arg in
      let add_ampersand = label (atom "&") in
      if polymorphic then
        if i == 0 && not opt_ampersand then
          ct
        else
          add_ampersand ct
      else
        ct
    in
    let args = List.mapi ampersand_helper pcd_args in
    let gadtRes = match pcd_res with
      | None -> None
      | Some x -> Some (
          makeList ~inline:(true, true) ~break:IfNeed [ (* Single row just so the entire return type breaks onto its own line *)
            formatJustTheTypeConstraint (self#core_type x)
          ]
      ) in
      let normalize lst = match lst with
        | [] -> raise (NotPossible "should not be called")
        | [hd] -> hd
        | _::_ -> makeList ~inline:(true, true) ~break:IfNeed ~postSpace:true lst
      in
      let add_bar name args =
        let lbl = label ~space:true name args in
        makeList ~postSpace:true (if print_bar then [atom "|"; lbl] else [lbl])
      in
      let everything = match (args, gadtRes) with
        | ([], None) -> barName
        | ([], Some res) -> add_bar sourceMappedName res
        | (_::_, None) -> add_bar nameOf (normalize args)
        | (_::_, Some res) -> add_bar nameOf (normalize (args@[res]))
      in
      let everythingWithAttrs =
        if stdAttrs <> [] then
          formatAttributed everything (self#attributes stdAttrs)
        else
          everything
      in
      (SourceMap (pcd_loc, everythingWithAttrs))

  (* Returns the type declaration partitioned into three segments - one
     suitable for appending to a label, the actual type manifest
     and the list of constraints. *)
  method type_declaration_binding_segments x =
    (* Segments of the type binding (occuring after the type keyword) that
       should begin with "=". Zero to two total sections.
       This is just a straightforward reverse mapping from the original parser:
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
    *)
    let privateAtom = (atom "pri") in
    let privatize scope lst = match scope with
      | Public -> lst
      | Private -> privateAtom::lst in

    let recordRow pld =
      let nameColon = SourceMap (pld.pld_name.loc, makeList [atom pld.pld_name.txt; atom ":"]) in
      let withMutable =
        match pld.pld_mutable with
          | Immutable -> nameColon
          | Mutable -> makeList ~postSpace:true [atom "mutable"; nameColon]
      in
      SourceMap (
        pld.pld_loc,
        label ~space:true withMutable (self#core_type pld.pld_type)
      )
    in
    let recordize ?assumeRecordLoc lst =
      let rows = List.map recordRow lst in
      let rowList = makeList ~wrap:("{", "}") ~sep:"," ~postSpace:true ~break:IfNeed rows in
      match assumeRecordLoc with
        | None -> rowList
        | Some loc -> SourceMap(loc, rowList)
    in

    let estimateRecordOpenBracePoint () =
      match x.ptype_params with
        | [] -> x.ptype_name.loc.loc_end
        | hd::tl ->
          (fst (List.nth x.ptype_params (List.length x.ptype_params - 1))).ptyp_loc.loc_end
    in

    let equalInitiatedSegments = match (x.ptype_kind, x.ptype_private, x.ptype_manifest) with
      (* /*empty*/ {(Ptype_abstract, Public, None)} *)
      | (Ptype_abstract, Public, None) -> [

        ]
      (* EQUAL core_type {(Ptype_abstract, Public, Some _)} *)
      | (Ptype_abstract, Public, Some y) -> [
          [self#core_type y]
        ]
      (* EQUAL PRIVATE core_type {(Ptype_abstract, Private, Some $3)} *)
      | (Ptype_abstract, Private, Some y) -> [
          [privateAtom; self#core_type y]
        ]
      (* EQUAL constructor_declarations {(Ptype_variant _., Public, None)} *)
      (* This case is redundant *)
      (* | (Ptype_variant lst, Public, None) -> [ *)
      (*     [makeSpacedBreakableInlineList (List.map type_variant_leaf lst)] *)
      (*   ] *)
      (* EQUAL PRIVATE constructor_declarations {(Ptype_variant _, Private, None)} *)
      | (Ptype_variant lst, Private, None) -> [
          [privateAtom; makeList ~break:IfNeed ~postSpace:true ~inline:(true, true) (List.map self#type_variant_leaf lst)]
        ]
      (* EQUAL private_flag BAR constructor_declarations {(Ptype_variant _, $2, None)} *)
      | (Ptype_variant lst, scope, None) ->  [
          privatize scope [makeList ~break:Always_rec ~postSpace:true ~inline:(true, true) (List.map self#type_variant_leaf lst)]
        ]
      (* EQUAL DOTDOT {(Ptype_open, Public, None)} *)
      | (Ptype_open, Public, None) -> [
          [atom ".."]
        ]
      (* Super confusing how record/variants' manifest is not actually the
         description of the structure. What's in the manifest in that case is
         the *second* EQUALS asignment. *)

      (* EQUAL private_flag LBRACE label_declarations opt_comma RBRACE {(Ptype_record _, $2, None)} *)
      | (Ptype_record lst, scope, None) ->
          let assumeRecordLoc = {loc_start = estimateRecordOpenBracePoint(); loc_end = x.ptype_loc.loc_end; loc_ghost = false} in
          [privatize scope [recordize ~assumeRecordLoc lst]]
      (* And now all of the forms involving *TWO* equals *)
      (* Again, super confusing how manifests of variants/records represent the
         structure after the second equals. *)
      (* ================================================*)


      (* EQUAL core_type EQUAL private_flag opt_bar constructor_declarations {
         (Ptype_variant _, _, Some _)} *)
      | (Ptype_variant lst, scope, Some mani) -> [
          [self#core_type mani];
          let variant = makeList ~break:IfNeed ~postSpace:true ~inline:(true, true) (List.map self#type_variant_leaf lst) in
          privatize scope [variant];
        ]

      (* EQUAL core_type EQUAL DOTDOT {(Ptype_open, Public, Some $2)} *)
      | (Ptype_open, Public, Some mani) -> [
          [atom ".."];
          [self#core_type mani];
        ]
      (* EQUAL core_type EQUAL private_flag LBRACE label_declarations opt_comma RBRACE
           {(Ptype_record _, $4, Some $2)} *)
      | (Ptype_record lst, scope, Some mani) -> [
          [self#core_type mani];
          privatize scope [recordize lst];
        ]

      (* Everything else is impossible *)
      (* ================================================*)

      | (_, _, _ ) ->  raise (NotPossible "Encountered impossible type specification")
    in

    let makeConstraint (ct1, ct2, _) =
      let constraintEq = makeList ~postSpace:true [
        atom "constraint";
        self#core_type ct1;
        atom "=";
      ] in
      label ~space:true constraintEq (self#core_type ct2) in
    let constraints = List.map makeConstraint x.ptype_cstrs in
    (equalInitiatedSegments, constraints)

  method non_arrowed_non_simple_core_type x =
    let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.ptyp_attributes in
    if stdAttrs <> [] then
      formatAttributed
        (self#non_arrowed_simple_core_type {x with ptyp_attributes=[]})
        (self#attributes stdAttrs)
    else
      match x.ptyp_desc with
    (* This significantly differs from the standard OCaml printer/parser:
       Type constructors are no longer simple *)
    | Ptyp_constr (li, l) ->

      (*
         The single identifier has to be wrapped in a [ensureSingleTokenSticksToLabel] to
         avoid (@see @avoidSingleTokenWrapping):
      *)
      let constr = match l with
        (* [ensureSingleTokenSticksToLabel] loses location information which is important
           when you are embedded inside a list and comments are to be interleaved around you.
           Therefore, we wrap the result in the correct [SourceMap].
         *)
        | [] -> SourceMap (li.loc, ensureSingleTokenSticksToLabel (self#longident_loc li))
        | hd::tl ->
            let sourceMappedIdent = SourceMap (li.loc, self#longident_loc li) in

            let typeList = match tl with
              | [] -> self#non_arrowed_simple_core_type hd
              | _ ->
                 let simpleTypeList = (List.map (self#non_arrowed_simple_core_type) (hd::tl)) in
                 (makeList ~inline:(true, true) ~postSpace:true ~break:IfNeed simpleTypeList)
            in
            (label ~space:true sourceMappedIdent typeList)
      in
      (* It's actually better without this source mapped *)
      constr
    | _ -> self#non_arrowed_simple_core_type x

  method non_arrowed_simple_core_type x =
    let (arityAttrs, docAttrs, stdAttrs, jsxAttrs) = partitionAttributes x.ptyp_attributes in
    if stdAttrs <> [] then
      formatSimpleAttributed
        (self#non_arrowed_simple_core_type {x with ptyp_attributes=[]})
        (self#attributes stdAttrs)
    else
      let result =
        match x.ptyp_desc with
        (*   LPAREN core_type_comma_list RPAREN %prec below_NEWDOT *)
        (*       { match $2 with *)
        (*         | [] -> raise Parse_error *)
        (*         | one::[] -> one *)
        (*         | moreThanOne -> mktyp(Ptyp_tuple(List.rev moreThanOne)) } *)
        | Ptyp_tuple l ->
            makeList ~wrap:("(",")") ~sep:"," ~postSpace:true ~break:IfNeed (List.map (self#core_type) l)
        | Ptyp_object (l, o) ->
          let core_field_type (s, attrs, ct) =
            let l = extractStdAttrs attrs in
            (match l with
              | [] -> label ~space:true
                      (label ~space:true (atom s) (atom ":"))
                      (self#core_type ct)
              | _::_ ->
                makeList
                  ~postSpace:true
                  ~break:IfNeed
                  [atom s; (self#attributes attrs); atom ":"; self#core_type ct]
            )
          in
          let openness = match o with
            | Closed -> [atom "."]
            | Open -> [atom ".."]
          in
          let rows = List.map core_field_type l in
          makeList ~break:IfNeed ~preSpace:(List.length rows > 0) ~wrap:("{", "}") (openness @ [makeList ~break:IfNeed ~inline:(true, (List.length rows > 0)) ~postSpace:true ~sep:"," rows])
        | Ptyp_package (lid, cstrs) ->
          let typeConstraint (s, ct) =
            label
              (makeList ~break:IfNeed ~postSpace:true [atom "type"; self#longident_loc s; atom "="])
              (self#core_type ct)
          in
          (
            match cstrs with
              | [] ->
                makeList ~wrap:("(", ")") [
                  (makeList ~postSpace:true [atom "module"; self#longident_loc lid])
                ]
              | _ ->
                makeList ~wrap:("(", ")") [
                  label ~space:true
                    (makeList ~postSpace:true [atom "module"; self#longident_loc lid])
                    (makeList
                      ~break:IfNeed
                      ~sep:" and"
                      ~wrap:("with", "")
                      ~pad:(true, false)
                      (List.map typeConstraint cstrs))
                ]
          )
        (*   | QUOTE ident *)
        (*       { mktyp(Ptyp_var $2) } *)
        | Ptyp_var s -> ensureSingleTokenSticksToLabel (self#tyvar s)
        (*   | UNDERSCORE *)
        (*       { mktyp(Ptyp_any) } *)
        | Ptyp_any -> ensureSingleTokenSticksToLabel (atom "_")
        (*   | type_longident *)
        (*       { mktyp(Ptyp_constr(mkrhs $1 1, [])) } *)
        | Ptyp_constr (li, []) ->
            (* Only simple if zero type paramaters *)
            ensureSingleTokenSticksToLabel (self#longident_loc li)
        | Ptyp_variant (l, closed, low) ->
          let pcd_loc = x.ptyp_loc in
          let pcd_attributes = x.ptyp_attributes in
          let pcd_res = None in
          let variant_helper rf =
            match rf with
              | Rtag (label, _, opt_ampersand, pcd_args) ->
                let pcd_name = {
                  txt = label;
                  loc = pcd_loc;
                } in
                self#type_variant_leaf ~opt_ampersand ~polymorphic:true {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes}
              | Rinherit ct -> self#core_type ct in
          let (designator, tl) =
            match (closed,low) with
              | (Closed,None) -> ("", [])
              | (Closed,Some tl) -> ("<", tl)
              | (Open,_) -> (">", []) in
          let node_list = List.map variant_helper l in
          let ll = (List.map (fun t -> atom ("`" ^ t)) tl) in
          let tag_list = makeList ~postSpace:true ~break:IfNeed ((atom ">")::ll) in
          let type_list = if List.length tl != 0 then node_list@[tag_list] else node_list in
          makeList ~wrap:("[" ^ designator,"]") ~pad:(true, false) ~postSpace:true ~break:IfNeed type_list
        | Ptyp_class (li, l) ->
          (match l with
            | [] -> makeList [atom "#"; self#longident_loc li]
            | _::_ ->
              label
                ~space:true
                (makeList [atom "#"; self#longident_loc li])
                (makeList ~postSpace:true ~inline:(true, false) (List.map self#core_type l))
          )
        | Ptyp_extension e -> self#extension e
        | Ptyp_constr (_, _::_)
        | Ptyp_arrow (_, _, _)
        | Ptyp_alias (_, _)
        | Ptyp_poly (_, _) ->
            makeList ~wrap:("(",")") ~break:IfNeed [self#core_type x]
      in
      SourceMap (x.ptyp_loc, result)
  (* TODO: ensure that we have a form of desugaring that protects *)
  (* when final argument of curried pattern is a type constraint: *)
  (* | COLON non_arrowed_core_type EQUALGREATER expr
      { mkexp_constraint $4 (Some $2, None) }         *)
  (*                         \----/   \--/
                             constraint coerce

                             Creates a ghost expression:
                             mkexp_constraint | Some t, None -> ghexp(Pexp_constraint(e, t))
  *)

  method pattern_list_split_cons acc = function
    | {
      ppat_desc = Ppat_construct (
        { txt = Lident("::"); loc=consLoc },
        Some {ppat_desc = Ppat_tuple ([pat1; pat2])}
      )
    } ->
        self#pattern_list_split_cons (pat1::acc) pat2
    | p -> (List.rev acc), p

  (*
   * Adds parens to the right sub-tree when it is not a single node:
   *
   * A | B                   is formatted as    A | B
   * A | (B | C)             is formatted as    A | (B | C)
   *
   * Also, adds parens to both sub-trees when both of them
   * are not a single node:
   * (A | B) | (C | D)       is formatted as    A | B | (C | D)
   * A | B | (C | D)         is formatted as    A | B | (C | D)
   * (A | B) | C             is formatted as    A | B | C
   * A | B | C               is formatted as    A | B | C
   *
   *)
  method or_pattern p1 p2 =
    let (p1_raw, p2_raw) = (self#pattern p1, self#pattern p2) in
    let (left, right) =
      match p2.ppat_desc with
        | Ppat_or _ -> (p1_raw, formatPrecedence p2_raw)
        | _ -> (p1_raw, p2_raw)
    in
    makeList
      ~break:IfNeed
      ~inline:(true, true)
      ~sep:"|"
      ~postSpace:true
      ~preSpace:true
      [left; right]

  method pattern_without_or x =
    let patternSourceMap pt layout = (SourceMap (pt.ppat_loc, layout)) in
    (* TODOATTRIBUTES: Handle the stdAttrs here *)
    let (arityAttrs, docAtrs, _, jsxAttrs) = partitionAttributes x.ppat_attributes in
    match x.ppat_desc with
      | Ppat_alias (p, s) ->
          let raw_pattern = (self#pattern p) in
          let pattern_with_precedence = match p.ppat_desc with
            | Ppat_or (p1, p2) -> formatPrecedence (self#or_pattern p1 p2)
            | _ -> raw_pattern
          in
          label ~space:true
            (patternSourceMap p pattern_with_precedence)
            (makeList ~postSpace:true [
              atom "as";
              (SourceMap (s.loc, (protectIdentifier s.txt)))
            ]) (* RA*)
      | Ppat_variant (l, Some p) ->
          if arityAttrs != [] then
            raise (NotPossible "Should never see embedded attributes on poly variant")
          else
            let layout = (self#constructor_pattern ~polyVariant:true ~arityIsClear:true (atom ("`" ^ l)) p) in
            SourceMap (x.ppat_loc, layout)
      | Ppat_lazy p -> label ~space:true (atom "lazy") (self#simple_pattern p)
      | Ppat_construct (({txt} as li), po) when not (txt = Lident "::")-> (* FIXME The third field always false *)
          let liSourceMapped = SourceMap (li.loc, (self#longident_loc li)) in
          let formattedConstruction = match po with
            (* TODO: Check the explicit_arity field on the pattern/constructor
               attributes to determine if should desugar to an *actual* tuple. *)
            (* | Some ({ *)
            (*   ppat_desc=Ppat_tuple l; *)
            (*   ppat_attributes=[{txt="explicit_arity"; loc}] *)
            (* }) -> *)
            (*   label ~space:true (self#longident_loc li) (makeSpacedBreakableInlineList (List.map self#simple_pattern l)) *)
            | Some xx ->
                let arityIsClear = isArityClear arityAttrs in
                self#constructor_pattern ~arityIsClear liSourceMapped xx
            | None ->
                liSourceMapped

          in
            SourceMap (x.ppat_loc, formattedConstruction)
      | _ -> self#simple_pattern x

  method pattern x=
    let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.ppat_attributes in
    if stdAttrs <> [] then
      formatAttributed
        (* Doesn't need to be simple_pattern because attributes are parse as
         * appyling to the entire "function application style" syntax preceeding them *)
        (self#pattern {x with ppat_attributes=arityAttrs})
        (self#attributes stdAttrs)
    else match x.ppat_desc with
      | Ppat_or (p1, p2) ->
        self#or_pattern p1 p2
      | _ -> self#pattern_without_or x

  method pattern_list_helper pat =
    let pat_list, pat_last = self#pattern_list_split_cons [] pat in
    match pat_last with
    | {ppat_desc = Ppat_construct ({txt=Lident "[]"},_)} -> (* [x,y,z] *)
        makeList ~break:IfNeed ~wrap:("[", "]") ~sep:"," ~postSpace:true (List.map self#pattern pat_list)
    | _ -> (* x::y *)
        makeES6List (List.map self#pattern pat_list) (self#pattern pat_last)

  method potentiallyConstrainedPattern1 x = match x.ppat_desc with
    | Ppat_constraint (p, ct) ->
        formatTypeConstraint (self#pattern p) (self#core_type ct)
    | _  -> self#pattern x

  method simple_pattern x =
    let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.ppat_attributes in
    if stdAttrs <> [] then
      formatSimpleAttributed
        (self#simple_pattern {x with ppat_attributes=arityAttrs})
        (self#attributes stdAttrs)
    else
      let itm =
        match x.ppat_desc with
          | Ppat_construct (({loc; txt=Lident ("()"|"[]" as x)}), _) ->
              (* Patterns' locations might include a leading bar depending on the
               * context it was parsed in. Therefore, we need to include further
               * information about the contents of the pattern such as tokens etc,
               * in order to get comments to be distributed correctly.*)

              SourceMap (loc, (atom x))
          | Ppat_construct (({txt=Lident "::"}), po) ->
                self#pattern_list_helper x (* LIST PATTERN *)
          | Ppat_construct (({txt} as li), None) ->
              let liSourceMapped = SourceMap (li.loc, (self#longident_loc li)) in
              SourceMap (x.ppat_loc, liSourceMapped)
          | Ppat_any -> atom "_"
          | Ppat_var ({loc; txt = txt}) ->
            (*
               To prevent this:

                 let oneArgShouldWrapToAlignWith
                   theFunctionNameBinding => theFunctionNameBinding;

               And instead do:

                 let oneArgShouldWrapToAlignWith
                     theFunctionNameBinding => theFunctionNameBinding;

               We have to do something to the non "listy" patterns. Non listy
               patterns don't indent the same amount as listy patterns when docked
               to a label.

               If wrapping the non-listy pattern in [ensureSingleTokenSticksToLabel]
               you'll get the following (even though it should wrap)

                 let oneArgShouldWrapToAlignWith theFunctionNameBinding => theFunctionNameBinding;

             *)
              SourceMap (loc, (protectIdentifier txt))
          | Ppat_array l ->
              makeList ~wrap:("[|", "|]") ~break:IfNeed ~postSpace:true ~sep:"," (List.map self#pattern l)
          | Ppat_unpack (s) ->
              makeList ~wrap:("(", ")") ~break:IfNeed ~postSpace:true [atom "module"; atom s.txt]
          | Ppat_type li ->
              makeList [atom "#"; self#longident_loc li]
          | Ppat_record (l, closed) ->
              let longident_x_pattern (li, p) =
                match (li, p.ppat_desc) with
                  | ({txt=Lident s}, Ppat_var {txt}) when s = txt ->
                      self#longident_loc li
                  | _ ->
                      label ~space:true (makeList [self#longident_loc li; atom ":"]) (self#pattern p)
              in
              let rows = (List.map longident_x_pattern l)@(
                match closed with
                  | Closed -> []
                  | _ -> [atom "_"]
              ) in
              makeList ~wrap:("{", "}") ~break:IfNeed ~sep:"," ~postSpace:true rows
          | Ppat_tuple l ->
              makeList ~wrap:("(", ")") ~sep:"," ~postSpace:true ~break:IfNeed (List.map (self#potentiallyConstrainedPattern1) l)
          | Ppat_constant (c) -> (self#constant c)
          | Ppat_interval (c1, c2) -> makeList [self#constant c1; atom ".."; self#constant c2]
          | Ppat_variant (l, None) -> makeList[atom "`"; atom l]
          | Ppat_constraint (p, ct) ->
              formatPrecedence (formatTypeConstraint (self#pattern p) (self#core_type ct))
          | Ppat_lazy p ->formatPrecedence (label ~space:true (atom "lazy") (self#simple_pattern p))
          | Ppat_extension e -> self#extension e
          | Ppat_exception p ->
              makeList ~postSpace:true [atom "exception"; self#pattern p]
          | _ -> formatPrecedence (self#pattern x) (* May have a redundant sourcemap *)
        in
        SourceMap (x.ppat_loc, itm)

  method label_exp (l,opt,p) =
    if l = "" then
      self#simple_pattern p (*single case pattern parens needed here *)
    else
    if l.[0] = '?' then
      let len = String.length l - 1 in
      let lbl = String.sub l 1 len in
        let lbl = pun_labelled_pattern p lbl in
        (formatLabeledArgument
           (atom lbl)
           ""
           (label
             (makeList [(self#simple_pattern p); atom "="])
             (match opt with None -> (atom "?") | Some o -> (self#simplifyUnparseExpr o))))
    else
      match p.ppat_desc with
        | _ ->
          let lbl = pun_labelled_pattern p l in
          formatLabeledArgument
            (atom lbl)
            ""
            (self#simple_pattern p)

  method access op cls e1 e2 = makeList ~interleaveComments:false [
    (* Important that this be not breaking - at least to preserve same
       behavior as stock desugarer. It might even be required (double check
       in parser.mly) *)
    e1;
    atom ".";
    atom op;
    e2;
    atom cls;
  ]

  method simple_get_application x =
    let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.pexp_attributes in
    match (x.pexp_desc, stdAttrs, jsxAttrs) with
    | (_, attrHd::attrTl, []) -> None (* Has some printed attributes - not simple *)
    | (Pexp_apply ({pexp_desc=Pexp_ident loc}, l), [], jsx::_) -> (
      (* TODO: Soon, we will allow the final argument to be an identifier which
         represents the entire list. This would be written as
         `<tag>...list</tag>`. If you imagine there being an implicit [] inside
         the tag, then it would be consistent with array spread:
         [...list] evaluates to the thing as list.
      *)
      let rec isLabeledArgsAndFinalList arguments =
        match arguments with
        | ("", {pexp_desc = Pexp_construct ({txt = Lident "::"}, _)}) :: []
        | ("", {pexp_desc = Pexp_construct ({txt = Lident "[]"}, _)}) :: [] -> true
        (* Any other kind of non-named argument besides the above disqualifies *)
        | ("", _) :: _ -> false
        | (lbl, _)::tail -> isLabeledArgsAndFinalList tail
        | [] -> false
      in
      let moduleNameList = List.rev (List.tl (List.rev (Longident.flatten loc.txt))) in
      if List.length moduleNameList > 0 then
        if Longident.last loc.txt = "createElement" && isLabeledArgsAndFinalList l then
          Some (self#formatJSXComponent (String.concat "." moduleNameList) l)
        else
          None
      else if isLabeledArgsAndFinalList l then
        Some (self#formatJSXComponent (Longident.last loc.txt) l)
      else
        None
    )
    | (Pexp_apply (eFun, ls), [], []) -> (
      match (printedStringAndFixityExpr eFun, ls) with
      (* We must take care not to print two subsequent prefix operators without
         spaces between them (`! !` could become `!!` which is totally
         different).  *)
      | (AlmostSimplePrefix prefixStr, [("", rightExpr)]) ->
        let forceSpace = match rightExpr.pexp_desc with
          | Pexp_apply (ee, lsls) ->
            (match printedStringAndFixityExpr ee with | AlmostSimplePrefix _ -> true | _ -> false)
          | _ -> false
        in
        let rightItm = self#simplifyUnparseExpr rightExpr in
        Some (label ~space:forceSpace (atom prefixStr) rightItm)
      | (Infix infixStr, [(_, leftExpr); (_, rightExpr)]) when infixStr.[0] = '#' ->
        (* Little hack. We check the right expression to see if it's also a SHARPOP, if it is
           we call `formatPrecedence` on the result of `simplifyUnparseExpr` to add the appropriate
           parens. This is done because `unparseExpr` doesn't seem to be able to handle
           high enough precedence things. Using the normal precedence handling, something like

              ret #= (Some 10)

            gets pretty printed to

              ret #= Some 10

            Which seems to indicate that the pretty printer doesn't think `#=` is of
            high enough precedence for the parens to be worth adding back. *)
        let rightItm = (
          match rightExpr.pexp_desc with
          | Pexp_apply (eFun, ls) -> (
            match (printedStringAndFixityExpr eFun, ls) with
              | (Infix infixStr, [(_, _); (_, _)]) when infixStr.[0] = '#' -> formatPrecedence (self#simplifyUnparseExpr rightExpr)
              | _ -> self#simplifyUnparseExpr rightExpr
          )
          | _ -> self#simplifyUnparseExpr rightExpr
        ) in
        Some (makeList [self#simple_enough_to_be_lhs_dot_send leftExpr; atom infixStr; rightItm])
      | (_, _) -> (
        match (eFun, ls) with
        | ({pexp_desc = Pexp_ident {txt = Ldot (Lident ("String"),"get")}}, [(_,e1);(_,e2)]) ->
          Some (self#access "[" "]" (self#simplifyUnparseExpr e1) (self#unparseExpr e2))
        | ({pexp_desc = Pexp_ident {txt = Ldot (Lident ("Array"),"get")}}, [(_,e1);(_,e2)]) ->
          Some (self#access "(" ")" (self#simplifyUnparseExpr e1) (self#unparseExpr e2))
        | (
            {pexp_desc= Pexp_ident {txt=Ldot (Ldot (Lident "Bigarray", "Genarray" ), "get")}},
            [(_,a); (_,{pexp_desc=Pexp_array ls})]
          ) ->
          let formattedList = List.map self#simplifyUnparseExpr ls in
          Some (self#access "{" "}" (self#simplifyUnparseExpr a) (makeCommaBreakableList formattedList))
        | ({pexp_desc= Pexp_ident {txt=Ldot (Ldot (Lident "Bigarray", ("Array1"|"Array2"|"Array3")), "get")}}, (_,a)::rest) ->
          let formattedList = List.map self#simplifyUnparseExpr (List.map snd rest) in
          Some (self#access "{" "}" (self#simplifyUnparseExpr a) (makeCommaBreakableList formattedList))
        | _ -> None
      )
    )
    | _ ->  None

  (** Detects "sugar expressions" (sugar for array/string setters) and returns their separate
      parts.  *)
  method sugar_set_expr_parts e =
    if e.pexp_attributes <> [] then None
    (* should also check attributes underneath *)
    else match e.pexp_desc with
      | Pexp_apply ({pexp_desc=Pexp_ident{txt=Ldot (Lident ("Array"), "set")}}, [(_,e1);(_,e2);(_,e3)]) ->
        Some (self#access "(" ")" (self#simplifyUnparseExpr e1) (self#unparseExpr e2), e3)
      | Pexp_apply ({pexp_desc=Pexp_ident {txt=Ldot (Lident "String", "set")}}, [(_,e1);(_,e2);(_,e3)]) ->
        Some ((self#access "[" "]" (self#simplifyUnparseExpr e1) (self#unparseExpr e2)), e3)
      | Pexp_apply (
        {pexp_desc=Pexp_ident {txt = Ldot (Ldot (Lident "Bigarray", array), "set")}},
        label_exprs
      ) -> (
        match array with
          | "Genarray" -> (
            match label_exprs with
            | [(_,a);(_,{pexp_desc=Pexp_array ls});(_,c)] ->
              let formattedList = List.map self#simplifyUnparseExpr ls in
              Some (self#access "{" "}" (self#simplifyUnparseExpr a) (makeCommaBreakableList formattedList), c)
            | _ -> None
          )
          | ("Array1"|"Array2"|"Array3") -> (
            match label_exprs with
            | (_,a)::rest -> (
              match List.rev rest with
              | (_,v)::rest ->
                let args = List.map snd (List.rev rest) in
                let formattedList = List.map self#simplifyUnparseExpr args in
                Some (self#access "{" "}" (self#simplifyUnparseExpr a) (makeCommaBreakableList formattedList), v)
              | _ -> assert false
            )
            | _ -> assert false
          )
          | _ -> None
        )
      | _ -> None

  (*

     How would we know not to print the sequence without { }; protecting the let a?

                            let a
                             |
                           sequence
                          /        \
                    let a           print a
                    alert a
     let res = {
       let a = something();
       {                     \
         alert(a);           | portion to be parsed as a sequence()
         let a = 20;         | The final ; print(a) causes the entire
         alert(a);           | portion to be parsed as a sequence()
       };                    |
       print (a);            /
     }

     ******************************************************************
     Any time the First expression of a sequence is another sequence, or (as in
     this case) a let, wrapping the first sequence expression in { } is
     required.
     ******************************************************************
  *)

  (**
     TODO: Configure the optional ability to print the *minimum* number of
     parens. It's simply a matter of changing [higherPrecedenceThan] to
     [higherOrEqualPrecedenceThan].
   *)

  (* Ensures a rule doesn't reduce until *after* `reducesAfterRight` gets a chance
     to reduce. Example: The addtion rule which has precedence of rightmost
     token "+", in `x + a * b` should not reduce until after the a * b gets
     a chance to reduce. This function would determine the minimum parens to
     ensure that. *)
  method ensureContainingRule ~withPrecedence ~reducesAfterRight =
    match self#unparseExprRecurse reducesAfterRight with
    | (SpecificInfixPrecedence ({reducePrecedence; shiftPrecedence}, rightRecurse)) ->
      if higherPrecedenceThan shiftPrecedence withPrecedence then rightRecurse
      else if (higherPrecedenceThan withPrecedence shiftPrecedence) then
        formatPrecedence ~loc:reducesAfterRight.pexp_loc rightRecurse
      else (
        if isRightAssociative withPrecedence then
          rightRecurse
        else
          formatPrecedence ~loc:reducesAfterRight.pexp_loc rightRecurse
      )
    | FunctionApplication itms ->
      formatAttachmentApplication applicationFinalWrapping None (itms, Some reducesAfterRight.pexp_loc)
    | PotentiallyLowPrecedence itm -> formatPrecedence ~loc:reducesAfterRight.pexp_loc itm
    | Simple itm -> itm

  method ensureExpression expr ~reducesOnToken =
    match self#unparseExprRecurse expr with
    | SpecificInfixPrecedence ({reducePrecedence; shiftPrecedence}, leftRecurse) ->
      if higherPrecedenceThan reducePrecedence reducesOnToken then leftRecurse
      else if higherPrecedenceThan reducesOnToken reducePrecedence then
        formatPrecedence ~loc:expr.pexp_loc leftRecurse
      else (
        if isLeftAssociative reducesOnToken then
          leftRecurse
        else
          formatPrecedence ~loc:expr.pexp_loc leftRecurse
      )
    | FunctionApplication itms -> formatAttachmentApplication applicationFinalWrapping None (itms, Some expr.pexp_loc)
    | PotentiallyLowPrecedence itm -> formatPrecedence ~loc:expr.pexp_loc itm
    | Simple itm -> itm


  (** Attempts to unparse: The beginning of a more general printing algorithm,
      that determines how to print based on precedence of tokens and rules.
      The end goal is that this should be completely auto-generated from the
      Menhir parsing tables. We could move more and more into this function.

      You could always just call self#expression, but `unparseExpr` will render
      infix/prefix/unary/terary fixities in their beautiful forms while
      minimizing parenthesis.
  *)
  method unparseExpr x =
    match self#unparseExprRecurse x with
    | SpecificInfixPrecedence ({reducePrecedence; shiftPrecedence}, itm) -> itm
    | FunctionApplication itms -> formatAttachmentApplication applicationFinalWrapping None (itms, Some x.pexp_loc)
    | PotentiallyLowPrecedence itm -> itm
    | Simple itm -> itm

  method simplifyUnparseExpr x =
    match self#unparseExprRecurse x with
    | SpecificInfixPrecedence ({reducePrecedence; shiftPrecedence}, itm) -> formatPrecedence ~loc:x.pexp_loc itm
    | FunctionApplication itms ->
      formatPrecedence ~loc:x.pexp_loc (formatAttachmentApplication applicationFinalWrapping None (itms, Some x.pexp_loc))
    | PotentiallyLowPrecedence itm -> formatPrecedence ~loc:x.pexp_loc itm
    | Simple itm -> itm

  method unparseExprApplicationItems x =
    match self#unparseExprRecurse x with
    | SpecificInfixPrecedence ({reducePrecedence; shiftPrecedence}, itm) -> ([itm], Some x.pexp_loc)
    | FunctionApplication itms -> (itms, Some x.pexp_loc)
    | PotentiallyLowPrecedence itm -> ([itm], Some x.pexp_loc)
    | Simple itm -> ([itm], Some x.pexp_loc)

  method unparseExprRecurse x =
    (* If there are any attributes, render unary like `(~-) x [@ppx]`, and infix like `(+) x y [@attr]` *)
    let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.pexp_attributes in
    (* If there's any attributes, recurse without them, then apply them to
       the ends of functions, or simplify infix printings then append. *)
    if stdAttrs <> [] then
      let withoutVisibleAttrs = {x with pexp_attributes=(arityAttrs @ jsxAttrs)} in
      let attributesAsList = (List.map self#attribute stdAttrs) in
      let itms = match self#unparseExprRecurse withoutVisibleAttrs with
        | SpecificInfixPrecedence ({reducePrecedence; shiftPrecedence}, itm) -> [formatPrecedence ~loc:x.pexp_loc itm]
        | FunctionApplication itms -> itms
        | PotentiallyLowPrecedence itm -> [formatPrecedence ~loc:x.pexp_loc itm]
        | Simple itm -> [itm]
      in
      FunctionApplication [
        makeList
          ~break:IfNeed
          ~inline:(true, true)
          ~indent:0
          ~postSpace:true
          (List.concat [itms; attributesAsList])
      ]
    else
    match self#simplest_expression x with
    | Some se -> Simple se
    | None ->
    match x.pexp_desc with
    | Pexp_apply (e, ls) -> (
      match (self#sugar_set_expr_parts x) with
      (* Returns None if there's attributes - would render as regular function *)
      (* Format as if it were an infix function application with identifier "=" *)
      | Some (simplyFormatedLeftItm, rightExpr) -> (
        let tokenPrec = Token updateToken in
        let rightItm = self#ensureContainingRule ~withPrecedence:tokenPrec ~reducesAfterRight:rightExpr in
        let leftWithOp = makeList ~postSpace:true [simplyFormatedLeftItm; atom updateToken] in
        let expr = label ~space:true leftWithOp rightItm in
        SpecificInfixPrecedence ({reducePrecedence=tokenPrec; shiftPrecedence=tokenPrec}, expr)
      )
      | None -> (
        match (printedStringAndFixityExpr e, ls) with
        | (Infix printedIdent, [("", leftExpr); ("", rightExpr)]) ->
          let infixToken = Token printedIdent in
          let rightItm = self#ensureContainingRule ~withPrecedence:infixToken ~reducesAfterRight:rightExpr in
          let leftItm = self#ensureExpression leftExpr ~reducesOnToken:infixToken in
          let leftWithOp = makeList ~postSpace:true [leftItm; atom printedIdent] in
          let indent = infixTokenRequiresIndent printedIdent in
          let expr = label ~space:true ?indent leftWithOp rightItm in
          SpecificInfixPrecedence ({reducePrecedence=infixToken; shiftPrecedence=infixToken}, expr)
        (* Will be rendered as `(+) a b c` which is parsed with higher precedence than all
           the other forms unparsed here.*)
        | (UnaryPlusPrefix printedIdent, [("", rightExpr)]) ->
          let prec = Custom "prec_unary_plus" in
          let rightItm = self#ensureContainingRule ~withPrecedence:prec ~reducesAfterRight:rightExpr in
          let expr = label ~space:true (atom printedIdent) rightItm in
          SpecificInfixPrecedence ({reducePrecedence=prec; shiftPrecedence=Token printedIdent}, expr)
        | (UnaryMinusPrefix printedIdent, [("", rightExpr)]) ->
          let prec = Custom "prec_unary_minus" in
          let rightItm = self#ensureContainingRule ~withPrecedence:prec ~reducesAfterRight:rightExpr in
          let expr = label ~space:true (atom printedIdent) rightItm in
          SpecificInfixPrecedence ({reducePrecedence=prec; shiftPrecedence=Token printedIdent}, expr)
        (* Will need to be rendered in self#expression as (~-) x y z. *)
        | (_, _) ->
        (* This case will happen when there is something like

             Bar.createElement a::1 b::2 [] [@bla] [@JSX]

           At this point the bla will be stripped (because it's a visible
           attribute) but the JSX will still be there.
         *)
        (match detectJSXComponent e.pexp_desc x.pexp_attributes ls with
          | Some componentName -> FunctionApplication [self#formatJSXComponent componentName ls]
          | None ->
          (* If there was a JSX attribute BUT JSX component wasn't detected,
             that JSX attribute needs to be pretty printed so it doesn't get
             lost
           *)
          let maybeJSXAttr = (match jsxAttrs with
            | [] -> []
            | jsx -> (List.map self#attribute jsx)
          ) in
          let theFunc = SourceMap (e.pexp_loc, (self#simplifyUnparseExpr e)) in
          (*reset here only because [function,match,try,sequence] are lower priority*)
          let theArgs = List.map self#reset#label_x_expression_param ls in
          FunctionApplication (theFunc::theArgs @ maybeJSXAttr)
        )
      )
    )
    | Pexp_construct (li, Some eo) when not (is_simple_construct (view_expr x)) -> (
        match view_expr x with
        (* TODO: Explicit arity *)
        | `normal ->
            let arityIsClear = isArityClear arityAttrs in
            FunctionApplication [self#constructor_expression ~arityIsClear stdAttrs (self#longident_loc li) eo]
        | _ -> assert false
      )
    | Pexp_variant (l, Some eo) ->
        if arityAttrs != [] then
          raise (NotPossible "Should never see embedded attributes on poly variant")
        else
          FunctionApplication [self#constructor_expression ~polyVariant:true ~arityIsClear:true stdAttrs (atom ("`" ^ l)) eo]
    (* TODO: Should protect this identifier *)
    | Pexp_setinstvar (s, rightExpr) ->
      let rightItm = self#ensureContainingRule ~withPrecedence:(Token updateToken) ~reducesAfterRight:rightExpr in
      let expr = label ~space:true (makeList ~postSpace:true [(protectIdentifier s.txt); atom updateToken]) rightItm in
      SpecificInfixPrecedence ({reducePrecedence=(Token updateToken); shiftPrecedence=(Token updateToken)}, expr)
    | Pexp_setfield (leftExpr, li, rightExpr) ->
      let rightItm = self#ensureContainingRule ~withPrecedence:(Token updateToken) ~reducesAfterRight:rightExpr in
      let leftItm =
        label
          (makeList ~interleaveComments:false [self#simple_enough_to_be_lhs_dot_send leftExpr; atom "."])
          (self#longident_loc li) in
      let expr = label ~space:true (makeList ~postSpace:true [leftItm; atom updateToken]) rightItm in
      SpecificInfixPrecedence ({reducePrecedence=(Token updateToken); shiftPrecedence=(Token updateToken)}, expr)
    | Pexp_match (e, l) when detectTernary l != None -> (
      match detectTernary l with
      | None -> raise (Invalid_argument "Impossible")
      | Some (tt, ff) ->
        let ifTrue = self#unparseExpr tt in
        let testItm = self#ensureExpression e ~reducesOnToken:(Token "?") in
        let ifFalse = self#ensureContainingRule ~withPrecedence:(Token ":") ~reducesAfterRight:ff in
        let withQuestion = SourceMap (e.pexp_loc, makeList ~postSpace:true [testItm; atom "?"]) in
        let trueFalseBranches =
          makeList ~inline:(true, true) ~break:IfNeed ~sep:":" ~postSpace:true ~preSpace:true [ifTrue; ifFalse]
        in
        let expr = label ~space:true withQuestion trueFalseBranches in
        SpecificInfixPrecedence ({reducePrecedence=Token ":"; shiftPrecedence=Token "?"}, expr)
    )
    | _ -> (
      match self#expression_requiring_parens_in_infix x with
      | Some e -> PotentiallyLowPrecedence e
      | None -> raise (Invalid_argument "No match for unparsing expression")
    )

  (*
     It's not enough to only check if precedence of an infix left/right is
     greater than the infix itself. We also should likely pay attention to
     left/right associativity. So how do we render the minimum number of
     parenthesis?

     The intuition is that sequential right associative operators will
     naturally build up deep trees on the right side (left builds up left-deep
     trees). So by default, we add parens to model the tree structure that
     we're rendering except when the parser will *naturally* parse the tree
     structure that the parens assert.

     Sequential identical infix operators:
     ------------------------------------
     So if we see a nested infix operator of precedence Y, as one side of
     another infix operator that has the same precedence (Y), that is S
     associative on the S side of the function application, we don't need to
     wrap in parens. In more detail:

     -Add parens around infix binary function application
       Exception 1: Unless we are a left-assoc operator of precedence X in the left branch of an operator w/ precedence X.
       Exception 2: Unless we are a right-assoc operator of precedence X in the right branch of an operator w/ precedence X.
       Exception 3: Unless we are a _any_-assoc X operator in the _any_ branch of an Y operator where X has greater precedence than Y.

     Note that the exceptions do not specify any special cases for mixing
     left/right associativity. Precedence is what determines necessity of
     parens for operators with non-identical precedences. Associativity
     only determines necessity of parens for identically precedented operators.

     PLUS is left assoc:
     - So this one *shouldn't* expand into two consecutive infix +:


            [Pexp_apply]
              /      \
         first +   [Pexp_apply]
                      /   \
                  second + third


     - This one *should*:

                    [Pexp_apply]
                      /      \
           [  Pexp_apply  ] + third
              /     \
           first +  second



     COLONCOLON is right assoc, so
     - This one *should* expand into two consecutive infix ::  :

            [Pexp_apply]
              /      \
         first ::   [Pexp_apply]
                      /   \
                  second :: third


     - This one *shouldn't*:

                    [Pexp_apply]
                      /      \
           [  Pexp_apply  ] :: third
              /     \
           first ::  second




     Sequential differing infix operators:
     ------------------------------------

     Neither of the following require paren grouping because of rule 3.


            [Pexp_apply]
              /      \
         first  +  [Pexp_apply]
                      /   \
                  second * third


                    [Pexp_apply]
                      /      \
            [Pexp_apply  +  third
              /     \
           first *  second

      The previous has nothing to do with the fact that + and * have the same
      associativity. Exception 3 applies to the following where :: is right assoc
      and + is left. + has higher precedence than ::

      - so parens aren't required to group + when it is in a branch of a
        lower precedence ::

            [Pexp_apply]
              /      \
         first ::   [Pexp_apply]
                      /   \
                  second + third


      - Whereas there is no Exception that applies in this case (Exception 3
        doesn't apply) so parens are required around the :: in this case.

                    [Pexp_apply]
                      /      \
           [  Pexp_apply  ] + third
              /     \
           first ::  second

  *)

  method classExpressionToFormattedApplicationItems x =
    let itms =
      match x.pcl_desc with
        | Pcl_apply (ce, l) ->
          (self#simple_class_expr ce)::
          (List.map self#label_x_expression_param l)
        | _ -> [self#class_expr x]
    in
    (itms, None)


  (**
        How JSX is formatted/wrapped. We want the attributes to wrap independently
        of children.

        <xxx
          attr1=blah
          attr2=foo>
          child
          child
          child
        </x>

      +-------------------------------+
      |  left   right (list of attrs) |
      |   / \   /   \                 |
      |   <tag                        |
      |     attr1=blah                |
      |     attr2=foo                 |
      +-------------------------------+
       |
       |
       |
       |      left       right  list of children with
       |   /       \    /  \     open,close = > </tag>
       |  +---------+
       +--|         |    >
          +---------+

          </tag>           *)
  method formatJSXComponent componentName args =
    let rec processArguments arguments processedAttrs children =
      match arguments with
      | ("", {pexp_desc = Pexp_construct (_, None)}) :: tail ->
        processArguments tail processedAttrs None
      | ("", {pexp_desc = Pexp_construct ({txt = Lident"::"}, Some {pexp_desc = Pexp_tuple(components)} )}) :: tail ->
        processArguments tail processedAttrs (self#formatChildren components [])
      | (lbl, expression) :: tail ->
         let nextAttr =
           match expression.pexp_desc with
           | Pexp_ident (ident) when (Longident.last ident.txt) = lbl -> atom lbl
           | _ -> (
             let firstChar = String.get lbl 0 in
             if firstChar == '?' then
                let lbl = String.sub lbl 1 ((String.length lbl) - 1) in
                makeList ([atom lbl; atom "=?"; self#simplifyUnparseExpr expression])
             else
                makeList ([atom lbl; atom "="; self#simplifyUnparseExpr expression])
                )
         in
         processArguments tail (nextAttr :: processedAttrs) children
      | [] -> (processedAttrs, children)
    in
    let (reversedAttributes, children) = processArguments args [] None in
    match children with
    | None ->
      makeList
        ~break:IfNeed
        ~wrap:("<" ^ componentName, "/>")
        ~pad:(true, true)
        ~inline:(false, false)
        ~postSpace:true
        (List.rev reversedAttributes)
    | Some renderedChildren ->
      let openTagAndAttrs =
        match reversedAttributes with
        | [] -> (atom ("<" ^ componentName ^ ">"))
        | revAttrHd::revAttrTl ->
          let finalAttrList = (List.rev (makeList ~break:Never [revAttrHd; atom ">"] :: revAttrTl)) in
          let renderedAttrList = (makeList ~inline:(true, true) ~break:IfNeed ~pad:(false, false) ~preSpace:true finalAttrList) in
          label
            ~space:true
            (atom ("<" ^ componentName))
            renderedAttrList
      in
      label
        openTagAndAttrs
        (makeList
          ~wrap:("", "</" ^ componentName ^ ">")
          ~inline:(true, false)
          ~break:IfNeed
          ~pad:(true, true)
          ~postSpace:true
          renderedChildren)


  (* Creates a list of simple module expressions corresponding to module
     expression or functor application. *)
  method moduleExpressionToFormattedApplicationItems x =
    let rec functorApplicationList xx = match xx.pmod_desc with
      | Pmod_apply (me1, me2) ->
          SourceMap (me2.pmod_loc, (self#simple_module_expr me2))::
            (functorApplicationList me1)
      | _ -> SourceMap (xx.pmod_loc, (self#module_expr xx))::[]
    in
    (List.rev (functorApplicationList x), None)


  (*

     Watch out, if you see something like below (sixteenTuple getting put on a
     newline), yet a paren-wrapped list wouldn't have had an extra newlin, you
     might need to wrap the single token (sixteenTuple) in [ensureSingleTokenSticksToLabel].
     let (
        axx,
        oxx,
        pxx
      ):
        sixteenTuple = echoTuple (
        0,
        0,
        0
      );
  *)

  method formatSimplePatternBinding labelOpener layoutPattern typeConstraint appTerms =
    let letPattern = label ~break:`Never ~space:true (atom labelOpener) layoutPattern in
    let upUntilEqual =
      match typeConstraint with
        | None -> letPattern
        | Some tc -> formatTypeConstraint letPattern tc
    in
    let includingEqual = makeList ~postSpace:true [upUntilEqual; atom "="] in
    formatAttachmentApplication applicationFinalWrapping (Some (true, includingEqual)) appTerms

  (* Only formats a type annotation for a value binding. *)
  method formatSimpleSignatureBinding labelOpener bindingPattern typeConstraint =
    let letPattern = (label ~space:true (atom labelOpener) bindingPattern) in
    (formatTypeConstraint letPattern typeConstraint)


  (*
     The [bindingLabel] is either the function name (if let binding) or first
     arg (if lambda).

     For defining layout of the following form:

         lbl one
             two
             constraint => {
           ...
         }

     If using "=" as the arrow, can also be used for:

         met private
             myMethod
             constraint = fun ...

   *)
  method wrapCurriedFunctionBinding
         ?(attachTo)
         ?(arrow="=>")
         prefixText
         bindingLabel
         patternList
         returnedAppTerms =
    let allPatterns = bindingLabel::patternList in
    let partitioning = curriedFunctionFinalWrapping allPatterns in
    let everythingButReturnVal = match settings.returnStyle with
        (*
         Because align_closing is set to false, you get:

         (Brackets[] inserted to show boundaries between open/close of pattern list)
         let[firstThing
             secondThing
             thirdThing]

         It only wraps to indent four by coincidence: If the "opening" token was
         longer, you'd get:

         letReallyLong[firstThing
                       secondThing
                       thirdThing]

         For curried let bindings, we stick the arrow in the *last* pattern:
         let[firstThing
             secondThing
             thirdThing =>]

         But it could have just as easily been the "closing" token corresponding to
         "let". This works because we have [align_closing = false]. The benefit of
         shoving it in the last pattern, is that we can turn [align_closing = true]
         and still have the arrow stuck to the last pattern (which is usually what we
         want) (See modeTwo below).
      *)
      | ReturnValOnSameLine -> (
          match partitioning with
            | None ->
                (* We want the binding label to break *with* the arguments. Again,
                   there's no apparent way to add additional indenting for the
                   args with this setting. *)

                (**
                   Formats lambdas by treating the first pattern as the
                   "bindingLabel" which is kind of strange in some cases (when
                   you only have one arg that wraps)...

                      echoTheEchoer (
                        fun (
                              a,
                              p
                            ) => (
                          a,
                          b
                        )

                   But it makes sense in others (where you have multiple args):

                      echoTheEchoer (
                        fun (
                              a,
                              p
                            )
                            mySecondArg
                            myThirdArg => (
                          a,
                          b
                        )

                   Try any other convention for wrapping that first arg and it
                   won't look as balanced when adding multiple args.

                *)
              makeList
                ~pad:(true, true)
                ~wrap:(prefixText, arrow)
                ~indent:(settings.space * settings.indentWrappedPatternArgs)
                ~postSpace:true
                ~inline:(true, true)
                ~break:IfNeed
                allPatterns
            | Some (attachedList, wrappedListy) ->
                (* To get *only* the final argument to "break", while not
                   necessarily breaking the prior arguments, we dock everything
                   but the last item to a created label *)
              label
                ~space:true
                (
                  makeList
                    ~pad:(true, true)
                    ~wrap:(prefixText, arrow)
                    ~indent:(settings.space * settings.indentWrappedPatternArgs)
                    ~postSpace:true
                    ~inline:(true, true)
                    ~break:IfNeed
                    attachedList
                )
                wrappedListy
        )
    in

    let everythingButAppTerms = match attachTo with
      | None -> everythingButReturnVal
      | Some toThis -> label ~space:true toThis everythingButReturnVal
    in
    formatAttachmentApplication
      applicationFinalWrapping
      (Some (true, everythingButAppTerms))
      returnedAppTerms

  method leadingCurriedAbstractTypes x =
    let rec argsAndReturn xx =
      match xx.pexp_desc with
        | Pexp_newtype (str,e) ->
            let (nextArgs, return) = argsAndReturn e in
            (str::nextArgs, return)
        | _ -> ([], xx.pexp_desc)
    in argsAndReturn x

  method curriedConstructorPatternsAndReturnVal cl =
    let rec argsAndReturn xx =
      if xx.pcl_attributes <> [] then ([], xx)
      else match xx.pcl_desc with
      | Pcl_fun (label, eo, p, e) ->
        let (nextArgs, return) = argsAndReturn e in
        if label="" then
          let args = SourceMap (p.ppat_loc, (self#simple_pattern p))::nextArgs in
          (args, return)
        else
          let args = SourceMap (p.ppat_loc, (self#label_exp (label, eo, p)))::nextArgs in
          (args, return)
      | _ -> ([], xx)
    in argsAndReturn cl


  (*
    Returns the arguments list (if any, that occur before the =>), and the
    final expression (that is either returned from the function (after =>) or
    that is bound to the value (if there are no arguments, and this is just a
    let pattern binding)).
  *)
  method curriedPatternsAndReturnVal x =
    let rec argsAndReturn xx =
      if xx.pexp_attributes <> [] then ([], xx)
      else match xx.pexp_desc with
        (* label * expression option * pattern * expression *)
        | Pexp_fun (label, eo, p, e) ->
            let (nextArgs, return) = argsAndReturn e in
            if label="" then
              let args = SourceMap (p.ppat_loc, (self#simple_pattern p))::nextArgs in
              (args, return)
            else
              let args = SourceMap (p.ppat_loc, (self#label_exp (label, eo, p)))::nextArgs in
              (args, return)
        | Pexp_newtype (str,e) ->
           let typeParamLayout = atom ("(type " ^ str ^ ")") in
           let (nextArgs, return) = argsAndReturn e in
           ((typeParamLayout)::nextArgs, return)
        | _ -> ([], xx)
    in argsAndReturn x

  (* Returns the (curriedModule, returnStructure) for a functor *)
  method curriedFunctorPatternsAndReturnStruct me = match me.pmod_desc with
    (* string loc * module_type option * module_expr *)
    | Pmod_functor(s, mt, me2) ->
        let firstOne =
          match mt with
            | None -> atom "()"
            | Some mt' -> makeList ~wrap:("(",")") ~break:IfNeed [formatTypeConstraint (atom s.txt) (self#module_type mt')]
        in
        let (functorArgsRecurse, returnStructure) = (self#curriedFunctorPatternsAndReturnStruct me2) in
        (firstOne::functorArgsRecurse, returnStructure)
    | _ -> ([], me)

  method isRenderableAsPolymorphicAbstractTypes
         typeVars
         polyType
         leadingAbstractVars
         nonVarifiedType =
      same_ast_modulo_varification_and_extensions polyType nonVarifiedType &&
      trueForEachPair typeVars leadingAbstractVars (fun x y -> String.compare x y == 0)
  (* Reinterpret this as a pattern constraint since we don't currently have a
     way to disambiguate. There is currently a way to disambiguate a parsing
     from Ppat_constraint vs.  Pexp_constraint. Currently (and consistent with
     OCaml standard parser):

       let (x: typ) = blah;
         Becomes Ppat_constraint
       let x:poly . type = blah;
         Becomes Ppat_constraint
       let x:typ = blah;
         Becomes Pexp_constraint(ghost)
       let x = (blah:typ);
         Becomes Pexp_constraint(ghost)

     How are double constraints represented?
     let (x:typ) = (blah:typ);
     If currently both constraints are parsed into a single Pexp_constraint,
     then something must be lost, and how could you fail type checking on:
     let x:int = (10:string) ?? Answer: It probably parses into a nested
     Pexp_constraint.

     Proposal:

       let (x: typ) = blah;
         Becomes Ppat_constraint   (still)
       let x:poly . type = blah;
         Becomes Ppat_constraint   (still)
       let x:typ = blah;
         Becomes Ppat_constraint
       let x = blah:typ;
         Becomes Pexp_constraint


     Reasoning: Allows parsing of any of the currently valid ML forms, but
     combines the two most similar into one form. The only lossyness is the
     unnecessary parens, which there is already precedence for dropping in
     expressions. In the existing approach, preserving a paren-constrained
     expression is *impossible* because it becomes pretty printed as
     let x:t =.... In the proposal, it is not impossible - it is only
     impossible to preserve unnecessary parenthesis around the let binding.

     The one downside is that integrating with existing code that uses [let x =
     (blah:typ)] in standard OCaml will be parsed as a Pexp_constraint. There
     might be some lossiness (beyond parens) that occurs in the original OCaml
     parser.
  *)

  method locallyAbstractPolymorphicFunctionBinding prefixText layoutPattern funWithNewTypes absVars bodyType =
    let appTerms = self#unparseExprApplicationItems funWithNewTypes in
    let locallyAbstractTypes = (List.map atom absVars) in
    let typeLayout =
      SourceMap (bodyType.ptyp_loc, (self#core_type bodyType)) in
    let polyType =
      label
        ~space:true
        (* TODO: This isn't a correct use of sep! It ruins how
         * comments are interleaved. *)
        (makeList [makeList ~sep:" " (atom "type"::locallyAbstractTypes); atom "."])
        typeLayout
      in
    self#formatSimplePatternBinding
      prefixText
      layoutPattern
      (Some polyType)
      appTerms

  (**
      Intelligently switches between:
      Curried function binding w/ constraint on return expr:
         lbl patt
             pattAux
             arg
             :constraint => {
           ...
         }

      Constrained:
         lbl patt
             pattAux...
             :constraint = {
           ...
         }
   *)
  method wrappedBinding prefixText pattern patternAux expr =
    let (argsList, return) = self#curriedPatternsAndReturnVal expr in
    let patternList =
      match patternAux with
        | [] -> pattern
        | _::_ -> makeList ~postSpace:true ~inline:(true, true) ~break:IfNeed (pattern::patternAux)
    in
    match (argsList, return.pexp_desc) with
      | ([], Pexp_constraint (e, ct)) ->
          let typeLayout = SourceMap (ct.ptyp_loc, (self#core_type ct)) in
          let appTerms = self#unparseExprApplicationItems e in
          self#formatSimplePatternBinding prefixText patternList (Some typeLayout) appTerms
      | ([], _) ->
          let appTerms = self#unparseExprApplicationItems expr  in
          self#formatSimplePatternBinding prefixText patternList None appTerms
      | (_::_, _) ->
          let (argsWithConstraint, actualReturn) = self#normalizeFunctionArgsConstraint argsList return in
          let fauxArgs =
            List.concat [patternAux; argsWithConstraint] in
          let returnedAppTerms = self#unparseExprApplicationItems actualReturn in
          self#wrapCurriedFunctionBinding prefixText pattern fauxArgs returnedAppTerms

  (* Similar to the above method. *)
  method wrappedClassBinding prefixText pattern patternAux expr =
    let (argsList, return) = self#curriedConstructorPatternsAndReturnVal expr in
    let patternList =
      match patternAux with
        | [] -> pattern
        | _::_ -> makeList ~postSpace:true ~inline:(true, true) ~break:IfNeed (pattern::patternAux)
    in
    match (argsList, return.pcl_desc) with
      | ([], Pcl_constraint (e, ct)) ->
          let typeLayout = SourceMap (ct.pcty_loc, (self#class_constructor_type ct)) in
          let appTerms = self#classExpressionToFormattedApplicationItems e in
          self#formatSimplePatternBinding prefixText patternList (Some typeLayout) appTerms
      | ([], _) ->
          let appTerms = self#classExpressionToFormattedApplicationItems expr in
          self#formatSimplePatternBinding prefixText patternList None appTerms
      | (_::_, _) ->
          let (argsWithConstraint, actualReturn) =
            self#normalizeConstructorArgsConstraint argsList return in
          let returnedAppTerms = self#classExpressionToFormattedApplicationItems actualReturn in
          let fauxArgs =
            List.concat [patternAux; argsWithConstraint] in
          self#wrapCurriedFunctionBinding prefixText pattern fauxArgs returnedAppTerms

  method binding {pvb_pat; pvb_expr=x} prefixText = (* TODO: print attributes *)
    match (pvb_pat.ppat_desc) with
      | (Ppat_var {txt}) ->
          let pattern = SourceMap (pvb_pat.ppat_loc, self#simple_pattern pvb_pat) in
          self#wrappedBinding prefixText pattern [] x
      (*
         Ppat_constraint is used in bindings of the form

            let (inParenVar:typ) = ...

         And in the case of let bindings for explicitly polymorphic type
         annotations (see parser for more details).

         See reason_parser.mly for explanation of how we encode the two primary
         forms of explicit polymorphic annotations in the parse tree, and how
         we must recover them here.
       *)
      | (Ppat_constraint(p, ty)) -> (
          (* Locally abstract forall types are *seriously* mangled by the parsing
             stage, and we have to be very smart about how to recover it.

              let df_locallyAbstractFuncAnnotated:
                type a b.
                  a =>
                  b =>
                  (inputEchoRecord a, inputEchoRecord b) =
                fun (input: a) (input2: b) => (
                  {inputIs: input},
                  {inputIs: input2}
                );

             becomes:

               let df_locallyAbstractFuncAnnotatedTwo:
                 'a 'b .
                 'a => 'b => (inputEchoRecord 'a, inputEchoRecord 'b)
                =
                 fun (type a) (type b) => (
                   fun (input: a) (input2: b) => ({inputIs: input}, {inputIs:input2}):
                     a => b => (inputEchoRecord a, inputEchoRecord b)
                 );
          *)
          let layoutPattern =
            SourceMap (pvb_pat.ppat_loc, (self#simple_pattern p)) in
          let leadingAbsTypesAndExpr = self#leadingCurriedAbstractTypes x in
          match (p.ppat_desc, ty.ptyp_desc, leadingAbsTypesAndExpr) with
            | (
                Ppat_var s,
                Ptyp_poly (typeVars, varifiedPolyType),
                (_::_ as absVars, Pexp_constraint(funWithNewTypes, nonVarifiedExprType))
              )
              when self#isRenderableAsPolymorphicAbstractTypes
                  typeVars
                  (* If even artificially varified - don't know until returns*)
                  varifiedPolyType
                  absVars
                  nonVarifiedExprType ->
              (*
                 We assume was the case whenever we see this pattern in the
                 AST, it was because the parser parsed the polymorphic locally
                 abstract type sugar.

                 Ppat_var..Ptyp_poly...Pexp_constraint:

                    let x: 'a 'b . 'a => 'b => 'b =
                      fun (type a) (type b) =>
                         (fun aVal bVal => bVal : a => b => b);

                 We need to be careful not to accidentally detect similar
                 forms, that cannot be printed as sugar.

                    let x: 'a 'b . 'a => 'b => 'b =
                      fun (type a) (type b) =>
                         (fun aVal bVal => bVal : int => int => int);

                 Should *NOT* be formatted as:

                    let x: type a b. int => int => int = fun aVal bVal => bVal;

                 The helper function
                 [same_ast_modulo_varification_and_extensions] was created to
                 help compare the varified constraint pattern body, and the
                 non-varified expression constraint type.

                 The second requirement that we check before assuming that the
                 sugar form is correct, is to make sure the list of type vars
                 corresponds to a leading prefix of the Pexp_newtype variables.
              *)
              self#locallyAbstractPolymorphicFunctionBinding
                prefixText
                layoutPattern
                funWithNewTypes
                absVars
                nonVarifiedExprType
            | _ ->
              let typeLayout = SourceMap (ty.ptyp_loc, (self#core_type ty)) in
              let appTerms = self#unparseExprApplicationItems x in
              self#formatSimplePatternBinding
                prefixText
                layoutPattern
                (Some typeLayout)
                appTerms
        )
      | (_) ->
          let layoutPattern =
            SourceMap (pvb_pat.ppat_loc, (self#pattern pvb_pat)) in
          let appTerms = self#unparseExprApplicationItems x in
          self#formatSimplePatternBinding prefixText layoutPattern None appTerms


  (* Ensures that the constraint is formatted properly for sake of function
     binding (formatted without arrows)
     let x y z : no_unguareded_arrows_allowed_here => ret;
   *)
  method normalizeFunctionArgsConstraint argsList return =
    match return.pexp_desc with
      | Pexp_constraint (e, ct) ->
        let typeLayout = SourceMap (ct.ptyp_loc, (self#non_arrowed_non_simple_core_type ct)) in
        (argsList@[formatJustTheTypeConstraint typeLayout], e)
      | _ -> (argsList, return)

  method normalizeConstructorArgsConstraint argsList return =
    match return.pcl_desc with
      | Pcl_constraint (e, ct) when return.pcl_attributes = [] ->
        let typeLayout = SourceMap (ct.pcty_loc, (self#non_arrowed_class_constructor_type ct)) in
        (argsList@[formatJustTheTypeConstraint typeLayout], e)
      | _ -> (argsList, return)

  method bindingsLocationRange l =
    let len = List.length l in
    let fstLoc = (List.nth l 0).pvb_loc in
    let lstLoc = (List.nth l (len - 1)).pvb_loc in
    {
      loc_start = fstLoc.loc_start;
      loc_end = lstLoc.loc_end;
      loc_ghost = false
    }

  method bindings (rf, l) =
    let firstLine = (
      match l with
        | [] -> raise (NotPossible "no bindings supplied")
        | x::[]
        | x::_ ->
          let label = match rf with
            | Nonrecursive -> "let"
            | Recursive -> "let rec" in
          SourceMap (x.pvb_loc, (self#binding x label))

    ) in
    let forEachRemaining = fun t -> SourceMap (t.pvb_loc, (self#binding t "and")) in
    let remainingBindings = (
      match l with
        | [] -> []
        | x::[] -> []
        | x::x2::xtl -> List.map forEachRemaining (x2::xtl)
    ) in
    match remainingBindings with
    | [] -> firstLine
    | _ ->
    makeList
      ~postSpace:true
      ~break:Always
      ~indent:0
      ~inline:(true, true)
      (firstLine::remainingBindings)

  method letList exprTerm =
    match (exprTerm.pexp_attributes, exprTerm.pexp_desc) with
      | ([], Pexp_let (rf, l, e)) ->
        (* For "letList" bindings, the start/end isn't as simple as with
         * module value bindings. For "let lists", the sequences were formed
         * within braces {}. The parser relocates the first let binding to the
         * first brace. *)
         let bindingsLayout = (self#bindings (rf, l)) in
         let bindingsLoc = self#bindingsLocationRange l in
         let bindingsSourceMapped = SourceMap (bindingsLoc, bindingsLayout) in
         bindingsSourceMapped::(self#letList e)
      | ([], Pexp_open (ovf, lid, e)) ->
        let listItems = (self#letList e) in
        if (List.length listItems == 1) && ovf == Fresh then
            (* The following logic is a syntax sugar
             * for an 'open' expression that has only one let item.
             *
             * Instead of printing:
             * let result =  {
             *   open Fmt;
             *   strf
             *     "-pkgs %a"
             *     (list sep::(unit ",") string)
             * }
             *
             * We format as:
             *
             * let result = Fmt.(strf "-pkgs %a" (list sep::(unit ",") string))
             *
             * (Also see https://github.com/facebook/Reason/issues/114)
             *)
            let expression = match e.pexp_desc with
                (* syntax sugar for M.{x:1} *)
                | Pexp_record _
                (* syntax sugar for M.(a, b) *)
                | Pexp_tuple _
                (* syntax sugar for M.{} *)
                | Pexp_object {pcstr_fields = []}
                (* syntax sugar for M.[x,y] *)
                | Pexp_construct ( {txt= Lident"::"},Some _) ->
                    (self#simplifyUnparseExpr e)
                (* syntax sugar for the rest, wrap with parens to avoid ambiguity.
                 * E.g., avoid M.(M2.v) being printed as M.M2.v
                 *)
                | _ ->
                    (makeList ~wrap:("(",")") ~break:IfNeed listItems)
            in
            let openLayout = label
              (label (self#longident_loc lid) (atom (".")))
              expression
            in [openLayout]
         else
            let overrideStr = match ovf with | Override -> "!" | Fresh -> "" in
            let openLayout = label ~space:true
              (atom ("open" ^ overrideStr))
              (self#longident_loc lid)
            in
            (* Just like the bindings, have to synthesize a location since the
             * Pexp location is parsed (potentially) beginning with the open
             * brace {} in the let sequence. *)
            let openSourceMapped = SourceMap (lid.loc, openLayout) in
            openSourceMapped::listItems
      | ([], Pexp_letmodule (s, me, e)) ->
          let prefixText = "module" in
          let bindingName = atom ~loc:s.loc s.txt in
          let moduleExpr = me in
          let letModuleLayout =
            (self#let_module_binding prefixText bindingName moduleExpr) in
          let letModuleLoc = {
            loc_start = s.loc.loc_start;
            loc_end = me.pmod_loc.loc_end;
            loc_ghost = false
          } in
          (* Just like the bindings, have to synthesize a location since the
           * Pexp location is parsed (potentially) beginning with the open
           * brace {} in the let sequence. *)
          let letModuleSourceMapped = SourceMap (letModuleLoc, letModuleLayout) in
           letModuleSourceMapped::(self#letList e)
      | ([], Pexp_sequence (({pexp_desc=Pexp_sequence _ }) as e1, e2))
      | ([], Pexp_sequence (({pexp_desc=Pexp_let _      }) as e1, e2))
      | ([], Pexp_sequence (({pexp_desc=Pexp_open _     }) as e1, e2))
      | ([], Pexp_sequence (({pexp_desc=Pexp_letmodule _}) as e1, e2))
      | ([], Pexp_sequence (e1, e2)) ->
          let e1Layout = (self#unparseExpr e1) in
          (* It's kind of difficult to synthesize a location here in the case
           * where this is the first expression in the braces. We could consider
           * deeply inspecting the leftmost token/term in the expression. *)
          let e1SourceMapped = SourceMap (e1.pexp_loc, e1Layout) in
          e1SourceMapped::(self#letList e2)
      | _ ->
          let exprTermLayout = (self#unparseExpr exprTerm) in
          let exprTermSourceMapped = SourceMap (exprTerm.pexp_loc, exprTermLayout) in
          (* Should really do something to prevent infinite loops here. Never
             allowing a top level call into letList to recurse back to
             self#unparseExpr- top level calls into letList *must* be one of the
             special forms above whereas lower level recursive calls may be of
             any form. *)
          [exprTermSourceMapped]

  method constructor_expression ?(polyVariant=false) ~arityIsClear stdAttrs ctor eo =
    let (implicit_arity, arguments) =
      match eo.pexp_desc with
        | Pexp_tuple l when not polyVariant -> (
            let exprs = match (List.map self#simplifyUnparseExpr l) with
              | [] -> raise (NotPossible "no tuple items")
              | hd::[] -> hd
              | hd::tl as all -> makeSpacedBreakableInlineList all
            in
            (* There is no ambiguity when the number of tuple components is 1.
               We don't need put implicit_arity in that case *)
            (List.length l > 1 && not arityIsClear, exprs)
          )
        | _ -> (false, self#simplifyUnparseExpr eo)
    in
    let construction =
      label ~space:true
        ctor
        (if isSequencey arguments then arguments else (ensureSingleTokenSticksToLabel arguments))
    in
    let attrs =
      if implicit_arity && (not polyVariant) then
        ({txt="implicit_arity"; loc=eo.pexp_loc}, PStr []) :: stdAttrs
      else
        stdAttrs
    in
    match attrs with
      | [] -> construction
      | _::_ -> formatAttributed construction (self#attributes attrs)

  (* TODOATTRIBUTES: Handle stdAttrs here (merge with implicit_arity) *)
  method constructor_pattern ?(polyVariant=false) ~arityIsClear ctor po =
    let (implicit_arity, arguments) =
      match po.ppat_desc with
        | Ppat_tuple l when not polyVariant ->
            let exprs = match (List.map self#simple_pattern l) with
              | [] -> raise (NotPossible "no tuple items")
              | [hd] -> hd
              | hd::tl as all -> makeSpacedBreakableInlineList all
            in
              (* There is no ambiguity when the number of tuple components is 1.
               We don't need put implicit_arity in that case *)
            (List.length l > 1 && not arityIsClear, exprs)
        | _ -> (false, self#simple_pattern po)
    in
    let construction = label ~space:true
      ctor
      (if isSequencey arguments then arguments else (ensureSingleTokenSticksToLabel arguments)) in
    if implicit_arity && (not polyVariant) then
      formatAttributed construction (self#attributes [({txt="implicit_arity"; loc=po.ppat_loc}, PStr [])])
    else
      construction

  method patternFunction loc l =
    let estimatedFunLocation = {
        loc_start = loc.loc_start;
        loc_end = {loc.loc_start with pos_cnum = loc.loc_start.Lexing.pos_cnum + 3};
        loc_ghost = false;
    } in
    makeList
      ~postSpace:true
      ~break:IfNeed
      ~inline:(true, true)
      ~pad:(false, false)
      ((atom ~loc:estimatedFunLocation "fun") :: (self#case_list l))

  (* Expressions requiring parens, in most contexts such as separated by infix *)
  method expression_requiring_parens_in_infix x =
    let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.pexp_attributes in
    assert (stdAttrs == []);
    match x.pexp_desc with
      (* The only reason Pexp_fun must also be wrapped in parens when under
         pipe, is that its => token will be confused with the match token.
         Simple expression will also invoke `#reset`. *)
      | Pexp_function _ when pipe || semi -> None (* Would be rendered as simplest_expression  *)
      | Pexp_function l -> Some (self#patternFunction x.pexp_loc l)
      | _ ->
        (* The Pexp_function cases above don't use location because comment printing
          breaks for them. *)
        let itm = match x.pexp_desc with
          | Pexp_fun _
          | Pexp_newtype _ ->
            let (args, ret) = self#curriedPatternsAndReturnVal x in
            (match args with
            | [] -> raise (NotPossible ("no arrow args in unparse "))
            | firstArg::tl ->
              (* Suboptimal printing of parens:

                    something >>= fun x => x + 1;

                 Will be printed as:

                    something >>= (fun x => x + 1);

                 Because the arrow has lower precedence than >>=, but it wasn't
                 needed because

                    (something >>= fun x) => x + 1;

                 Is not a valid parse. Parens around the `=>` weren't needed to
                 prevent reducing instead of shifting. To optimize this part, we need
                 a much deeper encoding of the parse rules to print parens only when
                 needed, testing which rules will be reduced. It really should be
                 integrated deeply with Menhir.

                 One question is, if it's this difficult to describe when parens are
                 needed, should we even print them with the minimum amount?  We can
                 instead model everything as "infix" with ranked precedences.  *)
              let retValUnparsed = self#unparseExprApplicationItems ret in
              Some (self#wrapCurriedFunctionBinding "fun" firstArg tl retValUnparsed)
            )
          | Pexp_try (e, l) ->
            let estimatedBracePoint = {
              loc_start = e.pexp_loc.loc_end;
              loc_end = x.pexp_loc.loc_end;
              loc_ghost = false;
            }
            in
            let cases = (self#case_list ~allowUnguardedSequenceBodies:true l) in
            let switchWith = label ~space:true (atom "try") (self#reset#simplifyUnparseExpr e) in
            Some (
              label
                ~space:true
                switchWith
                (SourceMap (estimatedBracePoint, (makeList ~indent:settings.trySwitchIndent ~wrap:("{", "}") ~break:Always_rec ~postSpace:true cases)))
            )
          (* These should have already been handled and we should never havgotten this far. *)
          | Pexp_setinstvar (s, e) -> raise (Invalid_argument "Cannot handle setinstvar here - call unparseExpr")
          | Pexp_setfield (_, _, _) -> raise (Invalid_argument "Cannot handle setfield here - call unparseExpr")
          | Pexp_apply (e, l) -> raise (Invalid_argument "Cannot handle apply here - call unparseExpr")
          | Pexp_match (e, l) ->
             let estimatedBracePoint = {
               loc_start = e.pexp_loc.loc_end;
               loc_end = x.pexp_loc.loc_end;
               loc_ghost = false;
             }
             in
             let cases = (self#case_list ~allowUnguardedSequenceBodies:true l) in
             let switchWith = label ~space:true (atom "switch") (self#reset#simplifyUnparseExpr e) in
             let lbl =
               label
                 ~space:true
                 switchWith
                 (SourceMap (estimatedBracePoint, (makeList ~indent:settings.trySwitchIndent ~wrap:("{", "}") ~break:Always_rec ~postSpace:true cases)))
             in
             Some lbl
          | Pexp_ifthenelse (e1, e2, eo) ->
            let (blocks, finalExpression) = sequentialIfBlocks eo in
            let rec singleExpression exp =
              match exp.pexp_desc with
              | Pexp_ident _ -> true
              | Pexp_constant _ -> true
              | Pexp_construct (_, arg) ->
                (match arg with
                | None -> true
                | Some x -> singleExpression x)
              | _ -> false
            in
            let singleLineIf =
              (singleExpression e1) &&
              (singleExpression e2) &&
              (match eo with
               | Some expr -> singleExpression expr
               | None -> true
              )
            in
            let makeLetSequence =
              if singleLineIf then
                makeLetSequenceSingleLine
              else
                makeLetSequence
            in
            let rec sequence soFar remaining = (
              match (remaining, finalExpression) with
                | ([], None) -> soFar
                | ([], Some e) ->
                  let soFarWithElseAppended = makeList ~postSpace:true [soFar; atom "else"] in
                  label ~space:true soFarWithElseAppended (makeLetSequence (self#letList e))
                | (hd::tl, _) ->
                  let (e1, e2) = hd in
                  let soFarWithElseIfAppended =
                    label
                      ~space:true
                      (makeList ~postSpace:true [soFar; atom "else if"])
                      (self#simplifyUnparseExpr e1)
                  in
                  let nextSoFar =
                    label ~space:true soFarWithElseIfAppended (makeLetSequence (self#letList e2)) in
                  sequence nextSoFar tl
            ) in
            let init =
              label
                ~space:true
                (SourceMap (e1.pexp_loc, (label ~space:true (atom "if") (self#simplifyUnparseExpr e1))))
                (makeLetSequence (self#letList e2)) in
            Some (sequence init blocks)
          | Pexp_while (e1, e2) ->
            let lbl =
              label
                ~space:true
                (label ~space:true (atom "while") (self#simplifyUnparseExpr e1))
                (makeLetSequence (self#letList e2)) in
            Some lbl
          | Pexp_for (s, e1, e2, df, e3) ->
            (*
             *  for longIdentifier in
             *      (longInit expr) to
             *      (longEnd expr) {
             *    print_int longIdentifier;
             *  };
             *)
            let identifierIn = (makeList ~postSpace:true [self#pattern s; atom "in";]) in
            let dockedToFor =
                (makeList
                  ~break:IfNeed
                  ~postSpace:true
                  ~inline:(true, true)
                  [
                    identifierIn;
                    makeList ~postSpace:true [self#simplifyUnparseExpr e1; self#direction_flag df];
                    (self#simplifyUnparseExpr e2);
                  ]
                )
            in
            let upToBody = makeList ~inline:(true, true) ~postSpace:true [atom "for"; dockedToFor] in
            Some (label ~space:true upToBody (makeLetSequence (self#letList e3)))
          | Pexp_new (li) ->
            Some (label ~space:true (atom "new") (self#longident_class_or_type_loc li))
          | Pexp_assert e ->
            Some (
              label ~space:true
                (atom "assert")
                (self#reset#simplifyUnparseExpr e);
            )
          | Pexp_lazy (e) ->
              Some (label ~space:true (atom "lazy") (self#simplifyUnparseExpr e))
          | Pexp_poly _ ->
            failwith (
              "This version of the pretty printer assumes it is impossible to " ^
              "construct a Pexp_poly outside of a method definition - yet it sees one."
            )
          | _ -> None
        in
        match itm with
          | None -> None
          | Some i -> Some (SourceMap (x.pexp_loc, i))

  method potentiallyConstrainedExpr x =
    match x.pexp_desc with
      | Pexp_constraint (e, ct) ->
          formatTypeConstraint (self#unparseExpr e) (self#core_type ct)
      | _ -> self#unparseExpr x


  (*
   * Because the rule BANG simple_expr was given %prec below_DOT_AND_SHARP,
   * !x.y.z will parse as !(x.y.z) and not (!x).y.z.
   *
   *     !x.y.z == !((x.y).z)
   *     !x#y#z == !((x#y)#z)
   *
   * So the intuition is: In general, any simple expression can exist to the
   * left of a `.`, except `BANG simple_expr`, which has special precedence,
   * and must be guarded in this one case.
   *
   * TODO: Instead of special casing this here, we should continue to extend
   * unparseExpr to also unparse simple expressions, (by encoding the
   * rules precedence below_DOT_AND_SHARP).
   *
   * TODO:
   *  Some would even have the prefix application be parsed with lower
   *  precedence function *application*. In the case of !, where ! means not,
   *  it makes a lot of sense because (!identifier)(arg) would be meaningless.
   *
   *  !callTheFunction(1, 2, 3)(andEvenCurriedArgs)
   *
   * Only problem is that it could then not appear anywhere simple expressions
   * would appear.
   *
   * We could make a special case for ! followed by one simple expression, and
   * consider the result simple.
   *
   * Alternatively, we can figure out a way to not require simple expressions
   * in the most common locations such as if/while tests. This is really hard
   * (impossible w/ grammars Menhir supports?)
   *
   * if ! myFunc argOne argTwo {
   *
   * } else {
   *
   * };
   *
   *)
  method simple_enough_to_be_lhs_dot_send x = match x.pexp_desc with
    | (Pexp_apply (eFun, _)) -> (
      match printedStringAndFixityExpr eFun with
        | AlmostSimplePrefix _ ->
          SourceMap (x.pexp_loc, formatPrecedence (self#simplifyUnparseExpr x))
        | UnaryPlusPrefix _
        | UnaryMinusPrefix _
        | Infix _
        | Normal -> self#simplifyUnparseExpr x
    )
    | _ -> self#simplifyUnparseExpr x

  method unparseRecord ?withStringKeys:(withStringKeys=false) ?allowPunning:(allowPunning=true) l eo =
    let quote = (atom "\"") in
    let maybeQuoteFirstElem l =
      if withStringKeys then
      (match l with
      | fst::rest -> quote::fst::quote::rest
      | _ -> l
      )
      else l
    in
    let makeRow (li, e) appendComma shouldPun =
      let comma = atom "," in
      let totalRowLoc = {
        loc_start = li.Asttypes.loc.loc_start;
        loc_end = e.pexp_loc.loc_end;
        loc_ghost = false;
      } in
      let theRow =
        match e.pexp_desc with
          (* Punning *)
          |  Pexp_ident {txt} when li.txt = txt && shouldPun && allowPunning ->
              makeList (maybeQuoteFirstElem ((self#longident_loc li)::(if appendComma then [comma] else [])))
          | _ ->
             let (argsList, return) = self#curriedPatternsAndReturnVal e in (
               match (argsList, return.pexp_desc) with
                 | ([], _) ->
                   let appTerms = self#unparseExprApplicationItems e in
                   let upToColon = makeList (maybeQuoteFirstElem [self#longident_loc li; atom ":"]) in
                   let labelExpr =
                     formatAttachmentApplication
                       applicationFinalWrapping
                       (Some (true, upToColon))
                       appTerms in
                   if appendComma then
                     makeList [labelExpr; comma;]
                   else
                     labelExpr
                 | (firstArg::tl, _) ->
                   let upToColon = makeList (maybeQuoteFirstElem [self#longident_loc li; atom ":"]) in
                   let returnedAppTerms = self#unparseExprApplicationItems return in
                   let labelExpr =
                       (self#wrapCurriedFunctionBinding ~attachTo:upToColon "fun" firstArg tl returnedAppTerms) in
                   if appendComma then makeList [labelExpr; comma;] else labelExpr
             )
      in SourceMap (totalRowLoc, theRow)
    in
    let rec getRows l =
      match l with
        | [] -> []
        | hd::[] -> [makeRow hd false true]
        | hd::hd2::tl -> (makeRow hd true true)::(getRows (hd2::tl))
    in

    let allRows = match eo with
      | None -> (
        match l with
          (* No punning (or comma) for records with only a single field. *)
          (* See comment in parser.mly for lbl_expr_list_with_at_least_one_non_punned_field *)
          | [hd] -> [makeRow hd false false]
          | _ -> getRows l
        )
      | Some withRecord ->
        let firstRow = (
          (* Unclear why "sugar_expr" was special cased here. *)
          let appTerms = self#unparseExprApplicationItems withRecord in
          let firstRowContents =
            formatAttachmentApplication applicationFinalWrapping (Some (false, (atom "..."))) appTerms in
          if l == [] then firstRowContents else makeList [firstRowContents; atom ","]
        ) in
        SourceMap (withRecord.pexp_loc, firstRow)::(getRows l)
    in
    makeList ~wrap:("{", "}") ~break:IfNeed ~preSpace:true allRows

  method simplest_expression x =
    let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.pexp_attributes in
    if stdAttrs <> [] then
      None
    else
      let item = match x.pexp_desc with
        (* The only reason Pexp_fun must also be wrapped in parens is that its =>
           token will be confused with the match token. *)
        | Pexp_fun _ when pipe || semi -> Some (self#reset#simplifyUnparseExpr x)
        | Pexp_function l when pipe || semi -> Some (formatPrecedence ~loc:x.pexp_loc (self#reset#patternFunction x.pexp_loc l))
        | Pexp_apply (e, l) -> (
          match self#simple_get_application x with
          (* If it's the simple form of application. *)
          | Some simpleGet -> Some simpleGet
          | None -> None
        )
        | Pexp_object cs ->
          let obj =
            makeList
              ~sep:";"
              ~wrap:("{", "}")
              ~break:IfNeed
              ~postSpace:true
              ~inline:(true, false)
              (self#class_self_pattern_and_structure cs) in
          Some obj
        | Pexp_override l -> (* FIXME *)
          let string_x_expression (s, e) =
            label ~space:true (atom (s.txt ^ ":")) (self#unparseExpr e)
          in
          Some (
            makeList
              ~postSpace:true
              ~wrap:("{<", ">}")
              ~sep:","
              (List.map string_x_expression l)
          )
        | Pexp_construct _  when is_simple_construct (view_expr x) ->
            let hasJsxAttribute = jsxAttrs != [] in
            Some (
              match view_expr x with
              | `nil -> if hasJsxAttribute then atom "<> </>" else atom "[]"
              | `tuple -> atom "()"
              | `list xs -> (* LIST EXPRESSION *)
                if hasJsxAttribute then
                  let actualChildren =
                    match self#formatChildren xs [] with
                    | None -> []
                    | Some ch -> ch
                  in
                    makeList
                      ~break:IfNeed
                      ~inline:(false, false)
                      ~postSpace:true
                      ~wrap:("<>", "</>")
                      ~pad:(true, true)
                      actualChildren
                else
                  makeList ~break:IfNeed ~wrap:("[", "]") ~sep:"," ~postSpace:true (List.map self#unparseExpr xs)
              | `cons xs ->
                let seq, ext = match List.rev xs with
                  | ext :: seq_rev -> (List.rev seq_rev, ext)
                  | [] -> assert false in
                 makeES6List (List.map self#unparseExpr seq) (self#unparseExpr ext)
              | `simple x -> self#longident x
              | _ -> assert false
            )
        | Pexp_ident li ->
            (* Lone identifiers shouldn't break when to the right of a label *)
            Some (ensureSingleTokenSticksToLabel (self#longident_loc li))
        | Pexp_constant c ->
            (* Constants shouldn't break when to the right of a label *)
            Some (ensureSingleTokenSticksToLabel (self#constant c))
        | Pexp_pack me ->
          Some (
            makeList
              ~break:IfNeed
              ~postSpace:true
              ~wrap:("(", ")")
              ~inline:(true, true)
              [atom "module"; self#module_expr me;]
          )
        | Pexp_tuple l ->
            (* TODO: These may be simple, non-simple, or type constrained
               non-simple expressions *)
          Some (
            makeList
              ~wrap:("(", ")")
              ~sep:","
              ~break:IfNeed
              ~postSpace:true
              (List.map self#potentiallyConstrainedExpr l)
          )
        | Pexp_constraint (e, ct) ->
          Some (
            makeList
              ~break:IfNeed
              ~wrap:("(", ")")
              [formatTypeConstraint (self#unparseExpr e) (self#core_type ct)]
          )
        | Pexp_coerce (e, cto1, ct) ->
            let optFormattedType = match cto1 with
              | None -> None
              | Some typ -> Some (self#core_type typ) in
            Some (
              makeList
                ~break:IfNeed
                ~wrap:("(", ")")
                [formatCoerce (self#unparseExpr e) optFormattedType (self#core_type ct)]
            )
        | Pexp_variant (l, None) ->
            Some (ensureSingleTokenSticksToLabel (atom ("`" ^ l)))
        | Pexp_record (l, eo) -> Some (self#unparseRecord l eo)
        | Pexp_array (l) ->
          Some (
            makeList
              ~break:IfNeed
              ~sep:","
              ~postSpace:true
              ~wrap:("[|", "|]")
              (List.map self#unparseExpr l)
          )
        | Pexp_let (rf, l, e) ->
            Some (makeLetSequence (self#letList x))
        | Pexp_letmodule (s, me, e) ->
            Some (makeLetSequence (self#letList x))
        | Pexp_open (ovf, lid, e) ->
            let letItems = (self#letList x) in
            (match letItems with
                (* if an open expression has only one letItem in the list,
                 * we don't wrap it in "{}" so it becomes something like:
                 *
                 * let a = Fmt.(strf "-pkgs %a" (list sep::(unit ",") string))
                 *
                 * instead of:
                 *
                 * let a = {
                 *   Fmt.(strf "-pkgs %a" (list sep::(unit ",") string))
                 * }
                 *)
                | [item] -> Some item
                | _ -> Some (makeLetSequence letItems)
             )
        | Pexp_sequence _ ->
            Some (makeLetSequence (self#letList x))
        | Pexp_field (e, li) ->
          Some (label (makeList [self#simple_enough_to_be_lhs_dot_send e; atom "."]) (self#longident_loc li))
        | Pexp_send (e, s) ->
          Some (label (makeList [self#simple_enough_to_be_lhs_dot_send e; atom "#";]) (atom s))
        | Pexp_extension e -> Some (self#extension e)
        | _ -> None
      in
      match item with
        | None -> None
        | Some i -> Some (SourceMap (x.pexp_loc, i))

  method formatChildren children processedRev =
    match children with
    | {pexp_desc = Pexp_constant (constant)} :: remaining ->
      self#formatChildren remaining (self#constant constant :: processedRev)
    | {pexp_desc = Pexp_construct ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple(children)} )} :: remaining ->
      self#formatChildren (remaining @ children) processedRev
    | {pexp_desc = Pexp_apply(expr, l); pexp_attributes} :: remaining ->
      (match detectJSXComponent expr.pexp_desc pexp_attributes l with
        | Some componentName -> self#formatChildren remaining (self#formatJSXComponent componentName l :: processedRev)
        | None -> self#formatChildren remaining (self#simplifyUnparseExpr (List.hd children) :: processedRev))
    | {pexp_desc = Pexp_ident li} :: remaining ->
      self#formatChildren remaining (self#longident_loc li :: processedRev)
    | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} :: remaining -> self#formatChildren remaining processedRev
    | head :: remaining -> self#formatChildren remaining (self#simplifyUnparseExpr head :: processedRev)
    | [] -> match processedRev with
        | [] -> None
        | _::_ -> Some (List.rev processedRev)
  method direction_flag = function
    | Upto -> atom "to"
    | Downto -> atom "downto"

  method payload ppxToken ppxId e =
    let wrap = ("[" ^ ppxToken ^ ppxId.txt, "]") in
    let break = IfNeed in
    let pad = (true, false) in
    let postSpace = true in
    let sep = ";" in
    match e with
      | PStr [] -> atom ("[" ^ ppxToken  ^ ppxId.txt  ^ "]")
      | PStr [itm] ->
        makeList ~wrap ~break ~pad [self#structure_item itm]
      | PStr (_::_ as items) ->
        let rows = (List.map (self#structure_item) items) in
        makeList ~wrap ~break ~pad ~postSpace ~sep rows
      | PTyp x ->
        makeList ~wrap ~break ~pad [label ~space:true (atom ":") (self#core_type x)]
      (* Signatures in attributes were added recently *)
      (* | PSig x -> makeList [atom ":"; self#signature x] *)
      | PPat (x, None) ->
        makeList ~wrap ~break ~pad [label ~space:true (atom "?") (self#pattern x)]
      | PPat (x, Some e) ->
        makeList ~wrap ~break ~pad ~postSpace [
          label ~space:true (atom "?") (self#pattern x);
          label ~space:true (atom "when") (self#unparseExpr e)
        ]

  method extension (s, e) =
    match (s.txt) with
    (* We special case "bs.obj" for now to allow for a nicer interop with
     * BuckleScript. We might be able to generalize to any kind of record
     * looking thing with struct keys. *)
    | "bs.obj" -> (
      match e with
      | PStr [itm] -> (
        match itm with
        | {pstr_desc = Pstr_eval ({ pexp_desc = Pexp_record (l, eo) }, []) } ->
          self#unparseRecord ~withStringKeys:true ~allowPunning:false l eo
        | _ -> assert false
      )
      | _ -> assert false
    )
    | _ -> (self#payload "%" s e)

  method item_extension (s, e) = (self#payload "%%" s e)


  (* @[ ...] Simple attributes *)
  method attribute (s, e) = (self#payload "@" s e)

  (* [@@ ... ] Attributes that occur after a major item in a structure/class *)
  method item_attribute (s, e) = (self#payload "@@" s e)

  (* [@@@ ...] Attributes that occur not *after* an item in some structure/class/sig, but
     rather as their own standalone item. Note that syntactic distinction
     between item_attribute and floating_attribute is no longer necessary with
     Reason. Thank you semicolons. *)
  method floating_attribute (s, e) = (self#payload "@@@" s e)


  method attributes l =
	    makeList ~break:IfNeed ~postSpace:true (List.map self#attribute l)

  method attach_std_attrs l toThis =
    let l = extractStdAttrs l in
    match l with
      | [] -> toThis
      | _::_ -> makeList ~postSpace:true [toThis; (self#attributes l)]

  method attach_std_item_attrs l toThis =
    let l = extractStdAttrs l in
    match l with
      | [] -> toThis
      | _::_ ->
        makeList ~postSpace:true ~indent:0 ~break:IfNeed ~inline:(true, true) [
          toThis;
          makeList ~break:IfNeed ~postSpace:true (List.map self#item_attribute l);
        ]

  method exception_declaration ed =
    let pcd_name = ed.pext_name in
    let pcd_loc = ed.pext_loc in
    let pcd_attributes = ed.pext_attributes in
    let exn_arg = match ed.pext_kind with
      | Pext_decl (args, type_opt) ->
          let pcd_args, pcd_res = args, type_opt in
          [self#type_variant_leaf_nobar {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes}]
      | Pext_rebind id ->
          [atom pcd_name.txt; atom "="; (self#longident_loc id)] in
    makeList ~postSpace:true ((atom "exception")::exn_arg)

  (*
    Note: that override doesn't appear in class_sig_field, but does occur in
    class/object expressions.
    TODO: TODOATTRIBUTES
   *)
  method method_sig_flags_for s = function
    | Virtual -> [atom "virtual"; atom s]
    | Concrete ->  [atom s]

  method value_type_flags_for s = function
    | (Virtual, Mutable) -> [atom "virtual"; atom "mutable"; atom s]
    | (Virtual, Immutable) -> [atom "virtual"; atom s]
    | (Concrete, Mutable) -> [atom "mutable"; atom s]
    | (Concrete, Immutable) -> [atom s]

  method class_sig_field x =
    match x.pctf_desc with
    | Pctf_inherit (ct) ->
      label ~space:true (atom "inherit") (self#class_constructor_type ct)
    | Pctf_val (s, mf, vf, ct) ->
      let valueFlags = self#value_type_flags_for (s ^ ":") (vf, mf) in
      label
        ~space:true
        (
          label ~space:true
            (atom "val")
            (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed valueFlags)
        )
        (self#core_type ct)
    | Pctf_method (s, pf, vf, ct) ->
      let methodFlags = self#method_sig_flags_for (s ^ ":") vf
      in
      let pubOrPrivate =
        match pf with
        | Private -> "pri"
        | Public -> "pub"
      in
      label
        ~space:true
        (label ~space:true
            (atom pubOrPrivate)
            (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed methodFlags)
        )
        (self#core_type ct)
    | Pctf_constraint (ct1, ct2) ->
      label
        (atom "constraint")
        (label ~space:true
            (makeList ~postSpace:true [self#core_type ct1; atom "="])
            (self#core_type ct2)
        )
    | Pctf_attribute a -> self#floating_attribute a
    | Pctf_extension e -> self#item_extension e


  (* The type of something returned from a constructor. Formerly [class_signature]  *)
  method shouldDisplayClassInstTypeItem x = match x.pctf_desc with
    | Pctf_attribute (s, _) -> (not (s.txt = "ocaml.text") && not (s.txt = "ocaml.doc"))
    | _ -> true

  method shouldDisplayClassField x = match x.pcf_desc with
    | Pcf_attribute (s, _) -> (not (s.txt = "ocaml.text") && not (s.txt = "ocaml.doc"))
    | _ -> true

  method shouldDisplaySigItem x = match x.psig_desc with
    | Psig_attribute (s, _) -> (not (s.txt = "ocaml.text") && not (s.txt = "ocaml.doc"))
    | _ -> true

  method shouldDisplayStructureItem x = match x.pstr_desc with
    | Pstr_attribute (s, _) -> (not (s.txt = "ocaml.text") && not (s.txt = "ocaml.doc"))
    | _ -> true


  method class_instance_type x = match x.pcty_desc with
    | Pcty_signature cs ->
        let {pcsig_self = ct; pcsig_fields = l} = cs in
        let instTypeFields =
          List.map self#class_sig_field (List.filter self#shouldDisplayClassInstTypeItem l) in
        let allItems = match ct.ptyp_desc with
          | Ptyp_any -> instTypeFields
          | _ ->
            label ~space:true (atom "as") (self#core_type ct) ::
            instTypeFields
        in
        makeList
          ~wrap:("{", "}")
          ~postSpace:true
          ~break:Always_rec
          ~sep:";"
          allItems
    | Pcty_constr (li, l) -> (
        match l with
          | [] -> self#longident_loc li
          | _::_ ->
            label
              ~space:true
              (makeList ~wrap:("(", ")") ~sep:"," (List.map self#core_type l))
              (self#longident_loc li)
      )
    | Pcty_extension e -> self#extension e
    | Pcty_arrow _ -> failwith "class_instance_type should not be printed with Pcty_arrow"

  method class_declaration_list l =
    let class_declaration ?(class_keyword=false)
        ({pci_params=ls; pci_name={txt}; pci_virt; pci_expr={pcl_desc}; pci_loc} as x) =
      let (firstToken, pattern, patternAux) = self#class_opening class_keyword txt pci_virt ls in
      let classBinding = self#wrappedClassBinding firstToken pattern patternAux x.pci_expr in
      let itm = self#attach_std_item_attrs x.pci_attributes classBinding in
      SourceMap (pci_loc, itm)
    in
    (match l with
      | [] -> raise (NotPossible "Class definitions will have at least one item.")
      | x::rest ->
        makeNonIndentedBreakingList (
          class_declaration ~class_keyword:true x ::
          List.map class_declaration rest
        )
    )
  (* For use with [class type a = class_instance_type]. Class type
     declarations/definitions declare the types of instances generated by class
     constructors.
     We have to call self#class_instance_type because self#class_constructor_type
     would add a "new" before the type.
     TODO: TODOATTRIBUTES:
  *)
  method class_type_declaration_list l =
    let class_type_declaration kwd ({pci_params=ls;pci_name={txt};pci_attributes} as x) =
      let opener = match x.pci_virt with
        | Virtual -> kwd ^ " " ^ "virtual"
        | Concrete -> kwd
      in

      let upToName =
        if ls == [] then
          label ~space:true (atom opener) (atom txt)
        else
          label
            ~space:true
            (label ~space:true (atom opener) (atom txt))
            (self#class_params_def ls)
      in
      let includingEqual = makeList ~postSpace:true [upToName; atom "="] in
      let itm = label ~space:true includingEqual (self#class_instance_type x.pci_expr) in
      self#attach_std_item_attrs pci_attributes itm
    in
    match l with
    | [] -> failwith "Should not call class_type_declaration with no classes"
    | [x] -> class_type_declaration "class type" x
    | x :: xs ->
      makeList
        ~break:Always_rec
        ~indent:0
        ~inline:(true, true)
        (
          (class_type_declaration "class type" x)::
          List.map (class_type_declaration "and") xs
        )

  (*
     Formerly the [class_type]
     Notice how class_constructor_type doesn't have any type attributes -
     class_instance_type does.
     TODO: Divide into class_constructor_types that allow arrows and ones
     that don't.
   *)
  method class_constructor_type x =
    match x.pcty_desc with
    | Pcty_arrow (l, co, cl) ->
      let rec allArrowSegments xx = match xx.pcty_desc with
        | Pcty_arrow (l, ct1, ct2) ->
            (self#type_with_label (l, ct1))::(allArrowSegments ct2)
        (* This "new" is unfortunate. See reason_parser.mly for details. *)
        | _ -> [self#class_constructor_type xx]
      in
      let normalized =
        makeList
          ~break:IfNeed
          ~sep:"=>"
          ~preSpace:true
          ~postSpace:true
          ~inline:(true, true)
          (allArrowSegments x)
      in
      SourceMap (x.pcty_loc, normalized)
    | _ ->
      (* Unfortunately, we have to have final components of a class_constructor_type
         be prefixed with the `new` keyword.  Hopefully this is temporary. *)
      label ~space:true (atom "new") (self#class_instance_type x)

  method non_arrowed_class_constructor_type x =
    match x.pcty_desc with
    | Pcty_arrow (l, co, cl) ->
      let normalized = formatPrecedence (self#class_constructor_type x) in
      SourceMap (x.pcty_loc, normalized)
    | _ -> self#class_instance_type x

  (* TODO: TODOATTRIBUTES. *)
  method class_field x =
    let itm =
      match x.pcf_desc with
      | Pcf_inherit (ovf, ce, so) ->
        let inheritText = ("inherit" ^ override ovf) in
        let inheritExp = self#class_expr ce in
        label
          ~space:true
          (atom inheritText)
          (
            match so with
            | None -> inheritExp;
            | Some (s) -> label ~space:true inheritExp (atom ("as " ^ s))
          )
      | Pcf_val (s, mf, Cfk_concrete (ovf, e)) ->
        let opening = match mf with
          | Mutable ->
            let mutableName = [atom "mutable"; atom s.txt] in
            label
              ~space:true
              (atom ("val" ^ override ovf))
              (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed mutableName)
          | Immutable -> label ~space:true (atom ("val" ^ override ovf)) (atom s.txt)
        in
        let valExprAndConstraint = match e.pexp_desc with
          | Pexp_constraint (ex, ct) ->
            let openingWithTypeConstraint = formatTypeConstraint opening (self#core_type ct) in
            label
              ~space:true
              (makeList ~postSpace:true [openingWithTypeConstraint; atom "="])
              (self#unparseExpr ex)
          | _ ->
            label ~space:true (makeList ~postSpace:true [opening; atom "="]) (self#unparseExpr e)
        in
        valExprAndConstraint
      | Pcf_val (s, mf, Cfk_virtual ct) ->
        let opening = match mf with
          | Mutable ->
            let mutableVirtualName = [atom "mutable"; atom "virtual"; atom s.txt] in
            let openingTokens =
              (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed mutableVirtualName) in
            label ~space:true (atom "val") openingTokens
          | Immutable ->
            let virtualName = [atom "virtual"; atom s.txt] in
            let openingTokens =
              (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed virtualName) in
            label ~space:true (atom "val") openingTokens
        in
        formatTypeConstraint opening (self#core_type ct)
      | Pcf_method (s, pf, Cfk_virtual ct) ->
        let opening = match pf with
          | Private ->
            let privateVirtualName = [atom "virtual"; atom s.txt] in
            let openingTokens =
              (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed privateVirtualName) in
            label ~space:true (atom "pri") openingTokens
          | Public ->
            let virtualName = [atom "virtual"; atom s.txt] in
            let openingTokens =
              (makeList ~postSpace:true ~inline:(false, true) ~break:IfNeed virtualName) in
            label ~space:true (atom "pub") openingTokens
        in
        formatTypeConstraint opening (self#core_type ct)
      | Pcf_method (s, pf, Cfk_concrete (ovf, e)) ->
        let methodText =
           let postFix = if ovf == Override then "!" else "" in
           (
           match pf with
           | Private -> "pri" ^ postFix
           | Public -> "pub" ^ postFix
           ) in
        (* Should refactor the binding logic so faking out the AST isn't needed,
           currently, it includes a ton of nuanced logic around recovering explicitly
           polymorphic type definitions, and that furthermore, that representation...
           Actually, let's do it.

           For some reason, concrete methods are only ever parsed as Pexp_poly.
           If there *is* no polymorphic function for the method, then the return
           value of the function is wrapped in a ghost Pexp_poly with [None] for
           the type vars.*)
        (match e.pexp_desc with
          | (Pexp_poly
              ({pexp_desc=Pexp_constraint (methodFunWithNewtypes, nonVarifiedExprType)},
                Some ({ptyp_desc=Ptyp_poly (typeVars, varifiedPolyType)})
              )
            ) when (
              let (leadingAbstractVars, nonVarified) =
                self#leadingCurriedAbstractTypes methodFunWithNewtypes in
              self#isRenderableAsPolymorphicAbstractTypes
                typeVars
                (* If even artificially varified. Don't know until this returns*)
                varifiedPolyType
                leadingAbstractVars
                nonVarifiedExprType
          ) ->
            let (leadingAbstractVars, nonVarified) =
              self#leadingCurriedAbstractTypes methodFunWithNewtypes in
            self#locallyAbstractPolymorphicFunctionBinding
              methodText
              (atom s.txt)
              methodFunWithNewtypes
              leadingAbstractVars
              nonVarifiedExprType
          | Pexp_poly (e, Some ct) ->
            let typeLayout = SourceMap (ct.ptyp_loc, (self#core_type ct)) in
            let appTerms = self#unparseExprApplicationItems e in
            self#formatSimplePatternBinding methodText (atom s.txt) (Some typeLayout) appTerms
          (* This form means that there is no type constraint - it's a strange node name.*)
          | Pexp_poly (e, None) ->
            self#wrappedBinding methodText (atom s.txt) [] e
          | _ -> failwith "Concrete methods should only ever have Pexp_poly."
        )
      | Pcf_constraint (ct1, ct2) ->
        label
          ~space:true
          (atom "constraint")
          (
            makeList ~postSpace:true ~inline:(true, false) [
              makeList ~postSpace:true [self#core_type ct1; atom "="];
              self#core_type ct2
            ]
          )
      | Pcf_initializer (e) ->
        label
          ~space:true
          (atom "initializer =>")
          (self#unparseExpr e)
      | Pcf_attribute a -> self#floating_attribute a
      | Pcf_extension e ->
        (* And don't forget, we still need to print post_item_attributes even for
           this case *)
        self#item_extension e
    in
    SourceMap (x.pcf_loc, itm)

  method class_self_pattern_and_structure {pcstr_self = p; pcstr_fields = l} =
    let fields = (List.map self#class_field (List.filter self#shouldDisplayClassField l)) in
    (* Recall that by default self is bound to "this" at parse time. You'd
       have to go out of your way to bind it to "_". *)
    match (p.ppat_attributes, p.ppat_desc) with
      | ([], Ppat_var ({loc; txt = "this"})) -> fields
      | _ ->
        SourceMap (p.ppat_loc, (label ~space:true (atom "as") (self#pattern p)))
        ::fields

  method simple_class_expr x =
    let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.pcl_attributes in
    if stdAttrs <> [] then
      formatSimpleAttributed
        (self#simple_class_expr {x with pcl_attributes=[]})
        (self#attributes stdAttrs)
    else
      let itm =
        match x.pcl_desc with
        | Pcl_constraint (ce, ct) ->
          formatTypeConstraint (self#class_expr ce) (self#class_constructor_type ct)
        (* In OCaml,
          - In the most recent version of OCaml, when in the top level of a
            module, let _ = ... is a PStr_eval.
          - When in a function, it is a Pexp_let PPat_any
          - When in class pre-member let bindings it is a Pcl_let PPat_any

           Reason normalizes all of these to be simple imperative expressions
           with trailing semicolons, *except* in the case of classes because it
           will likely introduce a conflict with some proposed syntaxes for
           objects.
        *)
        | Pcl_let _
        | Pcl_structure _ ->
          let rows = (self#classExprLetsAndRest x) in
          makeList ~wrap:("{", "}") ~inline:(true, false) ~postSpace:true ~break:Always_rec (List.map semiTerminated rows)
        | Pcl_extension e -> self#extension e
        | _ -> formatPrecedence (self#class_expr x)
     in SourceMap (x.pcl_loc, itm)

  method classExprLetsAndRest x =
    match x.pcl_desc with
      | Pcl_structure cs -> self#class_self_pattern_and_structure cs
      | Pcl_let (rf, l, ce) ->
        (* For "letList" bindings, the start/end isn't as simple as with
         * module value bindings. For "let lists", the sequences were formed
         * within braces {}. The parser relocates the first let binding to the
         * first brace. *)
         let bindingsLayout = (self#bindings (rf, l)) in
         let bindingsLoc = self#bindingsLocationRange l in
         let bindingsSourceMapped = SourceMap (bindingsLoc, bindingsLayout) in
         bindingsSourceMapped::(self#classExprLetsAndRest ce)
      | _ -> [self#class_expr x]

  method class_expr x =
    let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.pcl_attributes in
    (* We cannot handle the attributes here. Must handle them in each item *)
    if stdAttrs <> [] then
      (* Do not need a "simple" attributes precedence wrapper. *)
      formatAttributed
        (self#simple_class_expr {x with pcl_attributes=[]})
        (self#attributes stdAttrs)
    else
      match x.pcl_desc with
      | Pcl_fun (l, eo, p, e) ->
          label
            ~space:true
            (makeList ~postSpace:true [
               (label ~space:true (atom "fun") (self#label_exp (l, eo, p)));
              (atom "=>");
            ])
            (self#class_expr e);
      | Pcl_apply (ce, l) ->
        let applicationItems = self#classExpressionToFormattedApplicationItems x in
        formatAttachmentApplication applicationFinalWrapping None applicationItems
      | Pcl_constr (li, l) ->
          (* TODO: Allow classes to use the same syntax as every other type
             application. *)
        (match l with
          | [] -> label ~space:true (atom "class") (self#longident_loc li)
          | ll ->
            let typeParameters =
              makeList
                ~break:IfNeed
                ~postSpace:true
                ~inline:(true, true)
                (List.map self#non_arrowed_simple_core_type l)
            in
            label
              ~space:true
              (makeList ~postSpace:true [atom "class"; self#longident_loc li])
              typeParameters
        )
      | Pcl_constraint _
      | Pcl_extension _
      | Pcl_let _
      | Pcl_structure _ -> self#simple_class_expr x;

  method signature signatureItems =
    let signatureItems = List.filter self#shouldDisplaySigItem signatureItems in
    if List.length signatureItems == 0 then
      atom ""
    else
      let signatureItems = List.filter self#shouldDisplaySigItem signatureItems in
      let first = List.nth signatureItems 0 in
      let last = List.nth signatureItems (List.length signatureItems - 1) in
      SourceMap (
        {loc_start=first.psig_loc.loc_start; loc_end=last.psig_loc.loc_end; loc_ghost=false},
        makeList
          ~newlinesAboveComments:1
          ~newlinesAboveItems:1
          ~newlinesAboveDocComments:1
          ~renderFinalSep:true
          ~postSpace:true
          ~break:Always_rec
          ~indent:0
          ~inline:(true, false)
          ~sep:";"
          (List.map self#signature_item signatureItems)
      )

  method value_description x =
    if x.pval_prim<>[] then
      makeList ~postSpace:true [
        self#core_type x.pval_type;
        atom "=";
        makeSpacedBreakableInlineList (List.map self#constant_string x.pval_prim)
      ]
    else
      self#core_type x.pval_type;

  method signature_item x :layoutNode =
    let item: layoutNode =
      match x.psig_desc with
        | Psig_type l ->
            self#type_def_list l
        | Psig_value vd ->
            let intro = if vd.pval_prim = [] then atom "let" else atom "external" in
            (formatTypeConstraint
               (label ~space:true intro (wrapLayoutWithLoc (Some (vd.pval_name.loc)) (protectIdentifier vd.pval_name.txt)))
               (self#value_description vd)
            )

        | Psig_typext te ->
            self#type_extension te
        | Psig_exception ed ->
            self#exception_declaration ed
        | Psig_class l ->
            let class_description
                ?(class_keyword=false)
                ({pci_params=ls; pci_name={txt}; pci_loc} as x) =
              let (firstToken, pattern, patternAux) = self#class_opening class_keyword txt x.pci_virt ls in
              let withColon = self#wrapCurriedFunctionBinding
                ~arrow:":"
                firstToken
                pattern
                patternAux
                ([(self#class_constructor_type x.pci_expr)], None)
              in
              let itm = self#attach_std_item_attrs x.pci_attributes withColon in
              SourceMap (pci_loc, itm)
            in
            makeNonIndentedBreakingList (
              match l with
              | [] -> raise (NotPossible "No recursive class bindings")
              | [x] -> [class_description ~class_keyword:true x]
              | x :: xs ->
                 (class_description ~class_keyword:true x)::
                 (List.map class_description xs)
            )
        | Psig_module {pmd_name; pmd_type={pmty_desc=Pmty_alias alias}} ->
            label ~space:true
              (makeList ~postSpace:true [
                 atom "module";
                 atom pmd_name.txt;
                 atom "="
               ])
              (self#longident_loc alias)
        | Psig_module pmd ->
            self#formatSimpleSignatureBinding
              "module"
              (atom pmd.pmd_name.txt)
              (self#module_type pmd.pmd_type);
        | Psig_open od ->
            label ~space:true
              (atom ("open" ^ (override od.popen_override)))
              (self#longident_loc od.popen_lid)
        | Psig_include incl ->
            label ~space:true
              (atom "include")
              (self#module_type incl.pincl_mod)
        | Psig_modtype {pmtd_name=s; pmtd_type=md} -> (
            match md with
              | None -> makeList ~postSpace:true [atom "module type"; atom s.txt]
              | Some mt ->
                  label ~space:true
                    (makeList ~postSpace:true [atom "module type"; atom s.txt; atom "="])
                    (self#module_type mt)
          )
        | Psig_class_type l -> self#class_type_declaration_list l
        | Psig_recmodule decls ->
            let first xx =
              self#formatSimpleSignatureBinding
                "module rec"
                (atom xx.pmd_name.txt)
                (self#module_type xx.pmd_type)
            in
            let notFirst xx =
              self#formatSimpleSignatureBinding
                "and"
                (atom xx.pmd_name.txt)
                (self#module_type xx.pmd_type)
            in

            let moduleBindings = match decls with
              | [] -> raise (NotPossible "No recursive module bindings")
              | hd::tl -> (first hd)::(List.map notFirst tl)
            in
            makeNonIndentedBreakingList moduleBindings
        | Psig_attribute a -> self#floating_attribute a
        | Psig_extension (e, a) ->
          self#attach_std_item_attrs a (self#item_extension e)
    in
    SourceMap (x.psig_loc, item)

  method non_arrowed_module_type x =
    match x.pmty_desc with
      | Pmty_alias li ->
          formatPrecedence (label ~space:true (atom "module") (self#longident_loc li))
      | Pmty_typeof me ->
          label ~space:true
            (atom "module type of")
            (self#module_expr me)
      | _ -> self#simple_module_type x

  method simple_module_type x =
    match x.pmty_desc with
      | Pmty_ident li ->
          self#longident_loc li;
      | Pmty_signature s ->
          makeList
            ~break:IfNeed
            ~inline:(true, false)
            ~wrap:("{", "}")
            ~newlinesAboveComments:0
            ~newlinesAboveItems:0
            ~newlinesAboveDocComments:1
            ~renderFinalSep:true
            ~postSpace:true
            ~sep:";"
            (List.map self#signature_item (List.filter self#shouldDisplaySigItem s))
      (* Not sure what this is about. *)
      | Pmty_extension _ -> assert false
      | _ -> makeList ~break:IfNeed ~wrap:("(", ")") [self#module_type x]

  method module_type x =
    (* The segments that should be separated by arrows. *)
    let rec functorTypeArgs xx = match xx.pmty_desc with
      | Pmty_functor (_, None, mt2) -> (atom "()")::(functorTypeArgs mt2)
      | Pmty_functor (s, Some mt1, mt2) ->
          if s.txt = "_" then
            (self#module_type mt1)::(functorTypeArgs mt2)
          else
            let cur =
              makeList ~wrap:("(",")") [
                formatTypeConstraint
                  (atom s.txt)
                  (self#module_type mt1)
              ] in
            cur::(functorTypeArgs mt2)
      | _ -> [self#module_type xx]
    in

    let pmty = match x.pmty_desc with
      | Pmty_functor _ ->
          let functorArgs = functorTypeArgs x in
          makeList ~break:IfNeed ~sep:"=>" ~preSpace:true ~postSpace:true ~inline:(true, true) functorArgs

      (* See comments in sugar_parser.mly about why WITH constraints aren't "non
       * arrowed" *)
      | Pmty_with (mt, l) ->
          let modSub atm li2 token = makeList ~postSpace:true [
            atom "module";
            atm;
            atom token;
            self#longident_loc li2
          ] in
          let typeAtom = atom "type" in
          let eqAtom = atom "=" in
          let destrAtom = atom ":=" in
          let with_constraint = function
            | Pwith_type (li, ({ptype_params} as td)) ->
                self#formatOneTypeDef
                  typeAtom
                  (SourceMap (li.loc, (self#longident_loc li)))
                  eqAtom
                  td
            | Pwith_module (li, li2) ->
                modSub (self#longident_loc li) li2 "="
            | Pwith_typesubst ({ptype_params} as td) ->
                self#formatOneTypeDef
                  typeAtom
                  (SourceMap (td.ptype_name.loc, (atom td.ptype_name.txt)))
                  destrAtom
                  td
            | Pwith_modsubst (s, li2) -> modSub (atom s.txt) li2 ":="
          in
          (match l with
            | [] -> self#module_type mt
            | _ ->
                label ~space:true
                  (makeList ~postSpace:true [self#module_type mt; atom "with"])
                  (makeList
                     ~break:IfNeed
                     ~inline:(true, true)
                     ~sep:"and"
                     ~postSpace:true
                     ~preSpace:true
                     (List.map with_constraint l));
          )
        (* Seems like an infinite loop just waiting to happen. *)
        | _ -> self#non_arrowed_module_type x
    in
    SourceMap (x.pmty_loc, pmty)

  method simple_module_expr x = match x.pmod_desc with
    | Pmod_unpack e ->
        formatPrecedence (makeList ~postSpace:true [atom "val"; self#unparseExpr e])
    | Pmod_ident (li) ->
        ensureSingleTokenSticksToLabel (self#longident_loc li)
    | Pmod_constraint (unconstrainedRet, mt) ->
        formatPrecedence (
          formatTypeConstraint
            (self#module_expr unconstrainedRet)
            (self#module_type mt)
        )
    | Pmod_structure (s) ->
      makeList
        ~break:Always_rec
        ~inline:(true, false)
        ~wrap:("{", "}")
        ~newlinesAboveComments:0
        ~newlinesAboveItems:0
        ~newlinesAboveDocComments:1
        ~renderFinalSep:true
        ~postSpace:true
        ~sep:";"
        (List.map self#structure_item (List.filter self#shouldDisplayStructureItem s))

    | _ ->
        (* For example, functor application will be wrapped. *)
        formatPrecedence (self#module_expr x)

  method module_expr x =
    match x.pmod_desc with
      | Pmod_functor _ ->
          let (argsList, return) = self#curriedFunctorPatternsAndReturnStruct x in (
            match (argsList, return.pmod_desc) with
              | ([], _) -> raise (NotPossible "functor must have some arg")
              | (firstArg::restArgs, _) ->
                (* See #19/20 in syntax.mls - cannot annotate return type at
                   the moment. *)
                let returnedAppTerms = self#moduleExpressionToFormattedApplicationItems return in
                self#wrapCurriedFunctionBinding "fun" firstArg restArgs returnedAppTerms
          )
      | Pmod_apply (me1, me2) ->
          let appTerms = self#moduleExpressionToFormattedApplicationItems x in
          (match appTerms with
            | ([], _) -> raise (NotPossible "no functor application terms")
            | ([hd], _) -> raise (NotPossible "one functor application terms")
            | (hd::tl, _) -> formatIndentedApplication hd tl
          )
      | Pmod_extension _ -> assert false
      | Pmod_unpack _
      | Pmod_ident _
      | Pmod_constraint _
      | Pmod_structure _ -> self#simple_module_expr x


  method structure structureItems =
    if List.length structureItems == 0 then
      atom ""
    else
      let structureItems = List.filter self#shouldDisplayStructureItem structureItems in
      let first = List.nth structureItems 0 in
      let last = List.nth structureItems (List.length structureItems - 1) in
      SourceMap (
        {loc_start=first.pstr_loc.loc_start; loc_end=last.pstr_loc.loc_end; loc_ghost=false},
        makeList
          ~newlinesAboveComments:1
          ~newlinesAboveItems:1
          ~newlinesAboveDocComments:1
          ~renderFinalSep:true
          ~postSpace:true
          ~break:Always_rec
          ~indent:0
          ~inline:(true, false)
          ~sep:";"
          (List.map self#structure_item structureItems)
      )


  (*
     How do modules become parsed?
     let module (X: sig) = blah;
       Will not parse! (Should just make it parse to let [X:sig =]).
     let module X: sig = blah;
       Becomes Pmod_constraint
     let module X: sig = (blah:sig);
       Becomes Pmod_constraint .. Pmod_constraint
     let module X = blah:typ;
       Becomes Pmod_constraint
     let module X (Y:y) (Z:z):r => Q
       Becomes Pmod_functor...=> Pmod_constraint

     let module X (Y:y) (Z:z):r => (Q:r2)
       Probably becomes Pmod_functor...=> (Pmod_constraint..
       Pmod_constraint)

    let (module X) =
      Is a *completely* different thing (unpacking/packing first class modules).
      We should make sure this is very well distinguished.
      - Just replace all "let module" with a new three letter keyword (mod)?
      - Reserve let (module X) for unpacking first class modules.

    See the notes about how Ppat_constraint become parsed and attempt to unify
    those as well.
  *)

  method let_module_binding prefixText bindingName moduleExpr =
    let (argsList, return) = self#curriedFunctorPatternsAndReturnStruct moduleExpr in (
      match (argsList, return.pmod_desc) with
        (* Simple module with type constraint, no functor args. *)
        | ([], Pmod_constraint (unconstrainedRetTerm, ct)) ->
            let appTerms = self#moduleExpressionToFormattedApplicationItems unconstrainedRetTerm in
            self#formatSimplePatternBinding prefixText bindingName (Some (self#module_type ct)) appTerms
        (* Simple module with type no constraint, no functor args. *)
        | ([], _) ->
            let appTerms = self#moduleExpressionToFormattedApplicationItems return in
            self#formatSimplePatternBinding prefixText bindingName None appTerms
        | (_, _) ->
            (* A functor *)
            let (argsWithConstraint, actualReturn) = (
              match return.pmod_desc with
                (* A functor with constrained return type:
                 *
                 * let module X = (A) (B) : Ret => ...
                 * *)
                | Pmod_constraint (me, ct) -> (argsList@[formatJustTheTypeConstraint (self#non_arrowed_module_type ct)], me)
                | _ -> (argsList, return)
            ) in
            let returnedAppTerms = self#moduleExpressionToFormattedApplicationItems actualReturn in
            self#wrapCurriedFunctionBinding prefixText bindingName argsWithConstraint returnedAppTerms
    )

    method class_opening class_keyword name pci_virt ls =
      let firstToken = if class_keyword then "class" else "and" in
      match (pci_virt, ls) with
        (* When no class params, it's a very simple formatting for the
           opener - no breaking. *)
        | (Virtual, []) ->
          (firstToken, atom "virtual", [atom name])
        | (Concrete, []) ->
          (firstToken, atom name, [])
        | (Virtual, _::_) ->
          (firstToken, atom "virtual", [atom name; self#class_params_def ls])
        | (Concrete, _::_) ->
          (firstToken, atom name, [self#class_params_def ls])


  (* TODO: TODOATTRIBUTES: Structure items don't have attributes, but each
     pstr_desc *)
  method structure_item term =
    let item = (
      match term.pstr_desc with
        | Pstr_eval (e, _attrs) -> self#unparseExpr e
        | Pstr_type [] -> assert false
        | Pstr_type l  -> (self#type_def_list l)
        | Pstr_value (rf, l) -> (self#bindings (rf, l))
        | Pstr_typext te -> (self#type_extension te)
        | Pstr_exception ed -> (self#exception_declaration ed)
        | Pstr_module x ->
            let prefixText = "module" in
            let bindingName = atom ~loc:x.pmb_name.loc x.pmb_name.txt in
            let moduleExpr = x.pmb_expr in
            self#let_module_binding prefixText bindingName moduleExpr
        | Pstr_open od -> (
            makeList ~postSpace:true [
              atom ("open" ^ (override od.popen_override));
              self#longident_loc od.popen_lid;
            ]
        )
        | Pstr_modtype {pmtd_name=s; pmtd_type=md} -> (
            match md with
              | None -> makeList ~postSpace:true [atom "module type";atom s.txt]
              | Some mt ->
                  label ~space:true
                    (makeList ~postSpace:true [atom "module type";atom s.txt; atom "="])
                    (self#module_type mt)
          )
        | Pstr_class l -> self#class_declaration_list l
        | Pstr_class_type (l) -> self#class_type_declaration_list l
        | Pstr_primitive vd ->
            let attrs =  List.map (fun x -> self#item_attribute x) vd.pval_attributes in
            let lst = List.append [
              atom ("external");
              protectIdentifier vd.pval_name.txt;
              atom (":");
              self#value_description vd;
            ] attrs in
            makeList ~postSpace:true lst
        | Pstr_include incl ->
            (* Kind of a hack *)
            let moduleExpr = incl.pincl_mod in
            let returnedAppTerms = self#moduleExpressionToFormattedApplicationItems moduleExpr in
            formatAttachmentApplication
              applicationFinalWrapping
              (Some (true, atom "include"))
              returnedAppTerms

        | Pstr_recmodule decls -> (* 3.07 *)
            let first xx =
              let prefixText = "module rec" in
              self#let_module_binding prefixText (atom xx.pmb_name.txt) xx.pmb_expr in
            let notFirst xx =
              let prefixText = "and" in
              self#let_module_binding prefixText (atom xx.pmb_name.txt) xx.pmb_expr in

            let moduleBindings = match decls with
              | [] -> raise (NotPossible "No recursive module bindings")
              | hd::tl -> (first hd)::(List.map notFirst tl)
            in
            (makeNonIndentedBreakingList moduleBindings)
        | Pstr_attribute a -> self#floating_attribute a
        | Pstr_extension (e, a) ->
          (* Notice how extensions have attributes - but not every structure
             item does. *)
          self#item_extension e
    ) in
    SourceMap(term.pstr_loc, item)

  method type_extension = wrap default#type_extension
  method extension_constructor = wrap default#extension_constructor
  (* [allowUnguardedSequenceBodies] allows sequence expressions {} to the right of `=>` to not
     be guarded in `{}` braces. *)
  method case_list ?(allowUnguardedSequenceBodies=false) l =
    let rec appendLabelToLast items rhs =
      match items with
        | hd::[] -> (label ~indent:0 ~space:true hd rhs)::[]
        | hd::tl -> hd::(appendLabelToLast tl rhs)
        | [] -> raise (NotPossible "Cannot append to last of nothing")
    in

    let case_row {pc_lhs; pc_guard; pc_rhs} =
      let theOrs = orList pc_lhs in

      (* match x with *)
      (* | AnotherReallyLongVariantName (_, _, _)   *)
      (* | AnotherReallyLongVariantName2 (_, _, _)
           when true => {                           *)

      (*   }                                        *)

      (*<sbi><X>match x with</X>   *)
      (*     <Y>everythingElse</Y> *)
      (*</sbi>                     *)



      (*     ............................................................
             :    each or segment has a spaced list <> that ties its    :
             : bar "|" to its pattern                                   :
             ...:..........................................................:.....
             :  :  each or-patterned match is grouped in SpacedBreakableInline  :
             :  :                                                          :    :
             v  v                                                          v    v
             <sbi><>|<lb><A><>     FirstThingStandalone t =></A></><B>t</B></lb></></sbi>
             <sbi><>|<C>           AnotherReallyLongVariantName (_, _, _)</C></>
             ^    <>|<lb><><lb><D>AnotherReallyLongVariantNam2 (_, _, _)</D>             (label the last in or ptn for or and label it again for arrow)
             :        ^  ^   ^     <E>when true<E></lb> =></><F>{
             :        :  :   :    </F>}</lb></sbi> ^       ^
             :        :  :   :            ^     ^   :      :
             :        :  :   :            :     :   :      :
             :        :  :   :If there is :a WHERE  :      :
             :        :  :   :an extra    :label is :      :
             :        :  :   :inserted bef:ore the  :      :
             :        :  :   :arrow.      :     :   :      :
             :        :  :   :............:.....:...:      :
             :        :  :                :     :          :
             :        :  :                :     :          :
             :        :  :                :     :          :
             :        :  :The left side of:this final label:
             :        :  :uses a list to  :append the arrow:
             :        :  :................:.....:..........:
             :        :                   :     :
             :        :                   :     :
             :        :                   :     :
             :        :Final or segment is:     :
             :        :wrapped in lbl that:     :
             :        :partitions pattern :     :
             :        :and arrow from     :     :
             :        :expression.        :     :
             :        :                   :     :
             :        :...................:     :
             :     [orsWithWhereAndArrowOnLast] :
             :                                  :
             :..................................:
                         [row]

      *)
      let bar xx = makeList ~postSpace:true [atom "|"; xx] in
      let appendWhereAndArrow p = match pc_guard with
          | None -> makeList ~interleaveComments:false ~postSpace:true [p; atom "=>"]
          | Some g ->
            (* when x should break as a whole - extra list added around it to make it break as one *)
            let withWhen = label ~space:true p (makeList ~break:Never ~inline:(true, true) ~postSpace:true [label ~space:true (atom "when") (self#unparseExpr g)]) in
            makeList ~interleaveComments:false ~inline:(true, true) ~postSpace:true [withWhen; atom "=>"]
      in

      let rec appendWhereAndArrowToLastOr = function
        | [] -> []
        | hd::tl -> (
          let formattedHd = match tl with
            | [] -> appendWhereAndArrow (self#pattern hd)
            | tl::tlTl -> (self#pattern hd)
          in
          formattedHd::(appendWhereAndArrowToLastOr tl)
        )
      in
      let orsWithWhereAndArrowOnLast = appendWhereAndArrowToLastOr theOrs in
      let rhs =
        if allowUnguardedSequenceBodies then
          match (self#under_pipe#letList pc_rhs) with
            (* TODO: Still render a list with located information here so that
               comments (eol) are interleaved *)
            | [hd] -> hd
            (* In this case, we don't need any additional indentation, because there aren't
               wrapping {} which would cause zero indentation to look strange. *)
            | lst -> makeUngaurdedLetSequence lst
        else self#under_pipe#unparseExpr pc_rhs in
      let row =
        let withoutBars = appendLabelToLast orsWithWhereAndArrowOnLast rhs in
        makeList ~break:Always_rec ~inline:(true, true) (List.map bar withoutBars)
      in
        SourceMap (
          (* Fake shift the location to accomodate for the bar, to make sure
           * the wrong comments don't make their way past the next bar. *)
          expandLocation ~expand:(0, 0) {
            loc_start = pc_lhs.ppat_loc.loc_start;
            loc_end = pc_rhs.pexp_loc.loc_end;
            loc_ghost = false;
          },
          row
        )

    in
    (List.map case_row l)

  method label_x_expression_param (l, e) =
    let param =
      match l with
        | ""  -> self#simplifyUnparseExpr e; (* level 2*)
        | lbl ->
            if lbl.[0] = '?' then
              let str = String.sub lbl 1 (String.length lbl-1) in
              let lbl = pun_labelled_expression e str in
              formatLabeledArgument (atom lbl) "?" (self#simplifyUnparseExpr e)
            else
              let lbl = pun_labelled_expression e lbl in
              formatLabeledArgument (atom lbl) "" (self#simplifyUnparseExpr e)
    in
    SourceMap (e.pexp_loc, param)

  method directive_argument = wrap default#directive_argument
  method toplevel_phrase = wrap default#toplevel_phrase
end;;


let easy = new printer ()

let toplevel_phrase f x =
  match x with
    | Ptop_def (s) ->
      easyFormatToFormatter f (layoutToEasyFormatNoComments (easy#structure s))
    | Ptop_dir (s, da) -> print_string "(* top directives not supported *)"

let case_list f x =
  let l = easy#case_list x in
  (List.iter (fun x -> easyFormatToFormatter f (layoutToEasyFormatNoComments x)) l)

let top_phrase f x =
  pp_print_newline f () ;
  toplevel_phrase f x;
  pp f ";;" ;
  pp_print_newline f ();;

(* Convert a Longident to a list of strings.
   E.g. M.Constructor will be ["Constructor"; "M.Constructor"]
   Also support ".Constructor" to specify access without a path.
 *)
let longident_for_arity lid =
  let rec toplevel = function
    | Lident s ->
        [s]
    | Ldot (lid, s) ->
        let append_s x = x ^ "." ^ s in
        s :: (List.map append_s (toplevel lid))
    | Lapply (y,s) ->
        toplevel s in
   match lid with
    | Lident s ->
        ("." ^ s) :: toplevel lid
    | _ ->
        toplevel lid

(* add expilcit_arity to a list of attributes
 *)
let add_explicit_arity loc attributes =
  ({txt="explicit_arity"; loc}, PStr []) ::
  normalized_attributes "explicit_arity" attributes

(* explicit_arity_exists check if expilcit_arity exists
 *)
let explicit_arity_not_exists attributes =
  not (attribute_exists "explicit_arity" attributes)

(* wrap_expr_with_tuple wraps an expression
 * with tuple as a sole argument.
 *)
let wrap_expr_with_tuple exp =
  {exp with pexp_desc = Pexp_tuple [exp]}

(* wrap_pat_with_tuple wraps an pattern
 * with tuple as a sole argument.
 *)
let wrap_pat_with_tuple pat =
  {pat with ppat_desc = Ppat_tuple [pat]}



(* explicit_arity_constructors is a set of constructors that are known to have
 * multiple arguments
 *
 *)

module StringSet = Set.Make(String);;

let built_in_explicit_arity_constructors = ["Some"; "Assert_failure"; "Match_failure"]

let explicit_arity_constructors = StringSet.of_list(built_in_explicit_arity_constructors @ (!configuredSettings).constructorLists)

let add_explicit_arity_mapper =
{ default_mapper with
  expr = begin fun mapper expr ->
    let expr =
      match expr with
        | {pexp_desc=Pexp_construct(lid, Some sp);
           pexp_loc;
           pexp_attributes} when
             List.exists
                (fun c -> StringSet.mem c explicit_arity_constructors)
                (longident_for_arity lid.txt) &&
             explicit_arity_not_exists pexp_attributes ->
           {pexp_desc=Pexp_construct(lid, Some (wrap_expr_with_tuple sp));
            pexp_loc;
            pexp_attributes=add_explicit_arity pexp_loc pexp_attributes}
        | x -> x
    in
    default_mapper.expr mapper expr
  end;
  pat = begin fun mapper pat ->
    let pat =
      match pat with
        | {ppat_desc=Ppat_construct(lid, Some sp);
           ppat_loc;
           ppat_attributes} when
              List.exists
                  (fun c -> StringSet.mem c explicit_arity_constructors)
                  (longident_for_arity lid.txt) &&
              explicit_arity_not_exists ppat_attributes ->
           {ppat_desc=Ppat_construct(lid, Some (wrap_pat_with_tuple sp));
            ppat_loc;
            ppat_attributes=add_explicit_arity ppat_loc ppat_attributes}
        | x -> x
    in
    default_mapper.pat mapper pat
  end;
}

let preprocessing_chain = [add_explicit_arity_mapper; escape_stars_slashes_mapper; ml_to_reason_swap_operator_mapper]

let core_type f x =
  easyFormatToFormatter f (layoutToEasyFormatNoComments (easy#core_type (apply_mapper_chain_to_type x preprocessing_chain)))
let pattern f x =
  easyFormatToFormatter f (layoutToEasyFormatNoComments (easy#pattern (apply_mapper_chain_to_pattern x preprocessing_chain)))
let signature (comments : commentWithCategory) f x =
  easyFormatToFormatter f (layoutToEasyFormat (easy#signature (apply_mapper_chain_to_signature x preprocessing_chain)) comments)
let structure (comments : commentWithCategory) f x =
  easyFormatToFormatter f (layoutToEasyFormat (easy#structure (apply_mapper_chain_to_structure x preprocessing_chain)) comments)
let expression f x =
  easyFormatToFormatter f (layoutToEasyFormatNoComments (easy#unparseExpr (apply_mapper_chain_to_expr x preprocessing_chain)))
let case_list = case_list
end
in
object
  method core_type = Formatter.core_type
  method pattern = Formatter.pattern
  method signature = Formatter.signature
  method structure = Formatter.structure
  (* For merlin-destruct *)
  method toplevel_phrase = Formatter.toplevel_phrase
  method expression = Formatter.expression
  method case_list = Formatter.case_list
end

let defaultSettings = defaultSettings
