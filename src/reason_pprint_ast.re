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
/* TODO more fine-grained precedence pretty-printing */
open Asttypes;

open Format;

open Location;

open Lexing;

open Longident;

open Parsetree;

open Easy_format;

open Syntax_util;

open Ast_mapper;

type commentCategory =
  | EndOfLine
  | SingleLine
  | Regular;

/* (comment text, attachment_location, physical location) */
type commentWithCategory = list (String.t, commentCategory, Location.t);

let print_easy easyFormatted =>
  switch easyFormatted {
  | Atom s _ [@implicit_arity] => s
  | List _ _ [@implicit_arity] => "list"
  | Label _ _ [@implicit_arity] => "label"
  | Custom _ => "custom"
  };

let (|>) x f => f x;

let (<|) f x => f x;

exception NotPossible string;

let useSingleColonForNamedArgs = false;

let case_not_implemented msg loc (file, line, column) =>
  Format.fprintf
    Format.err_formatter
    "Not Implemented Yet %s %a (from: %s:%s:%s)@."
    msg
    Location.print_loc
    loc
    file
    (string_of_int line)
    (string_of_int column);

let exprDescrString x =>
  x.pexp_loc.loc_start.Lexing.pos_fname ^
  "[" ^
  string_of_int x.pexp_loc.loc_start.Lexing.pos_lnum ^
  ", " ^ string_of_int x.pexp_loc.loc_end.Lexing.pos_lnum ^ "]";

type ruleInfoData = {reducePrecedence: precedence, shiftPrecedence: precedence}
and ruleCategory =
  /* Printing will be parsed with very high precedence, so not much need to
     worry about ensuring it will reduce correctly. In short, you can put
     `FunctionApplication` content anywhere around an infix identifier without
     wrapping in parens. For example `myFunc x y z` or `if x {y} else {z}`
     The layout is kept in list form only to allow for elegant wrapping rules
     to take into consideration the *number* of high precedence parsed items. */
  | FunctionApplication (list layoutNode)
  /* Care should be taken to ensure the rule that caused it to be parsed will
     reduce again on the printed output - context should carefully consider
     wrapping in parens according to the ruleInfoData. */
  | SpecificInfixPrecedence ruleInfoData layoutNode
  /* Not safe to include anywhere between infix operators without wrapping in
     parens. This describes expressions like `fun x => x` which doesn't fit into
     our simplistic algorithm for printing function applications separated by infix.

     It might be possible to include these in between infix, but there are
     tricky rules to determining when these must be guarded by parens (it
     depends highly on context that is hard to reason about). It's so nuanced
     that it's easier just to always wrap them in parens.  */
  | PotentiallyLowPrecedence layoutNode
  | Simple layoutNode
and associativity =
  | Right
  | Nonassoc
  | Left
and precedenceEntryType =
  | TokenPrecedence
  | CustomPrecedence
and precedence =
  | Token string
  | Custom string
/* Make a standard list */
and whenToDoSomething =
  | Never
  | IfNeed
  | Always
  /* Always_rec not only will break, it will break recursively up to the root */
  | Always_rec
/* Describes the "fixity" of a token, and stores its *printed* representation
   should it be rendered as infix/prefix (This rendering may be different than
   how it is stored in the AST). */
and tokenFixity =
  /* Such as !simple_expr and ~!simple_expr. These function applications are
     considered *almost* "simple" because they may be allowed anywhere a simple
     expression is accepted, except for when on the left hand side of a
     dot/send. */
  | AlmostSimplePrefix string
  | UnaryPlusPrefix string
  | UnaryMinusPrefix string
  | Infix string
  | Normal
and easyFormatLabelFormatter = Easy_format.t => Easy_format.t => Easy_format.t
and listConfig = {
  /* Newlines above items that do not have any comments immediately above it.
     Only really useful when used with break:Always/Always_rec */
  newlinesAboveItems: int,
  /* Newlines above regular comments */
  newlinesAboveComments: int,
  /* Newlines above doc comments */
  newlinesAboveDocComments: int,
  /* If you are only grouping something for the sake of visual appearance, and
   * not forming an actual conceptual sequence of items, then this is often
   * useful. For example, if you're appending a semicolon etc. */
  interleaveComments: bool,
  /* Whether or not to render the final separator */
  renderFinalSep: bool,
  break: whenToDoSomething,
  /* Break setting that becomes activated if a comment becomes interleaved into
   * this list. Typically, if not specified, the behavior from [break] will be
   * used.
   */
  wrap: (string, string),
  inline: (bool, bool),
  sep: string,
  indent: int,
  sepLeft: bool,
  preSpace: bool,
  postSpace: bool,
  pad: (bool, bool),
  /* A function, because the system might rearrange your previous settings, and
   * a function allows you to not be locked into some configuration that is made
   * out of date by the formatting system (suppose it removes the separator
   * token etc.) Having a function allows you to instruct our formatter how to
   * extend the "freshest" notion of the list config when comments are
   * interleaved. */
  listConfigIfCommentsInterleaved: option (listConfig => listConfig),
  /* Formatting to use if an item in a list had an end-of-line comment appended */
  listConfigIfEolCommentsInterleaved: option (listConfig => listConfig)
}
/**
 * These represent "intent to format" the AST, with some parts being annotated
 * with original source location. The benefit of tracking this in an
 * intermediate structure, is that we can then interleave comments throughout
 * the tree before generating the final representation. That prevents the
 * formatting code from having to thread comments everywhere.
 *
 * The final representation is rendered using Easy_format.
 */
and layoutNode =
  | SourceMap Location.t layoutNode /* a layout with location info */
  | WithEOLComment string layoutNode /* a layout with comment attached */
  | Sequence listConfig (list layoutNode)
  | Label easyFormatLabelFormatter layoutNode layoutNode
  | Easy Easy_format.t;

let print_comment_type =
  fun
  | Regular => "Regular"
  | EndOfLine => "End of Line"
  | SingleLine => "SingleLine";

let rec print_comments =
  fun
  | [] => ()
  | [(s, t, loc), ...tl] => {
      printf
        "%d (%d:%d)-%d (%d:%d) -- %s:||%s||\n"
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
    };

let rec print_easy_rec indent::indent=0 easyFormatted => {
  let space = Array.fold_left (^) "" (Array.make indent " ");
  switch easyFormatted {
  | Atom s _ [@implicit_arity] => printf "%s Atom:'%s'\n" space s
  | List (opening, sep, closing, config) items [@implicit_arity] =>
    let break =
      switch config.wrap_body {
      | `No_breaks => "No_breaks"
      | `Wrap_atoms => "Wrap_atoms"
      | `Never_wrap => "Never_wrap"
      | `Force_breaks => "Force_breaks"
      | `Force_breaks_rec => "Force_breaks_rec"
      | `Always_wrap => "Always_wrap"
      };
    printf "%s List: open %s close %s sep %s break %s \n" space opening closing sep break;
    let _ = List.map (print_easy_rec indent::(indent + 2)) items;
    ()
  | Label (left, config) right [@implicit_arity] =>
    let break =
      switch config.label_break {
      | `Never => "Never"
      | `Always_rec => "Always_rec"
      | `Auto => "Auto"
      | `Always => "Always"
      };
    printf "%s Label (break = %s): \n" space break;
    printf "  %s left \n" space;
    print_easy_rec indent::(indent + 2) left;
    printf "  %s right \n" space;
    print_easy_rec indent::(indent + 2) right
  | Custom _ => printf "custom \n"
  }
};

let rec print_layout indent::indent=0 layout => {
  let space = Array.fold_left (^) "" (Array.make indent " ");
  switch layout {
  | SourceMap loc layout [@implicit_arity] =>
    printf
      "%s [%d (%d:%d)-%d (%d:%d)]\n"
      space
      loc.loc_start.Lexing.pos_cnum
      loc.loc_start.Lexing.pos_lnum
      (loc.loc_start.Lexing.pos_cnum - loc.loc_start.Lexing.pos_bol)
      loc.loc_end.Lexing.pos_cnum
      loc.loc_end.Lexing.pos_lnum
      (loc.loc_end.Lexing.pos_cnum - loc.loc_end.Lexing.pos_bol);
    print_layout indent::(indent + 2) layout
  | Sequence config layout_list [@implicit_arity] =>
    let break =
      switch config.break {
      | Never => "Never"
      | IfNeed => "if need"
      | Always => "Always"
      | Always_rec => "Always_rec"
      };
    printf
      "%s Sequence of %d, sep: %s stick_to_left: %s break: %s\n"
      space
      (List.length layout_list)
      config.sep
      (string_of_bool config.sepLeft)
      break;
    let _ = List.map (print_layout indent::(indent + 2)) layout_list;
    ()
  | WithEOLComment comment layout [@implicit_arity] =>
    printf "%s WithEOLComment: \n" space;
    printf "  %s node \n" space;
    print_layout indent::(indent + 2) layout;
    printf "  %s comments : \n" space;
    printf "  %s %s\n" space comment;
    printf "\n"
  | Label _ left right [@implicit_arity] =>
    printf "%s Label: \n" space;
    printf "  %s left \n" space;
    print_layout indent::(indent + 2) left;
    printf "  %s right \n" space;
    print_layout indent::(indent + 2) right
  | Easy e => printf "%s Easy: %s \n" space (print_easy e)
  }
};

let rec longIdentSame =
  fun
  | (Lident l1, Lident l2) => String.compare l1 l2 === 0
  | (Ldot path1 l1 [@implicit_arity], Ldot path2 l2 [@implicit_arity]) =>
    longIdentSame (path1, path2) && String.compare l1 l2 === 0
  | (Lapply l11 l12 [@implicit_arity], Lapply l21 l22 [@implicit_arity]) =>
    longIdentSame (l11, l21) && longIdentSame (l12, l22)
  | _ => false;

let rec trueForEachPair l1 l2 tester =>
  switch (l1, l2) {
  | ([], []) => true
  | ([], [_, ..._]) => false
  | ([_, ..._], []) => false
  | ([hd1, ...tl1], [hd2, ...tl2]) => tester hd1 hd2 && trueForEachPair tl1 tl2 tester
  };

/*
   Checks to see if two types are the same modulo the process of varification
   which turns abstract types into type variables of the same name.
   For example, [same_ast_modulo_varification] would consider (a => b) and ('a
   => 'b) to have the same ast. This is useful in recovering syntactic sugar
   for explicit polymorphic types with locally abstract types.

   Does not compare attributes, or extensions intentionally.

   TODO: This has one more issue: We need to compare only accepting t1's type
   variables, to be considered compatible with t2's type constructors - not the
   other way around.
 */
let same_ast_modulo_varification_and_extensions t1 t2 => {
  let rec loop t1 t2 =>
    switch (t1.ptyp_desc, t2.ptyp_desc) {
    /* Importantly, cover the case where type constructors (of the form [a])
         are converted to type vars of the form ['a].
       */
    | (Ptyp_constr {txt: Lident s1} [] [@implicit_arity], Ptyp_var s2) =>
      String.compare s1 s2 === 0
    /* Now cover the case where type variables (of the form ['a]) are
         converted to type constructors of the form [a].
       */
    | (Ptyp_var s1, Ptyp_constr {txt: Lident s2} [] [@implicit_arity]) =>
      String.compare s1 s2 === 0
    /* Now cover the typical case */
    | (
        Ptyp_constr longident1 lst1 [@implicit_arity],
        Ptyp_constr longident2 lst2 [@implicit_arity]
      ) =>
      longIdentSame (longident1.txt, longident2.txt) && trueForEachPair lst1 lst2 loop
    | (Ptyp_any, Ptyp_any) => true
    | (Ptyp_var x1, Ptyp_var x2) => String.compare x1 x2 === 0
    | (
        Ptyp_arrow label1 core_type1 core_type1' [@implicit_arity],
        Ptyp_arrow label2 core_type2 core_type2' [@implicit_arity]
      ) =>
      String.compare label1 label2 === 0 &&
      loop core_type1 core_type2 && loop core_type1' core_type2'
    | (Ptyp_tuple lst1, Ptyp_tuple lst2) => trueForEachPair lst1 lst2 loop
    | (Ptyp_object lst1 o1 [@implicit_arity], Ptyp_object lst2 o2 [@implicit_arity]) =>
      let tester (s1, attrs1, t1) (s2, attrs2, t2) => String.compare s1 s2 === 0 && loop t1 t2;
      trueForEachPair lst1 lst2 tester && o1 === o2
    | (Ptyp_class longident1 lst1 [@implicit_arity], Ptyp_class longident2 lst2 [@implicit_arity]) =>
      longIdentSame (longident1.txt, longident2.txt) && trueForEachPair lst1 lst2 loop
    | (
        Ptyp_alias core_type1 string1 [@implicit_arity],
        Ptyp_alias core_type2 string2 [@implicit_arity]
      ) =>
      loop core_type1 core_type2 && String.compare string1 string2 === 0
    | (
        Ptyp_variant row_field_list1 flag1 lbl_lst_option1 [@implicit_arity],
        Ptyp_variant row_field_list2 flag2 lbl_lst_option2 [@implicit_arity]
      ) =>
      trueForEachPair row_field_list1 row_field_list2 rowFieldEqual &&
      flag1 === flag2 && lbl_lst_option1 === lbl_lst_option2
    | (
        Ptyp_poly string_lst1 core_type1 [@implicit_arity],
        Ptyp_poly string_lst2 core_type2 [@implicit_arity]
      ) =>
      trueForEachPair string_lst1 string_lst2 (fun s1 s2 => String.compare s1 s2 === 0) &&
      loop core_type1 core_type2
    | (
        Ptyp_package longident1 lst1 [@implicit_arity],
        Ptyp_package longident2 lst2 [@implicit_arity]
      ) =>
      longIdentSame (longident1.txt, longident2.txt) && trueForEachPair lst1 lst2 testPackageType
    | (Ptyp_extension s1 arg1 [@implicit_arity], Ptyp_extension s2 arg2 [@implicit_arity]) =>
      String.compare s1.txt s2.txt === 0
    | _ => false
    }
  and testPackageType (lblLongIdent1, ct1) (lblLongIdent2, ct2) =>
    longIdentSame (lblLongIdent1.txt, lblLongIdent2.txt) && loop ct1 ct2
  and rowFieldEqual f1 f2 =>
    switch (f1, f2) {
    | (
        Rtag label1 attrs1 flag1 lst1 [@implicit_arity],
        Rtag label2 attrs2 flag2 lst2 [@implicit_arity]
      ) =>
      String.compare label1 label2 === 0 && flag1 === flag2 && trueForEachPair lst1 lst2 loop
    | (Rinherit t1, Rinherit t2) => loop t1 t2
    | _ => false
    };
  loop t1 t2
};

let wrapLayoutWithLoc loc layout =>
  switch loc {
  | None => layout
  | Some loc => SourceMap loc layout [@implicit_arity]
  };

let expandLocation pos expand::(startPos, endPos) => {
  ...pos,
  loc_start: {...pos.loc_start, Lexing.pos_cnum: pos.loc_start.Lexing.pos_cnum + startPos},
  loc_end: {...pos.loc_end, Lexing.pos_cnum: pos.loc_end.Lexing.pos_cnum + endPos}
};

/*
 * Returns (arityAttrs, docAttrs, standard_attrs)
 */
let rec partitionAttributes attrs =>
  switch attrs {
  | [] => ([], [], [], [])
  | [({txt: "JSX", loc}, _) as jsx, ...atTl] =>
    let (tlArity, tlDoc, tlStandard, tlJsx) = partitionAttributes atTl;
    (tlArity, tlDoc, tlStandard, [jsx, ...tlJsx])
  | [({txt: "explicit_arity", loc}, _) as arity_attr, ...atTl]
  | [({txt: "implicit_arity", loc}, _) as arity_attr, ...atTl] =>
    let (tlArity, tlDoc, tlStandard, tlJsx) = partitionAttributes atTl;
    ([arity_attr, ...tlArity], tlDoc, tlStandard, tlJsx)
  | [({txt: "ocaml.text", loc}, _) as doc, ...atTl]
  | [({txt: "ocaml.doc", loc}, _) as doc, ...atTl] =>
    let (tlArity, tlDoc, tlStandard, tlJsx) = partitionAttributes atTl;
    (tlArity, [doc, ...tlDoc], tlStandard, tlJsx)
  | [atHd, ...atTl] =>
    let (tlArity, tlDoc, tlStandard, tlJsx) = partitionAttributes atTl;
    (tlArity, tlDoc, [atHd, ...tlStandard], tlJsx)
  };

let partitionNonrecAttr attrs =>
  List.partition
    (
      fun attr =>
        switch attr {
        | ({txt: "nonrec", _}, _) => true
        | _ => false
        }
    )
    attrs;

let extractStdAttrs attrs => {
  let (arityAttrs, docAttrs, standard_attrs, jsxAttrs) = partitionAttributes attrs;
  standard_attrs
};

let rec sequentialIfBlocks x =>
  switch x {
  | Some {pexp_desc: Pexp_ifthenelse e1 e2 els [@implicit_arity]} =>
    let (nestedIfs, finalExpression) = sequentialIfBlocks els;
    ([(e1, e2), ...nestedIfs], finalExpression)
  | Some e => ([], Some e)
  | None => ([], None)
  };

/*
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

 */
/*
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

 */
/*
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
  */
/* "Almost Simple Prefix" function applications parse with the rule:

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

   */
let almost_simple_prefix_symbols = ['!', '?', '~'];

/* Subset of prefix symbols that have special "unary precedence" */
let unary_minus_prefix_symbols = ["~-", "~-."];

let unary_plus_prefix_symbols = ["~+", "~+."];

let infix_symbols = ['=', '<', '>', '@', '^', '|', '&', '+', '-', '*', '/', '$', '%', '\\', '#'];

let operator_chars = [
  '!',
  '$',
  '%',
  '&',
  '*',
  '+',
  '-',
  '.',
  '/',
  ':',
  '<',
  '=',
  '>',
  '?',
  '@',
  '^',
  '|',
  '~'
];

let numeric_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

let special_infix_strings = [
  "asr",
  "land",
  "lor",
  "lsl",
  "lsr",
  "lxor",
  "mod",
  "or",
  ":=",
  "!=",
  "!=="
];

let updateToken = "=";

let requireIndentFor = [updateToken, ":="];

let infixTokenRequiresIndent printedIdent =>
  if (List.exists (fun i => i == printedIdent) requireIndentFor) {
    None
  } else {
    Some 0
  };

let getPrintableUnaryIdent s =>
  if (List.mem s unary_minus_prefix_symbols || List.mem s unary_plus_prefix_symbols) {
    String.sub s 1 (String.length s - 1)
  } else {
    s
  };

/* determines if the string is an infix string.
   checks backwards, first allowing a renaming postfix ("_102") which
   may have resulted from Pexp -> Texp -> Pexp translation, then checking
   if all the characters in the beginning of the string are valid infix
   characters. */
let printedStringAndFixity =
  fun
  | s when List.mem s special_infix_strings => Infix s
  | s when List.mem s.[0] infix_symbols => Infix s
  /* Correctness under assumption that unary operators are stored in AST with
     leading "~" */
  | s
      when
        List.mem s.[0] almost_simple_prefix_symbols &&
        not (List.mem s special_infix_strings) && not (s == "?") =>
    /* What *kind* of prefix fixity? */
    if (List.mem s unary_plus_prefix_symbols) {
      UnaryPlusPrefix (getPrintableUnaryIdent s)
    } else if (
      List.mem s unary_minus_prefix_symbols
    ) {
      UnaryMinusPrefix (getPrintableUnaryIdent s)
    } else {
      AlmostSimplePrefix s
    }
  | _ => Normal;

/* Also, this doesn't account for != and !== being infixop!!! */
let isSimplePrefixToken s =>
  switch (printedStringAndFixity s) {
  | AlmostSimplePrefix _ => true
  | _ => false
  };

/* Convenient bank of information that represents the parser's precedence
   rankings.  Each instance describes a precedence table entry. The function
   tests either a token string encountered by the parser, or (in the case of
   `CustomPrecedence`) the string name of a custom rule precedence declared
   using %prec */
let rules = [
  [(TokenPrecedence, fun s => (Nonassoc, isSimplePrefixToken s))],
  [
    (CustomPrecedence, fun s => (Nonassoc, s == "prec_unary_minus")),
    (CustomPrecedence, fun s => (Nonassoc, s == "prec_unary_plus"))
  ],
  /* Note the special case for "*\*", BARBAR, and LESSMINUS, AMPERSAND(s) */
  [
    (
      TokenPrecedence,
      fun s => (Right, String.length s > 1 && s.[0] === '*' && s.[1] === '\\' && s.[2] === '*')
    ),
    (TokenPrecedence, fun s => (Right, s == "lsl")),
    (TokenPrecedence, fun s => (Right, s == "lsr")),
    (TokenPrecedence, fun s => (Right, s == "asr"))
  ],
  [
    (TokenPrecedence, fun s => (Left, s.[0] === '*' && (String.length s === 1 || s !== "*\\*"))),
    (TokenPrecedence, fun s => (Left, s.[0] === '/')),
    (TokenPrecedence, fun s => (Left, s.[0] === '%')),
    (TokenPrecedence, fun s => (Left, s == "mod")),
    (TokenPrecedence, fun s => (Left, s == "land")),
    (TokenPrecedence, fun s => (Left, s == "lor")),
    (TokenPrecedence, fun s => (Left, s == "lxor"))
  ],
  [
    /* Even though these use the same *tokens* as unary plus/minus at parse
       time, when unparsing infix -/+, the CustomPrecedence rule would be
       incorrect to use, and instead we need a rule that models what infix
       parsing would use - just the regular token precedence without a custom
       precedence. */
    (TokenPrecedence, fun s => (Left, s.[0] === '+')),
    (TokenPrecedence, fun s => (Left, s.[0] === '-'))
  ],
  [(TokenPrecedence, fun s => (Right, s == "::"))],
  [
    (TokenPrecedence, fun s => (Right, s.[0] === '@')),
    (TokenPrecedence, fun s => (Right, s.[0] === '^'))
  ],
  [
    (TokenPrecedence, fun s => (Left, s.[0] === '=' && not (s == "=") && not (s == "=>"))),
    (TokenPrecedence, fun s => (Left, s.[0] === '<' && not (s == "<"))),
    (TokenPrecedence, fun s => (Left, s.[0] === '>' && not (s == ">"))),
    (TokenPrecedence, fun s => (Left, s == "!=")), /* Not preset in the RWO table! */
    (TokenPrecedence, fun s => (Left, s == "!==")), /* Not preset in the RWO table! */
    (TokenPrecedence, fun s => (Left, s == "==")),
    (TokenPrecedence, fun s => (Left, s == "===")),
    (TokenPrecedence, fun s => (Left, s == "<")),
    (TokenPrecedence, fun s => (Left, s == ">")),
    (TokenPrecedence, fun s => (Left, s.[0] === '|' && not (s == "||"))),
    (TokenPrecedence, fun s => (Left, s.[0] === '&' && not (s == "&") && not (s == "&&"))),
    (TokenPrecedence, fun s => (Left, s.[0] === '$'))
  ],
  [(TokenPrecedence, fun s => (Right, s == "&")), (TokenPrecedence, fun s => (Right, s == "&&"))],
  [(TokenPrecedence, fun s => (Right, s == "or")), (TokenPrecedence, fun s => (Right, s == "||"))],
  [
    /* The Left shouldn't ever matter in practice. Should never get in a
       situation with two consecutive infix ? - the colon saves us. */
    (TokenPrecedence, fun s => (Left, s == "?"))
  ],
  [(TokenPrecedence, fun s => (Right, s == ":="))],
  [(TokenPrecedence, fun s => (Right, s == updateToken))],
  /* It's important to account for ternary ":" being lower precedence than "?" */
  [(TokenPrecedence, fun s => (Right, s == ":"))],
  [(TokenPrecedence, fun s => (Nonassoc, s == "=>"))]
];

/* without_prefixed_backslashes removes any prefixing backslashes */
let without_prefixed_backslashes str =>
  Re_str.replace_first (Re_str.regexp "\\\\*\\(.*\\)") "\\1" str;

let indexOfFirstMatch prec::prec lst => {
  let rec indexOfFirstMatchN prec::prec lst n =>
    switch lst {
    | [] => None
    | [[], ...tl] => indexOfFirstMatchN prec::prec tl (n + 1)
    | [[hdHd, ...hdTl], ...tl] =>
      let (kind, tester) = hdHd;
      switch (prec, kind) {
      | (Token str, TokenPrecedence)
      | (Custom str, CustomPrecedence) =>
        let (associativity, foundMatch) = tester str;
        if foundMatch {
          Some (associativity, n)
        } else {
          indexOfFirstMatchN prec::prec [hdTl, ...tl] n
        }
      | _ => indexOfFirstMatchN prec::prec [hdTl, ...tl] n
      }
    };
  indexOfFirstMatchN prec::prec lst 0
};

/* Assuming it's an infix function application. */
let precedenceInfo prec::prec => {
  /* Removes prefixed backslashes in order to do proper conversion */
  let normalizedCheck =
    switch prec {
    | Token str => Token (without_prefixed_backslashes str)
    | Custom str => prec
    };
  indexOfFirstMatch prec::normalizedCheck rules
};

let isLeftAssociative prec::prec =>
  switch (precedenceInfo prec::prec) {
  | None => false
  | Some (Left, _) => true
  | Some (Right, _) => false
  | Some (Nonassoc, _) => false
  };

let isRightAssociative prec::prec =>
  switch (precedenceInfo prec::prec) {
  | None => false
  | Some (Right, _) => true
  | Some (Left, _) => false
  | Some (Nonassoc, _) => false
  };

let higherPrecedenceThan c1 c2 =>
  switch (precedenceInfo c1, precedenceInfo c2) {
  | (_, None)
  | (None, _) =>
    let (str1, str2) =
      switch (c1, c2) {
      | (Token s1, Token s2) => ("Token " ^ s1, "Token " ^ s2)
      | (Token s1, Custom s2) => ("Token " ^ s1, "Custom " ^ s2)
      | (Custom s1, Token s2) => ("Custom " ^ s1, "Token " ^ s2)
      | (Custom s1, Custom s2) => ("Custom " ^ s1, "Custom " ^ s2)
      };
    raise (NotPossible ("Cannot determine precedence of two checks " ^ str1 ^ " vs. " ^ str2))
  | (Some (_, p1), Some (_, p2)) => p1 < p2
  };

let printedStringAndFixityExpr =
  fun
  | {pexp_desc: Pexp_ident {txt: Lident l}} => printedStringAndFixity l
  | _ => Normal;

let is_predef_option =
  fun
  | Ldot (Lident "*predef*") "option" [@implicit_arity] => true
  | _ => false;

/* which identifiers are in fact operators needing parentheses */
let needs_parens txt =>
  switch (printedStringAndFixity txt) {
  | Infix _ => true
  | UnaryPlusPrefix _ => true
  | UnaryMinusPrefix _ => true
  | AlmostSimplePrefix _ => true
  | Normal => false
  };

/* some infixes need spaces around parens to avoid clashes with comment
   syntax. This isn't needed for comment syntax /* */ */
let needs_spaces txt => txt.[0] == '*' || txt.[String.length txt - 1] == '*';

/* add parentheses to binders when they are in fact infix or prefix operators */
let protect_ident ppf txt => {
  let format: format _ _ _ =
    if (not (needs_parens txt)) {
      "%s"
    } else if (needs_spaces txt) {
      "(@;%s@;)"
    } else {
      "(%s)"
    };
  fprintf ppf format txt
};

let protect_longident ppf print_longident longprefix txt => {
  let format: format _ _ _ =
    if (not (needs_parens txt)) {
      "%a.%s"
    } else if (needs_spaces txt) {
      "(@;%a.%s@;)"
    } else {
      "(%a.%s)"
    };
  fprintf ppf format print_longident longprefix txt
};

let rec longident f =>
  fun
  | Lident s => protect_ident f s
  | Ldot y s [@implicit_arity] => protect_longident f longident y s
  | Lapply y s [@implicit_arity] => fprintf f "%a(%a)" longident y longident s;

let rec orList =
  fun /* only consider ((A|B)|C)*/
  | {ppat_desc: Ppat_or p1 p2 [@implicit_arity]} => orList p1 @ orList p2
  | x => [x];

type space_formatter = format unit Format.formatter unit;

let override =
  fun
  | Override => "!"
  | Fresh => "";

/* variance encoding: need to sync up with the [parser.mly] */
let type_variance =
  fun
  | Invariant => ""
  | Covariant => "+"
  | Contravariant => "-";

type construct = [
  | `cons (list expression)
  | `list (list expression)
  | `nil
  | `normal
  | `simple Longident.t
  | `tuple
];

let view_expr x =>
  switch x.pexp_desc {
  | Pexp_construct {txt: Lident "()", _} _ [@implicit_arity] => `tuple
  | Pexp_construct {txt: Lident "[]"} _ [@implicit_arity] => `nil
  | Pexp_construct {txt: Lident "::"} (Some _) [@implicit_arity] =>
    let rec loop exp acc =>
      switch exp {
      | {pexp_desc: Pexp_construct {txt: Lident "[]"} _ [@implicit_arity]} => (List.rev acc, true)
      | {
          pexp_desc:
            Pexp_construct {txt: Lident "::"} (Some {pexp_desc: Pexp_tuple [e1, e2]})
            [@implicit_arity]
        } =>
        loop e2 [e1, ...acc]
      | e => (List.rev [e, ...acc], false)
      };
    let (ls, b) = loop x [];
    if b {
      `list ls
    } else {
      `cons ls
    }
  | Pexp_construct x None [@implicit_arity] => `simple x.txt
  | _ => `normal
  };

let is_simple_construct: construct => bool =
  fun
  | `nil
  | `tuple
  | `list _
  | `simple _
  | `cons _ => true
  | `normal => false;

let pp = fprintf;

let default = (new Pprintast.printer) ();

type funcReturnStyle =
  | ReturnValOnSameLine;

let rec detectJSXComponent e attributes l =>
  switch (e, attributes) {
  | (Pexp_ident loc, [({txt: "JSX", _}, PStr []), ...tail]) =>
    let rec checkChildren arguments nrOfChildren =>
      switch arguments {
      | [("", {pexp_desc: Pexp_construct {txt: Lident "::"} _ [@implicit_arity]}), ...tail]
      | [("", {pexp_desc: Pexp_construct {txt: Lident "[]"} _ [@implicit_arity]}), ...tail] =>
        checkChildren tail (nrOfChildren + 1)
      | [("", _), ...tail] => false
      | [(lbl, _), ...tail] => checkChildren tail nrOfChildren
      | [] => nrOfChildren == 1
      };
    let moduleNameList = List.rev (List.tl (List.rev (Longident.flatten loc.txt)));
    if (List.length moduleNameList > 0) {
      if (Longident.last loc.txt == "createElement" && checkChildren l 0) {
        Some (String.concat "." moduleNameList)
      } else {
        None
      }
    } else if (
      checkChildren l 0
    ) {
      Some (Longident.last loc.txt)
    } else {
      None
    }
  | (Pexp_ident loc, [hd, ...tail]) => detectJSXComponent e tail l
  | _ => None
  };

let detectTernary l =>
  switch l {
  | [
      {
        pc_lhs: {ppat_desc: Ppat_construct {txt: Lident "true"} _ [@implicit_arity]},
        pc_guard: None,
        pc_rhs: ifTrue
      },
      {
        pc_lhs: {ppat_desc: Ppat_construct {txt: Lident "false"} _ [@implicit_arity]},
        pc_guard: None,
        pc_rhs: ifFalse
      }
    ] =>
    Some (ifTrue, ifFalse)
  | _ => None
  };

type funcApplicationLabelStyle =
  /* No attaching to the label, but if the entire application fits on one line,
     the entire application will appear next to the label as you 'd expect. */
  | NeverWrapFinalItem
  /* Attach the first term if there are exactly two terms involved in the
        application.

        let x = firstTerm (secondTerm_1 secondTerm_2) thirdTerm;

        Ideally, we'd be able to attach all but the last argument into the label any
        time all but the last term will fit - and *not* when (attaching all but
        the last term isn't enough to prevent a wrap) - But there's no way to tell
        ahead of time if it would prevent a wrap.

        However, the number two is somewhat convenient. This models the
        indentation that you'd prefer in non-curried syntax languages like
        JavaScript, where application only ever has two terms.
     */
  | WrapFinalListyItemIfFewerThan int;

/*
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

 */
type formatSettings = {
  /* Whether or not to expect that the original parser that generated the AST
        would have annotated constructor argument tuples with explicit arity to
        indicate that they are multiple arguments. (True if parsed in original
        OCaml AST, false if using Reason parser).
     */
  constructorTupleImplicitArity: bool,
  space: int,
  /* Whether or not to begin a curried function's return expression immediately
        after the [=>] without a newline.
     */
  returnStyle: funcReturnStyle,
  /* For curried arguments in function *definitions* only: Number of [space]s
        to offset beyond the [let] keyword. Default 1.
     */
  listsRecordsIndent: int,
  /* When [funcReturnStyle] = [ReturnValOnSameLine],
     [indentWrappedPatternArgs] is not adjustable - wrapped arguments will
     always be aligned with the function name. */
  indentWrappedPatternArgs: int,
  indentMatchCases: int,
  /* Amount to indent in label-like constructs such as wrapped function
     applications, etc - or even record fields. This is not the same concept as an
     indented curried argument list. */
  indentAfterLabels: int,
  /* Amount to indent after the opening brace of switch/try.
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
     */
  trySwitchIndent: int,
  /* In the case of two term function application (when flattened), the first
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
     */
  funcApplicationLabelStyle: funcApplicationLabelStyle,
  funcCurriedPatternStyle: funcApplicationLabelStyle,
  width: int,
  assumeExplicitArity: bool,
  constructorLists: list string
};

let defaultSettings = {
  constructorTupleImplicitArity: false,
  space: 1,
  returnStyle: ReturnValOnSameLine,
  listsRecordsIndent: 2,
  indentWrappedPatternArgs: 2,
  indentMatchCases: 2,
  indentAfterLabels: 2,
  trySwitchIndent: 0,
  funcApplicationLabelStyle: WrapFinalListyItemIfFewerThan 3,
  /* WrapFinalListyItemIfFewerThan is currently a bad idea for curried
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

     */
  funcCurriedPatternStyle: NeverWrapFinalItem,
  width: 90,
  assumeExplicitArity: false,
  constructorLists: []
};

let configuredSettings = ref defaultSettings;

let configure
    width::width
    assumeExplicitArity::assumeExplicitArity
    constructorLists::constructorLists =>
  configuredSettings := {...defaultSettings, width, assumeExplicitArity, constructorLists};

let string_of_formatter f x => Format.asprintf "%a" f x;

let createFormatter () => {
  let module Formatter = {
    let settings = !configuredSettings;
    /* How do we make
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
       */
    let isArityClear attrs =>
      (!configuredSettings).assumeExplicitArity ||
      List.exists
        (
          fun
          | ({txt: "explicit_arity", loc}, _) => true
          | _ => false
        )
        attrs;
    let list_settings = {
      Easy_format.space_after_opening: false,
      space_after_separator: false,
      space_before_separator: false,
      separators_stick_left: true,
      space_before_closing: false,
      stick_to_label: true,
      align_closing: true,
      wrap_body: `No_breaks,
      indent_body: settings.listsRecordsIndent * settings.space,
      list_style: Some "list",
      opening_style: None,
      body_style: None,
      separator_style: None,
      closing_style: None
    };
    let nullStyle = {Easy_format.atom_style: Some "null"};
    let boolStyle = {Easy_format.atom_style: Some "bool"};
    let intStyle = {Easy_format.atom_style: Some "int"};
    let stringStyle = {Easy_format.atom_style: Some "string"};
    let labelStringStyle = {Easy_format.atom_style: Some "atomClss"};
    let colonStyle = {Easy_format.atom_style: Some "punct"};
    let simplifiedApplicationSettings = {
      ...list_settings,
      align_closing: true, /* So the semicolon sticks to end of application */
      /* This must be true to support this case:

             let oneNestedInvocationThatWraps = outerFunc (
               nestedFuncToInvokeThatCausesWrapping
               []
             );

            Otherwise, we would get:
             let oneNestedInvocationThatWraps = outerFunc
               (nestedFuncToInvokeThatCausesWrapping []);
         */
      stick_to_label: true, /* I don't believe this has a purpose */
      space_after_separator: true,
      wrap_body: `Never_wrap
    };
    let easyListSettingsFromListConfig listConfig => {
      let {break, wrap, inline, indent, sepLeft, preSpace, postSpace, pad, sep} = listConfig;
      let (opn, cls) = wrap;
      let (padOpn, padCls) = pad;
      let (inlineStart, inlineEnd) = inline;
      (
        opn,
        sep,
        cls,
        {
          ...list_settings,
          wrap_body:
            switch break {
            | Never => `No_breaks
            /* Yes, `Never_wrap is a horrible name - really means "if needed". */
            | IfNeed => `Never_wrap
            | Always => `Force_breaks
            | Always_rec => `Force_breaks_rec
            },
          indent_body: indent,
          space_after_separator: postSpace,
          space_before_separator: preSpace,
          space_after_opening: padOpn,
          space_before_closing: padCls,
          stick_to_label: inlineStart,
          align_closing: not inlineEnd
        }
      )
    };
    let makeListConfig
        newlinesAboveItems::newlinesAboveItems=0
        newlinesAboveComments::newlinesAboveComments=0
        newlinesAboveDocComments::newlinesAboveDocComments=0
        interleaveComments::interleaveComments=true
        listConfigIfCommentsInterleaved::listConfigIfCommentsInterleaved=?
        listConfigIfEolCommentsInterleaved::listConfigIfEolCommentsInterleaved=?
        renderFinalSep::renderFinalSep=false
        break::break=Never
        wrap::wrap=("", "")
        inline::inline=(true, false)
        sep::sep=""
        indent::indent=list_settings.indent_body
        sepLeft::sepLeft=true
        preSpace::preSpace=false
        postSpace::postSpace=false
        pad::pad=(false, false)
        () => {
      newlinesAboveItems,
      newlinesAboveComments,
      newlinesAboveDocComments,
      interleaveComments,
      listConfigIfCommentsInterleaved,
      listConfigIfEolCommentsInterleaved,
      renderFinalSep,
      break,
      wrap,
      inline,
      sep,
      indent,
      sepLeft,
      preSpace,
      postSpace,
      pad
    };
    let easyListWithConfig listConfig easyListItems => {
      let (opn, sep, cls, settings) = easyListSettingsFromListConfig listConfig;
      Easy_format.List (opn, sep, cls, settings) easyListItems [@implicit_arity]
    };
    let makeEasyList
        newlinesAboveItems::newlinesAboveItems=0
        newlinesAboveComments::newlinesAboveComments=0
        newlinesAboveDocComments::newlinesAboveDocComments=0
        interleaveComments::interleaveComments=true
        renderFinalSep::renderFinalSep=false
        break::break=Never
        wrap::wrap=("", "")
        inline::inline=(true, false)
        sep::sep=""
        indent::indent=list_settings.indent_body
        sepLeft::sepLeft=true
        preSpace::preSpace=false
        postSpace::postSpace=false
        pad::pad=(false, false)
        easyListItems => {
      let listConfig =
        makeListConfig
          newlinesAboveItems::newlinesAboveItems
          newlinesAboveComments::newlinesAboveComments
          newlinesAboveDocComments::newlinesAboveDocComments
          interleaveComments::interleaveComments
          /* This is unused at this point - separators are handled by our pretty printer,
             not Easy_format (so that we can interleave comments intelligently) */
          renderFinalSep::renderFinalSep
          break::break
          wrap::wrap
          inline::inline
          sep::sep
          indent::indent
          sepLeft::sepLeft
          preSpace::preSpace
          postSpace::postSpace
          pad::pad
          ();
      let (opn, sep, cls, listSettings) = easyListSettingsFromListConfig listConfig;
      Easy_format.List (opn, sep, cls, listSettings) easyListItems [@implicit_arity]
    };
    let makeList
        /* Allows a fallback in the event that comments were interleaved with the
         * list */
        newlinesAboveItems::newlinesAboveItems=0
        newlinesAboveComments::newlinesAboveComments=0
        newlinesAboveDocComments::newlinesAboveDocComments=0
        interleaveComments::interleaveComments=true
        listConfigIfCommentsInterleaved::listConfigIfCommentsInterleaved=?
        listConfigIfEolCommentsInterleaved::listConfigIfEolCommentsInterleaved=?
        renderFinalSep::renderFinalSep=false
        break::break=Never
        wrap::wrap=("", "")
        inline::inline=(true, false)
        sep::sep=""
        indent::indent=list_settings.indent_body
        sepLeft::sepLeft=true
        preSpace::preSpace=false
        postSpace::postSpace=false
        pad::pad=(false, false)
        lst => {
      let config =
        makeListConfig
          newlinesAboveItems::newlinesAboveItems
          newlinesAboveComments::newlinesAboveComments
          newlinesAboveDocComments::newlinesAboveDocComments
          interleaveComments::interleaveComments
          listConfigIfCommentsInterleaved::?listConfigIfCommentsInterleaved
          listConfigIfEolCommentsInterleaved::?listConfigIfEolCommentsInterleaved
          renderFinalSep::renderFinalSep
          break::break
          wrap::wrap
          inline::inline
          sep::sep
          indent::indent
          sepLeft::sepLeft
          preSpace::preSpace
          postSpace::postSpace
          pad::pad
          ();
      Sequence config lst [@implicit_arity]
    };
    let makeAppList l =>
      switch l {
      | [hd] => hd
      | _ => makeList inline::(true, true) postSpace::true break::IfNeed l
      };
    let ensureSingleTokenSticksToLabel x =>
      makeList
        interleaveComments::true
        listConfigIfCommentsInterleaved::(
          fun currentConfig => {
            ...currentConfig,
            break: Always_rec,
            postSpace: true,
            indent: 0,
            inline: (true, true)
          }
        )
        [x];
    let unbreakLabelFormatter formatter => {
      let newFormatter labelTerm term =>
        switch (formatter labelTerm term) {
        | Easy_format.Label (labelTerm, settings) term [@implicit_arity] =>
          Easy_format.Label (labelTerm, {...settings, label_break: `Never}) term [@implicit_arity]
        | _ => failwith "not a label"
        };
      newFormatter
    };
    let inlineLabel labelTerm term => {
      let settings = {
        label_break: `Never,
        space_after_label: true,
        indent_after_label: 0,
        label_style: Some "inlineLabel"
      };
      Easy_format.Label (labelTerm, settings) term [@implicit_arity]
    };
    /* Just for debugging: Set debugWithHtml = true */
    let debugWithHtml = ref false;
    let html_escape_string s => {
      let buf = Buffer.create (2 * String.length s);
      for i in 0 to (String.length s - 1) {
        switch s.[i] {
        | '&' => Buffer.add_string buf "&amp;"
        | '<' => Buffer.add_string buf "&lt;"
        | '>' => Buffer.add_string buf "&gt;"
        | c => Buffer.add_char buf c
        }
      };
      Buffer.contents buf
    };
    let html_escape = `Escape_string html_escape_string;
    let html_style = [
      ("atom", {Easy_format.tag_open: "<a>", tag_close: "</a>"}),
      ("body", {tag_open: "<lb>", tag_close: "</lb>"}),
      ("list", {tag_open: "<l>", tag_close: "</l>"}),
      ("op", {tag_open: "<op>", tag_close: "</op>"}),
      ("cl", {tag_open: "<cl>", tag_close: "</cl>"}),
      ("sep", {tag_open: "<sep>", tag_close: "</sep>"}),
      ("label", {tag_open: "<la>", tag_close: "</la>"})
    ];
    let easyLabel
        break::break=`Auto
        space::space=false
        indent::indent=settings.indentAfterLabels
        labelTerm
        term => {
      let settings = {
        label_break: break,
        space_after_label: space,
        indent_after_label: indent,
        label_style: Some "label"
      };
      Easy_format.Label (labelTerm, settings) term [@implicit_arity]
    };
    let label
        break::break=`Auto
        space::space=false
        indent::indent=settings.indentAfterLabels
        (labelTerm: layoutNode)
        (term: layoutNode) =>
      Label (fun x y => easyLabel break::break indent::indent space::space x y) labelTerm term
      [@implicit_arity];
    let labelSpace l r => label space::true l r;
    let atom loc::loc=? str => {
      let layout = Easy (Easy_format.Atom str labelStringStyle [@implicit_arity]);
      wrapLayoutWithLoc loc layout
    };
    let easyAtom str => Easy_format.Atom str labelStringStyle [@implicit_arity];

    /** Take x,y,z and n and generate [x, y, z, ...n] */
    let makeES6List lst last => {
      let last_dots = makeList [atom "...", last];
      makeList wrap::("[", "]") break::IfNeed postSpace::true sep::"," (lst @ [last_dots])
    };
    let makeNonIndentedBreakingList lst =>
      /* No align closing: So that semis stick to the ends of every break */
      makeList break::Always_rec indent::0 inline::(true, true) lst;
    let break =
      /* No align closing: So that semis stick to the ends of every break */
      makeListConfig break::Always_rec indent::0 inline::(true, true) ();
    let makeBreakableList lst => makeList break::IfNeed inline::(true, true) lst;
    let makeNonIndentedBreakableEasyList lst =>
      makeEasyList break::IfNeed inline::(true, true) indent::0 lst;
    /* Like a <span> could place with other breakableInline lists without upsetting final semicolons */
    let makeSpacedBreakableInlineList lst =>
      makeList break::IfNeed inline::(true, true) postSpace::true lst;
    let makeCommaBreakableList lst => makeList break::IfNeed postSpace::true lst;
    let makeCommaBreakableListSurround opn cls lst =>
      makeList break::IfNeed postSpace::true sep::"," wrap::(opn, cls) lst;
    /* TODO: Allow configuration of spacing around colon symbol */
    let formatPrecedence loc::loc=? formattedTerm => {
      let withParens = makeList wrap::("(", ")") break::IfNeed [formattedTerm];
      switch loc {
      | None => withParens
      | Some l => SourceMap l withParens [@implicit_arity]
      }
    };
    /* What to do when a comment wasn't interleaved in a list - default is to attach and break. */
    let fallbackCommentListConfig = break;
    let eolCommentListConfig = makeListConfig break::Never postSpace::true inline::(true, true) ();
    let isListy =
      fun
      | Easy_format.List _ => true
      | _ => false;
    let strip_trailing_whitespace str => Re_str.global_replace (Re_str.regexp " +$") "" str;
    let easyFormatToFormatter f x => {
      let buf = Buffer.create 1000;
      let fauxmatter = Format.formatter_of_buffer buf;
      let _ = Format.pp_set_margin fauxmatter settings.width;
      if debugWithHtml.contents {
        Easy_format.Pretty.define_styles fauxmatter html_escape html_style
      };
      let _ = Easy_format.Pretty.to_formatter fauxmatter x;
      let trimmed = strip_trailing_whitespace (Buffer.contents buf) |> String.trim;
      Format.fprintf f "%s\n" trimmed;
      pp_print_flush f ()
    };
    let wrap fn term => {
      ignore (flush_str_formatter ());
      let f = str_formatter;
      fn f term;
      atom (flush_str_formatter ())
    };

    /** Either an ItemComment  (not eol) designates if it's a doc comment (which
        have extra leading stars).  Or an Item which might include its eol
        comments. */
    type commentOrItem =
      | ItemComment Easy_format.t bool
      /* The item, and a list of "end of line" comments to render */
      | Item (Easy_format.t, list Easy_format.t);

    /**
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
     */
    let rec extractComments comments tester =>
      Lexing.
        /* There might be an issue here - we shouldn't have to split "up to and including".
           Up to should be sufficient. Comments' end position might be off by one (too large) */
        (
          comments |>
          List.partition (
            fun (str, attLoc, physLoc) => {
              let oneGreaterThanAttachmentLocEnd = attLoc.loc_end.pos_cnum;
              let attachmentLocLastChar = oneGreaterThanAttachmentLocEnd - 1;
              let oneGreaterThanPhysLocEnd = physLoc.loc_end.pos_cnum;
              let physLastChar = oneGreaterThanPhysLocEnd - 1;
              tester
                attLoc.loc_start.pos_cnum
                attachmentLocLastChar
                physLoc.loc_start.pos_cnum
                physLastChar
            }
          )
        );
    let space = " ";
    /* Can't you tell the difference? */
    let tab = "\t";
    let lineZeroHasMeaningfulContent str => not (
      Re_str.string_match (Re_str.regexp ("^/[\\*" ^ space ^ tab ^ "]*$")) str 0
    );
    let beginsWithStar str =>
      Re_str.string_match (Re_str.regexp ("^[" ^ space ^ tab ^ "]*\\*")) str 0;
    let numLeadingSpace str =>
      /* Actually, always true */
      if (Re_str.string_match (Re_str.regexp ("^[" ^ space ^ tab ^ "]*")) str 0) {
        String.length (Re_str.matched_string str)
      } else {
        0
      };
    let spaceBeforeMeaningfulContent str =>
      if (Re_str.string_match (Re_str.regexp ("^/[\\*" ^ space ^ tab ^ "]*")) str 0) {
        String.length (Re_str.matched_string str)
      } else {
        0
      };
    /* Computes the smallest leading spaces for non-empty lines */
    let smallestLeadingSpaces strs => {
      let rec smallestLeadingSpaces curMin strs =>
        switch strs {
        | [] => curMin
        | [hd, ...tl] =>
          if (hd == "") {
            smallestLeadingSpaces curMin tl
          } else {
            let leadingSpace = numLeadingSpace hd;
            let nextMin = min curMin leadingSpace;
            smallestLeadingSpaces nextMin tl
          }
        };
      smallestLeadingSpaces 99999 strs
    };
    let convertIsListyToIsSequencey isListyImpl => {
      let rec isSequencey layoutNode =>
        switch layoutNode {
        | SourceMap _ subLayoutNode [@implicit_arity] => isSequencey subLayoutNode
        | Sequence _ => true
        | WithEOLComment _ sub [@implicit_arity] => isSequencey sub
        | Label _ _ _ [@implicit_arity] => false
        | Easy easy => isListyImpl easy
        };
      isSequencey
    };
    let isSequencey = convertIsListyToIsSequencey isListy;
    let inline preSpace::preSpace=false postSpace::postSpace=false labelTerm term =>
      makeList
        inline::(true, true)
        postSpace::postSpace
        preSpace::preSpace
        indent::0
        break::Never
        [labelTerm, term];
    let breakline labelTerm term =>
      makeList inline::(true, true) indent::0 break::Always_rec [labelTerm, term];
    let insertBlankLines n term =>
      if (n == 0) {
        term
      } else {
        makeList
          inline::(true, true)
          indent::0
          break::Always_rec
          (Array.to_list (Array.make n (atom "")) @ [term])
      };
    let string_after s n => String.sub s n (String.length s - n);
    let wrapComment txt => "/*" ^ txt ^ "*/";
    let formatComment_ txt => {
      let commLines = Re_str.split_delim (Re_str.regexp "\n") (wrapComment txt);
      switch commLines {
      | [] => atom ""
      | [hd] =>
        makeList
          inline::(true, true) postSpace::true preSpace::true indent::0 break::IfNeed [atom hd]
      | [zero, one, ...tl] =>
        let hasMeaningfulContentOnLineZero = lineZeroHasMeaningfulContent zero;
        let attemptRemoveCount = smallestLeadingSpaces [one, ...tl];
        let leftPad =
          if (beginsWithStar one) {
            1
          } else if hasMeaningfulContentOnLineZero {
            spaceBeforeMeaningfulContent zero
          } else {
            1
          };
        let padNonOpeningLine s => {
          let numLeadingSpaceForThisLine = numLeadingSpace s;
          if (String.length s === 0) {
            ""
          } else {
            String.make leftPad ' ' ^
            string_after s (min attemptRemoveCount numLeadingSpaceForThisLine)
          }
        };
        let lines = [zero, ...List.map padNonOpeningLine [one, ...tl]];
        makeList inline::(true, true) indent::0 break::Always_rec (List.map atom lines)
      }
    };
    let formatComment locOpt::locOpt=? txt => {
      let list = formatComment_ txt;
      switch locOpt {
      | None => list
      | Some loc => SourceMap loc list [@implicit_arity]
      }
    };

    /** [hasComment layout] checks if a layout has comment attached to it */
    let rec hasComment =
      fun
      | WithEOLComment _ _ [@implicit_arity] => true
      | SourceMap _ sub [@implicit_arity] => hasComment sub
      | _ => false;
    let rec append space::space=false txt =>
      fun
      | SourceMap loc sub [@implicit_arity] =>
        SourceMap loc (append space::space txt sub) [@implicit_arity]
      | Sequence config l [@implicit_arity] when snd config.wrap != "" => {
          let sep = if space {" "} else {""};
          Sequence {...config, wrap: (fst config.wrap, snd config.wrap ^ sep ^ txt)} l
          [@implicit_arity]
        }
      | Sequence config l [@implicit_arity] when config.sep == "" => {
          let sub =
            List.mapi
              (
                fun i layout =>
                  if (i + 1 == List.length l) {
                    append space::space txt layout
                  } else {
                    layout
                  }
              )
              l;
          Sequence config sub [@implicit_arity]
        }
      | Label formatter left right [@implicit_arity] =>
        Label formatter left (append space::space txt right) [@implicit_arity]
      | layout => inline postSpace::space layout (atom txt);
    let appendSep spaceBeforeSep sep layout => {
      let sep =
        if spaceBeforeSep {
          " " ^ sep
        } else {
          sep
        };
      append sep layout
    };
    let rec flattenCommentAndSep spaceBeforeSep::spaceBeforeSep=false sep::sep=? =>
      fun
      | WithEOLComment txt sub [@implicit_arity] =>
        switch sep {
        | None => append space::true (wrapComment txt) sub
        | Some sep => append space::true (wrapComment txt) (appendSep spaceBeforeSep sep sub)
        }
      | Sequence listConfig [hd] [@implicit_arity] when hasComment hd =>
        Sequence listConfig [flattenCommentAndSep spaceBeforeSep::spaceBeforeSep sep::?sep hd]
        [@implicit_arity]
      | SourceMap loc sub [@implicit_arity] =>
        SourceMap loc (flattenCommentAndSep spaceBeforeSep::spaceBeforeSep sep::?sep sub)
        [@implicit_arity]
      | layout =>
        switch sep {
        | None => layout
        | Some sep => appendSep spaceBeforeSep sep layout
        };
    let rec preOrderWalk f layout =>
      switch (f layout) {
      | Sequence listConfig sublayouts [@implicit_arity] =>
        let newSublayouts = List.map (preOrderWalk f) sublayouts;
        Sequence listConfig newSublayouts [@implicit_arity]
      | Label formatter left right [@implicit_arity] =>
        let newLeftLayout = preOrderWalk f left;
        let newRightLayout = preOrderWalk f right;
        Label formatter newLeftLayout newRightLayout [@implicit_arity]
      | SourceMap loc sub [@implicit_arity] =>
        let newSub = preOrderWalk f sub;
        SourceMap loc newSub [@implicit_arity]
      | WithEOLComment c sub [@implicit_arity] =>
        let newSub = preOrderWalk f sub;
        WithEOLComment c newSub [@implicit_arity]
      | _ => layout
      };

    /** Recursively unbreaks a layout to make sure they stay within the same line */
    let unbreaklayout = preOrderWalk (
      fun
      | Sequence listConfig sublayouts [@implicit_arity] =>
        Sequence {...listConfig, break: Never} sublayouts [@implicit_arity]
      | Label formatter left right [@implicit_arity] =>
        Label (unbreakLabelFormatter formatter) left right [@implicit_arity]
      | layout => layout
    );

    /** [consolidateSeparator layout] walks the [layout], extract separators out of each
     *  list and insert them into PrintTree as separated items
     */
    let consolidateSeparator = preOrderWalk (
      fun
      | Sequence listConfig sublayouts [@implicit_arity]
          when listConfig.sep != "" && listConfig.sepLeft => {
          let layoutsWithSepAndComment =
            List.mapi
              (
                fun i layout =>
                  /* Do not render the final separator */
                  if (not listConfig.renderFinalSep && i + 1 == List.length sublayouts) {
                    flattenCommentAndSep spaceBeforeSep::listConfig.preSpace layout
                  } else {
                    flattenCommentAndSep
                      spaceBeforeSep::listConfig.preSpace sep::listConfig.sep layout
                  }
              )
              sublayouts;
          let break =
            if (List.exists hasComment sublayouts) {
              Always_rec
            } else {
              listConfig.break
            };
          let sep = "";
          let preSpace = false;
          Sequence {...listConfig, sep, break, preSpace} layoutsWithSepAndComment [@implicit_arity]
        }
      | WithEOLComment _ as layout =>
        makeList
          inline::(true, true)
          postSpace::false
          preSpace::true
          indent::0
          break::Always_rec
          [flattenCommentAndSep layout]
      | layout => layout
    );

    /** [insertLinesAboveItems layout] walkts the [layout] and insert empty lines
     *  based on the configuration of newlinesAboveItems
     */
    let insertLinesAboveItems = preOrderWalk (
      fun
      | Sequence listConfig sublayouts [@implicit_arity] when listConfig.newlinesAboveItems != 0 => {
          let layoutsWithLinesInjected =
            List.map (insertBlankLines listConfig.newlinesAboveItems) sublayouts;
          Sequence {...listConfig, newlinesAboveItems: 0} layoutsWithLinesInjected
          [@implicit_arity]
        }
      | layout => layout
    );

    /** Union of two locations */
    let unionLoc loc1 loc2 =>
      switch (loc1, loc2) {
      | (None, _) => loc2
      | (_, None) => loc1
      | (Some loc1, Some loc2) => Some {...loc1, loc_end: loc2.loc_end}
      };

    /** [getLocFromLayout] recursively takes the unioned location of its children,
     *  and returns the max one
     */
    let rec getLocFromLayout =
      fun
      | Sequence listConfig subLayouts [@implicit_arity] => {
          let locs = List.map getLocFromLayout subLayouts;
          List.fold_left unionLoc None locs
        }
      | Label formatter left right [@implicit_arity] => {
          let leftLoc = getLocFromLayout left;
          let rightLoc = getLocFromLayout right;
          unionLoc leftLoc rightLoc
        }
      | SourceMap loc _ [@implicit_arity] => Some loc
      | WithEOLComment _ sub [@implicit_arity] => getLocFromLayout sub
      | _ => None;

    /**
     * Returns true if loc1 contains loc2
     */
    let containLoc loc1 loc2 =>
      loc1.loc_start.Lexing.pos_cnum <= loc2.loc_start.Lexing.pos_cnum &&
      loc1.loc_end.Lexing.pos_cnum >= loc2.loc_end.Lexing.pos_cnum;

    /**
     * Returns true if loc1 is before loc2
     */
    let beforeLoc loc1 loc2 => loc1.loc_end.Lexing.pos_cnum <= loc2.loc_start.Lexing.pos_cnum;
    let attachEOLComment layout txt => WithEOLComment txt layout [@implicit_arity];

    /**
     * Returns true if the layout's location contains loc
     */
    let layoutContainsLoc loc layout =>
      switch (getLocFromLayout layout) {
      | None => false
      | Some subLoc => containLoc subLoc loc
      };

    /**
     * Returns true if any of the subLayout's location contains loc
     */
    let anySublayoutContainLocation loc => List.exists (layoutContainsLoc loc);
    let isDocComment (c, _, _) => String.length c > 0 && c.[0] === '*';

    /**
     * prependSingleLineComment inserts a single line comment right above layout
     */
    let rec prependSingleLineComment
            newlinesAboveDocComments::newlinesAboveDocComments=0
            comment
            layout => {
      let (txt, _, loc) = comment;
      switch layout {
      | WithEOLComment c sub [@implicit_arity] =>
        WithEOLComment
          c
          (prependSingleLineComment newlinesAboveDocComments::newlinesAboveDocComments comment sub)
        [@implicit_arity]
      | SourceMap loc sub [@implicit_arity] =>
        SourceMap
          loc
          (prependSingleLineComment newlinesAboveDocComments::newlinesAboveDocComments comment sub)
        [@implicit_arity]
      | Sequence config [hd, ...tl] [@implicit_arity] when config.break == Always_rec =>
        Sequence
          config
          [
            prependSingleLineComment newlinesAboveDocComments::newlinesAboveDocComments comment hd,
            ...tl
          ]
        [@implicit_arity]
      | layout =>
        let withComment = breakline (formatComment locOpt::loc txt) layout;
        if (isDocComment comment) {
          insertBlankLines newlinesAboveDocComments withComment
        } else {
          withComment
        }
      }
    };

    /**
     * [looselyAttachComment layout comment] preorderly walks the layout and
     * find a place where the comment can be loosely attached to
     */
    let rec looselyAttachComment layout ((txt, _, commentLoc) as comment) =>
      switch layout {
      | SourceMap loc sub [@implicit_arity] =>
        SourceMap loc (looselyAttachComment sub comment) [@implicit_arity]
      | WithEOLComment c sub [@implicit_arity] =>
        WithEOLComment c (looselyAttachComment sub comment) [@implicit_arity]
      | Easy e => inline postSpace::true layout (formatComment txt)
      | Sequence listConfig subLayouts [@implicit_arity]
          when anySublayoutContainLocation commentLoc subLayouts =>
        /* If any of the subLayout strictly contains this comment, recurse into to it */
        let subLayouts =
          List.map
            (
              fun layout =>
                if (layoutContainsLoc commentLoc layout) {
                  looselyAttachComment layout comment
                } else {
                  layout
                }
            )
            subLayouts;
        Sequence listConfig subLayouts [@implicit_arity]
      | Sequence listConfig subLayouts [@implicit_arity] =>
        let (beforeComment, afterComment) =
          Syntax_util.pick_while
            (
              fun layout =>
                switch (getLocFromLayout layout) {
                | None => true
                | Some loc => beforeLoc loc commentLoc
                }
            )
            subLayouts;
        let newSubLayout =
          switch (List.rev beforeComment) {
          | [] => [
              prependSingleLineComment comment (List.hd afterComment),
              ...List.tl afterComment
            ]
          | [hd, ...tl] => ([attachEOLComment hd txt, ...tl] |> List.rev) @ afterComment
          };
        Sequence listConfig newSubLayout [@implicit_arity]
      | Label formatter left right [@implicit_arity] =>
        let leftLoc = getLocFromLayout left;
        let rightLoc = getLocFromLayout right;
        let (newLeft, newRight) =
          switch (leftLoc, rightLoc) {
          | (None, None) => (left, looselyAttachComment right comment)
          | (_, Some loc2) when containLoc loc2 commentLoc => (
              left,
              looselyAttachComment right comment
            )
          | (Some loc1, _) when containLoc loc1 commentLoc => (
              looselyAttachComment left comment,
              right
            )
          | (Some loc1, Some loc2) when beforeLoc commentLoc loc1 => (
              prependSingleLineComment comment left,
              right
            )
          | (Some loc1, Some loc2) when beforeLoc commentLoc loc2 => (
              left,
              prependSingleLineComment comment right
            )
          | _ => (left, attachEOLComment right txt)
          };
        Label formatter newLeft newRight [@implicit_arity]
      };

    /**
     * [insertSingleLineComment layout comment] preorderly walks the layout and
     * find a place where the SingleLineComment can be fit into
     */
    let rec insertSingleLineComment layout comment => {
      let (txt, _, commentLoc) = comment;
      switch layout {
      | SourceMap loc sub [@implicit_arity] =>
        SourceMap loc (insertSingleLineComment sub comment) [@implicit_arity]
      | WithEOLComment c sub [@implicit_arity] =>
        WithEOLComment c (insertSingleLineComment sub comment) [@implicit_arity]
      | Easy e => prependSingleLineComment comment layout
      | Sequence listConfig subLayouts [@implicit_arity] =>
        let newlinesAboveDocComments = listConfig.newlinesAboveDocComments;
        let (beforeComment, afterComment) =
          Syntax_util.pick_while
            (
              fun layout =>
                switch (getLocFromLayout layout) {
                | None => true
                | Some loc => beforeLoc loc commentLoc
                }
            )
            subLayouts;
        switch afterComment {
        | /* Nothing in the list is after comment, attach comment to the statement before the comment */ [] =>
          let revBeforeComment = List.rev beforeComment;
          let lastItemBeforeComment = List.hd revBeforeComment;
          Sequence
            listConfig
            (
              List.rev [
                breakline lastItemBeforeComment (formatComment locOpt::commentLoc txt),
                ...List.tl revBeforeComment
              ]
            )
          [@implicit_arity]
        | [hd, ...tl] =>
          let afterComment =
            switch (getLocFromLayout hd) {
            | Some loc when containLoc loc commentLoc => [
                insertSingleLineComment hd comment,
                ...tl
              ]
            | Some loc => [
                SourceMap
                  loc
                  (
                    prependSingleLineComment
                      newlinesAboveDocComments::newlinesAboveDocComments comment hd
                  )
                [@implicit_arity],
                ...tl
              ]
            | _ => [
                prependSingleLineComment
                  newlinesAboveDocComments::newlinesAboveDocComments comment hd,
                ...tl
              ]
            };
          Sequence listConfig (beforeComment @ afterComment) [@implicit_arity]
        }
      | Label formatter left right [@implicit_arity] =>
        let leftLoc = getLocFromLayout left;
        let rightLoc = getLocFromLayout right;
        let (newLeft, newRight) =
          switch (leftLoc, rightLoc) {
          | (None, None) => (left, insertSingleLineComment right comment)
          | (_, Some loc2) when containLoc loc2 commentLoc => (
              left,
              insertSingleLineComment right comment
            )
          | (Some loc1, _) when containLoc loc1 commentLoc => (
              insertSingleLineComment left comment,
              right
            )
          | (Some loc1, Some loc2) when beforeLoc commentLoc loc1 => (
              prependSingleLineComment comment left,
              right
            )
          | (Some loc1, Some loc2) when beforeLoc commentLoc loc2 => (
              left,
              prependSingleLineComment comment right
            )
          | _ => (left, breakline right (formatComment locOpt::commentLoc txt))
          };
        Label formatter newLeft newRight [@implicit_arity]
      }
    };
    let rec attachCommentToNodeRight layout ((txt, t, loc) as comment) =>
      switch layout {
      | Sequence config sub [@implicit_arity] when snd config.wrap != "" =>
        Sequence {...config, wrap: (fst config.wrap, snd config.wrap ^ " " ^ wrapComment txt)} sub
        [@implicit_arity]
      | SourceMap loc sub [@implicit_arity] =>
        SourceMap loc (attachCommentToNodeRight sub comment) [@implicit_arity]
      | layout =>
        switch t {
        | EndOfLine => WithEOLComment txt layout [@implicit_arity]
        | _ => inline postSpace::true layout (formatComment txt)
        }
      };
    let rec attachCommentToNodeLeft ((txt, _, loc) as comment) layout =>
      switch layout {
      | Sequence config sub [@implicit_arity] when snd config.wrap != "" =>
        Sequence {...config, wrap: (wrapComment txt ^ " " ^ fst config.wrap, snd config.wrap)} sub
        [@implicit_arity]
      | SourceMap loc sub [@implicit_arity] =>
        SourceMap loc (attachCommentToNodeLeft comment sub) [@implicit_arity]
      | layout => Label inlineLabel (formatComment txt) layout [@implicit_arity]
      };
    let isNone opt =>
      switch opt {
      | None => true
      | _ => false
      };

    /** [tryPerfectlyAttachComment layout comment] postorderly walk the [layout] and tries
     *  to perfectly attach a comment with a layout node.
     *
     *  Perfectly attach here means a comment's start location is equal to the node's end location
     *  and vice versa.
     *
     *  If the comment can be perfectly attached to any layout node, returns (newLayout, None),
     *  meaning the comment is consumed. Otherwise returns the (unchangedLayout, Some comment),
     *  meaning the comment is not consumed.
     */
    let rec tryPerfectlyAttachComment layout comment =>
      switch comment {
      | None => (layout, comment)
      | Some ((s, t, commLoc) as c) =>
        switch layout {
        | Sequence listConfig subLayouts [@implicit_arity] =>
          let distributeCommentIntoSubLayouts (i, processed, newComment) layout => {
            let (layout, newComment) = tryPerfectlyAttachComment layout newComment;
            (i + 1, [layout, ...processed], newComment)
          };
          let (_, processed, consumed) =
            List.fold_left distributeCommentIntoSubLayouts (0, [], comment) (List.rev subLayouts);
          (Sequence listConfig processed [@implicit_arity], consumed)
        | Label labelFormatter left right [@implicit_arity] =>
          let (newRight, comment) = tryPerfectlyAttachComment right comment;
          let (newLeft, comment) = tryPerfectlyAttachComment left comment;
          (Label labelFormatter newLeft newRight [@implicit_arity], comment)
        | SourceMap loc subLayout [@implicit_arity] =>
          if (
            loc.loc_end.Lexing.pos_lnum == loc.loc_start.Lexing.pos_lnum &&
            commLoc.loc_start.Lexing.pos_cnum == loc.loc_end.Lexing.pos_cnum
          ) {
            (
              SourceMap
                loc
                (
                  makeList
                    inline::(true, true)
                    break::Always
                    [unbreaklayout (attachCommentToNodeRight subLayout c)]
                )
              [@implicit_arity],
              None
            )
          } else {
            let (layout, comment) = tryPerfectlyAttachComment subLayout comment;
            switch comment {
            | None => (SourceMap loc layout [@implicit_arity], None)
            | Some ((s, t, commLoc) as comment) =>
              if (commLoc.loc_end.Lexing.pos_cnum == loc.loc_start.Lexing.pos_cnum) {
                (SourceMap loc (attachCommentToNodeLeft comment layout) [@implicit_arity], None)
              } else if (
                commLoc.loc_start.Lexing.pos_cnum == loc.loc_end.Lexing.pos_cnum
              ) {
                (SourceMap loc (attachCommentToNodeRight layout comment) [@implicit_arity], None)
              } else {
                (SourceMap loc layout [@implicit_arity], Some comment)
              }
            }
          }
        | WithEOLComment c sub [@implicit_arity] =>
          let (processed, consumed) = tryPerfectlyAttachComment sub comment;
          (WithEOLComment c processed [@implicit_arity], consumed)
        | _ => (layout, comment)
        }
      };

    /** [insertComment layout comment] inserts comment into layout*/
    let insertComment layout comment => {
      /* print_layout layout; */
      let (txt, t, loc) = comment;
      let layout =
        switch t {
        | Regular
        | EndOfLine =>
          let (layout, c) = tryPerfectlyAttachComment layout (Some comment);
          switch c {
          | None => layout
          | Some _ => looselyAttachComment layout comment
          }
        | SingleLine => insertSingleLineComment layout comment
        };
      /* print_comments [comment]; */
      /* print_layout layout; */
      layout
    };

    /** [insertComments layout comments] inserts comments into layout*/
    let insertComments = List.fold_left insertComment;

    /** [isSingleLineComment comment] checks if a comment is singleline comment*/
    let isSingleLineComment (_, t, _) =>
      switch t {
      | SingleLine => true
      | _ => false
      };
    let rec layoutToEasyFormat_ =
      fun
      | Sequence listConfig subLayouts [@implicit_arity] =>
        easyListWithConfig listConfig (List.map layoutToEasyFormat_ subLayouts)
      | Label labelFormatter left right [@implicit_arity] =>
        labelFormatter (layoutToEasyFormat_ left) (layoutToEasyFormat_ right)
      | SourceMap _ subLayout [@implicit_arity] => layoutToEasyFormat_ subLayout
      | WithEOLComment _ sub [@implicit_arity] => layoutToEasyFormat_ sub
      | Easy e => e;
    let layoutToEasyFormatNoComments layoutNode => layoutToEasyFormat_ layoutNode;
    let layoutToEasyFormat layoutNode comments => {
      /* print_layout layoutNode; */
      let layout = layoutNode;
      let revComments = List.rev comments;
      let (singleLineComments, nonSingleLineComments) =
        List.partition isSingleLineComment revComments;
      let layout = insertComments layout nonSingleLineComments;
      let layout = consolidateSeparator layout;
      let layout = insertComments layout singleLineComments;
      let layout = insertLinesAboveItems layout;
      let easyFormat = layoutToEasyFormat_ layout;
      /* print_easy_rec easyFormat; */
      makeEasyList break::Always_rec indent::0 inline::(true, true) [easyFormat]
    };
    let partitionFinalWrapping listTester wrapFinalItemSetting x => {
      let rev = List.rev x;
      switch (rev, wrapFinalItemSetting) {
      | ([], _) => raise (NotPossible "shouldnt be partitioning 0 label attachments")
      | (_, NeverWrapFinalItem) => None
      | ([last, ...revEverythingButLast], WrapFinalListyItemIfFewerThan max) =>
        if (not (listTester last) || List.length x >= max) {
          None
        } else {
          Some (List.rev revEverythingButLast, last)
        }
      }
    };
    let semiTerminated term => makeList interleaveComments::false [term, atom ";"];
    /* postSpace is so that when comments are interleaved, we still use spacing rules. */
    let makeLetSequence letItems =>
      makeList
        break::Always_rec
        inline::(true, false)
        wrap::("{", "}")
        newlinesAboveComments::0
        newlinesAboveItems::0
        newlinesAboveDocComments::1
        renderFinalSep::false
        postSpace::true
        sep::";"
        letItems;
    let makeLetSequenceSingleLine letItems =>
      makeList
        break::IfNeed
        inline::(true, false)
        wrap::("{", "}")
        newlinesAboveComments::0
        newlinesAboveItems::0
        newlinesAboveDocComments::1
        renderFinalSep::false
        preSpace::true
        postSpace::true
        sep::";"
        letItems;
    /* postSpace is so that when comments are interleaved, we still use spacing rules. */
    let makeUngaurdedLetSequence letItems =>
      makeList
        break::Always_rec
        inline::(true, true)
        wrap::("", "")
        newlinesAboveComments::0
        indent::0
        newlinesAboveItems::0
        newlinesAboveDocComments::1
        renderFinalSep::false
        postSpace::true
        sep::";"
        letItems;
    let formatSimpleAttributed x y =>
      makeList wrap::("(", ")") break::IfNeed indent::0 postSpace::true [x, y];
    let formatAttributed x y =>
      makeList break::IfNeed inline::(true, true) indent::0 postSpace::true [x, y];
    /* For when the type constraint should be treated as a separate breakable line item itself
         not docked to some value/pattern label.
         fun x
             y
             : retType => blah;
       */
    let formatJustTheTypeConstraint =
      if useSingleColonForNamedArgs {
        fun typ => makeList postSpace::true [atom ":", typ]
      } else {
        fun typ => makeList postSpace::false [atom ":", typ]
      };
    let formatTypeConstraint =
      if useSingleColonForNamedArgs {
        fun one two => label space::true (makeList postSpace::true [one, atom ":"]) two
      } else {
        fun one two => label space::true (makeList postSpace::false [one, atom ":"]) two
      };
    let formatLabeledArgument =
      if useSingleColonForNamedArgs {
        fun lbl lblSuffix term => label space::false (makeList [lbl, atom (":" ^ lblSuffix)]) term
      } else {
        fun lbl lblSuffix term => label space::false (makeList [lbl, atom ("::" ^ lblSuffix)]) term
      };
    let formatCoerce expr optType coerced =>
      switch optType {
      | None => label space::true (makeList postSpace::true [expr, atom ":>"]) coerced
      | Some typ =>
        label
          space::true (makeList postSpace::true [formatTypeConstraint expr typ, atom ":>"]) coerced
      };
    /* Standard function application style indentation - no special wrapping
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
     */
    let formatIndentedApplication headApplicationItem argApplicationItems =>
      label space::true headApplicationItem (makeAppList argApplicationItems);
    /* The loc, is an optional location or the returned app terms */
    let formatAttachmentApplication
        finalWrapping
        (attachTo: option (bool, layoutNode))
        (appTermItems, loc) => {
      let partitioning = finalWrapping appTermItems;
      let maybeSourceMap maybeLoc x =>
        switch maybeLoc {
        | None => x
        | Some loc => SourceMap loc x [@implicit_arity]
        };
      switch partitioning {
      | None =>
        switch (appTermItems, attachTo) {
        | ([], _) => raise (NotPossible "No app terms")
        | ([hd], None) => maybeSourceMap loc hd
        | ([hd], Some (useSpace, toThis)) => label space::useSpace toThis (maybeSourceMap loc hd)
        | ([hd, ...tl], None) => maybeSourceMap loc (formatIndentedApplication hd tl)
        | ([hd, ...tl], Some (useSpace, toThis)) =>
          label space::useSpace toThis (maybeSourceMap loc (formatIndentedApplication hd tl))
        }
      | Some (attachedList, wrappedListy) =>
        switch (attachedList, attachTo) {
        | ([], Some (useSpace, toThis)) =>
          label space::useSpace toThis (maybeSourceMap loc wrappedListy)
        | ([], None) =>
          /* Not Sure when this would happen */
          maybeSourceMap loc wrappedListy
        | ([hd, ...tl], Some (useSpace, toThis)) =>
          /* TODO: Can't attach location to this - maybe rewrite anyways */
          let attachedArgs = makeAppList attachedList;
          label space::useSpace toThis (label space::true attachedArgs wrappedListy)
        | ([hd, ...tl], None) =>
          /* Args that are "attached to nothing" */
          let appList = makeAppList attachedList;
          maybeSourceMap loc (label space::true appList wrappedListy)
        }
      }
    };
    /*
      Preprocesses an expression term for the sake of label attachments ([letx =
      expr]or record [field: expr]). Function application should have special
      treatment when placed next to a label. (The invoked function term should
      "stick" to the label in some cases). In others, the invoked function term
      should become a new label for the remaining items to be indented under.
     */
    let applicationFinalWrapping x =>
      partitionFinalWrapping isSequencey settings.funcApplicationLabelStyle x;
    let curriedFunctionFinalWrapping x =>
      partitionFinalWrapping isSequencey settings.funcCurriedPatternStyle x;
    let typeApplicationFinalWrapping typeApplicationItems =>
      partitionFinalWrapping isSequencey settings.funcApplicationLabelStyle typeApplicationItems;
    /* add parentheses to binders when they are in fact infix or prefix operators */
    let protectIdentifier txt =>
      if (not (needs_parens txt)) {
        atom txt
      } else if (needs_spaces txt) {
        makeList interleaveComments::false wrap::("(", ")") pad::(true, true) [atom txt]
      } else {
        atom ("(" ^ txt ^ ")")
      };
    let protectLongIdentifier longPrefix txt =>
      makeList interleaveComments::false [longPrefix, atom ".", protectIdentifier txt];
    class printer () => {
      as (self: 'self);
      val pipe = false;
      val semi = false;
      /* The test and first branch of ternaries must be guarded */
      method under_pipe = {<pipe: true>};
      method under_semi = {<semi: true>};
      method reset_semi = {<semi: false>};
      method reset_pipe = {<pipe: false>};
      method reset = {<pipe: false, semi: false>};
      method list:
        'a .
        sep::space_formatter? =>
        first::space_formatter? =>
        last::space_formatter? =>
        (Format.formatter => 'a => unit) =>
        Format.formatter =>
        list 'a =>
        unit
       =
        default#list;
      method option:
        'a .
        first::space_formatter? =>
        last::space_formatter? =>
        (Format.formatter => 'a => unit) =>
        Format.formatter =>
        option 'a =>
        unit
       =
        default#option;
      method longident =
        fun
        | Lident s => protectIdentifier s
        | Ldot longPrefix s [@implicit_arity] =>
          protectLongIdentifier (self#longident longPrefix) s
        | Lapply y s [@implicit_arity] =>
          makeList
            interleaveComments::false [self#longident y, atom "(", self#longident s, atom ")"];
      /* This form allows applicative functors. */
      method longident_class_or_type_loc x => self#longident x.txt;
      /* TODO: Fail if observing applicative functors for this form. */
      method longident_loc (x: Location.loc Longident.t) =>
        SourceMap x.loc (self#longident x.txt) [@implicit_arity];
      method constant = wrap default#constant;
      method constant_string = wrap default#constant_string;
      method tyvar = wrap default#tyvar;
      /* c ['a,'b] */
      method class_params_def =
        fun
        | [] => atom ""
        | l => makeList postSpace::true (List.map self#type_param l);
      /* This will fall through to the simple version. */
      method non_arrowed_core_type x => self#non_arrowed_non_simple_core_type x;
      method core_type2 x => {
        let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.ptyp_attributes;
        if (stdAttrs != []) {
          formatAttributed
            (self#non_arrowed_simple_core_type {...x, ptyp_attributes: []})
            (self#attributes stdAttrs)
        } else {
          let rec allArrowSegments xx =>
            switch xx.ptyp_desc {
            | Ptyp_arrow l ct1 ct2 [@implicit_arity] => [
                self#type_with_label (l, ct1),
                ...allArrowSegments ct2
              ]
            | _ => [self#core_type2 xx]
            };
          switch x.ptyp_desc {
          | Ptyp_arrow l ct1 ct2 [@implicit_arity] =>
            let normalized =
              makeList
                break::IfNeed
                sep::"=>"
                preSpace::true
                postSpace::true
                inline::(true, true)
                (allArrowSegments x);
            SourceMap x.ptyp_loc normalized [@implicit_arity]
          | Ptyp_poly sl ct [@implicit_arity] =>
            let poly =
              makeList
                break::IfNeed
                [
                  makeList
                    postSpace::true
                    [makeList postSpace::true (List.map (fun x => self#tyvar x) sl), atom "."],
                  self#core_type ct
                ];
            SourceMap x.ptyp_loc poly [@implicit_arity]
          | _ => self#non_arrowed_core_type x
          }
        }
      };
      /* Same as core_type2 but can be aliased */
      method core_type x => {
        let (arityAttrs, docAtrs, stdAttrs, jsxAttrss) = partitionAttributes x.ptyp_attributes;
        if (stdAttrs != []) {
          formatAttributed
            (self#non_arrowed_simple_core_type {...x, ptyp_attributes: []})
            (self#attributes stdAttrs)
        } else {
          switch x.ptyp_desc {
          | Ptyp_alias ct s [@implicit_arity] =>
            SourceMap
              x.ptyp_loc
              (
                label
                  space::true
                  (self#core_type ct)
                  (makeList postSpace::true [atom "as", atom ("'" ^ s)])
              )
            [@implicit_arity]
          | _ => self#core_type2 x
          }
        }
      };
      method type_with_label (label, {ptyp_desc} as c) =>
        switch label {
        | "" => self#non_arrowed_non_simple_core_type c /* otherwise parenthesize */
        | s =>
          if (s.[0] == '?') {
            let len = String.length s - 1;
            let lbl = String.sub s 1 len;
            switch ptyp_desc {
            | Ptyp_constr {txt} l [@implicit_arity] =>
              assert (is_predef_option txt);
              let everythingButQuestion =
                formatLabeledArgument
                  (atom lbl)
                  ""
                  (
                    makeList
                      postSpace::true
                      break::IfNeed
                      inline::(true, true)
                      /* Why not support aliasing here? */
                      /* I don't think you'll have more than one l here. */
                      (List.map self#non_arrowed_non_simple_core_type l)
                  );
              makeList [everythingButQuestion, atom "?"]
            | _ => failwith "invalid input in print_type_with_label"
            }
          } else {
            formatLabeledArgument (atom s) "" (self#non_arrowed_non_simple_core_type c)
          }
        };
      method type_param (ct, a) => makeList [atom (type_variance a), self#core_type ct];
      /* According to the parse rule [type_declaration], the "type declaration"'s
       * physical location (as indicated by [td.ptype_loc]) begins with the
       * identifier and includes the constraints. */
      method formatOneTypeDef
             prepend
             name
             assignToken
             ({ptype_params, ptype_kind, ptype_manifest, ptype_loc} as td) => {
        let (equalInitiatedSegments, constraints) = self#type_declaration_binding_segments td;
        let formattedTypeParams = List.map self#type_param ptype_params;
        let binding = makeList postSpace::true [prepend, name];
        /*
             /-----------everythingButConstraints--------------  | -constraints--\
            /-innerL---| ------innerR--------------------------\
           /binding\     /typeparams\ /--equalInitiatedSegments-\
           type name      'v1    'v1  =  foo = private bar        constraint a = b
         */
        let labelWithParams =
          switch formattedTypeParams {
          | [] => binding
          | [phd, ...ptl] =>
            label
              space::true
              binding
              (makeList postSpace::true break::IfNeed inline::(true, true) [phd, ...ptl])
          };
        let everythingButConstraints = {
          let nameParamsEquals = makeList postSpace::true [labelWithParams, assignToken];
          switch equalInitiatedSegments {
          | [] => labelWithParams
          | [hd, hd2, hd3, ...tl] => raise (NotPossible "More than two type segments.")
          | [hd] =>
            formatAttachmentApplication
              typeApplicationFinalWrapping (Some (true, nameParamsEquals)) (hd, None)
          | [hd, hd2] =>
            let first = makeList postSpace::true break::IfNeed inline::(true, true) hd;
            let second = makeList postSpace::true break::IfNeed inline::(true, true) hd2;
            label
              space::true
              nameParamsEquals
              (label space::true (makeList postSpace::true [first, atom "="]) second)
          }
        };
        let everything =
          switch constraints {
          | [] => everythingButConstraints
          | [hd, ...tl] =>
            makeList
              break::IfNeed
              postSpace::true
              indent::0
              inline::(true, true)
              [everythingButConstraints, hd, ...tl]
          };
        SourceMap ptype_loc everything [@implicit_arity]
      };
      /* shared by [Pstr_type,Psig_type]*/
      method type_def_list l => {
        /* As oposed to used in type substitution. */
        let formatOneTypeDefStandard prepend td => {
          let itm =
            self#formatOneTypeDef
              prepend
              (SourceMap td.ptype_name.loc (atom td.ptype_name.txt) [@implicit_arity])
              (atom "=")
              td;
          self#attach_std_item_attrs td.ptype_attributes itm
        };
        switch l {
        | [] => raise (NotPossible "asking for type list of nothing")
        | [hd, ...tl] =>
          let first =
            switch (partitionNonrecAttr hd.ptype_attributes) {
            | ([], _) => formatOneTypeDefStandard (atom "type") hd
            | (_, attrs) =>
              let newHd = {...hd, ptype_attributes: attrs};
              formatOneTypeDefStandard (atom "type nonrec") newHd
            };
          switch tl {
          /* Exactly one type */
          | [] => first
          | [tlhd, ...tltl] =>
            makeList
              indent::0
              inline::(true, true)
              break::Always_rec
              [first, ...List.map (formatOneTypeDefStandard (atom "and")) [tlhd, ...tltl]]
          }
        }
      };
      method type_variant_leaf opt_ampersand::a=false polymorphic::p=false =>
        self#type_variant_leaf1 a p true;
      method type_variant_leaf_nobar opt_ampersand::a=false polymorphic::p=false =>
        self#type_variant_leaf1 a p false;
      /* TODOATTRIBUTES: Attributes on the entire variant leaf are likely
       * not parsed or printed correctly. */
      method type_variant_leaf1 opt_ampersand polymorphic print_bar x => {
        let {pcd_name, pcd_args, pcd_res, pcd_loc, pcd_attributes} = x;
        let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes pcd_attributes;
        let prefix = if polymorphic {"`"} else {""};
        let sourceMappedName =
          SourceMap pcd_name.loc (atom (prefix ^ pcd_name.txt)) [@implicit_arity];
        let nameOf = makeList postSpace::true [sourceMappedName];
        let barName = {
          let lst =
            if print_bar {
              [atom "|", sourceMappedName]
            } else {
              [sourceMappedName]
            };
          makeList postSpace::true lst
        };
        let ampersand_helper i arg => {
          let ct = self#non_arrowed_simple_core_type arg;
          let add_ampersand = label (atom "&");
          if polymorphic {
            if (i === 0 && not opt_ampersand) {
              ct
            } else {
              add_ampersand ct
            }
          } else {
            ct
          }
        };
        let args = List.mapi ampersand_helper pcd_args;
        let gadtRes =
          switch pcd_res {
          | None => None
          | Some x =>
            Some (
              makeList
                inline::(true, true)
                break::IfNeed
                [
                  /* Single row just so the entire return type breaks onto its own line */
                  formatJustTheTypeConstraint (self#core_type x)
                ]
            )
          };
        let normalize lst =>
          switch lst {
          | [] => raise (NotPossible "should not be called")
          | [hd] => hd
          | [_, ..._] => makeList inline::(true, true) break::IfNeed postSpace::true lst
          };
        let add_bar name args => {
          let lbl = label space::true name args;
          makeList
            postSpace::true
            (
              if print_bar {
                [atom "|", lbl]
              } else {
                [lbl]
              }
            )
        };
        let everything =
          switch (args, gadtRes) {
          | ([], None) => barName
          | ([], Some res) => add_bar sourceMappedName res
          | ([_, ..._], None) => add_bar nameOf (normalize args)
          | ([_, ..._], Some res) => add_bar nameOf (normalize (args @ [res]))
          };
        let everythingWithAttrs =
          if (stdAttrs != []) {
            formatAttributed everything (self#attributes stdAttrs)
          } else {
            everything
          };
        SourceMap pcd_loc everythingWithAttrs [@implicit_arity]
      };
      /* Returns the type declaration partitioned into three segments - one
         suitable for appending to a label, the actual type manifest
         and the list of constraints. */
      method type_declaration_binding_segments x => {
        /* Segments of the type binding (occuring after the type keyword) that
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
           */
        let privateAtom = atom "private";
        let privatize scope lst =>
          switch scope {
          | Public => lst
          | Private => [privateAtom, ...lst]
          };
        let recordRow pld => {
          let nameColon =
            SourceMap pld.pld_name.loc (makeList [atom pld.pld_name.txt, atom ":"])
            [@implicit_arity];
          let withMutable =
            switch pld.pld_mutable {
            | Immutable => nameColon
            | Mutable => makeList postSpace::true [atom "mutable", nameColon]
            };
          SourceMap pld.pld_loc (label space::true withMutable (self#core_type pld.pld_type))
          [@implicit_arity]
        };
        let recordize assumeRecordLoc::assumeRecordLoc=? lst => {
          let rows = List.map recordRow lst;
          let rowList = makeList wrap::("{", "}") sep::"," postSpace::true break::IfNeed rows;
          switch assumeRecordLoc {
          | None => rowList
          | Some loc => SourceMap loc rowList [@implicit_arity]
          }
        };
        let estimateRecordOpenBracePoint () =>
          switch x.ptype_params {
          | [] => x.ptype_name.loc.loc_end
          | [hd, ...tl] =>
            (fst (List.nth x.ptype_params (List.length x.ptype_params - 1))).ptyp_loc.loc_end
          };
        let equalInitiatedSegments =
          switch (x.ptype_kind, x.ptype_private, x.ptype_manifest) {
          /* /*empty*/ {(Ptype_abstract, Public, None)} */
          | (Ptype_abstract, Public, None) => []
          /* EQUAL core_type {(Ptype_abstract, Public, Some _)} */
          | (Ptype_abstract, Public, Some y) => [[self#core_type y]]
          /* EQUAL PRIVATE core_type {(Ptype_abstract, Private, Some $3)} */
          | (Ptype_abstract, Private, Some y) => [[privateAtom, self#core_type y]]
          /* EQUAL constructor_declarations {(Ptype_variant _., Public, None)} */
          /* This case is redundant */
          /* | (Ptype_variant lst, Public, None) -> [ */
          /*     [makeSpacedBreakableInlineList (List.map type_variant_leaf lst)] */
          /*   ] */
          /* EQUAL PRIVATE constructor_declarations {(Ptype_variant _, Private, None)} */
          | (Ptype_variant lst, Private, None) => [
              [
                privateAtom,
                makeList
                  break::IfNeed
                  postSpace::true
                  inline::(true, true)
                  (List.map self#type_variant_leaf lst)
              ]
            ]
          /* EQUAL private_flag BAR constructor_declarations {(Ptype_variant _, $2, None)} */
          | (Ptype_variant lst, scope, None) => [
              privatize
                scope
                [
                  makeList
                    break::Always_rec
                    postSpace::true
                    inline::(true, true)
                    (List.map self#type_variant_leaf lst)
                ]
            ]
          /* EQUAL DOTDOT {(Ptype_open, Public, None)} */
          | (Ptype_open, Public, None) => [[atom ".."]]
          /* Super confusing how record/variants' manifest is not actually the
             description of the structure. What's in the manifest in that case is
             the *second* EQUALS asignment. */
          /* EQUAL private_flag LBRACE label_declarations opt_comma RBRACE {(Ptype_record _, $2, None)} */
          | (Ptype_record lst, scope, None) =>
            let assumeRecordLoc = {
              loc_start: estimateRecordOpenBracePoint (),
              loc_end: x.ptype_loc.loc_end,
              loc_ghost: false
            };
            [privatize scope [recordize assumeRecordLoc::assumeRecordLoc lst]]
          /* And now all of the forms involving *TWO* equals */
          /* Again, super confusing how manifests of variants/records represent the
             structure after the second equals. */
          /* ================================================*/
          /* EQUAL core_type EQUAL private_flag opt_bar constructor_declarations {
             (Ptype_variant _, _, Some _)} */
          | (Ptype_variant lst, scope, Some mani) => [
              [self#core_type mani],
              {
                let variant =
                  makeList
                    break::IfNeed
                    postSpace::true
                    inline::(true, true)
                    (List.map self#type_variant_leaf lst);
                privatize scope [variant]
              }
            ]
          /* EQUAL core_type EQUAL DOTDOT {(Ptype_open, Public, Some $2)} */
          | (Ptype_open, Public, Some mani) => [[self#core_type mani], [atom ".."]]
          /* EQUAL core_type EQUAL private_flag LBRACE label_declarations opt_comma RBRACE
             {(Ptype_record _, $4, Some $2)} */
          | (Ptype_record lst, scope, Some mani) => [
              [self#core_type mani],
              privatize scope [recordize lst]
            ]
          /* Everything else is impossible */
          /* ================================================*/
          | (_, _, _) => raise (NotPossible "Encountered impossible type specification")
          };
        let makeConstraint (ct1, ct2, _) => {
          let constraintEq =
            makeList postSpace::true [atom "constraint", self#core_type ct1, atom "="];
          label space::true constraintEq (self#core_type ct2)
        };
        let constraints = List.map makeConstraint x.ptype_cstrs;
        (equalInitiatedSegments, constraints)
      };
      method non_arrowed_non_simple_core_type x => {
        let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.ptyp_attributes;
        if (stdAttrs != []) {
          formatAttributed
            (self#non_arrowed_simple_core_type {...x, ptyp_attributes: []})
            (self#attributes stdAttrs)
        } else {
          switch x.ptyp_desc {
          /* This significantly differs from the standard OCaml printer/parser:
             Type constructors are no longer simple */
          | Ptyp_constr li l [@implicit_arity] =>
            /*
                The single identifier has to be wrapped in a [ensureSingleTokenSticksToLabel] to
                avoid (@see @avoidSingleTokenWrapping):
             */
            let constr =
              switch l {
              /* [ensureSingleTokenSticksToLabel] loses location information which is important
                   when you are embedded inside a list and comments are to be interleaved around you.
                   Therefore, we wrap the result in the correct [SourceMap].
                 */
              | [] =>
                SourceMap li.loc (ensureSingleTokenSticksToLabel (self#longident_loc li))
                [@implicit_arity]
              | [hd, ...tl] =>
                let sourceMappedIdent = SourceMap li.loc (self#longident_loc li) [@implicit_arity];
                let typeList =
                  switch tl {
                  | [] => self#non_arrowed_simple_core_type hd
                  | _ =>
                    let simpleTypeList = List.map self#non_arrowed_simple_core_type [hd, ...tl];
                    makeList inline::(true, true) postSpace::true break::IfNeed simpleTypeList
                  };
                label space::true sourceMappedIdent typeList
              };
            /* It's actually better without this source mapped */
            constr
          | _ => self#non_arrowed_simple_core_type x
          }
        }
      };
      method non_arrowed_simple_core_type x => {
        let (arityAttrs, docAttrs, stdAttrs, jsxAttrs) = partitionAttributes x.ptyp_attributes;
        if (stdAttrs != []) {
          formatSimpleAttributed
            (self#non_arrowed_simple_core_type {...x, ptyp_attributes: []})
            (self#attributes stdAttrs)
        } else {
          let result =
            switch x.ptyp_desc {
            /*   LPAREN core_type_comma_list RPAREN %prec below_NEWDOT */
            /*       { match $2 with */
            /*         | [] -> raise Parse_error */
            /*         | one::[] -> one */
            /*         | moreThanOne -> mktyp(Ptyp_tuple(List.rev moreThanOne)) } */
            | Ptyp_tuple l =>
              makeList
                wrap::("(", ")") sep::"," postSpace::true break::IfNeed (List.map self#core_type l)
            | Ptyp_object l o [@implicit_arity] =>
              let core_field_type (s, attrs, ct) => {
                let l = extractStdAttrs attrs;
                switch l {
                | [] =>
                  label space::true (label space::true (atom s) (atom ":")) (self#core_type ct)
                | [_, ..._] =>
                  makeList
                    postSpace::true
                    break::IfNeed
                    [atom s, self#attributes attrs, atom ":", self#core_type ct]
                }
              };
              let openness =
                switch o {
                | Closed => []
                | Open => [atom ".."]
                };
              let rows = List.concat [List.map core_field_type l, openness];
              if (List.length rows == 0) {
                atom "<>"
              } else {
                makeList break::IfNeed postSpace::true wrap::("< ", " >") sep::"," rows
              }
            | Ptyp_package lid cstrs [@implicit_arity] =>
              let typeConstraint (s, ct) =>
                label
                  (
                    makeList
                      break::IfNeed postSpace::true [atom "type", self#longident_loc s, atom "="]
                  )
                  (self#core_type ct);
              switch cstrs {
              | [] =>
                makeList
                  wrap::("(", ")")
                  [makeList postSpace::true [atom "module", self#longident_loc lid]]
              | _ =>
                makeList
                  wrap::("(", ")")
                  [
                    label
                      space::true
                      (makeList postSpace::true [atom "module", self#longident_loc lid])
                      (
                        makeList
                          break::IfNeed
                          sep::" and"
                          wrap::("with", "")
                          pad::(true, false)
                          (List.map typeConstraint cstrs)
                      )
                  ]
              }
            /*   | QUOTE ident */
            /*       { mktyp(Ptyp_var $2) } */
            | Ptyp_var s => ensureSingleTokenSticksToLabel (self#tyvar s)
            /*   | UNDERSCORE */
            /*       { mktyp(Ptyp_any) } */
            | Ptyp_any => ensureSingleTokenSticksToLabel (atom "_")
            /*   | type_longident */
            /*       { mktyp(Ptyp_constr(mkrhs $1 1, [])) } */
            | Ptyp_constr li [] [@implicit_arity] =>
              /* Only simple if zero type paramaters */
              ensureSingleTokenSticksToLabel (self#longident_loc li)
            | Ptyp_variant l closed low [@implicit_arity] =>
              let pcd_loc = x.ptyp_loc;
              let pcd_attributes = x.ptyp_attributes;
              let pcd_res = None;
              let variant_helper rf =>
                switch rf {
                | Rtag label _ opt_ampersand pcd_args [@implicit_arity] =>
                  let pcd_name = {txt: label, loc: pcd_loc};
                  self#type_variant_leaf
                    opt_ampersand::opt_ampersand
                    polymorphic::true
                    {pcd_name, pcd_args, pcd_res, pcd_loc, pcd_attributes}
                | Rinherit ct => self#core_type ct
                };
              let (designator, tl) =
                switch (closed, low) {
                | (Closed, None) => ("", [])
                | (Closed, Some tl) => ("<", tl)
                | (Open, _) => (">", [])
                };
              let node_list = List.map variant_helper l;
              let ll = List.map (fun t => atom ("`" ^ t)) tl;
              let tag_list = makeList postSpace::true break::IfNeed [atom ">", ...ll];
              let type_list =
                if (List.length tl !== 0) {
                  node_list @ [tag_list]
                } else {
                  node_list
                };
              makeList
                wrap::("[" ^ designator, "]")
                pad::(true, false)
                postSpace::true
                break::IfNeed
                type_list
            | Ptyp_class li l [@implicit_arity] =>
              switch l {
              | [] => makeList [atom "#", self#longident_loc li]
              | [_, ..._] =>
                label
                  space::true
                  (makeList [atom "#", self#longident_loc li])
                  (makeList postSpace::true inline::(true, false) (List.map self#core_type l))
              }
            | Ptyp_extension e => self#extension e
            | Ptyp_constr _ [_, ..._] [@implicit_arity]
            | Ptyp_arrow _ _ _ [@implicit_arity]
            | Ptyp_alias _ _ [@implicit_arity]
            | Ptyp_poly _ _ [@implicit_arity] =>
              makeList wrap::("(", ")") break::IfNeed [self#core_type x]
            };
          SourceMap x.ptyp_loc result [@implicit_arity]
        }
      };
      /* TODO: ensure that we have a form of desugaring that protects */
      /* when final argument of curried pattern is a type constraint: */
      /* | COLON non_arrowed_core_type EQUALGREATER expr
         { mkexp_constraint $4 (Some $2, None) }         */
      /*                         \----/   \--/
                                                             constraint coerce

                                                             Creates a ghost expression:
                                                             mkexp_constraint | Some t, None -> ghexp(Pexp_constraint(e, t))
                                  */
      method pattern_list_split_cons acc =>
        fun
        | {
            ppat_desc:
              Ppat_construct
                {txt: Lident "::", loc: consLoc} (Some {ppat_desc: Ppat_tuple [pat1, pat2]})
              [@implicit_arity]
          } =>
          self#pattern_list_split_cons [pat1, ...acc] pat2
        | p => (List.rev acc, p);
      /*
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
       */
      method or_pattern p1 p2 => {
        let (p1_raw, p2_raw) = (self#pattern p1, self#pattern p2);
        let (left, right) =
          switch p2.ppat_desc {
          | Ppat_or _ => (p1_raw, formatPrecedence p2_raw)
          | _ => (p1_raw, p2_raw)
          };
        makeList
          break::IfNeed inline::(true, true) sep::"|" postSpace::true preSpace::true [left, right]
      };
      method pattern_without_or x => {
        let patternSourceMap pt layout => SourceMap pt.ppat_loc layout [@implicit_arity];
        /* TODOATTRIBUTES: Handle the stdAttrs here */
        let (arityAttrs, docAtrs, _, jsxAttrs) = partitionAttributes x.ppat_attributes;
        switch x.ppat_desc {
        | Ppat_alias p s [@implicit_arity] =>
          let raw_pattern = self#pattern p;
          let pattern_with_precedence =
            switch p.ppat_desc {
            | Ppat_or p1 p2 [@implicit_arity] => formatPrecedence (self#or_pattern p1 p2)
            | _ => raw_pattern
            };
          label
            space::true
            (patternSourceMap p pattern_with_precedence)
            (
              makeList
                postSpace::true
                [atom "as", SourceMap s.loc (protectIdentifier s.txt) [@implicit_arity]] /* RA*/
            )
        | Ppat_variant l (Some p) [@implicit_arity] =>
          if (arityAttrs !== []) {
            raise (NotPossible "Should never see embedded attributes on poly variant")
          } else {
            let layout =
              self#constructor_pattern polyVariant::true arityIsClear::true (atom ("`" ^ l)) p;
            SourceMap x.ppat_loc layout [@implicit_arity]
          }
        | Ppat_lazy p => label space::true (atom "lazy") (self#simple_pattern p)
        | Ppat_construct ({txt} as li) po [@implicit_arity] when not (txt == Lident "::") =>
          /* FIXME The third field always false */
          let liSourceMapped = SourceMap li.loc (self#longident_loc li) [@implicit_arity];
          let formattedConstruction =
            switch po {
            /* TODO: Check the explicit_arity field on the pattern/constructor
               attributes to determine if should desugar to an *actual* tuple. */
            /* | Some ({ */
            /*   ppat_desc=Ppat_tuple l; */
            /*   ppat_attributes=[{txt="explicit_arity"; loc}] */
            /* }) -> */
            /*   label ~space:true (self#longident_loc li) (makeSpacedBreakableInlineList (List.map self#simple_pattern l)) */
            | Some xx =>
              let arityIsClear = isArityClear arityAttrs;
              self#constructor_pattern arityIsClear::arityIsClear liSourceMapped xx
            | None => liSourceMapped
            };
          SourceMap x.ppat_loc formattedConstruction [@implicit_arity]
        | _ => self#simple_pattern x
        }
      };
      method pattern x => {
        let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.ppat_attributes;
        if (stdAttrs != []) {
          formatAttributed
            /* Doesn't need to be simple_pattern because attributes are parse as
             * appyling to the entire "function application style" syntax preceeding them */
            (self#pattern {...x, ppat_attributes: arityAttrs}) (self#attributes stdAttrs)
        } else {
          switch x.ppat_desc {
          | Ppat_or p1 p2 [@implicit_arity] => self#or_pattern p1 p2
          | _ => self#pattern_without_or x
          }
        }
      };
      method pattern_list_helper pat => {
        let (pat_list, pat_last) = self#pattern_list_split_cons [] pat;
        switch pat_last {
        | {ppat_desc: Ppat_construct {txt: Lident "[]"} _ [@implicit_arity]} =>
          /* [x,y,z] */
          makeList
            break::IfNeed
            wrap::("[", "]")
            sep::","
            postSpace::true
            (List.map self#pattern pat_list)
        | _ =>
          /* x::y */
          makeES6List (List.map self#pattern pat_list) (self#pattern pat_last)
        }
      };
      method potentiallyConstrainedPattern1 x =>
        switch x.ppat_desc {
        | Ppat_constraint p ct [@implicit_arity] =>
          formatTypeConstraint (self#pattern p) (self#core_type ct)
        | _ => self#pattern x
        };
      method simple_pattern x => {
        let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.ppat_attributes;
        if (stdAttrs != []) {
          formatSimpleAttributed
            (self#simple_pattern {...x, ppat_attributes: arityAttrs}) (self#attributes stdAttrs)
        } else {
          let itm =
            switch x.ppat_desc {
            | Ppat_construct {loc, txt: Lident (("()" | "[]") as x)} _ [@implicit_arity] =>
              /* Patterns' locations might include a leading bar depending on the
               * context it was parsed in. Therefore, we need to include further
               * information about the contents of the pattern such as tokens etc,
               * in order to get comments to be distributed correctly.*/
              SourceMap loc (atom x) [@implicit_arity]
            | Ppat_construct {txt: Lident "::"} po [@implicit_arity] =>
              self#pattern_list_helper x /* LIST PATTERN */
            | Ppat_construct ({txt} as li) None [@implicit_arity] =>
              let liSourceMapped = SourceMap li.loc (self#longident_loc li) [@implicit_arity];
              SourceMap x.ppat_loc liSourceMapped [@implicit_arity]
            | Ppat_any => atom "_"
            | Ppat_var {loc, txt} =>
              /*
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

               */
              SourceMap loc (protectIdentifier txt) [@implicit_arity]
            | Ppat_array l =>
              makeList
                wrap::("[|", "|]") break::IfNeed postSpace::true sep::"," (List.map self#pattern l)
            | Ppat_unpack s =>
              makeList wrap::("(", ")") break::IfNeed postSpace::true [atom "module", atom s.txt]
            | Ppat_type li => makeList [atom "#", self#longident_loc li]
            | Ppat_record l closed [@implicit_arity] =>
              let longident_x_pattern (li, p) =>
                switch (li, p.ppat_desc) {
                | ({txt: Lident s}, Ppat_var {txt}) when s == txt => self#longident_loc li
                | _ =>
                  label space::true (makeList [self#longident_loc li, atom ":"]) (self#pattern p)
                };
              let rows =
                List.map longident_x_pattern l @ (
                  switch closed {
                  | Closed => []
                  | _ => [atom "_"]
                  }
                );
              makeList wrap::("{", "}") break::IfNeed sep::"," postSpace::true rows
            | Ppat_tuple l =>
              makeList
                wrap::("(", ")")
                sep::","
                postSpace::true
                break::IfNeed
                (List.map self#potentiallyConstrainedPattern1 l)
            | Ppat_constant c => self#constant c
            | Ppat_interval c1 c2 [@implicit_arity] =>
              makeList [self#constant c1, atom "..", self#constant c2]
            | Ppat_variant l None [@implicit_arity] => makeList [atom "`", atom l]
            | Ppat_constraint p ct [@implicit_arity] =>
              formatPrecedence (formatTypeConstraint (self#pattern p) (self#core_type ct))
            | Ppat_lazy p =>
              formatPrecedence (label space::true (atom "lazy") (self#simple_pattern p))
            | Ppat_extension e => self#extension e
            | Ppat_exception p => makeList postSpace::true [atom "exception", self#pattern p]
            | _ => formatPrecedence (self#pattern x) /* May have a redundant sourcemap */
            };
          SourceMap x.ppat_loc itm [@implicit_arity]
        }
      };
      method label_exp (l, opt, p) =>
        if (l == "") {
          self#simple_pattern p /*single case pattern parens needed here */
        } else if (
          l.[0] == '?'
        ) {
          let len = String.length l - 1;
          let lbl = String.sub l 1 len;
          formatLabeledArgument
            (atom lbl)
            ""
            (
              label
                (makeList [self#simple_pattern p, atom "="])
                (
                  switch opt {
                  | None => atom "?"
                  | Some o => self#simplifyUnparseExpr o
                  }
                )
            )
        } else {
          switch p.ppat_desc {
          | _ => formatLabeledArgument (atom l) "" (self#simple_pattern p)
          }
        };
      method access op cls e1 e2 =>
        makeList
          interleaveComments::false
          [
            /* Important that this be not breaking - at least to preserve same
               behavior as stock desugarer. It might even be required (double check
               in parser.mly) */
            e1,
            atom ".",
            atom op,
            e2,
            atom cls
          ];
      method simple_get_application x => {
        let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.pexp_attributes;
        switch (x.pexp_desc, stdAttrs, jsxAttrs) {
        | (_, [attrHd, ...attrTl], []) => None /* Has some printed attributes - not simple */
        | (Pexp_apply {pexp_desc: Pexp_ident loc} l [@implicit_arity], [], [jsx, ..._]) =>
          /* TODO: Soon, we will allow the final argument to be an identifier which
                represents the entire list. This would be written as
                `<tag>...list</tag>`. If you imagine there being an implicit [] inside
                the tag, then it would be consistent with array spread:
                [...list] evaluates to the thing as list.
             */
          let rec isLabeledArgsAndFinalList arguments =>
            switch arguments {
            | [("", {pexp_desc: Pexp_construct {txt: Lident "::"} _ [@implicit_arity]})]
            | [("", {pexp_desc: Pexp_construct {txt: Lident "[]"} _ [@implicit_arity]})] => true
            /* Any other kind of non-named argument besides the above disqualifies */
            | [("", _), ..._] => false
            | [(lbl, _), ...tail] => isLabeledArgsAndFinalList tail
            | [] => false
            };
          let moduleNameList = List.rev (List.tl (List.rev (Longident.flatten loc.txt)));
          if (List.length moduleNameList > 0) {
            if (Longident.last loc.txt == "createElement" && isLabeledArgsAndFinalList l) {
              Some (self#formatJSXComponent (String.concat "." moduleNameList) l)
            } else {
              None
            }
          } else if (
            isLabeledArgsAndFinalList l
          ) {
            Some (self#formatJSXComponent (Longident.last loc.txt) l)
          } else {
            None
          }
        | (Pexp_apply eFun ls [@implicit_arity], [], []) =>
          switch (printedStringAndFixityExpr eFun, ls) {
          /* We must take care not to print two subsequent prefix operators without
             spaces between them (`! !` could become `!!` which is totally
             different).  */
          | (AlmostSimplePrefix prefixStr, [("", rightExpr)]) =>
            let forceSpace =
              switch rightExpr.pexp_desc {
              | Pexp_apply ee lsls [@implicit_arity] =>
                switch (printedStringAndFixityExpr ee) {
                | AlmostSimplePrefix _ => true
                | _ => false
                }
              | _ => false
              };
            let rightItm = self#simplifyUnparseExpr rightExpr;
            Some (label space::forceSpace (atom prefixStr) rightItm)
          | (Infix infixStr, [(_, leftExpr), (_, rightExpr)]) when infixStr.[0] == '#' =>
            /* Little hack. We check the right expression to see if it's also a SHARPOP, if it is
               we call `formatPrecedence` on the result of `simplifyUnparseExpr` to add the appropriate
               parens. This is done because `unparseExpr` doesn't seem to be able to handle
               high enough precedence things. Using the normal precedence handling, something like

                  ret #= (Some 10)

                gets pretty printed to

                  ret #= Some 10

                Which seems to indicate that the pretty printer doesn't think `#=` is of
                high enough precedence for the parens to be worth adding back. */
            let rightItm =
              switch rightExpr.pexp_desc {
              | Pexp_apply eFun ls [@implicit_arity] =>
                switch (printedStringAndFixityExpr eFun, ls) {
                | (Infix infixStr, [(_, _), (_, _)]) when infixStr.[0] == '#' =>
                  formatPrecedence (self#simplifyUnparseExpr rightExpr)
                | _ => self#simplifyUnparseExpr rightExpr
                }
              | _ => self#simplifyUnparseExpr rightExpr
              };
            Some (
              makeList [self#simple_enough_to_be_lhs_dot_send leftExpr, atom infixStr, rightItm]
            )
          | (_, _) =>
            switch (eFun, ls) {
            | (
                {pexp_desc: Pexp_ident {txt: Ldot (Lident "String") "get" [@implicit_arity]}},
                [(_, e1), (_, e2)]
              ) =>
              Some (self#access "[" "]" (self#simplifyUnparseExpr e1) (self#unparseExpr e2))
            | (
                {pexp_desc: Pexp_ident {txt: Ldot (Lident "Array") "get" [@implicit_arity]}},
                [(_, e1), (_, e2)]
              ) =>
              Some (self#access "(" ")" (self#simplifyUnparseExpr e1) (self#unparseExpr e2))
            | (
                {
                  pexp_desc:
                    Pexp_ident {
                      txt:
                        Ldot (Ldot (Lident "Bigarray") "Genarray" [@implicit_arity]) "get"
                        [@implicit_arity]
                    }
                },
                [(_, a), (_, {pexp_desc: Pexp_array ls})]
              ) =>
              let formattedList = List.map self#simplifyUnparseExpr ls;
              Some (
                self#access
                  "{" "}" (self#simplifyUnparseExpr a) (makeCommaBreakableList formattedList)
              )
            | (
                {
                  pexp_desc:
                    Pexp_ident {
                      txt:
                        Ldot
                          (
                            Ldot (Lident "Bigarray") ("Array1" | "Array2" | "Array3")
                            [@implicit_arity]
                          )
                          "get"
                        [@implicit_arity]
                    }
                },
                [(_, a), ...rest]
              ) =>
              let formattedList = List.map self#simplifyUnparseExpr (List.map snd rest);
              Some (
                self#access
                  "{" "}" (self#simplifyUnparseExpr a) (makeCommaBreakableList formattedList)
              )
            | _ => None
            }
          }
        | _ => None
        }
      };
      /** Detects "sugar expressions" (sugar for array/string setters) and returns their separate
          parts.  */
      method sugar_set_expr_parts e =>
        if (e.pexp_attributes != []) {
          None
        } else {
          /* should also check attributes underneath */
          switch e.pexp_desc {
          | Pexp_apply
              {pexp_desc: Pexp_ident {txt: Ldot (Lident "Array") "set" [@implicit_arity]}}
              [(_, e1), (_, e2), (_, e3)]
            [@implicit_arity] =>
            Some (self#access "(" ")" (self#simplifyUnparseExpr e1) (self#unparseExpr e2), e3)
          | Pexp_apply
              {pexp_desc: Pexp_ident {txt: Ldot (Lident "String") "set" [@implicit_arity]}}
              [(_, e1), (_, e2), (_, e3)]
            [@implicit_arity] =>
            Some (self#access "[" "]" (self#simplifyUnparseExpr e1) (self#unparseExpr e2), e3)
          | Pexp_apply
              {
                pexp_desc:
                  Pexp_ident {
                    txt:
                      Ldot (Ldot (Lident "Bigarray") array [@implicit_arity]) "set"
                      [@implicit_arity]
                  }
              }
              label_exprs
            [@implicit_arity] =>
            switch array {
            | "Genarray" =>
              switch label_exprs {
              | [(_, a), (_, {pexp_desc: Pexp_array ls}), (_, c)] =>
                let formattedList = List.map self#simplifyUnparseExpr ls;
                Some (
                  self#access
                    "{" "}" (self#simplifyUnparseExpr a) (makeCommaBreakableList formattedList),
                  c
                )
              | _ => None
              }
            | "Array1"
            | "Array2"
            | "Array3" =>
              switch label_exprs {
              | [(_, a), ...rest] =>
                switch (List.rev rest) {
                | [(_, v), ...rest] =>
                  let args = List.map snd (List.rev rest);
                  let formattedList = List.map self#simplifyUnparseExpr args;
                  Some (
                    self#access
                      "{" "}" (self#simplifyUnparseExpr a) (makeCommaBreakableList formattedList),
                    v
                  )
                | _ => assert false
                }
              | _ => assert false
              }
            | _ => None
            }
          | _ => None
          }
        };
      /*

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
       */
      /**
         TODO: Configure the optional ability to print the *minimum* number of
         parens. It's simply a matter of changing [higherPrecedenceThan] to
         [higherOrEqualPrecedenceThan].
       */
      /* Ensures a rule doesn't reduce until *after* `reducesAfterRight` gets a chance
         to reduce. Example: The addtion rule which has precedence of rightmost
         token "+", in `x + a * b` should not reduce until after the a * b gets
         a chance to reduce. This function would determine the minimum parens to
         ensure that. */
      method ensureContainingRule
             withPrecedence::withPrecedence
             reducesAfterRight::reducesAfterRight =>
        switch (self#unparseExprRecurse reducesAfterRight) {
        | SpecificInfixPrecedence {reducePrecedence, shiftPrecedence} rightRecurse
          [@implicit_arity] =>
          if (higherPrecedenceThan shiftPrecedence withPrecedence) {
            rightRecurse
          } else if (
            higherPrecedenceThan withPrecedence shiftPrecedence
          ) {
            formatPrecedence loc::reducesAfterRight.pexp_loc rightRecurse
          } else if (
            isRightAssociative withPrecedence
          ) {
            rightRecurse
          } else {
            formatPrecedence loc::reducesAfterRight.pexp_loc rightRecurse
          }
        | FunctionApplication itms =>
          formatAttachmentApplication
            applicationFinalWrapping None (itms, Some reducesAfterRight.pexp_loc)
        | PotentiallyLowPrecedence itm => formatPrecedence loc::reducesAfterRight.pexp_loc itm
        | Simple itm => itm
        };
      method ensureExpression expr reducesOnToken::reducesOnToken =>
        switch (self#unparseExprRecurse expr) {
        | SpecificInfixPrecedence {reducePrecedence, shiftPrecedence} leftRecurse [@implicit_arity] =>
          if (higherPrecedenceThan reducePrecedence reducesOnToken) {
            leftRecurse
          } else if (
            higherPrecedenceThan reducesOnToken reducePrecedence
          ) {
            formatPrecedence loc::expr.pexp_loc leftRecurse
          } else if (
            isLeftAssociative reducesOnToken
          ) {
            leftRecurse
          } else {
            formatPrecedence loc::expr.pexp_loc leftRecurse
          }
        | FunctionApplication itms =>
          formatAttachmentApplication applicationFinalWrapping None (itms, Some expr.pexp_loc)
        | PotentiallyLowPrecedence itm => formatPrecedence loc::expr.pexp_loc itm
        | Simple itm => itm
        };
      /** Attempts to unparse: The beginning of a more general printing algorithm,
              that determines how to print based on precedence of tokens and rules.
              The end goal is that this should be completely auto-generated from the
              Menhir parsing tables. We could move more and more into this function.

              You could always just call self#expression, but `unparseExpr` will render
              infix/prefix/unary/terary fixities in their beautiful forms while
              minimizing parenthesis.
          */
      method unparseExpr x =>
        switch (self#unparseExprRecurse x) {
        | SpecificInfixPrecedence {reducePrecedence, shiftPrecedence} itm [@implicit_arity] => itm
        | FunctionApplication itms =>
          formatAttachmentApplication applicationFinalWrapping None (itms, Some x.pexp_loc)
        | PotentiallyLowPrecedence itm => itm
        | Simple itm => itm
        };
      method simplifyUnparseExpr x =>
        switch (self#unparseExprRecurse x) {
        | SpecificInfixPrecedence {reducePrecedence, shiftPrecedence} itm [@implicit_arity] =>
          formatPrecedence loc::x.pexp_loc itm
        | FunctionApplication itms =>
          formatPrecedence
            loc::x.pexp_loc
            (formatAttachmentApplication applicationFinalWrapping None (itms, Some x.pexp_loc))
        | PotentiallyLowPrecedence itm => formatPrecedence loc::x.pexp_loc itm
        | Simple itm => itm
        };
      method unparseExprApplicationItems x =>
        switch (self#unparseExprRecurse x) {
        | SpecificInfixPrecedence {reducePrecedence, shiftPrecedence} itm [@implicit_arity] => (
            [itm],
            Some x.pexp_loc
          )
        | FunctionApplication itms => (itms, Some x.pexp_loc)
        | PotentiallyLowPrecedence itm => ([itm], Some x.pexp_loc)
        | Simple itm => ([itm], Some x.pexp_loc)
        };
      method unparseExprRecurse x => {
        /* If there are any attributes, render unary like `(~-) x [@ppx]`, and infix like `(+) x y [@attr]` */
        let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.pexp_attributes;
        /* If there's any attributes, recurse without them, then apply them to
           the ends of functions, or simplify infix printings then append. */
        if (stdAttrs != []) {
          let withoutVisibleAttrs = {...x, pexp_attributes: arityAttrs @ jsxAttrs};
          let attributesAsList = List.map self#attribute stdAttrs;
          let itms =
            switch (self#unparseExprRecurse withoutVisibleAttrs) {
            | SpecificInfixPrecedence {reducePrecedence, shiftPrecedence} itm [@implicit_arity] => [
                formatPrecedence loc::x.pexp_loc itm
              ]
            | FunctionApplication itms => itms
            | PotentiallyLowPrecedence itm => [formatPrecedence loc::x.pexp_loc itm]
            | Simple itm => [itm]
            };
          FunctionApplication [
            makeList
              break::IfNeed
              inline::(true, true)
              indent::0
              postSpace::true
              (List.concat [itms, attributesAsList])
          ]
        } else {
          switch (self#simplest_expression x) {
          | Some se => Simple se
          | None =>
            switch x.pexp_desc {
            | Pexp_apply e ls [@implicit_arity] =>
              switch (self#sugar_set_expr_parts x) {
              /* Returns None if there's attributes - would render as regular function */
              /* Format as if it were an infix function application with identifier "=" */
              | Some (simplyFormatedLeftItm, rightExpr) =>
                let tokenPrec = Token updateToken;
                let rightItm =
                  self#ensureContainingRule withPrecedence::tokenPrec reducesAfterRight::rightExpr;
                let leftWithOp =
                  makeList postSpace::true [simplyFormatedLeftItm, atom updateToken];
                let expr = label space::true leftWithOp rightItm;
                SpecificInfixPrecedence
                  {reducePrecedence: tokenPrec, shiftPrecedence: tokenPrec} expr
                [@implicit_arity]
              | None =>
                switch (printedStringAndFixityExpr e, ls) {
                | (Infix printedIdent, [("", leftExpr), ("", rightExpr)]) =>
                  let infixToken = Token printedIdent;
                  let rightItm =
                    self#ensureContainingRule
                      withPrecedence::infixToken reducesAfterRight::rightExpr;
                  let leftItm = self#ensureExpression leftExpr reducesOnToken::infixToken;
                  let leftWithOp = makeList postSpace::true [leftItm, atom printedIdent];
                  let indent = infixTokenRequiresIndent printedIdent;
                  let expr = label space::true indent::?indent leftWithOp rightItm;
                  SpecificInfixPrecedence
                    {reducePrecedence: infixToken, shiftPrecedence: infixToken} expr
                  [@implicit_arity]
                /* Will be rendered as `(+) a b c` which is parsed with higher precedence than all
                   the other forms unparsed here.*/
                | (UnaryPlusPrefix printedIdent, [("", rightExpr)]) =>
                  let prec = Custom "prec_unary_plus";
                  let rightItm =
                    self#ensureContainingRule withPrecedence::prec reducesAfterRight::rightExpr;
                  let expr = label space::true (atom printedIdent) rightItm;
                  SpecificInfixPrecedence
                    {reducePrecedence: prec, shiftPrecedence: Token printedIdent} expr
                  [@implicit_arity]
                | (UnaryMinusPrefix printedIdent, [("", rightExpr)]) =>
                  let prec = Custom "prec_unary_minus";
                  let rightItm =
                    self#ensureContainingRule withPrecedence::prec reducesAfterRight::rightExpr;
                  let expr = label space::true (atom printedIdent) rightItm;
                  SpecificInfixPrecedence
                    {reducePrecedence: prec, shiftPrecedence: Token printedIdent} expr
                  [@implicit_arity]
                /* Will need to be rendered in self#expression as (~-) x y z. */
                | (_, _) =>
                  /* This case will happen when there is something like

                         Bar.createElement a::1 b::2 [] [@bla] [@JSX]

                       At this point the bla will be stripped (because it's a visible
                       attribute) but the JSX will still be there.
                     */
                  switch (detectJSXComponent e.pexp_desc x.pexp_attributes ls) {
                  | Some componentName =>
                    FunctionApplication [self#formatJSXComponent componentName ls]
                  | None =>
                    /* If there was a JSX attribute BUT JSX component wasn't detected,
                         that JSX attribute needs to be pretty printed so it doesn't get
                         lost
                       */
                    let maybeJSXAttr =
                      switch jsxAttrs {
                      | [] => []
                      | jsx => List.map self#attribute jsx
                      };
                    let theFunc =
                      SourceMap e.pexp_loc (self#simplifyUnparseExpr e) [@implicit_arity];
                    /*reset here only because [function,match,try,sequence] are lower priority*/
                    let theArgs = List.map self#reset#label_x_expression_param ls;
                    FunctionApplication ([theFunc, ...theArgs] @ maybeJSXAttr)
                  }
                }
              }
            | Pexp_construct li (Some eo) [@implicit_arity]
                when not (is_simple_construct (view_expr x)) =>
              switch (view_expr x) {
              /* TODO: Explicit arity */
              | `normal =>
                let arityIsClear = isArityClear arityAttrs;
                FunctionApplication [
                  self#constructor_expression
                    arityIsClear::arityIsClear stdAttrs (self#longident_loc li) eo
                ]
              | _ => assert false
              }
            | Pexp_variant l (Some eo) [@implicit_arity] =>
              if (arityAttrs !== []) {
                raise (NotPossible "Should never see embedded attributes on poly variant")
              } else {
                FunctionApplication [
                  self#constructor_expression
                    polyVariant::true arityIsClear::true stdAttrs (atom ("`" ^ l)) eo
                ]
              }
            /* TODO: Should protect this identifier */
            | Pexp_setinstvar s rightExpr [@implicit_arity] =>
              let rightItm =
                self#ensureContainingRule
                  withPrecedence::(Token updateToken) reducesAfterRight::rightExpr;
              let expr =
                label
                  space::true
                  (makeList postSpace::true [protectIdentifier s.txt, atom updateToken])
                  rightItm;
              SpecificInfixPrecedence
                {reducePrecedence: Token updateToken, shiftPrecedence: Token updateToken} expr
              [@implicit_arity]
            | Pexp_setfield leftExpr li rightExpr [@implicit_arity] =>
              let rightItm =
                self#ensureContainingRule
                  withPrecedence::(Token updateToken) reducesAfterRight::rightExpr;
              let leftItm =
                label
                  (
                    makeList
                      interleaveComments::false
                      [self#simple_enough_to_be_lhs_dot_send leftExpr, atom "."]
                  )
                  (self#longident_loc li);
              let expr =
                label space::true (makeList postSpace::true [leftItm, atom updateToken]) rightItm;
              SpecificInfixPrecedence
                {reducePrecedence: Token updateToken, shiftPrecedence: Token updateToken} expr
              [@implicit_arity]
            | Pexp_match e l [@implicit_arity] when detectTernary l !== None =>
              switch (detectTernary l) {
              | None => raise (Invalid_argument "Impossible")
              | Some (tt, ff) =>
                let ifTrue = self#unparseExpr tt;
                let testItm = self#ensureExpression e reducesOnToken::(Token "?");
                let ifFalse =
                  self#ensureContainingRule withPrecedence::(Token ":") reducesAfterRight::ff;
                let withQuestion =
                  SourceMap e.pexp_loc (makeList postSpace::true [testItm, atom "?"])
                  [@implicit_arity];
                let trueFalseBranches =
                  makeList
                    inline::(true, true)
                    break::IfNeed
                    sep::":"
                    postSpace::true
                    preSpace::true
                    [ifTrue, ifFalse];
                let expr = label space::true withQuestion trueFalseBranches;
                SpecificInfixPrecedence
                  {reducePrecedence: Token ":", shiftPrecedence: Token "?"} expr
                [@implicit_arity]
              }
            | _ =>
              switch (self#expression_requiring_parens_in_infix x) {
              | Some e => PotentiallyLowPrecedence e
              | None => raise (Invalid_argument "No match for unparsing expression")
              }
            }
          }
        }
      };
      /*
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

       */
      method classExpressionToFormattedApplicationItems x => {
        let itms =
          switch x.pcl_desc {
          | Pcl_apply ce l [@implicit_arity] => [
              self#simple_class_expr ce,
              ...List.map self#label_x_expression_param l
            ]
          | _ => [self#class_expr x]
          };
        (itms, None)
      };
      /**
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

           </tag>           */
      method formatJSXComponent componentName args => {
        let rec processArguments arguments processedAttrs children =>
          switch arguments {
          | [("", {pexp_desc: Pexp_construct _ None [@implicit_arity]}), ...tail] =>
            processArguments tail processedAttrs None
          | [
              (
                "",
                {
                  pexp_desc:
                    Pexp_construct {txt: Lident "::"} (Some {pexp_desc: Pexp_tuple components})
                    [@implicit_arity]
                }
              ),
              ...tail
            ] =>
            processArguments tail processedAttrs (self#formatChildren components [])
          | [(lbl, expression), ...tail] =>
            let nextAttr =
              switch expression.pexp_desc {
              | Pexp_ident ident when Longident.last ident.txt == lbl => atom lbl
              | _ => makeList [atom lbl, atom "=", self#simplifyUnparseExpr expression]
              };
            processArguments tail [nextAttr, ...processedAttrs] children
          | [] => (processedAttrs, children)
          };
        let (reversedAttributes, children) = processArguments args [] None;
        switch children {
        | None =>
          makeList
            break::IfNeed
            wrap::("<" ^ componentName, "/>")
            pad::(true, true)
            inline::(false, false)
            postSpace::true
            (List.rev reversedAttributes)
        | Some renderedChildren =>
          let openTagAndAttrs =
            switch reversedAttributes {
            | [] => atom ("<" ^ componentName ^ ">")
            | [revAttrHd, ...revAttrTl] =>
              let finalAttrList = List.rev [
                makeList break::Never [revAttrHd, atom ">"],
                ...revAttrTl
              ];
              let renderedAttrList =
                makeList
                  inline::(true, true)
                  break::IfNeed
                  pad::(false, false)
                  preSpace::true
                  finalAttrList;
              label space::true (atom ("<" ^ componentName)) renderedAttrList
            };
          label
            openTagAndAttrs
            (
              makeList
                wrap::("", "</" ^ componentName ^ ">")
                inline::(true, false)
                break::IfNeed
                pad::(true, true)
                postSpace::true
                renderedChildren
            )
        }
      };
      /* Creates a list of simple module expressions corresponding to module
         expression or functor application. */
      method moduleExpressionToFormattedApplicationItems x => {
        let rec functorApplicationList xx =>
          switch xx.pmod_desc {
          | Pmod_apply me1 me2 [@implicit_arity] => [
              SourceMap me2.pmod_loc (self#simple_module_expr me2) [@implicit_arity],
              ...functorApplicationList me1
            ]
          | _ => [SourceMap xx.pmod_loc (self#module_expr xx) [@implicit_arity]]
          };
        (List.rev (functorApplicationList x), None)
      };
      /*

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
       */
      method formatSimplePatternBinding labelOpener layoutPattern typeConstraint appTerms => {
        let letPattern = label break::`Never space::true (atom labelOpener) layoutPattern;
        let upUntilEqual =
          switch typeConstraint {
          | None => letPattern
          | Some tc => formatTypeConstraint letPattern tc
          };
        let includingEqual = makeList postSpace::true [upUntilEqual, atom "="];
        formatAttachmentApplication applicationFinalWrapping (Some (true, includingEqual)) appTerms
      };
      /* Only formats a type annotation for a value binding. */
      method formatSimpleSignatureBinding labelOpener bindingPattern typeConstraint => {
        let letPattern = label space::true (atom labelOpener) bindingPattern;
        formatTypeConstraint letPattern typeConstraint
      };
      /*
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

       */
      method wrapCurriedFunctionBinding
             attachTo::attachTo=?
             arrow::arrow="=>"
             prefixText
             bindingLabel
             patternList
             returnedAppTerms => {
        let allPatterns = [bindingLabel, ...patternList];
        let partitioning = curriedFunctionFinalWrapping allPatterns;
        let everythingButReturnVal =
          switch settings.returnStyle {
          /*
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
           */
          | ReturnValOnSameLine =>
            switch partitioning {
            | None =>
              /* We want the binding label to break *with* the arguments. Again,
                 there's no apparent way to add additional indenting for the
                 args with this setting. */
              /**
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

               */
              makeList
                pad::(true, true)
                wrap::(prefixText, arrow)
                indent::(settings.space * settings.indentWrappedPatternArgs)
                postSpace::true
                inline::(true, true)
                break::IfNeed
                allPatterns
            | Some (attachedList, wrappedListy) =>
              /* To get *only* the final argument to "break", while not
                 necessarily breaking the prior arguments, we dock everything
                 but the last item to a created label */
              label
                space::true
                (
                  makeList
                    pad::(true, true)
                    wrap::(prefixText, arrow)
                    indent::(settings.space * settings.indentWrappedPatternArgs)
                    postSpace::true
                    inline::(true, true)
                    break::IfNeed
                    attachedList
                )
                wrappedListy
            }
          };
        let everythingButAppTerms =
          switch attachTo {
          | None => everythingButReturnVal
          | Some toThis => label space::true toThis everythingButReturnVal
          };
        formatAttachmentApplication
          applicationFinalWrapping (Some (true, everythingButAppTerms)) returnedAppTerms
      };
      method leadingCurriedAbstractTypes x => {
        let rec argsAndReturn xx =>
          switch xx.pexp_desc {
          | Pexp_newtype str e [@implicit_arity] =>
            let (nextArgs, return) = argsAndReturn e;
            ([str, ...nextArgs], return)
          | _ => ([], xx.pexp_desc)
          };
        argsAndReturn x
      };
      method curriedConstructorPatternsAndReturnVal cl => {
        let rec argsAndReturn xx =>
          if (xx.pcl_attributes != []) {
            ([], xx)
          } else {
            switch xx.pcl_desc {
            | Pcl_fun label eo p e [@implicit_arity] =>
              let (nextArgs, return) = argsAndReturn e;
              if (label == "") {
                let args = [
                  SourceMap p.ppat_loc (self#simple_pattern p) [@implicit_arity],
                  ...nextArgs
                ];
                (args, return)
              } else {
                let args = [
                  SourceMap p.ppat_loc (self#label_exp (label, eo, p)) [@implicit_arity],
                  ...nextArgs
                ];
                (args, return)
              }
            | _ => ([], xx)
            }
          };
        argsAndReturn cl
      };
      /*
         Returns the arguments list (if any, that occur before the =>), and the
         final expression (that is either returned from the function (after =>) or
         that is bound to the value (if there are no arguments, and this is just a
         let pattern binding)).
       */
      method curriedPatternsAndReturnVal x => {
        let rec argsAndReturn xx =>
          if (xx.pexp_attributes != []) {
            ([], xx)
          } else {
            switch xx.pexp_desc {
            /* label * expression option * pattern * expression */
            | Pexp_fun label eo p e [@implicit_arity] =>
              let (nextArgs, return) = argsAndReturn e;
              if (label == "") {
                let args = [
                  SourceMap p.ppat_loc (self#simple_pattern p) [@implicit_arity],
                  ...nextArgs
                ];
                (args, return)
              } else {
                let args = [
                  SourceMap p.ppat_loc (self#label_exp (label, eo, p)) [@implicit_arity],
                  ...nextArgs
                ];
                (args, return)
              }
            | Pexp_newtype str e [@implicit_arity] =>
              let typeParamLayout = atom ("(type " ^ str ^ ")");
              let (nextArgs, return) = argsAndReturn e;
              ([typeParamLayout, ...nextArgs], return)
            | _ => ([], xx)
            }
          };
        argsAndReturn x
      };
      /* Returns the (curriedModule, returnStructure) for a functor */
      method curriedFunctorPatternsAndReturnStruct me =>
        switch me.pmod_desc {
        /* string loc * module_type option * module_expr */
        | Pmod_functor s mt me2 [@implicit_arity] =>
          let firstOne =
            switch mt {
            | None => atom "()"
            | Some mt' =>
              makeList
                wrap::("(", ")")
                break::IfNeed
                [formatTypeConstraint (atom s.txt) (self#module_type mt')]
            };
          let (functorArgsRecurse, returnStructure) = self#curriedFunctorPatternsAndReturnStruct me2;
          ([firstOne, ...functorArgsRecurse], returnStructure)
        | _ => ([], me)
        };
      method isRenderableAsPolymorphicAbstractTypes
             typeVars
             polyType
             leadingAbstractVars
             nonVarifiedType =>
        same_ast_modulo_varification_and_extensions polyType nonVarifiedType &&
        trueForEachPair typeVars leadingAbstractVars (fun x y => String.compare x y === 0);
      /* Reinterpret this as a pattern constraint since we don't currently have a
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
         */
      method locallyAbstractPolymorphicFunctionBinding
             prefixText
             layoutPattern
             funWithNewTypes
             absVars
             bodyType => {
        let appTerms = self#unparseExprApplicationItems funWithNewTypes;
        let locallyAbstractTypes = List.map atom absVars;
        let typeLayout = SourceMap bodyType.ptyp_loc (self#core_type bodyType) [@implicit_arity];
        let polyType =
          label
            space::true
            /* TODO: This isn't a correct use of sep! It ruins how
             * comments are interleaved. */
            (makeList [makeList sep::" " [atom "type", ...locallyAbstractTypes], atom "."])
            typeLayout;
        self#formatSimplePatternBinding prefixText layoutPattern (Some polyType) appTerms
      };
      /**
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
       */
      method wrappedBinding prefixText pattern patternAux expr => {
        let (argsList, return) = self#curriedPatternsAndReturnVal expr;
        let patternList =
          switch patternAux {
          | [] => pattern
          | [_, ..._] =>
            makeList postSpace::true inline::(true, true) break::IfNeed [pattern, ...patternAux]
          };
        switch (argsList, return.pexp_desc) {
        | ([], Pexp_constraint e ct [@implicit_arity]) =>
          let typeLayout = SourceMap ct.ptyp_loc (self#core_type ct) [@implicit_arity];
          let appTerms = self#unparseExprApplicationItems e;
          self#formatSimplePatternBinding prefixText patternList (Some typeLayout) appTerms
        | ([], _) =>
          let appTerms = self#unparseExprApplicationItems expr;
          self#formatSimplePatternBinding prefixText patternList None appTerms
        | ([_, ..._], _) =>
          let (argsWithConstraint, actualReturn) =
            self#normalizeFunctionArgsConstraint argsList return;
          let fauxArgs = List.concat [patternAux, argsWithConstraint];
          let returnedAppTerms = self#unparseExprApplicationItems actualReturn;
          self#wrapCurriedFunctionBinding prefixText pattern fauxArgs returnedAppTerms
        }
      };
      /* Similar to the above method. */
      method wrappedClassBinding prefixText pattern patternAux expr => {
        let (argsList, return) = self#curriedConstructorPatternsAndReturnVal expr;
        let patternList =
          switch patternAux {
          | [] => pattern
          | [_, ..._] =>
            makeList postSpace::true inline::(true, true) break::IfNeed [pattern, ...patternAux]
          };
        switch (argsList, return.pcl_desc) {
        | ([], Pcl_constraint e ct [@implicit_arity]) =>
          let typeLayout =
            SourceMap ct.pcty_loc (self#class_constructor_type ct) [@implicit_arity];
          let appTerms = self#classExpressionToFormattedApplicationItems e;
          self#formatSimplePatternBinding prefixText patternList (Some typeLayout) appTerms
        | ([], _) =>
          let appTerms = self#classExpressionToFormattedApplicationItems expr;
          self#formatSimplePatternBinding prefixText patternList None appTerms
        | ([_, ..._], _) =>
          let (argsWithConstraint, actualReturn) =
            self#normalizeConstructorArgsConstraint argsList return;
          let returnedAppTerms = self#classExpressionToFormattedApplicationItems actualReturn;
          let fauxArgs = List.concat [patternAux, argsWithConstraint];
          self#wrapCurriedFunctionBinding prefixText pattern fauxArgs returnedAppTerms
        }
      };
      method binding {pvb_pat, pvb_expr: x} prefixText =>
        /* TODO: print attributes */
        switch pvb_pat.ppat_desc {
        | Ppat_var {txt} =>
          let pattern = SourceMap pvb_pat.ppat_loc (self#simple_pattern pvb_pat) [@implicit_arity];
          self#wrappedBinding prefixText pattern [] x
        /*
           Ppat_constraint is used in bindings of the form

              let (inParenVar:typ) = ...

           And in the case of let bindings for explicitly polymorphic type
           annotations (see parser for more details).

           See reason_parser.mly for explanation of how we encode the two primary
           forms of explicit polymorphic annotations in the parse tree, and how
           we must recover them here.
         */
        | Ppat_constraint p ty [@implicit_arity] =>
          /* Locally abstract forall types are *seriously* mangled by the parsing
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
             */
          let layoutPattern = SourceMap pvb_pat.ppat_loc (self#simple_pattern p) [@implicit_arity];
          let leadingAbsTypesAndExpr = self#leadingCurriedAbstractTypes x;
          switch (p.ppat_desc, ty.ptyp_desc, leadingAbsTypesAndExpr) {
          | (
              Ppat_var s,
              Ptyp_poly typeVars varifiedPolyType [@implicit_arity],
              (
                [_, ..._] as absVars,
                Pexp_constraint funWithNewTypes nonVarifiedExprType [@implicit_arity]
              )
            )
              when
                self#isRenderableAsPolymorphicAbstractTypes
                  typeVars
                  /* If even artificially varified - don't know until returns*/
                  varifiedPolyType
                  absVars
                  nonVarifiedExprType =>
            /*
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
             */
            self#locallyAbstractPolymorphicFunctionBinding
              prefixText layoutPattern funWithNewTypes absVars nonVarifiedExprType
          | _ =>
            let typeLayout = SourceMap ty.ptyp_loc (self#core_type ty) [@implicit_arity];
            let appTerms = self#unparseExprApplicationItems x;
            self#formatSimplePatternBinding prefixText layoutPattern (Some typeLayout) appTerms
          }
        | _ =>
          let layoutPattern = SourceMap pvb_pat.ppat_loc (self#pattern pvb_pat) [@implicit_arity];
          let appTerms = self#unparseExprApplicationItems x;
          self#formatSimplePatternBinding prefixText layoutPattern None appTerms
        };
      /* Ensures that the constraint is formatted properly for sake of function
           binding (formatted without arrows)
           let x y z : no_unguareded_arrows_allowed_here => ret;
         */
      method normalizeFunctionArgsConstraint argsList return =>
        switch return.pexp_desc {
        | Pexp_constraint e ct [@implicit_arity] =>
          let typeLayout =
            SourceMap ct.ptyp_loc (self#non_arrowed_non_simple_core_type ct) [@implicit_arity];
          (argsList @ [formatJustTheTypeConstraint typeLayout], e)
        | _ => (argsList, return)
        };
      method normalizeConstructorArgsConstraint argsList return =>
        switch return.pcl_desc {
        | Pcl_constraint e ct [@implicit_arity] when return.pcl_attributes == [] =>
          let typeLayout =
            SourceMap ct.pcty_loc (self#non_arrowed_class_constructor_type ct) [@implicit_arity];
          (argsList @ [formatJustTheTypeConstraint typeLayout], e)
        | _ => (argsList, return)
        };
      method bindingsLocationRange l => {
        let len = List.length l;
        let fstLoc = (List.nth l 0).pvb_loc;
        let lstLoc = (List.nth l (len - 1)).pvb_loc;
        {loc_start: fstLoc.loc_start, loc_end: lstLoc.loc_end, loc_ghost: false}
      };
      method bindings (rf, l) => {
        let firstLine =
          switch l {
          | [] => raise (NotPossible "no bindings supplied")
          | [x]
          | [x, ..._] =>
            let label =
              switch rf {
              | Nonrecursive => "let"
              | Recursive => "let rec"
              };
            SourceMap x.pvb_loc (self#binding x label) [@implicit_arity]
          };
        let forEachRemaining t => SourceMap t.pvb_loc (self#binding t "and") [@implicit_arity];
        let remainingBindings =
          switch l {
          | [] => []
          | [x] => []
          | [x, x2, ...xtl] => List.map forEachRemaining [x2, ...xtl]
          };
        switch remainingBindings {
        | [] => firstLine
        | _ =>
          makeList
            postSpace::true
            break::Always
            indent::0
            inline::(true, true)
            [firstLine, ...remainingBindings]
        }
      };
      method letList exprTerm =>
        switch (exprTerm.pexp_attributes, exprTerm.pexp_desc) {
        | ([], Pexp_let rf l e [@implicit_arity]) =>
          /* For "letList" bindings, the start/end isn't as simple as with
           * module value bindings. For "let lists", the sequences were formed
           * within braces {}. The parser relocates the first let binding to the
           * first brace. */
          let bindingsLayout = self#bindings (rf, l);
          let bindingsLoc = self#bindingsLocationRange l;
          let bindingsSourceMapped = SourceMap bindingsLoc bindingsLayout [@implicit_arity];
          [bindingsSourceMapped, ...self#letList e]
        | ([], Pexp_open ovf lid e [@implicit_arity]) =>
          let listItems = self#letList e;
          if (List.length listItems === 1 && ovf === Fresh) {
            /* The following logic is a syntax sugar
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
             */
            let expression =
              switch e.pexp_desc {
              /* syntax sugar for M.{x:1} */
              | Pexp_record _
              /* syntax sugar for M.(a, b) */
              | Pexp_tuple _
              /* syntax sugar for M.{} */
              | Pexp_object {pcstr_fields: []}
              /* syntax sugar for M.[x,y] */
              | Pexp_construct {txt: Lident "::"} (Some _) [@implicit_arity] =>
                self#simplifyUnparseExpr e
              /* syntax sugar for the rest, wrap with parens to avoid ambiguity.
               * E.g., avoid M.(M2.v) being printed as M.M2.v
               */
              | _ => makeList wrap::("(", ")") break::IfNeed listItems
              };
            let openLayout = label (label (self#longident_loc lid) (atom ".")) expression;
            [openLayout]
          } else {
            let overrideStr =
              switch ovf {
              | Override => "!"
              | Fresh => ""
              };
            let openLayout =
              label space::true (atom ("open" ^ overrideStr)) (self#longident_loc lid);
            /* Just like the bindings, have to synthesize a location since the
             * Pexp location is parsed (potentially) beginning with the open
             * brace {} in the let sequence. */
            let openSourceMapped = SourceMap lid.loc openLayout [@implicit_arity];
            [openSourceMapped, ...listItems]
          }
        | ([], Pexp_letmodule s me e [@implicit_arity]) =>
          let prefixText = "module";
          let bindingName = atom loc::s.loc s.txt;
          let moduleExpr = me;
          let letModuleLayout = self#let_module_binding prefixText bindingName moduleExpr;
          let letModuleLoc = {
            loc_start: s.loc.loc_start,
            loc_end: me.pmod_loc.loc_end,
            loc_ghost: false
          };
          /* Just like the bindings, have to synthesize a location since the
           * Pexp location is parsed (potentially) beginning with the open
           * brace {} in the let sequence. */
          let letModuleSourceMapped = SourceMap letModuleLoc letModuleLayout [@implicit_arity];
          [letModuleSourceMapped, ...self#letList e]
        | ([], Pexp_sequence ({pexp_desc: Pexp_sequence _} as e1) e2 [@implicit_arity])
        | ([], Pexp_sequence ({pexp_desc: Pexp_let _} as e1) e2 [@implicit_arity])
        | ([], Pexp_sequence ({pexp_desc: Pexp_open _} as e1) e2 [@implicit_arity])
        | ([], Pexp_sequence ({pexp_desc: Pexp_letmodule _} as e1) e2 [@implicit_arity])
        | ([], Pexp_sequence e1 e2 [@implicit_arity]) =>
          let e1Layout = self#unparseExpr e1;
          /* It's kind of difficult to synthesize a location here in the case
           * where this is the first expression in the braces. We could consider
           * deeply inspecting the leftmost token/term in the expression. */
          let e1SourceMapped = SourceMap e1.pexp_loc e1Layout [@implicit_arity];
          [e1SourceMapped, ...self#letList e2]
        | _ =>
          let exprTermLayout = self#unparseExpr exprTerm;
          let exprTermSourceMapped = SourceMap exprTerm.pexp_loc exprTermLayout [@implicit_arity];
          /* Should really do something to prevent infinite loops here. Never
             allowing a top level call into letList to recurse back to
             self#unparseExpr- top level calls into letList *must* be one of the
             special forms above whereas lower level recursive calls may be of
             any form. */
          [exprTermSourceMapped]
        };
      method constructor_expression
             polyVariant::polyVariant=false
             arityIsClear::arityIsClear
             stdAttrs
             ctor
             eo => {
        let (implicit_arity, arguments) =
          switch eo.pexp_desc {
          | Pexp_tuple l when not polyVariant =>
            let exprs =
              switch (List.map self#simplifyUnparseExpr l) {
              | [] => raise (NotPossible "no tuple items")
              | [hd] => hd
              | [hd, ...tl] as all => makeSpacedBreakableInlineList all
              };
            /* There is no ambiguity when the number of tuple components is 1.
               We don't need put implicit_arity in that case */
            (List.length l > 1 && not arityIsClear, exprs)
          | _ => (false, self#simplifyUnparseExpr eo)
          };
        let construction =
          label
            space::true
            ctor
            (
              if (isSequencey arguments) {
                arguments
              } else {
                ensureSingleTokenSticksToLabel arguments
              }
            );
        let attrs =
          if (implicit_arity && not polyVariant) {
            [({txt: "implicit_arity", loc: eo.pexp_loc}, PStr []), ...stdAttrs]
          } else {
            stdAttrs
          };
        switch attrs {
        | [] => construction
        | [_, ..._] => formatAttributed construction (self#attributes attrs)
        }
      };
      /* TODOATTRIBUTES: Handle stdAttrs here (merge with implicit_arity) */
      method constructor_pattern polyVariant::polyVariant=false arityIsClear::arityIsClear ctor po => {
        let (implicit_arity, arguments) =
          switch po.ppat_desc {
          | Ppat_tuple l when not polyVariant =>
            let exprs =
              switch (List.map self#simple_pattern l) {
              | [] => raise (NotPossible "no tuple items")
              | [hd] => hd
              | [hd, ...tl] as all => makeSpacedBreakableInlineList all
              };
            /* There is no ambiguity when the number of tuple components is 1.
               We don't need put implicit_arity in that case */
            (List.length l > 1 && not arityIsClear, exprs)
          | _ => (false, self#simple_pattern po)
          };
        let construction =
          label
            space::true
            ctor
            (
              if (isSequencey arguments) {
                arguments
              } else {
                ensureSingleTokenSticksToLabel arguments
              }
            );
        if (implicit_arity && not polyVariant) {
          formatAttributed
            construction (self#attributes [({txt: "implicit_arity", loc: po.ppat_loc}, PStr [])])
        } else {
          construction
        }
      };
      method patternFunction loc l => {
        let estimatedFunLocation = {
          loc_start: loc.loc_start,
          loc_end: {...loc.loc_start, pos_cnum: loc.loc_start.Lexing.pos_cnum + 3},
          loc_ghost: false
        };
        makeList
          postSpace::true
          break::IfNeed
          inline::(true, true)
          pad::(false, false)
          [atom loc::estimatedFunLocation "fun", ...self#case_list l]
      };
      /* Expressions requiring parens, in most contexts such as separated by infix */
      method expression_requiring_parens_in_infix x => {
        let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.pexp_attributes;
        assert (stdAttrs === []);
        switch x.pexp_desc {
        /* The only reason Pexp_fun must also be wrapped in parens when under
           pipe, is that its => token will be confused with the match token.
           Simple expression will also invoke `#reset`. */
        | Pexp_function _ when pipe || semi => None /* Would be rendered as simplest_expression  */
        | Pexp_function l => Some (self#patternFunction x.pexp_loc l)
        | _ =>
          /* The Pexp_function cases above don't use location because comment printing
             breaks for them. */
          let itm =
            switch x.pexp_desc {
            | Pexp_fun _
            | Pexp_newtype _ =>
              let (args, ret) = self#curriedPatternsAndReturnVal x;
              switch args {
              | [] => raise (NotPossible "no arrow args in unparse ")
              | [firstArg, ...tl] =>
                /* Suboptimal printing of parens:

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
                   instead model everything as "infix" with ranked precedences.  */
                let retValUnparsed = self#unparseExprApplicationItems ret;
                Some (self#wrapCurriedFunctionBinding "fun" firstArg tl retValUnparsed)
              }
            | Pexp_try e l [@implicit_arity] =>
              let estimatedBracePoint = {
                loc_start: e.pexp_loc.loc_end,
                loc_end: x.pexp_loc.loc_end,
                loc_ghost: false
              };
              let cases = self#case_list allowUnguardedSequenceBodies::true l;
              let switchWith = label space::true (atom "try") (self#reset#simplifyUnparseExpr e);
              Some (
                label
                  space::true
                  switchWith
                  (
                    SourceMap
                      estimatedBracePoint
                      (
                        makeList
                          indent::settings.trySwitchIndent
                          wrap::("{", "}")
                          break::Always_rec
                          postSpace::true
                          cases
                      )
                    [@implicit_arity]
                  )
              )
            /* These should have already been handled and we should never havgotten this far. */
            | Pexp_setinstvar s e [@implicit_arity] =>
              raise (Invalid_argument "Cannot handle setinstvar here - call unparseExpr")
            | Pexp_setfield _ _ _ [@implicit_arity] =>
              raise (Invalid_argument "Cannot handle setfield here - call unparseExpr")
            | Pexp_apply e l [@implicit_arity] =>
              raise (Invalid_argument "Cannot handle apply here - call unparseExpr")
            | Pexp_match e l [@implicit_arity] =>
              let estimatedBracePoint = {
                loc_start: e.pexp_loc.loc_end,
                loc_end: x.pexp_loc.loc_end,
                loc_ghost: false
              };
              let cases = self#case_list allowUnguardedSequenceBodies::true l;
              let switchWith =
                label space::true (atom "switch") (self#reset#simplifyUnparseExpr e);
              let lbl =
                label
                  space::true
                  switchWith
                  (
                    SourceMap
                      estimatedBracePoint
                      (
                        makeList
                          indent::settings.trySwitchIndent
                          wrap::("{", "}")
                          break::Always_rec
                          postSpace::true
                          cases
                      )
                    [@implicit_arity]
                  );
              Some lbl
            | Pexp_ifthenelse e1 e2 eo [@implicit_arity] =>
              let (blocks, finalExpression) = sequentialIfBlocks eo;
              let rec singleExpression exp =>
                switch exp.pexp_desc {
                | Pexp_ident _ => true
                | Pexp_constant _ => true
                | Pexp_construct _ arg [@implicit_arity] =>
                  switch arg {
                  | None => true
                  | Some x => singleExpression x
                  }
                | _ => false
                };
              let singleLineIf =
                singleExpression e1 &&
                singleExpression e2 && (
                  switch eo {
                  | Some expr => singleExpression expr
                  | None => true
                  }
                );
              let makeLetSequence =
                if singleLineIf {makeLetSequenceSingleLine} else {makeLetSequence};
              let rec sequence soFar remaining =>
                switch (remaining, finalExpression) {
                | ([], None) => soFar
                | ([], Some e) =>
                  let soFarWithElseAppended = makeList postSpace::true [soFar, atom "else"];
                  label space::true soFarWithElseAppended (makeLetSequence (self#letList e))
                | ([hd, ...tl], _) =>
                  let (e1, e2) = hd;
                  let soFarWithElseIfAppended =
                    label
                      space::true
                      (makeList postSpace::true [soFar, atom "else if"])
                      (self#simplifyUnparseExpr e1);
                  let nextSoFar =
                    label space::true soFarWithElseIfAppended (makeLetSequence (self#letList e2));
                  sequence nextSoFar tl
                };
              let init =
                label
                  space::true
                  (
                    SourceMap
                      e1.pexp_loc (label space::true (atom "if") (self#simplifyUnparseExpr e1))
                    [@implicit_arity]
                  )
                  (makeLetSequence (self#letList e2));
              Some (sequence init blocks)
            | Pexp_while e1 e2 [@implicit_arity] =>
              let lbl =
                label
                  space::true
                  (label space::true (atom "while") (self#simplifyUnparseExpr e1))
                  (makeLetSequence (self#letList e2));
              Some lbl
            | Pexp_for s e1 e2 df e3 [@implicit_arity] =>
              /*
               *  for longIdentifier in
               *      (longInit expr) to
               *      (longEnd expr) {
               *    print_int longIdentifier;
               *  };
               */
              let identifierIn = makeList postSpace::true [self#pattern s, atom "in"];
              let dockedToFor =
                makeList
                  break::IfNeed
                  postSpace::true
                  inline::(true, true)
                  [
                    identifierIn,
                    makeList postSpace::true [self#simplifyUnparseExpr e1, self#direction_flag df],
                    self#simplifyUnparseExpr e2
                  ];
              let upToBody =
                makeList inline::(true, true) postSpace::true [atom "for", dockedToFor];
              Some (label space::true upToBody (makeLetSequence (self#letList e3)))
            | Pexp_new li =>
              Some (label space::true (atom "new") (self#longident_class_or_type_loc li))
            | Pexp_assert e =>
              Some (label space::true (atom "assert") (self#reset#simplifyUnparseExpr e))
            | Pexp_lazy e => Some (label space::true (atom "lazy") (self#simplifyUnparseExpr e))
            | Pexp_poly _ =>
              failwith (
                "This version of the pretty printer assumes it is impossible to " ^ "construct a Pexp_poly outside of a method definition - yet it sees one."
              )
            | _ => None
            };
          switch itm {
          | None => None
          | Some i => Some (SourceMap x.pexp_loc i [@implicit_arity])
          }
        }
      };
      method potentiallyConstrainedExpr x =>
        switch x.pexp_desc {
        | Pexp_constraint e ct [@implicit_arity] =>
          formatTypeConstraint (self#unparseExpr e) (self#core_type ct)
        | _ => self#unparseExpr x
        };
      /*
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
       */
      method simple_enough_to_be_lhs_dot_send x =>
        switch x.pexp_desc {
        | Pexp_apply eFun _ [@implicit_arity] =>
          switch (printedStringAndFixityExpr eFun) {
          | AlmostSimplePrefix _ =>
            SourceMap x.pexp_loc (formatPrecedence (self#simplifyUnparseExpr x)) [@implicit_arity]
          | UnaryPlusPrefix _
          | UnaryMinusPrefix _
          | Infix _
          | Normal => self#simplifyUnparseExpr x
          }
        | _ => self#simplifyUnparseExpr x
        };
      method unparseRecord
             withStringKeys::withStringKeys=false
             allowPunning::allowPunning=true
             l
             eo => {
        let quote = atom "\"";
        let maybeQuoteFirstElem l =>
          if withStringKeys {
            switch l {
            | [fst, ...rest] => [quote, fst, quote, ...rest]
            | _ => l
            }
          } else {
            l
          };
        let makeRow (li, e) appendComma shouldPun => {
          let comma = atom ",";
          let totalRowLoc = {
            loc_start: li.Asttypes.loc.loc_start,
            loc_end: e.pexp_loc.loc_end,
            loc_ghost: false
          };
          let theRow =
            switch e.pexp_desc {
            /* Punning */
            | Pexp_ident {txt} when li.txt == txt && shouldPun && allowPunning =>
              makeList (
                maybeQuoteFirstElem [
                  self#longident_loc li,
                  ...if appendComma {
                       [comma]
                     } else {
                       []
                     }
                ]
              )
            | _ =>
              let (argsList, return) = self#curriedPatternsAndReturnVal e;
              switch (argsList, return.pexp_desc) {
              | ([], _) =>
                let appTerms = self#unparseExprApplicationItems e;
                let upToColon = makeList (maybeQuoteFirstElem [self#longident_loc li, atom ":"]);
                let labelExpr =
                  formatAttachmentApplication
                    applicationFinalWrapping (Some (true, upToColon)) appTerms;
                if appendComma {
                  makeList [labelExpr, comma]
                } else {
                  labelExpr
                }
              | ([firstArg, ...tl], _) =>
                let upToColon = makeList (maybeQuoteFirstElem [self#longident_loc li, atom ":"]);
                let returnedAppTerms = self#unparseExprApplicationItems return;
                let labelExpr =
                  self#wrapCurriedFunctionBinding
                    attachTo::upToColon "fun" firstArg tl returnedAppTerms;
                if appendComma {
                  makeList [labelExpr, comma]
                } else {
                  labelExpr
                }
              }
            };
          SourceMap totalRowLoc theRow [@implicit_arity]
        };
        let rec getRows l =>
          switch l {
          | [] => []
          | [hd] => [makeRow hd false true]
          | [hd, hd2, ...tl] => [makeRow hd true true, ...getRows [hd2, ...tl]]
          };
        let allRows =
          switch eo {
          | None =>
            switch l {
            /* No punning (or comma) for records with only a single field. */
            /* See comment in parser.mly for lbl_expr_list_with_at_least_one_non_punned_field */
            | [hd] => [makeRow hd false false]
            | _ => getRows l
            }
          | Some withRecord =>
            let firstRow = {
              /* Unclear why "sugar_expr" was special cased here. */
              let appTerms = self#unparseExprApplicationItems withRecord;
              let firstRowContents =
                formatAttachmentApplication
                  applicationFinalWrapping (Some (false, atom "...")) appTerms;
              if (l === []) {
                firstRowContents
              } else {
                makeList [firstRowContents, atom ","]
              }
            };
            [SourceMap withRecord.pexp_loc firstRow [@implicit_arity], ...getRows l]
          };
        makeList wrap::("{", "}") break::IfNeed preSpace::true allRows
      };
      method simplest_expression x => {
        let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.pexp_attributes;
        if (stdAttrs != []) {
          None
        } else {
          let item =
            switch x.pexp_desc {
            /* The only reason Pexp_fun must also be wrapped in parens is that its =>
               token will be confused with the match token. */
            | Pexp_fun _ when pipe || semi => Some (self#reset#simplifyUnparseExpr x)
            | Pexp_function l when pipe || semi =>
              Some (formatPrecedence loc::x.pexp_loc (self#reset#patternFunction x.pexp_loc l))
            | Pexp_apply e l [@implicit_arity] =>
              switch (self#simple_get_application x) {
              /* If it's the simple form of application. */
              | Some simpleGet => Some simpleGet
              | None => None
              }
            | Pexp_object cs =>
              let obj =
                makeList
                  sep::";"
                  wrap::("{", "}")
                  break::IfNeed
                  postSpace::true
                  inline::(true, false)
                  (self#class_self_pattern_and_structure cs);
              Some obj
            | Pexp_override l =>
              /* FIXME */
              let string_x_expression (s, e) =>
                label space::true (atom (s.txt ^ ":")) (self#unparseExpr e);
              Some (
                makeList
                  postSpace::true wrap::("{<", ">}") sep::"," (List.map string_x_expression l)
              )
            | Pexp_construct _ when is_simple_construct (view_expr x) =>
              let hasJsxAttribute = jsxAttrs !== [];
              Some (
                switch (view_expr x) {
                | `nil =>
                  if hasJsxAttribute {
                    atom "<> </>"
                  } else {
                    atom "[]"
                  }
                | `tuple => atom "()"
                | `list xs =>
                  /* LIST EXPRESSION */
                  if hasJsxAttribute {
                    let actualChildren =
                      switch (self#formatChildren xs []) {
                      | None => []
                      | Some ch => ch
                      };
                    makeList
                      break::IfNeed
                      inline::(false, false)
                      postSpace::true
                      wrap::("<>", "</>")
                      pad::(true, true)
                      actualChildren
                  } else {
                    makeList
                      break::IfNeed
                      wrap::("[", "]")
                      sep::","
                      postSpace::true
                      (List.map self#unparseExpr xs)
                  }
                | `cons xs =>
                  let (seq, ext) =
                    switch (List.rev xs) {
                    | [ext, ...seq_rev] => (List.rev seq_rev, ext)
                    | [] => assert false
                    };
                  makeES6List (List.map self#unparseExpr seq) (self#unparseExpr ext)
                | `simple x => self#longident x
                | _ => assert false
                }
              )
            | Pexp_ident li =>
              /* Lone identifiers shouldn't break when to the right of a label */
              Some (ensureSingleTokenSticksToLabel (self#longident_loc li))
            | Pexp_constant c =>
              /* Constants shouldn't break when to the right of a label */
              Some (ensureSingleTokenSticksToLabel (self#constant c))
            | Pexp_pack me =>
              Some (
                makeList
                  break::IfNeed
                  postSpace::true
                  wrap::("(", ")")
                  inline::(true, true)
                  [atom "module", self#module_expr me]
              )
            | Pexp_tuple l =>
              /* TODO: These may be simple, non-simple, or type constrained
                 non-simple expressions */
              Some (
                makeList
                  wrap::("(", ")")
                  sep::","
                  break::IfNeed
                  postSpace::true
                  (List.map self#potentiallyConstrainedExpr l)
              )
            | Pexp_constraint e ct [@implicit_arity] =>
              Some (
                makeList
                  break::IfNeed
                  wrap::("(", ")")
                  [formatTypeConstraint (self#unparseExpr e) (self#core_type ct)]
              )
            | Pexp_coerce e cto1 ct [@implicit_arity] =>
              let optFormattedType =
                switch cto1 {
                | None => None
                | Some typ => Some (self#core_type typ)
                };
              Some (
                makeList
                  break::IfNeed
                  wrap::("(", ")")
                  [formatCoerce (self#unparseExpr e) optFormattedType (self#core_type ct)]
              )
            | Pexp_variant l None [@implicit_arity] =>
              Some (ensureSingleTokenSticksToLabel (atom ("`" ^ l)))
            | Pexp_record l eo [@implicit_arity] => Some (self#unparseRecord l eo)
            | Pexp_array l =>
              Some (
                makeList
                  break::IfNeed
                  sep::","
                  postSpace::true
                  wrap::("[|", "|]")
                  (List.map self#unparseExpr l)
              )
            | Pexp_let rf l e [@implicit_arity] => Some (makeLetSequence (self#letList x))
            | Pexp_letmodule s me e [@implicit_arity] => Some (makeLetSequence (self#letList x))
            | Pexp_open ovf lid e [@implicit_arity] =>
              let letItems = self#letList x;
              switch letItems {
              /* if an open expression has only one letItem in the list,
               * we don't wrap it in "{}" so it becomes something like:
               *
               * let a = Fmt.(strf "-pkgs %a" (list sep::(unit ",") string))
               *
               * instead of:
               *
               * let a = {
               *   Fmt.(strf "-pkgs %a" (list sep::(unit ",") string))
               * }
               */
              | [item] => Some item
              | _ => Some (makeLetSequence letItems)
              }
            | Pexp_sequence _ => Some (makeLetSequence (self#letList x))
            | Pexp_field e li [@implicit_arity] =>
              Some (
                label
                  (makeList [self#simple_enough_to_be_lhs_dot_send e, atom "."])
                  (self#longident_loc li)
              )
            | Pexp_send e s [@implicit_arity] =>
              Some (label (makeList [self#simple_enough_to_be_lhs_dot_send e, atom "#"]) (atom s))
            | Pexp_extension e => Some (self#extension e)
            | _ => None
            };
          switch item {
          | None => None
          | Some i => Some (SourceMap x.pexp_loc i [@implicit_arity])
          }
        }
      };
      method formatChildren children processedRev =>
        switch children {
        | [{pexp_desc: Pexp_constant constant}, ...remaining] =>
          self#formatChildren remaining [self#constant constant, ...processedRev]
        | [
            {
              pexp_desc:
                Pexp_construct {txt: Lident "::"} (Some {pexp_desc: Pexp_tuple children})
                [@implicit_arity]
            },
            ...remaining
          ] =>
          self#formatChildren (remaining @ children) processedRev
        | [{pexp_desc: Pexp_apply expr l [@implicit_arity], pexp_attributes}, ...remaining] =>
          switch (detectJSXComponent expr.pexp_desc pexp_attributes l) {
          | Some componentName =>
            self#formatChildren
              remaining [self#formatJSXComponent componentName l, ...processedRev]
          | None =>
            self#formatChildren
              remaining [self#simplifyUnparseExpr (List.hd children), ...processedRev]
          }
        | [{pexp_desc: Pexp_ident li}, ...remaining] =>
          self#formatChildren remaining [self#longident_loc li, ...processedRev]
        | [{pexp_desc: Pexp_construct {txt: Lident "[]"} None [@implicit_arity]}, ...remaining] =>
          self#formatChildren remaining processedRev
        | [head, ...remaining] =>
          self#formatChildren remaining [self#simplifyUnparseExpr head, ...processedRev]
        | [] =>
          switch processedRev {
          | [] => None
          | [_, ..._] => Some (List.rev processedRev)
          }
        };
      method direction_flag =
        fun
        | Upto => atom "to"
        | Downto => atom "downto";
      method payload ppxToken ppxId e => {
        let wrap = ("[" ^ ppxToken ^ ppxId.txt, "]");
        let break = IfNeed;
        let pad = (true, false);
        let postSpace = true;
        let sep = ";";
        switch e {
        | PStr [] => atom ("[" ^ ppxToken ^ ppxId.txt ^ "]")
        | PStr [itm] => makeList wrap::wrap break::break pad::pad [self#structure_item itm]
        | PStr ([_, ..._] as items) =>
          let rows = List.map self#structure_item items;
          makeList wrap::wrap break::break pad::pad postSpace::postSpace sep::sep rows
        | PTyp x =>
          makeList
            wrap::wrap break::break pad::pad [label space::true (atom ":") (self#core_type x)]
        /* Signatures in attributes were added recently */
        /* | PSig x -> makeList [atom ":"; self#signature x] */
        | PPat x None [@implicit_arity] =>
          makeList wrap::wrap break::break pad::pad [label space::true (atom "?") (self#pattern x)]
        | PPat x (Some e) [@implicit_arity] =>
          makeList
            wrap::wrap
            break::break
            pad::pad
            postSpace::postSpace
            [
              label space::true (atom "?") (self#pattern x),
              label space::true (atom "when") (self#unparseExpr e)
            ]
        }
      };
      method extension (s, e) =>
        switch s.txt {
        /* We special case "bs.obj" for now to allow for a nicer interop with
         * BuckleScript. We might be able to generalize to any kind of record
         * looking thing with struct keys. */
        | "bs.obj" =>
          switch e {
          | PStr [itm] =>
            switch itm {
            | {
                pstr_desc:
                  Pstr_eval {pexp_desc: Pexp_record l eo [@implicit_arity]} [] [@implicit_arity]
              } =>
              self#unparseRecord withStringKeys::true allowPunning::false l eo
            | _ => assert false
            }
          | _ => assert false
          }
        | _ => self#payload "%" s e
        };
      method item_extension (s, e) => self#payload "%%" s e;
      /* @[ ...] Simple attributes */
      method attribute (s, e) => self#payload "@" s e;
      /* [@@ ... ] Attributes that occur after a major item in a structure/class */
      method item_attribute (s, e) => self#payload "@@" s e;
      /* [@@@ ...] Attributes that occur not *after* an item in some structure/class/sig, but
         rather as their own standalone item. Note that syntactic distinction
         between item_attribute and floating_attribute is no longer necessary with
         Reason. Thank you semicolons. */
      method floating_attribute (s, e) => self#payload "@@@" s e;
      method attributes l => makeList break::IfNeed postSpace::true (List.map self#attribute l);
      method attach_std_attrs l toThis => {
        let l = extractStdAttrs l;
        switch l {
        | [] => toThis
        | [_, ..._] => makeList postSpace::true [toThis, self#attributes l]
        }
      };
      method attach_std_item_attrs l toThis => {
        let l = extractStdAttrs l;
        switch l {
        | [] => toThis
        | [_, ..._] =>
          makeList
            postSpace::true
            indent::0
            break::IfNeed
            inline::(true, true)
            [toThis, makeList break::IfNeed postSpace::true (List.map self#item_attribute l)]
        }
      };
      method exception_declaration ed => {
        let pcd_name = ed.pext_name;
        let pcd_loc = ed.pext_loc;
        let pcd_attributes = ed.pext_attributes;
        let exn_arg =
          switch ed.pext_kind {
          | Pext_decl args type_opt [@implicit_arity] =>
            let (pcd_args, pcd_res) = (args, type_opt);
            [self#type_variant_leaf_nobar {pcd_name, pcd_args, pcd_res, pcd_loc, pcd_attributes}]
          | Pext_rebind id => [atom pcd_name.txt, atom "=", self#longident_loc id]
          };
        makeList postSpace::true [atom "exception", ...exn_arg]
      };
      /*
        Note: that override doesn't appear in class_sig_field, but does occur in
        class/object expressions.
        TODO: TODOATTRIBUTES
       */
      method method_sig_flags_for s =>
        fun
        | (Private, Virtual) => [atom "private", atom "virtual", atom s]
        | (Private, Concrete) => [atom "private", atom s]
        | (Public, Virtual) => [atom "virtual", atom s]
        | (Public, Concrete) => [atom s];
      method method_flags_for s =>
        fun
        | (Private, Virtual) => [atom "private", atom "virtual", atom s]
        | (Private, Concrete) => [atom "private", atom s]
        | (Public, Virtual) => [atom "virtual", atom s]
        | (Public, Concrete) => [atom s];
      method value_type_flags_for s =>
        fun
        | (Virtual, Mutable) => [atom "virtual", atom "mutable", atom s]
        | (Virtual, Immutable) => [atom "virtual", atom s]
        | (Concrete, Mutable) => [atom "mutable", atom s]
        | (Concrete, Immutable) => [atom s];
      method class_sig_field x =>
        switch x.pctf_desc {
        | Pctf_inherit ct => label space::true (atom "inherit") (self#class_constructor_type ct)
        | Pctf_val s mf vf ct [@implicit_arity] =>
          let valueFlags = self#value_type_flags_for (s ^ ":") (vf, mf);
          label
            space::true
            (
              label
                space::true
                (atom "val")
                (makeList postSpace::true inline::(false, true) break::IfNeed valueFlags)
            )
            (self#core_type ct)
        | Pctf_method s pf vf ct [@implicit_arity] =>
          let methodFlags = self#method_sig_flags_for (s ^ ":") (pf, vf);
          label
            space::true
            (
              label
                space::true
                (atom "method")
                (makeList postSpace::true inline::(false, true) break::IfNeed methodFlags)
            )
            (self#core_type ct)
        | Pctf_constraint ct1 ct2 [@implicit_arity] =>
          label
            (atom "constraint")
            (
              label
                space::true
                (makeList postSpace::true [self#core_type ct1, atom "="])
                (self#core_type ct2)
            )
        | Pctf_attribute a => self#floating_attribute a
        | Pctf_extension e => self#item_extension e
        };
      /* The type of something returned from a constructor. Formerly [class_signature]  */
      method shouldDisplayClassInstTypeItem x =>
        switch x.pctf_desc {
        | Pctf_attribute s _ [@implicit_arity] =>
          not (s.txt == "ocaml.text") && not (s.txt == "ocaml.doc")
        | _ => true
        };
      method shouldDisplayClassField x =>
        switch x.pcf_desc {
        | Pcf_attribute s _ [@implicit_arity] =>
          not (s.txt == "ocaml.text") && not (s.txt == "ocaml.doc")
        | _ => true
        };
      method shouldDisplaySigItem x =>
        switch x.psig_desc {
        | Psig_attribute s _ [@implicit_arity] =>
          not (s.txt == "ocaml.text") && not (s.txt == "ocaml.doc")
        | _ => true
        };
      method shouldDisplayStructureItem x =>
        switch x.pstr_desc {
        | Pstr_attribute s _ [@implicit_arity] =>
          not (s.txt == "ocaml.text") && not (s.txt == "ocaml.doc")
        | _ => true
        };
      method class_instance_type x =>
        switch x.pcty_desc {
        | Pcty_signature cs =>
          let {pcsig_self: ct, pcsig_fields: l} = cs;
          let instTypeFields =
            List.map self#class_sig_field (List.filter self#shouldDisplayClassInstTypeItem l);
          let allItems =
            switch ct.ptyp_desc {
            | Ptyp_any => instTypeFields
            | _ => [label space::true (atom "as") (self#core_type ct), ...instTypeFields]
            };
          makeList wrap::("{", "}") postSpace::true break::Always_rec sep::";" allItems
        | Pcty_constr li l [@implicit_arity] =>
          switch l {
          | [] => self#longident_loc li
          | [_, ..._] =>
            label
              space::true
              (makeList wrap::("(", ")") sep::"," (List.map self#core_type l))
              (self#longident_loc li)
          }
        | Pcty_extension e => self#extension e
        | Pcty_arrow _ => failwith "class_instance_type should not be printed with Pcty_arrow"
        };
      method class_declaration_list l => {
        let class_declaration
            class_keyword::class_keyword=false
            ({pci_params: ls, pci_name: {txt}, pci_virt, pci_expr: {pcl_desc}, pci_loc} as x) => {
          let (firstToken, pattern, patternAux) = self#class_opening class_keyword txt pci_virt ls;
          let classBinding = self#wrappedClassBinding firstToken pattern patternAux x.pci_expr;
          let itm = self#attach_std_item_attrs x.pci_attributes classBinding;
          SourceMap pci_loc itm [@implicit_arity]
        };
        switch l {
        | [] => raise (NotPossible "Class definitions will have at least one item.")
        | [x, ...rest] =>
          makeNonIndentedBreakingList [
            class_declaration class_keyword::true x,
            ...List.map class_declaration rest
          ]
        }
      };
      /* For use with [class type a = class_instance_type]. Class type
            declarations/definitions declare the types of instances generated by class
            constructors.
            We have to call self#class_instance_type because self#class_constructor_type
            would add a "new" before the type.
            TODO: TODOATTRIBUTES:
         */
      method class_type_declaration_list l => {
        let class_type_declaration kwd ({pci_params: ls, pci_name: {txt}, pci_attributes} as x) => {
          let opener =
            switch x.pci_virt {
            | Virtual => kwd ^ " " ^ "virtual"
            | Concrete => kwd
            };
          let upToName =
            if (ls === []) {
              label space::true (atom opener) (atom txt)
            } else {
              label
                space::true (label space::true (atom opener) (atom txt)) (self#class_params_def ls)
            };
          let includingEqual = makeList postSpace::true [upToName, atom "="];
          let itm = label space::true includingEqual (self#class_instance_type x.pci_expr);
          self#attach_std_item_attrs pci_attributes itm
        };
        switch l {
        | [] => failwith "Should not call class_type_declaration with no classes"
        | [x] => class_type_declaration "class type" x
        | [x, ...xs] =>
          makeList
            break::Always_rec
            indent::0
            inline::(true, true)
            [class_type_declaration "class type" x, ...List.map (class_type_declaration "and") xs]
        }
      };
      /*
         Formerly the [class_type]
         Notice how class_constructor_type doesn't have any type attributes -
         class_instance_type does.
         TODO: Divide into class_constructor_types that allow arrows and ones
         that don't.
       */
      method class_constructor_type x =>
        switch x.pcty_desc {
        | Pcty_arrow l co cl [@implicit_arity] =>
          let rec allArrowSegments xx =>
            switch xx.pcty_desc {
            | Pcty_arrow l ct1 ct2 [@implicit_arity] => [
                self#type_with_label (l, ct1),
                ...allArrowSegments ct2
              ]
            /* This "new" is unfortunate. See reason_parser.mly for details. */
            | _ => [self#class_constructor_type xx]
            };
          let normalized =
            makeList
              break::IfNeed
              sep::"=>"
              preSpace::true
              postSpace::true
              inline::(true, true)
              (allArrowSegments x);
          SourceMap x.pcty_loc normalized [@implicit_arity]
        | _ =>
          /* Unfortunately, we have to have final components of a class_constructor_type
             be prefixed with the `new` keyword.  Hopefully this is temporary. */
          label space::true (atom "new") (self#class_instance_type x)
        };
      method non_arrowed_class_constructor_type x =>
        switch x.pcty_desc {
        | Pcty_arrow l co cl [@implicit_arity] =>
          let normalized = formatPrecedence (self#class_constructor_type x);
          SourceMap x.pcty_loc normalized [@implicit_arity]
        | _ => self#class_instance_type x
        };
      /* TODO: TODOATTRIBUTES. */
      method class_field x => {
        let itm =
          switch x.pcf_desc {
          | Pcf_inherit ovf ce so [@implicit_arity] =>
            let inheritText = "inherit" ^ override ovf;
            let inheritExp = self#class_expr ce;
            label
              space::true
              (atom inheritText)
              (
                switch so {
                | None => inheritExp
                | Some s => label space::true inheritExp (atom ("as " ^ s))
                }
              )
          | Pcf_val s mf (Cfk_concrete ovf e [@implicit_arity]) [@implicit_arity] =>
            let opening =
              switch mf {
              | Mutable =>
                let mutableName = [atom "mutable", atom s.txt];
                label
                  space::true
                  (atom ("val" ^ override ovf))
                  (makeList postSpace::true inline::(false, true) break::IfNeed mutableName)
              | Immutable => label space::true (atom ("val" ^ override ovf)) (atom s.txt)
              };
            let valExprAndConstraint =
              switch e.pexp_desc {
              | Pexp_constraint ex ct [@implicit_arity] =>
                let openingWithTypeConstraint = formatTypeConstraint opening (self#core_type ct);
                label
                  space::true
                  (makeList postSpace::true [openingWithTypeConstraint, atom "="])
                  (self#unparseExpr ex)
              | _ =>
                label
                  space::true (makeList postSpace::true [opening, atom "="]) (self#unparseExpr e)
              };
            valExprAndConstraint
          | Pcf_val s mf (Cfk_virtual ct) [@implicit_arity] =>
            let opening =
              switch mf {
              | Mutable =>
                let mutableVirtualName = [atom "mutable", atom "virtual", atom s.txt];
                let openingTokens =
                  makeList postSpace::true inline::(false, true) break::IfNeed mutableVirtualName;
                label space::true (atom "val") openingTokens
              | Immutable =>
                let virtualName = [atom "virtual", atom s.txt];
                let openingTokens =
                  makeList postSpace::true inline::(false, true) break::IfNeed virtualName;
                label space::true (atom "val") openingTokens
              };
            formatTypeConstraint opening (self#core_type ct)
          | Pcf_method s pf (Cfk_virtual ct) [@implicit_arity] =>
            let opening =
              switch pf {
              | Private =>
                let privateVirtualName = [atom "private", atom "virtual", atom s.txt];
                let openingTokens =
                  makeList postSpace::true inline::(false, true) break::IfNeed privateVirtualName;
                label space::true (atom "method") openingTokens
              | Public =>
                let virtualName = [atom "virtual", atom s.txt];
                let openingTokens =
                  makeList postSpace::true inline::(false, true) break::IfNeed virtualName;
                label space::true (atom "method") openingTokens
              };
            formatTypeConstraint opening (self#core_type ct)
          | Pcf_method s pf (Cfk_concrete ovf e [@implicit_arity]) [@implicit_arity] =>
            let methodText =
              if (ovf === Override) {
                "method!"
              } else {
                "method"
              };
            /* Should refactor the binding logic so faking out the AST isn't needed,
               currently, it includes a ton of nuanced logic around recovering explicitly
               polymorphic type definitions, and that furthermore, that representation...
               Actually, let's do it.

               For some reason, concrete methods are only ever parsed as Pexp_poly.
               If there *is* no polymorphic function for the method, then the return
               value of the function is wrapped in a ghost Pexp_poly with [None] for
               the type vars.*/
            switch e.pexp_desc {
            | Pexp_poly
                {
                  pexp_desc:
                    Pexp_constraint methodFunWithNewtypes nonVarifiedExprType [@implicit_arity]
                }
                (Some {ptyp_desc: Ptyp_poly typeVars varifiedPolyType [@implicit_arity]})
              [@implicit_arity]
                when {
                  let (leadingAbstractVars, nonVarified) = self#leadingCurriedAbstractTypes methodFunWithNewtypes;
                  self#isRenderableAsPolymorphicAbstractTypes
                    typeVars
                    /* If even artificially varified. Don't know until this returns*/
                    varifiedPolyType
                    leadingAbstractVars
                    nonVarifiedExprType
                } =>
              let (leadingAbstractVars, nonVarified) = self#leadingCurriedAbstractTypes methodFunWithNewtypes;
              let fauxBindingPattern =
                switch pf {
                | Private => makeList postSpace::true break::IfNeed [atom "private", atom s.txt]
                | Public => atom s.txt
                };
              self#locallyAbstractPolymorphicFunctionBinding
                methodText
                fauxBindingPattern
                methodFunWithNewtypes
                leadingAbstractVars
                nonVarifiedExprType
            | Pexp_poly e (Some ct) [@implicit_arity] =>
              let typeLayout = SourceMap ct.ptyp_loc (self#core_type ct) [@implicit_arity];
              let appTerms = self#unparseExprApplicationItems e;
              let fauxBindingPattern =
                switch pf {
                | Private => makeList postSpace::true break::IfNeed [atom "private", atom s.txt]
                | Public => atom s.txt
                };
              self#formatSimplePatternBinding
                methodText fauxBindingPattern (Some typeLayout) appTerms
            /* This form means that there is no type constraint - it's a strange node name.*/
            | Pexp_poly e None [@implicit_arity] =>
              let (pattern, patternAux) =
                switch pf {
                | Private => (atom "private", [atom s.txt])
                | Public => (atom s.txt, [])
                };
              self#wrappedBinding methodText pattern patternAux e
            | _ => failwith "Concrete methods should only ever have Pexp_poly."
            }
          | Pcf_constraint ct1 ct2 [@implicit_arity] =>
            label
              space::true
              (atom "constraint")
              (
                makeList
                  postSpace::true
                  inline::(true, false)
                  [makeList postSpace::true [self#core_type ct1, atom "="], self#core_type ct2]
              )
          | Pcf_initializer e => label space::true (atom "initializer =>") (self#unparseExpr e)
          | Pcf_attribute a => self#floating_attribute a
          | Pcf_extension e =>
            /* And don't forget, we still need to print post_item_attributes even for
               this case */
            self#item_extension e
          };
        SourceMap x.pcf_loc itm [@implicit_arity]
      };
      method class_self_pattern_and_structure {pcstr_self: p, pcstr_fields: l} => {
        let fields = List.map self#class_field (List.filter self#shouldDisplayClassField l);
        /* Recall that by default self is bound to "this" at parse time. You'd
           have to go out of your way to bind it to "_". */
        switch (p.ppat_attributes, p.ppat_desc) {
        | ([], Ppat_var {loc, txt: "this"}) => fields
        | _ => [
            SourceMap p.ppat_loc (label space::true (atom "as") (self#pattern p))
            [@implicit_arity],
            ...fields
          ]
        }
      };
      method simple_class_expr x => {
        let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.pcl_attributes;
        if (stdAttrs != []) {
          formatSimpleAttributed
            (self#simple_class_expr {...x, pcl_attributes: []}) (self#attributes stdAttrs)
        } else {
          let itm =
            switch x.pcl_desc {
            | Pcl_constraint ce ct [@implicit_arity] =>
              formatTypeConstraint (self#class_expr ce) (self#class_constructor_type ct)
            /* In OCaml,
                 - In the most recent version of OCaml, when in the top level of a
                   module, let _ = ... is a PStr_eval.
                 - When in a function, it is a Pexp_let PPat_any
                 - When in class pre-member let bindings it is a Pcl_let PPat_any

                  Reason normalizes all of these to be simple imperative expressions
                  with trailing semicolons, *except* in the case of classes because it
                  will likely introduce a conflict with some proposed syntaxes for
                  objects.
               */
            | Pcl_let _
            | Pcl_structure _ =>
              let rows = self#classExprLetsAndRest x;
              makeList
                wrap::("{", "}")
                inline::(true, false)
                postSpace::true
                break::Always_rec
                (List.map semiTerminated rows)
            | Pcl_extension e => self#extension e
            | _ => formatPrecedence (self#class_expr x)
            };
          SourceMap x.pcl_loc itm [@implicit_arity]
        }
      };
      method classExprLetsAndRest x =>
        switch x.pcl_desc {
        | Pcl_structure cs => self#class_self_pattern_and_structure cs
        | Pcl_let rf l ce [@implicit_arity] =>
          /* For "letList" bindings, the start/end isn't as simple as with
           * module value bindings. For "let lists", the sequences were formed
           * within braces {}. The parser relocates the first let binding to the
           * first brace. */
          let bindingsLayout = self#bindings (rf, l);
          let bindingsLoc = self#bindingsLocationRange l;
          let bindingsSourceMapped = SourceMap bindingsLoc bindingsLayout [@implicit_arity];
          [bindingsSourceMapped, ...self#classExprLetsAndRest ce]
        | _ => [self#class_expr x]
        };
      method class_expr x => {
        let (arityAttrs, docAtrs, stdAttrs, jsxAttrs) = partitionAttributes x.pcl_attributes;
        /* We cannot handle the attributes here. Must handle them in each item */
        if (stdAttrs != []) {
          /* Do not need a "simple" attributes precedence wrapper. */
          formatAttributed
            (self#simple_class_expr {...x, pcl_attributes: []}) (self#attributes stdAttrs)
        } else {
          switch x.pcl_desc {
          | Pcl_fun l eo p e [@implicit_arity] =>
            label
              space::true
              (
                makeList
                  postSpace::true
                  [label space::true (atom "fun") (self#label_exp (l, eo, p)), atom "=>"]
              )
              (self#class_expr e)
          | Pcl_apply ce l [@implicit_arity] =>
            let applicationItems = self#classExpressionToFormattedApplicationItems x;
            formatAttachmentApplication applicationFinalWrapping None applicationItems
          | Pcl_constr li l [@implicit_arity] =>
            /* TODO: Allow classes to use the same syntax as every other type
               application. */
            switch l {
            | [] => label space::true (atom "class") (self#longident_loc li)
            | ll =>
              let typeParameters =
                makeList
                  break::IfNeed
                  postSpace::true
                  inline::(true, true)
                  (List.map self#non_arrowed_simple_core_type l);
              label
                space::true
                (makeList postSpace::true [atom "class", self#longident_loc li])
                typeParameters
            }
          | Pcl_constraint _
          | Pcl_extension _
          | Pcl_let _
          | Pcl_structure _ => self#simple_class_expr x
          }
        }
      };
      method signature signatureItems => {
        let signatureItems = List.filter self#shouldDisplaySigItem signatureItems;
        if (List.length signatureItems === 0) {
          atom ""
        } else {
          let signatureItems = List.filter self#shouldDisplaySigItem signatureItems;
          let first = List.nth signatureItems 0;
          let last = List.nth signatureItems (List.length signatureItems - 1);
          SourceMap
            {loc_start: first.psig_loc.loc_start, loc_end: last.psig_loc.loc_end, loc_ghost: false}
            (
              makeList
                newlinesAboveComments::1
                newlinesAboveItems::1
                newlinesAboveDocComments::1
                renderFinalSep::true
                postSpace::true
                break::Always_rec
                indent::0
                inline::(true, false)
                sep::";"
                (List.map self#signature_item signatureItems)
            )
          [@implicit_arity]
        }
      };
      method value_description x =>
        if (x.pval_prim != []) {
          makeList
            postSpace::true
            [
              self#core_type x.pval_type,
              atom "=",
              makeSpacedBreakableInlineList (List.map self#constant_string x.pval_prim)
            ]
        } else {
          self#core_type x.pval_type
        };
      method signature_item x :layoutNode => {
        let item: layoutNode =
          switch x.psig_desc {
          | Psig_type l => self#type_def_list l
          | Psig_value vd =>
            let intro =
              if (vd.pval_prim == []) {
                atom "let"
              } else {
                atom "external"
              };
            formatTypeConstraint
              (
                label
                  space::true
                  intro
                  (wrapLayoutWithLoc (Some vd.pval_name.loc) (protectIdentifier vd.pval_name.txt))
              )
              (self#value_description vd)
          | Psig_typext te => self#type_extension te
          | Psig_exception ed => self#exception_declaration ed
          | Psig_class l =>
            let class_description
                class_keyword::class_keyword=false
                ({pci_params: ls, pci_name: {txt}, pci_loc} as x) => {
              let (firstToken, pattern, patternAux) =
                self#class_opening class_keyword txt x.pci_virt ls;
              let withColon =
                self#wrapCurriedFunctionBinding
                  arrow::":"
                  firstToken
                  pattern
                  patternAux
                  ([self#class_constructor_type x.pci_expr], None);
              let itm = self#attach_std_item_attrs x.pci_attributes withColon;
              SourceMap pci_loc itm [@implicit_arity]
            };
            makeNonIndentedBreakingList (
              switch l {
              | [] => raise (NotPossible "No recursive class bindings")
              | [x] => [class_description class_keyword::true x]
              | [x, ...xs] => [
                  class_description class_keyword::true x,
                  ...List.map class_description xs
                ]
              }
            )
          | Psig_module {pmd_name, pmd_type: {pmty_desc: Pmty_alias alias}} =>
            label
              space::true
              (makeList postSpace::true [atom "module", atom pmd_name.txt, atom "="])
              (self#longident_loc alias)
          | Psig_module pmd =>
            self#formatSimpleSignatureBinding
              "module" (atom pmd.pmd_name.txt) (self#module_type pmd.pmd_type)
          | Psig_open od =>
            label
              space::true
              (atom ("open" ^ override od.popen_override))
              (self#longident_loc od.popen_lid)
          | Psig_include incl =>
            label space::true (atom "include") (self#module_type incl.pincl_mod)
          | Psig_modtype {pmtd_name: s, pmtd_type: md} =>
            switch md {
            | None => makeList postSpace::true [atom "module type", atom s.txt]
            | Some mt =>
              label
                space::true
                (makeList postSpace::true [atom "module type", atom s.txt, atom "="])
                (self#module_type mt)
            }
          | Psig_class_type l => self#class_type_declaration_list l
          | Psig_recmodule decls =>
            let first xx =>
              self#formatSimpleSignatureBinding
                "module rec" (atom xx.pmd_name.txt) (self#module_type xx.pmd_type);
            let notFirst xx =>
              self#formatSimpleSignatureBinding
                "and" (atom xx.pmd_name.txt) (self#module_type xx.pmd_type);
            let moduleBindings =
              switch decls {
              | [] => raise (NotPossible "No recursive module bindings")
              | [hd, ...tl] => [first hd, ...List.map notFirst tl]
              };
            makeNonIndentedBreakingList moduleBindings
          | Psig_attribute a => self#floating_attribute a
          | Psig_extension e a [@implicit_arity] =>
            self#attach_std_item_attrs a (self#item_extension e)
          };
        SourceMap x.psig_loc item [@implicit_arity]
      };
      method non_arrowed_module_type x =>
        switch x.pmty_desc {
        | Pmty_alias li =>
          formatPrecedence (label space::true (atom "module") (self#longident_loc li))
        | Pmty_typeof me => label space::true (atom "module type of") (self#module_expr me)
        | _ => self#simple_module_type x
        };
      method simple_module_type x =>
        switch x.pmty_desc {
        | Pmty_ident li => self#longident_loc li
        | Pmty_signature s =>
          makeList
            break::IfNeed
            inline::(true, false)
            wrap::("{", "}")
            newlinesAboveComments::0
            newlinesAboveItems::0
            newlinesAboveDocComments::1
            renderFinalSep::true
            postSpace::true
            sep::";"
            (List.map self#signature_item (List.filter self#shouldDisplaySigItem s))
        /* Not sure what this is about. */
        | Pmty_extension _ => assert false
        | _ => makeList break::IfNeed wrap::("(", ")") [self#module_type x]
        };
      method module_type x => {
        /* The segments that should be separated by arrows. */
        let rec functorTypeArgs xx =>
          switch xx.pmty_desc {
          | Pmty_functor _ None mt2 [@implicit_arity] => [atom "()", ...functorTypeArgs mt2]
          | Pmty_functor s (Some mt1) mt2 [@implicit_arity] =>
            if (s.txt == "_") {
              [self#module_type mt1, ...functorTypeArgs mt2]
            } else {
              let cur =
                makeList
                  wrap::("(", ")") [formatTypeConstraint (atom s.txt) (self#module_type mt1)];
              [cur, ...functorTypeArgs mt2]
            }
          | _ => [self#module_type xx]
          };
        let pmty =
          switch x.pmty_desc {
          | Pmty_functor _ =>
            let functorArgs = functorTypeArgs x;
            makeList
              break::IfNeed
              sep::"=>"
              preSpace::true
              postSpace::true
              inline::(true, true)
              functorArgs
          /* See comments in sugar_parser.mly about why WITH constraints aren't "non
           * arrowed" */
          | Pmty_with mt l [@implicit_arity] =>
            let modSub atm li2 token =>
              makeList postSpace::true [atom "module", atm, atom token, self#longident_loc li2];
            let typeAtom = atom "type";
            let eqAtom = atom "=";
            let destrAtom = atom ":=";
            let with_constraint = (
              fun
              | Pwith_type li ({ptype_params} as td) [@implicit_arity] =>
                self#formatOneTypeDef
                  typeAtom (SourceMap li.loc (self#longident_loc li) [@implicit_arity]) eqAtom td
              | Pwith_module li li2 [@implicit_arity] => modSub (self#longident_loc li) li2 "="
              | Pwith_typesubst ({ptype_params} as td) =>
                self#formatOneTypeDef
                  typeAtom
                  (SourceMap td.ptype_name.loc (atom td.ptype_name.txt) [@implicit_arity])
                  destrAtom
                  td
              | Pwith_modsubst s li2 [@implicit_arity] => modSub (atom s.txt) li2 ":="
            );
            switch l {
            | [] => self#module_type mt
            | _ =>
              label
                space::true
                (makeList postSpace::true [self#module_type mt, atom "with"])
                (
                  makeList
                    break::IfNeed
                    inline::(true, true)
                    sep::"and"
                    postSpace::true
                    preSpace::true
                    (List.map with_constraint l)
                )
            }
          /* Seems like an infinite loop just waiting to happen. */
          | _ => self#non_arrowed_module_type x
          };
        SourceMap x.pmty_loc pmty [@implicit_arity]
      };
      method simple_module_expr x =>
        switch x.pmod_desc {
        | Pmod_unpack e =>
          formatPrecedence (makeList postSpace::true [atom "val", self#unparseExpr e])
        | Pmod_ident li => ensureSingleTokenSticksToLabel (self#longident_loc li)
        | Pmod_constraint unconstrainedRet mt [@implicit_arity] =>
          formatPrecedence (
            formatTypeConstraint (self#module_expr unconstrainedRet) (self#module_type mt)
          )
        | Pmod_structure s =>
          makeList
            break::Always_rec
            inline::(true, false)
            wrap::("{", "}")
            newlinesAboveComments::0
            newlinesAboveItems::0
            newlinesAboveDocComments::1
            renderFinalSep::true
            postSpace::true
            sep::";"
            (List.map self#structure_item (List.filter self#shouldDisplayStructureItem s))
        | _ =>
          /* For example, functor application will be wrapped. */
          formatPrecedence (self#module_expr x)
        };
      method module_expr x =>
        switch x.pmod_desc {
        | Pmod_functor _ =>
          let (argsList, return) = self#curriedFunctorPatternsAndReturnStruct x;
          switch (argsList, return.pmod_desc) {
          | ([], _) => raise (NotPossible "functor must have some arg")
          | ([firstArg, ...restArgs], _) =>
            /* See #19/20 in syntax.mls - cannot annotate return type at
               the moment. */
            let returnedAppTerms = self#moduleExpressionToFormattedApplicationItems return;
            self#wrapCurriedFunctionBinding "fun" firstArg restArgs returnedAppTerms
          }
        | Pmod_apply me1 me2 [@implicit_arity] =>
          let appTerms = self#moduleExpressionToFormattedApplicationItems x;
          switch appTerms {
          | ([], _) => raise (NotPossible "no functor application terms")
          | ([hd], _) => raise (NotPossible "one functor application terms")
          | ([hd, ...tl], _) => formatIndentedApplication hd tl
          }
        | Pmod_extension _ => assert false
        | Pmod_unpack _
        | Pmod_ident _
        | Pmod_constraint _
        | Pmod_structure _ => self#simple_module_expr x
        };
      method structure structureItems =>
        if (List.length structureItems === 0) {
          atom ""
        } else {
          let structureItems = List.filter self#shouldDisplayStructureItem structureItems;
          let first = List.nth structureItems 0;
          let last = List.nth structureItems (List.length structureItems - 1);
          SourceMap
            {loc_start: first.pstr_loc.loc_start, loc_end: last.pstr_loc.loc_end, loc_ghost: false}
            (
              makeList
                newlinesAboveComments::1
                newlinesAboveItems::1
                newlinesAboveDocComments::1
                renderFinalSep::true
                postSpace::true
                break::Always_rec
                indent::0
                inline::(true, false)
                sep::";"
                (List.map self#structure_item structureItems)
            )
          [@implicit_arity]
        };
      /*
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
       */
      method let_module_binding prefixText bindingName moduleExpr => {
        let (argsList, return) = self#curriedFunctorPatternsAndReturnStruct moduleExpr;
        switch (argsList, return.pmod_desc) {
        /* Simple module with type constraint, no functor args. */
        | ([], Pmod_constraint unconstrainedRetTerm ct [@implicit_arity]) =>
          let appTerms = self#moduleExpressionToFormattedApplicationItems unconstrainedRetTerm;
          self#formatSimplePatternBinding
            prefixText bindingName (Some (self#module_type ct)) appTerms
        /* Simple module with type no constraint, no functor args. */
        | ([], _) =>
          let appTerms = self#moduleExpressionToFormattedApplicationItems return;
          self#formatSimplePatternBinding prefixText bindingName None appTerms
        | (_, _) =>
          /* A functor */
          let (argsWithConstraint, actualReturn) =
            switch return.pmod_desc {
            /* A functor with constrained return type:
             *
             * let module X = (A) (B) : Ret => ...
             * */
            | Pmod_constraint me ct [@implicit_arity] => (
                argsList @ [formatJustTheTypeConstraint (self#non_arrowed_module_type ct)],
                me
              )
            | _ => (argsList, return)
            };
          let returnedAppTerms = self#moduleExpressionToFormattedApplicationItems actualReturn;
          self#wrapCurriedFunctionBinding
            prefixText bindingName argsWithConstraint returnedAppTerms
        }
      };
      method class_opening class_keyword name pci_virt ls => {
        let firstToken = if class_keyword {"class"} else {"and"};
        switch (pci_virt, ls) {
        /* When no class params, it's a very simple formatting for the
           opener - no breaking. */
        | (Virtual, []) => (firstToken, atom "virtual", [atom name])
        | (Concrete, []) => (firstToken, atom name, [])
        | (Virtual, [_, ..._]) => (
            firstToken,
            atom "virtual",
            [atom name, self#class_params_def ls]
          )
        | (Concrete, [_, ..._]) => (firstToken, atom name, [self#class_params_def ls])
        }
      };
      /* TODO: TODOATTRIBUTES: Structure items don't have attributes, but each
         pstr_desc */
      method structure_item term => {
        let item =
          switch term.pstr_desc {
          | Pstr_eval e _attrs [@implicit_arity] => self#unparseExpr e
          | Pstr_type [] => assert false
          | Pstr_type l => self#type_def_list l
          | Pstr_value rf l [@implicit_arity] => self#bindings (rf, l)
          | Pstr_typext te => self#type_extension te
          | Pstr_exception ed => self#exception_declaration ed
          | Pstr_module x =>
            let prefixText = "module";
            let bindingName = atom loc::x.pmb_name.loc x.pmb_name.txt;
            let moduleExpr = x.pmb_expr;
            self#let_module_binding prefixText bindingName moduleExpr
          | Pstr_open od =>
            makeList
              postSpace::true
              [atom ("open" ^ override od.popen_override), self#longident_loc od.popen_lid]
          | Pstr_modtype {pmtd_name: s, pmtd_type: md} =>
            switch md {
            | None => makeList postSpace::true [atom "module type", atom s.txt]
            | Some mt =>
              label
                space::true
                (makeList postSpace::true [atom "module type", atom s.txt, atom "="])
                (self#module_type mt)
            }
          | Pstr_class l => self#class_declaration_list l
          | Pstr_class_type l => self#class_type_declaration_list l
          | Pstr_primitive vd =>
            let attrs = List.map (fun x => self#item_attribute x) vd.pval_attributes;
            let lst =
              List.append
                [
                  atom "external",
                  protectIdentifier vd.pval_name.txt,
                  atom ":",
                  self#value_description vd
                ]
                attrs;
            makeList postSpace::true lst
          | Pstr_include incl =>
            /* Kind of a hack */
            let moduleExpr = incl.pincl_mod;
            let returnedAppTerms = self#moduleExpressionToFormattedApplicationItems moduleExpr;
            formatAttachmentApplication
              applicationFinalWrapping (Some (true, atom "include")) returnedAppTerms
          | Pstr_recmodule decls =>
            /* 3.07 */
            let first xx => {
              let prefixText = "module rec";
              self#let_module_binding prefixText (atom xx.pmb_name.txt) xx.pmb_expr
            };
            let notFirst xx => {
              let prefixText = "and";
              self#let_module_binding prefixText (atom xx.pmb_name.txt) xx.pmb_expr
            };
            let moduleBindings =
              switch decls {
              | [] => raise (NotPossible "No recursive module bindings")
              | [hd, ...tl] => [first hd, ...List.map notFirst tl]
              };
            makeNonIndentedBreakingList moduleBindings
          | Pstr_attribute a => self#floating_attribute a
          | Pstr_extension e a [@implicit_arity] =>
            /* Notice how extensions have attributes - but not every structure
               item does. */
            self#item_extension e
          };
        SourceMap term.pstr_loc item [@implicit_arity]
      };
      method type_extension = wrap default#type_extension;
      method extension_constructor = wrap default#extension_constructor;
      /* [allowUnguardedSequenceBodies] allows sequence expressions {} to the right of `=>` to not
         be guarded in `{}` braces. */
      method case_list allowUnguardedSequenceBodies::allowUnguardedSequenceBodies=false l => {
        let rec appendLabelToLast items rhs =>
          switch items {
          | [hd] => [label indent::0 space::true hd rhs]
          | [hd, ...tl] => [hd, ...appendLabelToLast tl rhs]
          | [] => raise (NotPossible "Cannot append to last of nothing")
          };
        let case_row {pc_lhs, pc_guard, pc_rhs} => {
          let theOrs = orList pc_lhs;
          /* match x with */
          /* | AnotherReallyLongVariantName (_, _, _)   */
          /* | AnotherReallyLongVariantName2 (_, _, _)
             when true => {                           */
          /*   }                                        */
          /*<sbi><X>match x with</X>   */
          /*     <Y>everythingElse</Y> */
          /*</sbi>                     */
          /*     ............................................................
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

                 */
          let bar xx => makeList postSpace::true [atom "|", xx];
          let appendWhereAndArrow p =>
            switch pc_guard {
            | None => makeList interleaveComments::false postSpace::true [p, atom "=>"]
            | Some g =>
              /* when x should break as a whole - extra list added around it to make it break as one */
              let withWhen =
                label
                  space::true
                  p
                  (
                    makeList
                      break::Never
                      inline::(true, true)
                      postSpace::true
                      [label space::true (atom "when") (self#unparseExpr g)]
                  );
              makeList
                interleaveComments::false
                inline::(true, true)
                postSpace::true
                [withWhen, atom "=>"]
            };
          let rec appendWhereAndArrowToLastOr =
            fun
            | [] => []
            | [hd, ...tl] => {
                let formattedHd =
                  switch tl {
                  | [] => appendWhereAndArrow (self#pattern hd)
                  | [tl, ...tlTl] => self#pattern hd
                  };
                [formattedHd, ...appendWhereAndArrowToLastOr tl]
              };
          let orsWithWhereAndArrowOnLast = appendWhereAndArrowToLastOr theOrs;
          let rhs =
            if allowUnguardedSequenceBodies {
              switch (self#under_pipe#letList pc_rhs) {
              /* TODO: Still render a list with located information here so that
                 comments (eol) are interleaved */
              | [hd] => hd
              /* In this case, we don't need any additional indentation, because there aren't
                 wrapping {} which would cause zero indentation to look strange. */
              | lst => makeUngaurdedLetSequence lst
              }
            } else {
              self#under_pipe#unparseExpr pc_rhs
            };
          let row = {
            let withoutBars = appendLabelToLast orsWithWhereAndArrowOnLast rhs;
            makeList break::Always_rec inline::(true, true) (List.map bar withoutBars)
          };
          SourceMap
            /* Fake shift the location to accomodate for the bar, to make sure
             * the wrong comments don't make their way past the next bar. */
            (
              expandLocation
                expand::(0, 0)
                {
                  loc_start: pc_lhs.ppat_loc.loc_start,
                  loc_end: pc_rhs.pexp_loc.loc_end,
                  loc_ghost: false
                }
            )
            row
          [@implicit_arity]
        };
        List.map case_row l
      };
      method label_x_expression_param (l, e) => {
        let param =
          switch l {
          | "" => self#simplifyUnparseExpr e /* level 2*/
          | lbl =>
            if (lbl.[0] == '?') {
              let str = String.sub lbl 1 (String.length lbl - 1);
              formatLabeledArgument (atom str) "?" (self#simplifyUnparseExpr e)
            } else {
              formatLabeledArgument (atom lbl) "" (self#simplifyUnparseExpr e)
            }
          };
        SourceMap e.pexp_loc param [@implicit_arity]
      };
      method directive_argument = wrap default#directive_argument;
      method toplevel_phrase = wrap default#toplevel_phrase;
    };
    let easy = (new printer) ();
    let toplevel_phrase f x =>
      switch x {
      | Ptop_def s => easyFormatToFormatter f (layoutToEasyFormatNoComments (easy#structure s))
      | Ptop_dir s da [@implicit_arity] => print_string "(* top directives not supported *)"
      };
    let case_list f x => {
      let l = easy#case_list x;
      List.iter (fun x => easyFormatToFormatter f (layoutToEasyFormatNoComments x)) l
    };
    let top_phrase f x => {
      pp_print_newline f ();
      toplevel_phrase f x;
      pp f ";;";
      pp_print_newline f ()
    };
    /* Convert a Longident to a list of strings.
         E.g. M.Constructor will be ["Constructor"; "M.Constructor"]
         Also support ".Constructor" to specify access without a path.
       */
    let longident_for_arity lid => {
      let rec toplevel =
        fun
        | Lident s => [s]
        | Ldot lid s [@implicit_arity] => {
            let append_s x => x ^ "." ^ s;
            [s, ...List.map append_s (toplevel lid)]
          }
        | Lapply y s [@implicit_arity] => toplevel s;
      switch lid {
      | Lident s => ["." ^ s, ...toplevel lid]
      | _ => toplevel lid
      }
    };
    /* add expilcit_arity to a list of attributes
     */
    let add_explicit_arity loc attributes => [
      ({txt: "explicit_arity", loc}, PStr []),
      ...normalized_attributes "explicit_arity" attributes
    ];
    /* explicit_arity_exists check if expilcit_arity exists
     */
    let explicit_arity_not_exists attributes => not (attribute_exists "explicit_arity" attributes);
    /* wrap_expr_with_tuple wraps an expression
     * with tuple as a sole argument.
     */
    let wrap_expr_with_tuple exp => {...exp, pexp_desc: Pexp_tuple [exp]};
    /* wrap_pat_with_tuple wraps an pattern
     * with tuple as a sole argument.
     */
    let wrap_pat_with_tuple pat => {...pat, ppat_desc: Ppat_tuple [pat]};
    /* explicit_arity_constructors is a set of constructors that are known to have
     * multiple arguments
     *
     */
    let module StringSet = Set.Make String;
    let built_in_explicit_arity_constructors = ["Some", "Assert_failure", "Match_failure"];
    let explicit_arity_constructors = StringSet.of_list (
      built_in_explicit_arity_constructors @ (!configuredSettings).constructorLists
    );
    let add_explicit_arity_mapper = {
      ...default_mapper,
      expr: fun mapper expr => {
        let expr =
          switch expr {
          | {pexp_desc: Pexp_construct lid (Some sp) [@implicit_arity], pexp_loc, pexp_attributes}
              when
                List.exists
                  (fun c => StringSet.mem c explicit_arity_constructors)
                  (longident_for_arity lid.txt) &&
                explicit_arity_not_exists pexp_attributes => {
              pexp_desc: Pexp_construct lid (Some (wrap_expr_with_tuple sp)) [@implicit_arity],
              pexp_loc,
              pexp_attributes: add_explicit_arity pexp_loc pexp_attributes
            }
          | x => x
          };
        default_mapper.expr mapper expr
      },
      pat: fun mapper pat => {
        let pat =
          switch pat {
          | {ppat_desc: Ppat_construct lid (Some sp) [@implicit_arity], ppat_loc, ppat_attributes}
              when
                List.exists
                  (fun c => StringSet.mem c explicit_arity_constructors)
                  (longident_for_arity lid.txt) &&
                explicit_arity_not_exists ppat_attributes => {
              ppat_desc: Ppat_construct lid (Some (wrap_pat_with_tuple sp)) [@implicit_arity],
              ppat_loc,
              ppat_attributes: add_explicit_arity ppat_loc ppat_attributes
            }
          | x => x
          };
        default_mapper.pat mapper pat
      }
    };
    let preprocessing_chain = [
      add_explicit_arity_mapper,
      escape_stars_slashes_mapper,
      ml_to_reason_swap_operator_mapper
    ];
    let core_type f x =>
      easyFormatToFormatter
        f
        (
          layoutToEasyFormatNoComments (
            easy#core_type (apply_mapper_chain_to_type x preprocessing_chain)
          )
        );
    let pattern f x =>
      easyFormatToFormatter
        f
        (
          layoutToEasyFormatNoComments (
            easy#pattern (apply_mapper_chain_to_pattern x preprocessing_chain)
          )
        );
    let signature (comments: commentWithCategory) f x =>
      easyFormatToFormatter
        f
        (
          layoutToEasyFormat
            (easy#signature (apply_mapper_chain_to_signature x preprocessing_chain)) comments
        );
    let structure (comments: commentWithCategory) f x =>
      easyFormatToFormatter
        f
        (
          layoutToEasyFormat
            (easy#structure (apply_mapper_chain_to_structure x preprocessing_chain)) comments
        );
    let expression f x =>
      easyFormatToFormatter
        f
        (
          layoutToEasyFormatNoComments (
            easy#unparseExpr (apply_mapper_chain_to_expr x preprocessing_chain)
          )
        );
    let case_list = case_list;
  };
  {
    as _;
    method core_type = Formatter.core_type;
    method pattern = Formatter.pattern;
    method signature = Formatter.signature;
    method structure = Formatter.structure;
    /* For merlin-destruct */
    method toplevel_phrase = Formatter.toplevel_phrase;
    method expression = Formatter.expression;
    method case_list = Formatter.case_list
  }
};

let defaultSettings = defaultSettings;
