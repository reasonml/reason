[@@@warning "-27-32-51-37-34"]

open Reason_omp
open Ast_411
open Asttypes
open Location
open Longident
open Parsetree
open PPrint
open Reason_syntax_util
open Reason_attributes
module Comment = Reason_comment
module Layout = Reason_layout
module WhitespaceRegion = Layout.WhitespaceRegion
module Range = Reason_location.Range

let source_map = Layout.source_map

exception NotPossible of string

let commaTrail =
  Layout.SepFinal (",", Reason_syntax_util.TrailingCommaMarker.string)

let commaSep = Layout.Sep ","

type ruleInfoData = {
  reducePrecedence : precedence;
  shiftPrecedence : precedence;
}

and ruleCategory =
  (* Printing will be parsed with very high precedence, so not much need to
     worry about ensuring it will reduce correctly. In short, you can put
     `FunctionApplication` content anywhere around an infix identifier without
     wrapping in parens. For example `myFunc x y z` or `if x {y} else {z}`
     The layout is kept in list form only to allow for elegant wrapping rules
     to take into consideration the *number* of high precedence parsed items. *)
  | FunctionApplication of Layout.t list
  (* Care should be taken to ensure the rule that caused it to be parsed will
     reduce again on the printed output - context should carefully consider
     wrapping in parens according to the ruleInfoData. *)
  | SpecificInfixPrecedence of ruleInfoData * resolvedRule
  (* Not safe to include anywhere between infix operators without wrapping in
     parens. This describes expressions like `fun x => x` which doesn't fit into
     our simplistic algorithm for printing function applications separated by infix.

     It might be possible to include these in between infix, but there are
     tricky rules to determining when these must be guarded by parens (it
     depends highly on context that is hard to reason about). It's so nuanced
     that it's easier just to always wrap them in parens. *)
  | PotentiallyLowPrecedence of Layout.t
  (* Simple means it is clearly one token (such as (anything) or [anything] or identifier *)
  | Simple of Layout.t

(* Represents a ruleCategory where the precedence has been resolved.
 * The precedence of a ruleCategory gets resolved in `ensureExpression` or
 * `ensureContainingRule`. The result is either a plain Layout.t (where
 * parens probably have been applied) or an InfixTree containing the operator and
 * a left & right resolvedRule. The latter indicates that the precedence has been resolved,
 * but the actual formatting is deferred to a later stadium.
 * Think `let x = foo |> f |> z |>`, which requires a certain formatting style when
 * things break over multiple lines. *)
and resolvedRule =
  | LayoutNode of Layout.t
  | InfixTree of string * resolvedRule * resolvedRule

and associativity = Right | Nonassoc | Left
and precedenceEntryType = TokenPrecedence | CustomPrecedence
and precedence = Token of string | Custom of string

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
  | UnaryNotPrefix of string
  | UnaryPostfix of string
  | Infix of string
  | Letop of string
  | Andop of string
  | Normal

(* Type which represents a resolvedRule's InfixTree flattened *)
type infixChain = InfixToken of string | Layout of Layout.t

(* Helpers for dealing with extension nodes (%expr) *)

let expression_extension_sugar x =
  if x.pexp_attributes != [] then None
  else
    match x.pexp_desc with
    | Pexp_extension (name, PStr [ { pstr_desc = Pstr_eval (expr, []) } ])
      when name.txt <> "bs.obj" ->
        Some (name, expr)
    | _ -> None

let expression_immediate_extension_sugar x =
  match expression_extension_sugar x with
  | None -> (None, x)
  | Some (name, expr) -> (
      match expr.pexp_desc with
      | Pexp_for _ | Pexp_while _ | Pexp_ifthenelse _ | Pexp_function _
      | Pexp_newtype _ | Pexp_try _ | Pexp_match _ ->
          (Some name, expr)
      | _ -> (None, x))

let expression_not_immediate_extension_sugar x =
  match expression_immediate_extension_sugar x with
  | Some _, _ -> None
  | None, _ -> expression_extension_sugar x

let add_extension_sugar keyword = function
  | None -> keyword
  | Some str -> keyword ^ "%" ^ str.txt

let string_equal : string -> string -> bool = ( = )

let string_loc_equal :
    string Ast_411.Asttypes.loc -> string Ast_411.Asttypes.loc -> bool =
 fun l1 l2 -> l1.txt = l2.txt

let longident_same l1 l2 =
  let rec equal l1 l2 =
    match (l1, l2) with
    | Lident l1, Lident l2 -> string_equal l1 l2
    | Ldot (path1, l1), Ldot (path2, l2) ->
        equal path1 path2 && string_equal l1 l2
    | Lapply (l11, l12), Lapply (l21, l22) -> equal l11 l21 && equal l12 l22
    | _ -> false
  in
  equal l1.txt l2.txt

(* A variant of List.for_all2 that returns false instead of failing on lists
   of different size *)
let for_all2' pred l1 l2 =
  List.length l1 = List.length l2 && List.for_all2 pred l1 l2

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
  let rec loop t1 t2 =
    match (t1.ptyp_desc, t2.ptyp_desc) with
    (* Importantly, cover the case where type constructors (of the form [a])
       are converted to type vars of the form ['a].
    *)
    | Ptyp_constr ({ txt = Lident s1 }, []), Ptyp_var s2 -> string_equal s1 s2
    (* Now cover the case where type variables (of the form ['a]) are
       converted to type constructors of the form [a].
    *)
    | Ptyp_var s1, Ptyp_constr ({ txt = Lident s2 }, []) -> string_equal s1 s2
    (* Now cover the typical case *)
    | Ptyp_constr (longident1, lst1), Ptyp_constr (longident2, lst2) ->
        longident_same longident1 longident2 && for_all2' loop lst1 lst2
    | Ptyp_any, Ptyp_any -> true
    | Ptyp_var x1, Ptyp_var x2 -> string_equal x1 x2
    | ( Ptyp_arrow (label1, core_type1, core_type1'),
        Ptyp_arrow (label2, core_type2, core_type2') ) ->
        (match (label1, label2) with
        | Nolabel, Nolabel -> true
        | Labelled s1, Labelled s2 -> string_equal s1 s2
        | Optional s1, Optional s2 -> string_equal s1 s2
        | _ -> false)
        && loop core_type1 core_type2
        && loop core_type1' core_type2'
    | Ptyp_tuple lst1, Ptyp_tuple lst2 -> for_all2' loop lst1 lst2
    | Ptyp_object (lst1, o1), Ptyp_object (lst2, o2) ->
        let tester t1 t2 =
          match (t1.pof_desc, t2.pof_desc) with
          | Otag (s1, t1), Otag (s2, t2) ->
              string_equal s1.txt s2.txt && loop t1 t2
          | Oinherit t1, Oinherit t2 -> loop t1 t2
          | _ -> false
        in
        for_all2' tester lst1 lst2 && o1 = o2
    | Ptyp_class (longident1, lst1), Ptyp_class (longident2, lst2) ->
        longident_same longident1 longident2 && for_all2' loop lst1 lst2
    | Ptyp_alias (core_type1, string1), Ptyp_alias (core_type2, string2) ->
        loop core_type1 core_type2 && string_equal string1 string2
    | ( Ptyp_variant (row_field_list1, flag1, lbl_lst_option1),
        Ptyp_variant (row_field_list2, flag2, lbl_lst_option2) ) ->
        for_all2' rowFieldEqual row_field_list1 row_field_list2
        && flag1 = flag2
        && lbl_lst_option1 = lbl_lst_option2
    | Ptyp_poly (string_lst1, core_type1), Ptyp_poly (string_lst2, core_type2)
      ->
        for_all2' string_loc_equal string_lst1 string_lst2
        && loop core_type1 core_type2
    | Ptyp_package (longident1, lst1), Ptyp_package (longident2, lst2) ->
        longident_same longident1 longident2
        && for_all2' testPackageType lst1 lst2
    | Ptyp_extension (s1, _), Ptyp_extension (s2, _) ->
        string_equal s1.txt s2.txt
    | _ -> false
  and testPackageType (lblLongIdent1, ct1) (lblLongIdent2, ct2) =
    longident_same lblLongIdent1 lblLongIdent2 && loop ct1 ct2
  and rowFieldEqual f1 f2 =
    match (f1.prf_desc, f2.prf_desc) with
    | Rtag (label1, flag1, lst1), Rtag (label2, flag2, lst2) ->
        string_equal label1.txt label2.txt
        && flag1 = flag2 && for_all2' loop lst1 lst2
    | Rinherit t1, Rinherit t2 -> loop t1 t2
    | _ -> false
  in
  loop t1 t2

let expandLocation pos ~expand:(startPos, endPos) =
  {
    pos with
    loc_start =
      {
        pos.loc_start with
        Lexing.pos_cnum = pos.loc_start.Lexing.pos_cnum + startPos;
      };
    loc_end =
      {
        pos.loc_end with
        Lexing.pos_cnum = pos.loc_end.Lexing.pos_cnum + endPos;
      };
  }

(* Computes the location of the attribute with the lowest line number
 * that isn't ghost. Useful to determine the start location of an item
 * in the parsetree that has attributes.
 * If there are no valid attributes, defaults to the passed location.
 * 1| [@attr]           --> notice how the "start" is determined
 * 2| let f = ...           by the attr on line 1, not the lnum of the `let`
 *)
let rec firstAttrLoc loc = function
  | ({ attr_name = attrLoc; _ } : Parsetree.attribute) :: attrs ->
      if
        attrLoc.loc.loc_start.pos_lnum < loc.loc_start.pos_lnum
        && not attrLoc.loc.loc_ghost
      then firstAttrLoc attrLoc.loc attrs
      else firstAttrLoc loc attrs
  | [] -> loc

let extractLocationFromValBindList expr vbs =
  let rec extract loc = function
    | x :: xs ->
        let { pvb_expr } = x in
        let loc = { loc with loc_end = pvb_expr.pexp_loc.loc_end } in
        extract loc xs
    | [] -> loc
  in
  let loc =
    match vbs with
    | x :: xs ->
        let { pvb_pat; pvb_expr } = x in
        let loc =
          { pvb_pat.ppat_loc with loc_end = pvb_expr.pexp_loc.loc_end }
        in
        extract loc xs
    | [] -> expr.pexp_loc
  in
  { loc with loc_start = expr.pexp_loc.loc_start }

let extractLocValBinding { pvb_pat; pvb_expr; pvb_attributes } =
  let estimatedLoc = firstAttrLoc pvb_pat.ppat_loc pvb_attributes in
  { estimatedLoc with loc_end = pvb_expr.pexp_loc.loc_end }

let extractLocBindingOp { pbop_pat; pbop_exp } =
  let estimatedLoc = firstAttrLoc pbop_pat.ppat_loc [] in
  { estimatedLoc with loc_end = pbop_exp.pexp_loc.loc_end }

let extractLocModuleBinding { pmb_expr; pmb_attributes } =
  let estimatedLoc = firstAttrLoc pmb_expr.pmod_loc pmb_attributes in
  { estimatedLoc with loc_end = pmb_expr.pmod_loc.loc_end }

let extractLocModDecl { pmd_type; pmd_attributes } =
  let estimatedLoc = firstAttrLoc pmd_type.pmty_loc pmd_attributes in
  { estimatedLoc with loc_end = pmd_type.pmty_loc.loc_end }

let rec sequentialIfBlocks x =
  match x with
  | Some { pexp_desc = Pexp_ifthenelse (e1, e2, els) } ->
      let nestedIfs, finalExpression = sequentialIfBlocks els in
      ((e1, e2) :: nestedIfs, finalExpression)
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
let almost_simple_prefix_symbols = [ '!'; '?'; '~' ]

(* Subset of prefix symbols that have special "unary precedence" *)
let unary_minus_prefix_symbols = [ "~-"; "~-." ]
let unary_plus_prefix_symbols = [ "~+"; "~+." ]

let infix_symbols =
  [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/'; '$'; '%'; '\\'; '#' ]

(* this should match "kwdopchar" from reason_declarative_lexer.mll *)
let special_infix_strings =
  [ "asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "!==" ]

(* REMOVE *)
let updateToken = "="
let sharpOpEqualToken = "#="
let pipeFirstToken = "->"
let requireIndentFor = [ updateToken; ":=" ]
let namedArgSym = "~"

let requireNoSpaceFor tok =
  tok = pipeFirstToken || (tok.[0] = '#' && tok <> "#=")

let funToken = "fun"

let getPrintableUnaryIdent s =
  if
    List.mem s unary_minus_prefix_symbols
    || List.mem s unary_plus_prefix_symbols
  then String.sub s 1 (String.length s - 1)
  else s

(* determines if the string is an infix string.
   checks backwards, first allowing a renaming postfix ("_102") which
   may have resulted from Pexp -> Texp -> Pexp translation, then checking
   if all the characters in the beginning of the string are valid infix
   characters. *)
let printedStringAndFixity = function
  | s when List.mem s special_infix_strings -> Infix s
  | "^" -> UnaryPostfix "^"
  | s when List.mem s.[0] infix_symbols -> Infix s
  (* Correctness under assumption that unary operators are stored in AST with
     leading "~" *)
  | s
    when List.mem s.[0] almost_simple_prefix_symbols
         && (not (List.mem s special_infix_strings))
         && not (s = "?") ->
      if (* What *kind* of prefix fixity? *)
         List.mem s unary_plus_prefix_symbols
      then UnaryPlusPrefix (getPrintableUnaryIdent s)
      else if List.mem s unary_minus_prefix_symbols then
        UnaryMinusPrefix (getPrintableUnaryIdent s)
      else if s = "!" then UnaryNotPrefix s
      else AlmostSimplePrefix s
  | s when is_letop s -> Letop s
  | s when is_andop s -> Andop s
  | _ -> Normal

(* Also, this doesn't account for != and !== being infixop!!! *)
let isSimplePrefixToken s =
  match printedStringAndFixity s with
  | AlmostSimplePrefix _ | UnaryPostfix "^" -> true
  | _ -> false

(* Convenient bank of information that represents the parser's precedence
   rankings.  Each instance describes a precedence table entry. The function
   tests either a token string encountered by the parser, or (in the case of
   `CustomPrecedence`) the string name of a custom rule precedence declared
   using %prec *)
let rules =
  [
    [
      (TokenPrecedence, fun s -> (Left, s = pipeFirstToken));
      ( TokenPrecedence,
        fun s -> (Left, s.[0] = '#' && s <> sharpOpEqualToken && s <> "#") );
      (TokenPrecedence, fun s -> (Left, s = "."));
      (CustomPrecedence, fun s -> (Left, s = "prec_lbracket"));
    ];
    [ (CustomPrecedence, fun s -> (Nonassoc, s = "prec_functionAppl")) ];
    [ (TokenPrecedence, fun s -> (Right, isSimplePrefixToken s)) ];
    [ (TokenPrecedence, fun s -> (Left, s = sharpOpEqualToken)) ];
    [ (CustomPrecedence, fun s -> (Nonassoc, s = "prec_unary")) ];
    (* Note the special case for "*\*", BARBAR, and LESSMINUS, AMPERSAND(s) *)
    [
      (TokenPrecedence, fun s -> (Right, s = "**"));
      ( TokenPrecedence,
        fun s ->
          ( Right,
            String.length s > 1 && s.[0] == '*' && s.[1] == '\\' && s.[2] == '*'
          ) );
      (TokenPrecedence, fun s -> (Right, s = "lsl"));
      (TokenPrecedence, fun s -> (Right, s = "lsr"));
      (TokenPrecedence, fun s -> (Right, s = "asr"));
    ];
    [
      ( TokenPrecedence,
        fun s -> (Left, s.[0] == '*' && (String.length s == 1 || s != "*\\*"))
      );
      (TokenPrecedence, fun s -> (Left, s.[0] == '/'));
      (TokenPrecedence, fun s -> (Left, s.[0] == '%'));
      (TokenPrecedence, fun s -> (Left, s = "mod"));
      (TokenPrecedence, fun s -> (Left, s = "land"));
      (TokenPrecedence, fun s -> (Left, s = "lor"));
      (TokenPrecedence, fun s -> (Left, s = "lxor"));
    ];
    [
      (* Even though these use the same *tokens* as unary plus/minus at parse
         time, when unparsing infix -/+, the CustomPrecedence rule would be
         incorrect to use, and instead we need a rule that models what infix
         parsing would use - just the regular token precedence without a custom
         precedence. *)
      ( TokenPrecedence,
        fun s ->
          ( Left,
            if String.length s > 1 && s.[0] == '+' && s.[1] == '+' then
              (*
          Explicitly call this out as false because the other ++ case below
          should have higher *lexing* priority. ++operator_chars* is considered an
          entirely different token than +(non_plus_operator_chars)*
        *)
              false
            else s.[0] == '+' ) );
      (TokenPrecedence, fun s -> (Left, s.[0] == '-' && s <> pipeFirstToken));
      (TokenPrecedence, fun s -> (Left, s = "!"));
    ];
    [ (TokenPrecedence, fun s -> (Right, s = "::")) ];
    [
      (TokenPrecedence, fun s -> (Right, s.[0] == '@'));
      (TokenPrecedence, fun s -> (Right, s.[0] == '^'));
      ( TokenPrecedence,
        fun s -> (Right, String.length s > 1 && s.[0] == '+' && s.[1] == '+') );
    ];
    [
      ( TokenPrecedence,
        fun s -> (Left, s.[0] == '=' && (not (s = "=")) && not (s = "=>")) );
      (TokenPrecedence, fun s -> (Left, s.[0] == '<' && not (s = "<")));
      (TokenPrecedence, fun s -> (Left, s.[0] == '>' && not (s = ">")));
      (TokenPrecedence, fun s -> (Left, s = "!="));
      (* Not preset in the RWO table! *)
      (TokenPrecedence, fun s -> (Left, s = "!=="));
      (* Not preset in the RWO table! *)
      (TokenPrecedence, fun s -> (Left, s = "=="));
      (TokenPrecedence, fun s -> (Left, s = "==="));
      (TokenPrecedence, fun s -> (Left, s = "<"));
      (TokenPrecedence, fun s -> (Left, s = ">"));
      (TokenPrecedence, fun s -> (Left, s.[0] == '|' && not (s = "||")));
      ( TokenPrecedence,
        fun s -> (Left, s.[0] == '&' && (not (s = "&")) && not (s = "&&")) );
      (TokenPrecedence, fun s -> (Left, s.[0] == '$'));
    ];
    [ (CustomPrecedence, fun s -> (Left, s = funToken)) ];
    [
      (TokenPrecedence, fun s -> (Right, s = "&"));
      (TokenPrecedence, fun s -> (Right, s = "&&"));
    ];
    [
      (TokenPrecedence, fun s -> (Right, s = "or"));
      (TokenPrecedence, fun s -> (Right, s = "||"));
    ];
    [
      (* The Left shouldn't ever matter in practice. Should never get in a
         situation with two consecutive infix ? - the colon saves us. *)
      (TokenPrecedence, fun s -> (Left, s = "?"));
    ];
    [ (TokenPrecedence, fun s -> (Right, s = ":=")) ];
    [ (TokenPrecedence, fun s -> (Right, s = updateToken)) ];
    (* It's important to account for ternary ":" being lower precedence than "?" *)
    [ (TokenPrecedence, fun s -> (Right, s = ":")) ];
    [ (TokenPrecedence, fun s -> (Nonassoc, s = "=>")) ];
  ]

(* remove all prefixing backslashes, e.g. \=== becomes === *)
let without_prefixed_backslashes str =
  if str = "" then str
  else if String.get str 0 = '\\' then String.sub str 1 (String.length str - 1)
  else str

let indexOfFirstMatch ~prec lst =
  let rec aux n = function
    | [] -> None
    | [] :: tl -> aux (n + 1) tl
    | ((kind, tester) :: hdTl) :: tl -> (
        match (prec, kind) with
        | Token str, TokenPrecedence | Custom str, CustomPrecedence ->
            let associativity, foundMatch = tester str in
            if foundMatch then Some (associativity, n) else aux n (hdTl :: tl)
        | _ -> aux n (hdTl :: tl))
  in
  aux 0 lst

(* Assuming it's an infix function application. *)
let precedenceInfo ~prec =
  (* Removes prefixed backslashes in order to do proper conversion *)
  let prec =
    match prec with
    | Token str -> Token (without_prefixed_backslashes str)
    | Custom _ -> prec
  in
  indexOfFirstMatch ~prec rules

let isLeftAssociative ~prec =
  match precedenceInfo ~prec with
  | None -> false
  | Some (Left, _) -> true
  | Some (Right, _) -> false
  | Some (Nonassoc, _) -> false

let isRightAssociative ~prec =
  match precedenceInfo ~prec with
  | None -> false
  | Some (Right, _) -> true
  | Some (Left, _) -> false
  | Some (Nonassoc, _) -> false

let higherPrecedenceThan c1 c2 =
  match (precedenceInfo ~prec:c1, precedenceInfo ~prec:c2) with
  | _, None | None, _ ->
      let str1, str2 =
        match (c1, c2) with
        | Token s1, Token s2 -> ("Token " ^ s1, "Token " ^ s2)
        | Token s1, Custom s2 -> ("Token " ^ s1, "Custom " ^ s2)
        | Custom s1, Token s2 -> ("Custom " ^ s1, "Token " ^ s2)
        | Custom s1, Custom s2 -> ("Custom " ^ s1, "Custom " ^ s2)
      in
      raise
        (NotPossible
           ("Cannot determine precedence of two checks " ^ str1 ^ " vs. " ^ str2))
  | Some (_, p1), Some (_, p2) -> p1 < p2

let printedStringAndFixityExpr = function
  | { pexp_desc = Pexp_ident { txt = Lident l } } -> printedStringAndFixity l
  | _ -> Normal

(* which identifiers are in fact operators needing parentheses *)
let needs_parens txt =
  match printedStringAndFixity txt with
  | Infix _ -> true
  | UnaryPostfix _ -> true
  | UnaryPlusPrefix _ -> true
  | UnaryMinusPrefix _ -> true
  | UnaryNotPrefix _ -> true
  | AlmostSimplePrefix _ -> true
  | Letop _ -> true
  | Andop _ -> true
  | Normal -> false

(* some infixes need spaces around parens to avoid clashes with comment
   syntax. This isn't needed for comment syntax /* */ *)
let needs_spaces txt = txt.[0] = '*' || txt.[String.length txt - 1] = '*'

let rec orList = function
  (* only consider ((A|B)|C)*)
  | { ppat_desc = Ppat_or (p1, p2) } -> orList p1 @ orList p2
  | x -> [ x ]

let override = function Override -> "!" | Fresh -> ""

(* variance encoding: need to sync up with the [parser.mly] *)
let type_variance = function
  | Invariant -> ""
  | Covariant -> "+"
  | Contravariant -> "-"

let moduleIdent ident = match ident.txt with None -> "_" | Some name -> name

type construct =
  [ `cons of expression list
  | `list of expression list
  | `nil
  | `normal
  | `simple of Longident.t
  | `tuple ]

let view_expr x =
  match x.pexp_desc with
  | Pexp_construct ({ txt = Lident "()" }, _) -> `tuple
  | Pexp_construct ({ txt = Lident "[]" }, _) -> `nil
  | Pexp_construct ({ txt = Lident "::" }, Some _) ->
      let rec loop exp acc =
        match exp with
        | { pexp_desc = Pexp_construct ({ txt = Lident "[]" }, _) } ->
            (List.rev acc, true)
        | {
         pexp_desc =
           Pexp_construct
             ({ txt = Lident "::" }, Some { pexp_desc = Pexp_tuple [ e1; e2 ] });
        } ->
            loop e2 (e1 :: acc)
        | e -> (List.rev (e :: acc), false)
      in
      let ls, b = loop x [] in
      if b then `list ls else `cons ls
  | Pexp_construct (x, None) -> `simple x.txt
  | _ -> `normal

let is_simple_list_expr x =
  match view_expr x with `list _ | `cons _ -> true | _ -> false

let is_simple_construct : construct -> bool = function
  | `nil | `tuple | `list _ | `simple _ | `cons _ -> true
  | `normal -> false

let uncurriedTable = Hashtbl.create 42

(* Determines if a list of expressions contains a single unit construct
 * e.g. used to check: MyConstructor() -> exprList == [()]
 * useful to determine if MyConstructor(()) should be printed as MyConstructor()
 * *)
let is_single_unit_construct exprList =
  match exprList with
  | x :: [] -> (
      let view = view_expr x in
      match view with `tuple -> true | _ -> false)
  | _ -> false

let detectTernary l =
  match l with
  | [
   {
     pc_lhs = { ppat_desc = Ppat_construct ({ txt = Lident "true" }, _) };
     pc_guard = None;
     pc_rhs = ifTrue;
   };
   {
     pc_lhs = { ppat_desc = Ppat_construct ({ txt = Lident "false" }, _) };
     pc_guard = None;
     pc_rhs = ifFalse;
   };
  ] ->
      Some (ifTrue, ifFalse)
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

type formatSettings = {
  (* Whether or not to expect that the original parser that generated the AST
     would have annotated constructor argument tuples with explicit arity to
     indicate that they are multiple arguments. (True if parsed in original
     OCaml AST, false if using Reason parser).
  *)
  constructorTupleImplicitArity : bool;
  space : int;
  (* For curried arguments in function *definitions* only: Number of [space]s
     to offset beyond the [let] keyword. Default 1.
  *)
  listsRecordsIndent : int;
  indentWrappedPatternArgs : int;
  indentMatchCases : int;
  (* Amount to indent in label-like constructs such as wrapped function
     applications, etc - or even record fields. This is not the same concept as an
     indented curried argument list. *)
  indentAfterLabels : int;
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
  trySwitchIndent : int;
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
  funcApplicationLabelStyle : funcApplicationLabelStyle;
  funcCurriedPatternStyle : funcApplicationLabelStyle;
  width : int;
  assumeExplicitArity : bool;
  constructorLists : string list;
}

let defaultSettings =
  {
    constructorTupleImplicitArity = false;
    space = 1;
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
    width = 80;
    assumeExplicitArity = false;
    constructorLists = [];
  }

let configuredSettings = ref defaultSettings

let configure ~width ~assumeExplicitArity ~constructorLists =
  configuredSettings :=
    { defaultSettings with width; assumeExplicitArity; constructorLists }

let settings = defaultSettings
let preserve_braces = true

let should_preserve_requested_braces expr =
  let { stylisticAttrs } = partitionAttributes expr.pexp_attributes in
  match expr.pexp_desc with
  | Pexp_ifthenelse _ | Pexp_try _ -> false
  | Pexp_sequence _ ->
      (* `let ... in` should _always_ preserve braces *)
      true
  | _ ->
      preserve_braces
      && Reason_attributes.has_preserve_braces_attrs stylisticAttrs

let rec structure_item term =
  match term.pstr_desc with
  | Pstr_eval (e, attrs) ->
      let attrs = partitionAttributes attrs in
      concat_map attribute attrs.docAttrs
      ^^ concat_map attribute attrs.stdAttrs
      ^^ group (expression e ^^ semi)
  | Pstr_value (rf, vb) -> value_bindings rf vb
  | Pstr_primitive v -> string "todo Pstr_primitive"
  | Pstr_type (r, tl) ->
      group
        (string "type" ^^ space
        ^^ separate_map
             (space ^^ hardline ^^ string "and" ^^ space)
             type_declaration tl
        ^^ semi)
  | Pstr_typext t -> string "todo Pstr_typext"
  | Pstr_exception e -> string "todo Pstr_exception"
  | Pstr_module mb ->
      group
        (string "module" ^^ space
        ^^ optional string mb.pmb_name.txt
        ^^ space ^^ equals ^^ space ^^ module_expr mb.pmb_expr)
  | Pstr_recmodule ml -> string "todo Pstr_recmodule"
  | Pstr_modtype mt -> string "todo Pstr_modtype"
  | Pstr_open o -> string "todo Pstr_open"
  | Pstr_class cl -> string "todo Pstr_class"
  | Pstr_class_type cl -> string "todo Pstr_class_type"
  | Pstr_include i -> string "todo Pstr_include"
  | Pstr_attribute a -> string "todo Pstr_attribute"
  | Pstr_extension (e, a) -> string "todo Pstr_extension"

and module_expr = function
  | { pmod_desc = Pmod_structure s; _ } ->
      lbrace ^^ structure s ^^ rbrace ^^ semi
  | _ -> string "module_expr: todo"

and value_bindings rf vb =
  let attrs = List.map (fun b -> partitionAttributes b.pvb_attributes) vb in
  let fstAttrs, restAttrs =
    match attrs with hd :: tl -> (hd, tl) | _ -> failwith "not supported"
  in

  concat_map attribute fstAttrs.docAttrs
  ^^ concat_map attribute fstAttrs.stdAttrs
  ^^ string "let" ^^ space
  ^^ (match rf with Nonrecursive -> empty | Recursive -> string "rec")
  ^^ separate_map (space ^^ hardline ^^ string "and" ^^ space) value_binding vb

and attribute = function
  | {
      attr_name = { Location.txt = "ocaml.doc" | "ocaml.text" };
      attr_payload =
        PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ( { pexp_desc = Pexp_constant (Pconst_string (text, _, None)) },
                    _ );
              pstr_loc;
            };
          ];
      _;
    } ->
      let text = if text = "" then "/**/" else "/**" ^ text ^ "*/" in
      string text ^^ hardline
  | { attr_name; attr_payload; _ } ->
      string "[@" ^^ string attr_name.txt ^^ payload attr_payload
      ^^ hardline (* self#payload "@" attr_name attr_payload *)

and payload = function
  | PStr s -> structure s
  | PSig s -> string "todo payload: sig"
  | PTyp t -> string "todo payload: typ"
  | PPat (p, eo) -> pattern p ^^ optional expression eo

and value_binding { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } =
  group
    (pattern pvb_pat ^^ space ^^ equals ^^ space
    ^^ group (expression ~depth:2 pvb_expr)
    ^^ semi)

and arg_label (a : Compiler_libs.Asttypes.arg_label) =
  match a with
  | Nolabel -> empty
  | Labelled s -> string "todo: Labelled"
  | Optional s -> string "todo: optional"

and arrow = space ^^ equals ^^ rangle ^^ space

(* add parentheses to binders when they are in fact infix or prefix operators *)
and protectIdentifier txt =
  let needs_parens = needs_parens txt in
  let txt =
    if is_andop txt || is_letop txt then
      Reason_syntax_util.compress_letop_identifier txt
    else txt
  in
  if not needs_parens then string txt
  else if needs_spaces txt then group (lparen ^^ string txt ^^ rparen)
  else string ("(" ^ txt ^ ")")

and protectLongIdentifier longPrefix txt =
  group (longPrefix ^^ string "." ^^ protectIdentifier txt)

and longident = function
  | Lident s -> protectIdentifier s
  | Ldot (longPrefix, s) -> protectLongIdentifier (longident longPrefix) s
  | Lapply (y, s) ->
      group (longident y ^^ string "(" ^^ longident s ^^ string ")")

and constant c =
  let open OCaml in
  match c with
  | Pconst_char i -> char i
  | Pconst_string (i, _, None) -> string i
  | Pconst_string (i, _, Some delim) ->
      string (Format.sprintf "{%s|%s|%s}" delim i delim)
  | Pconst_integer (i, None) -> int (int_of_string i)
  | Pconst_integer (i, Some m) -> int (int_of_string i)
  | Pconst_float (i, None) -> float (float_of_string i)
  | Pconst_float (i, Some m) -> float (float_of_string i)

and direction_flag (f : Compiler_libs.Asttypes.direction_flag) =
  match f with Upto -> string "to" | Downto -> string "downto"

and expression ?(depth = 0) ?(wrap = true) x =
  let { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } = x in
  match pexp_desc with
  | Pexp_ident { txt } -> longident txt
  | Pexp_constant c -> constant c
  | Pexp_let (rf, vb, e) ->
      break 0 ^^ value_bindings rf vb ^^ break 1 ^^ expression e ^^ semi
  | Pexp_function cl ->
      let lparen, rparen, n =
        if wrap then (lparen ^^ break 0, break 0 ^^ rparen, 2)
        else (empty, empty, 0)
      in
      nest n
        (lparen ^^ string "fun" ^^ break 0 ^^ bar ^^ space
        ^^ separate_map (hardline ^^ bar ^^ space) case cl)
      ^^ rparen
  | Pexp_fun (a, e, p, e2) ->
      let rec args p result =
        match p.pexp_desc with
        | Pexp_fun (a, e, p, e2) -> args e2 (pattern p :: result)
        | _ -> expression p :: result
      in
      let args = args e2 [ pattern p ] in
      let args, callback =
        match args with
        | callback :: [ e ] -> (e, callback)
        | callback :: lst ->
            (parens (separate (comma ^^ space) (List.rev lst)), callback)
        | _ -> (string "pepxfun: not supported yet", string "not supported yet")
      in
      let shouldPreserveBraces = should_preserve_requested_braces e2 in
      let callback =
        if shouldPreserveBraces then
          nest 2 (lbrace ^^ break 1 ^^ callback) ^^ break 0 ^^ rbrace
        else callback
      in
      arg_label a ^^ args ^^ arrow ^^ callback
  | Pexp_apply
      ( ({ pexp_desc = Pexp_ident { txt = Lident i; _ }; _ } as infixOperator),
        [ (_, a); (_, b) ] ) -> (
      match printedStringAndFixity i with
      | Infix o -> expression a ^^ space ^^ string o ^^ space ^^ expression b
      | _ ->
          expression a ^^ space ^^ expression infixOperator ^^ space
          ^^ expression b)
  | Pexp_apply (e, l) ->
      ifflat
        (expression e ^^ string "("
        ^^ separate_map comma (fun (_, e) -> expression ~wrap:false e) l)
        (group
           (break 0 ^^ expression e ^^ string "("
           ^^ nest 2
                (break 0
                ^^ separate_map
                     (comma ^^ break 1)
                     (fun (_, e) -> expression ~wrap:false e)
                     l
                ^^ ifflat empty (if List.length l = 1 then empty else comma))
           ^^ break 0 ^^ string ")"))
  | Pexp_match (e, cl) ->
      group
        (nest depth
           (break 1 ^^ string "switch" ^^ space
           ^^ parens (expression e)
           ^^ space ^^ lbrace ^^ hardline ^^ bar ^^ space
           ^^ separate_map (hardline ^^ bar ^^ space) case cl
           ^^ break 0 ^^ rbrace))
  | Pexp_try (e, cl) -> string "todo Pexp_try"
  | Pexp_tuple el ->
      nest 2
        (lparen ^^ break 0
        ^^ separate_map (comma ^^ break 1) expression el
        ^^ ifflat empty comma)
      ^^ break 0 ^^ rparen
  | Pexp_construct _ when is_simple_construct (view_expr x) -> (
      match view_expr x with
      | `nil -> string "[]"
      | `tuple -> string "()"
      | `list xs -> string "Pexp_construct list"
      | `cons xs -> string "Pexp_construct cons"
      | `simple x -> longident x
      | _ -> assert false)
  | Pexp_construct ({ txt = Lident s1 }, opt) ->
      group (string s1 ^^ optional (expression ~wrap:true) opt)
  | Pexp_construct (_, opt) -> string "todo Pexp_construct"
  | Pexp_variant (l, e) -> string "todo Pexp_variant"
  | Pexp_record (l, e) ->
      group
        (nest 2
           (lbrace ^^ break 0
           ^^ separate_map
                (comma ^^ break 1)
                (fun (l, e) ->
                  match l.txt with
                  | Lident name -> (
                      match e with
                      | { pexp_desc = Pexp_ident { txt = Lident value }; _ }
                        when name = value ->
                          string name
                      | _ -> string name ^^ colon ^^ space ^^ expression e)
                  | _ -> string "Ppat_record: not supported")
                l)
        ^^ ifflat empty comma ^^ break 0 ^^ rbrace)
  | Pexp_field (e, l) -> string "todo Pexp_field"
  | Pexp_setfield (e, l, e2) -> string "todo Pexp_setfield"
  | Pexp_array el -> string "todo Pexp_array"
  | Pexp_ifthenelse (e1, e2, e3) -> string "todo Pexp_ifthenelse"
  | Pexp_sequence (e1, e2) -> expression e1 ^^ semi ^^ hardline ^^ expression e2
  | Pexp_while (e1, e2) ->
      string "while" ^^ space ^^ lparen ^^ expression e1 ^^ rparen ^^ space
      ^^ nest 2 (lbrace ^^ break 1 ^^ expression e2)
      ^^ break 1 ^^ rbrace
  | Pexp_for (p, e1, e2, d, e3) ->
      string "for" ^^ space ^^ lparen ^^ pattern p ^^ space ^^ string "in"
      ^^ space ^^ expression e1 ^^ space ^^ direction_flag d ^^ space
      ^^ expression e2 ^^ rparen ^^ space
      ^^ nest 2 (lbrace ^^ break 1 ^^ expression e3)
      ^^ break 1 ^^ rbrace
  | Pexp_constraint (e1, c) ->
      lparen ^^ expression e1 ^^ colon ^^ space ^^ core_type c ^^ rparen
  | Pexp_coerce (e, c, c2) -> string "todo Pexp_coerce"
  | Pexp_send (e, l) -> string "todo Pexp_send"
  | Pexp_new l -> string "todo Pexp_new"
  | Pexp_setinstvar (l, e) -> string "todo Pexp_setinstvar"
  | Pexp_override l -> string "todo Pexp_override"
  | Pexp_letmodule (s, m, e) -> string "todo Pexp_letmodule"
  | Pexp_letexception (ec, e) -> string "todo Pexp_letexception"
  | Pexp_assert e -> string "todo Pexp_assert"
  | Pexp_lazy e -> string "todo Pexp_lazy"
  | Pexp_poly (e, ct) -> string "todo Pexp_poly"
  | Pexp_object c -> string "todo Pexp_object"
  | Pexp_newtype (s, e) -> string "todo Pexp_newtype"
  | Pexp_pack m -> string "todo Pexp_pack"
  | Pexp_open (o, e) -> string "todo Pexp_open"
  | Pexp_letop l -> string "todo Pexp_letop"
  | Pexp_extension e -> string "todo Pexp_extension"
  | Pexp_unreachable -> string "todo Pexp_unreachable"

and case { pc_lhs; pc_guard; pc_rhs } =
  let depth = match pc_lhs.ppat_desc with Ppat_or (_, _) -> 0 | _ -> 2 in
  group
    (nest depth (pattern ~wrap:false pc_lhs)
    ^^ optional
         (fun e -> space ^^ string "when" ^^ space ^^ expression e)
         pc_guard
    ^^ arrow
    ^^ nest 2 (expression ~wrap:false pc_rhs))

and pattern ?(wrap = true)
    ({ ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes } as px) =
  match ppat_desc with
  | Ppat_var v -> string v.txt
  | Ppat_any -> underscore
  | Ppat_alias (p, l) -> string "todo: Ppat_alias"
  | Ppat_constant c -> constant c
  | Ppat_interval (c, c2) ->
      constant c ^^ space ^^ string ".." ^^ space ^^ constant c2
  | Ppat_tuple pl ->
      group
        (nest 2 (lparen ^^ break 0 ^^ separate_map (comma ^^ break 1) pattern pl)
        ^^ ifflat empty comma ^^ break 0 ^^ rparen)
  | Ppat_construct ({ txt = Lident "::" }, l) -> (
      let rec list_items_cons acc = function
        | {
            ppat_desc =
              Ppat_construct
                ( { txt = Lident "::" },
                  Some ([], { ppat_desc = Ppat_tuple [ pat1; pat2 ] }) );
          } ->
            list_items_cons (pat1 :: acc) pat2
        | p -> (List.rev acc, p)
      in

      let pat_list, pat_last = list_items_cons [] px in
      match pat_last with
      | { ppat_desc = Ppat_construct ({ txt = Lident "[]" }, _) } ->
          (* [x,y,z] *)
          (* let lwrap, rwrap = wrap in *)
          lbracket ^^ separate_map (comma ^^ space) pattern pat_list ^^ rbracket
          (* makeList pat_list
             ~break:Layout.IfNeed ~sep:commaTrail ~postSpace:true
             ~wrap:(lwrap ^ "[", "]" ^ rwrap) *)
      | _ ->
          (* x::y *)
          lbracket
          ^^ separate2 (comma ^^ space)
               (comma ^^ space ^^ dot ^^ dot ^^ dot)
               (List.map pattern (pat_list @ [ pat_last ]))
          ^^ rbracket)
  | Ppat_construct ({ txt = Lident "()" }, None) -> string "()"
  | Ppat_construct ({ txt = Lident s }, c) ->
      string s ^^ optional (fun f -> pattern (snd f)) c
  | Ppat_construct (s, s2) -> string "todo Ppatconstruct"
  | Ppat_variant (l, po) -> string "todo: Ppat_variant"
  | Ppat_record (pl, c) ->
      group
        (nest 2
           (lbrace ^^ break 0
           ^^ separate_map
                (comma ^^ break 1)
                (fun (l, p) ->
                  match l.txt with
                  | Lident name -> (
                      match p with
                      | { ppat_desc = Ppat_var { txt = value }; _ }
                        when name = value ->
                          string name
                      | _ -> string name ^^ colon ^^ space ^^ pattern p)
                  | _ -> string "Ppat_record: not supported")
                pl)
        ^^ ifflat empty comma ^^ break 0 ^^ rbrace)
  | Ppat_array pl ->
      group
        (lbracket ^^ bar
        ^^ separate_map (comma ^^ space) pattern pl
        ^^ bar ^^ rbracket)
  | Ppat_or (p, p2) -> pattern p ^^ hardline ^^ bar ^^ space ^^ pattern p2
  | Ppat_constraint (p, c) ->
      lparen ^^ pattern p ^^ colon ^^ space ^^ core_type c ^^ rparen
  | Ppat_type l -> string "todo: Ppat_type"
  | Ppat_lazy p -> string "todo: Ppat_lazy"
  | Ppat_unpack l -> string "todo: Ppat_unpack"
  | Ppat_exception p -> string "todo: Ppat_exception"
  | Ppat_extension e -> string "todo: Ppat_extension"
  | Ppat_open (l, p) -> string "todo: Ppat_open"

and type_declaration
    {
      ptype_name;
      ptype_params;
      ptype_cstrs;
      ptype_kind;
      ptype_private;
      ptype_manifest;
      ptype_attributes;
      ptype_loc;
    } =
  group
    (string ptype_name.txt ^^ space ^^ string "=" ^^ space
    ^^ type_kind (ptype_kind, ptype_private, ptype_manifest))

and type_kind = function
  | Ptype_record ll, _, _ ->
      (* ifflat
         (braces (separate_map (comma ^^ blank 1) label_declaration ll)) *)
      string "{"
      ^^ nest 2
           (hardline ^^ separate_map (comma ^^ hardline) label_declaration ll)
      ^^ comma ^^ break 1 ^^ string "}"
      (* braces (nest 2 (hardline ^^ separate_map (comma ^^ blank 1) label_declaration ll)) *)
  | Ptype_variant cl, _, _ ->
      nest 2 (break 1 ^^ separate_map hardline constructor_declaration cl)
  | Ptype_abstract, Public, Some s -> core_type s
  | Ptype_abstract, _, _ -> string "todo: ptype_abstract"
  | Ptype_open, _, _ -> string "todo: Ptype_open"

and constructor_declaration { pcd_name; pcd_args; pcd_res; _ } =
  bar ^^ space ^^ string pcd_name.txt ^^ constructor_argument pcd_args

and constructor_argument = function
  | Pcstr_tuple [] -> empty
  | Pcstr_tuple l -> parens (separate_map (comma ^^ blank 1) core_type l)
  | Pcstr_record l -> space ^^ braces (separate_map comma label_declaration l)

and label_declaration { pld_name; pld_type; _ } =
  group (string pld_name.txt ^^ string ":" ^^ space ^^ core_type pld_type)

and core_type { ptyp_desc; _ } =
  match ptyp_desc with
  | Ptyp_var _ -> string "todo a"
  | Ptyp_constr ({ txt = Lident s1 }, []) -> string s1
  | Ptyp_constr ({ txt = Lident s1 }, cl) ->
      string s1 ^^ lparen ^^ separate_map star core_type cl ^^ rparen
  | _ -> string "other type..."

and structure structureItems =
  group (separate_map (hardline ^^ hardline) structure_item structureItems)

let createFormatter () =
  let module RFormatter = struct
    let case_list : Format.formatter -> case list -> unit = fun f l -> ()
    let core_type : Format.formatter -> core_type -> unit = fun f c -> ()
    let expression : Format.formatter -> expression -> unit = fun f e -> ()
    let pattern : Format.formatter -> pattern -> unit = fun f p -> ()

    let signature :
        Reason_comment.t list -> Format.formatter -> signature -> unit =
     fun l f s -> ()

    let structure :
        Reason_comment.t list -> Format.formatter -> structure -> unit =
     fun comments ppf x ->
      let buf = Buffer.create 100 in
      let width = 50 in
      let doc = structure x in
      (* PPrint.ToFormatter.pretty 1.0 width ppf doc *)
      PPrint.ToBuffer.pretty 1.0 width buf doc;
      print_endline (Buffer.contents buf)
    (* List.iter (fun comment -> printer#trackComment comment) comments; *)
    (* format_layout ppf ~comments *)
    (* (printer#structure (apply_mapper_to_structure x preprocessing_mapper)) *)
    (* (printer#structure x) *)

    let toplevel_phrase : Format.formatter -> toplevel_phrase -> unit =
     fun f l -> ()
  end in
  object
    method core_type = RFormatter.core_type
    method pattern = RFormatter.pattern
    method signature = RFormatter.signature
    method structure = RFormatter.structure

    (* For merlin-destruct *)
    method toplevel_phrase = RFormatter.toplevel_phrase
    method expression = RFormatter.expression
    method case_list = RFormatter.case_list
  end
