open Reason_omp
open Ast_411
open Asttypes
open Location
open Longident
open Parsetree
open Reason_syntax_util
open Reason_attributes
open PPrint

let infix_symbols =
  [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/'; '$'; '%'; '\\'; '#' ]

(* this should match "kwdopchar" from reason_declarative_lexer.mll *)
let special_infix_strings =
  [ "asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "!==" ]

let isInfix i = List.mem i.[0] infix_symbols || List.mem i special_infix_strings

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

let rec structure_item term =
  match term.pstr_desc with
  | Pstr_eval (e, a) -> group (expression e ^^ semi)
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
  | Pstr_module mb -> string "todo Pstr_module"
  | Pstr_recmodule ml -> string "todo Pstr_recmodule"
  | Pstr_modtype mt -> string "todo Pstr_modtype"
  | Pstr_open o -> string "todo Pstr_open"
  | Pstr_class cl -> string "todo Pstr_class"
  | Pstr_class_type cl -> string "todo Pstr_class_type"
  | Pstr_include i -> string "todo Pstr_include"
  | Pstr_attribute a -> string "todo Pstr_attribute"
  | Pstr_extension (e, a) -> string "todo Pstr_extension"

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
      string "todo attribute" (* self#payload "@" attr_name attr_payload *)

and value_binding { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } =
  group
    (pattern pvb_pat ^^ space ^^ equals ^^ space
    ^^ group (expression ~depth:2 pvb_expr)
    ^^ semi)

and arg_label = function
  | Nolabel -> empty
  | Labelled s -> string "todo: Labelled"
  | Optional s -> string "todo: optional"

and arrow = space ^^ equals ^^ rangle ^^ space

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

and expression ?(depth = 0) ?(wrap = true)
    { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } =
  match pexp_desc with
  | Pexp_ident { txt = Lident l } -> string l
  | Pexp_ident _ -> string "todo pexident"
  | Pexp_constant c -> constant c
  | Pexp_let (rf, vb, e) ->
      break 0 ^^ value_bindings rf vb ^^ break 1 ^^ expression e ^^ semi
  | Pexp_function cl ->
      let lparen, rparen =
        if wrap then (lparen ^^ break 0, rparen) else (empty, empty)
      in
      nest 2
        (lparen ^^ string "fun" ^^ break 0 ^^ bar ^^ space
        ^^ separate_map (hardline ^^ bar ^^ space) case cl)
      ^^ break 0 ^^ rparen
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
      arg_label a ^^ args ^^ arrow ^^ callback
  | Pexp_apply
      ( ({ pexp_desc = Pexp_ident { txt = Lident i; _ }; _ } as infixOperator),
        [ (_, a); (_, b) ] )
    when isInfix i ->
      expression a ^^ space ^^ expression infixOperator ^^ space ^^ expression b
  | Pexp_apply (e, l) ->
      break 0 ^^ expression e ^^ string "("
      ^^ nest 2
           (break 0
           ^^ separate_map
                (comma ^^ break 1)
                (fun (_, e) -> expression ~wrap:false e)
                l
           ^^ ifflat empty comma)
      ^^ break 0 ^^ string ")"
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
  | Pexp_construct ({ txt = Lident s1 }, opt) ->
      group (string s1 ^^ optional expression opt)
  | Pexp_construct (_, opt) -> string "todo Pexp_construct"
  | Pexp_variant (l, e) -> string "todo Pexp_variant"
  | Pexp_record (l, e) -> string "todo Pexp_record"
  | Pexp_field (e, l) -> string "todo Pexp_field"
  | Pexp_setfield (e, l, e2) -> string "todo Pexp_setfield"
  | Pexp_array el -> string "todo Pexp_array"
  | Pexp_ifthenelse (e1, e2, e3) -> string "todo Pexp_ifthenelse"
  | Pexp_sequence (e1, e2) -> string "todo Pexp_sequence"
  | Pexp_while (e1, e2) -> string "todo Pexp_while"
  | Pexp_for (p, e1, e2, d, e3) -> string "todo Pexp_for"
  | Pexp_constraint (e1, ct) -> string "todo Pexp_constraint"
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
  group (nest depth (pattern pc_lhs) ^^ arrow ^^ nest 2 (expression pc_rhs))

and pattern { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes } =
  match ppat_desc with
  | Ppat_var v -> string v.txt
  | Ppat_any -> underscore
  | Ppat_alias (p, l) -> string "todo: Ppat_alias"
  | Ppat_constant c -> constant c
  | Ppat_interval (c, c2) -> string "todo: Ppat_interval"
  | Ppat_tuple pl ->
      group
        (nest 2 (lparen ^^ break 0 ^^ separate_map (comma ^^ break 1) pattern pl)
        ^^ ifflat empty comma ^^ break 0 ^^ rparen)
  | Ppat_construct ({ txt = Lident s }, c) -> string s ^^ optional pattern c
  | Ppat_construct (s, s2) -> string "todo Ppatconstruct"
  | Ppat_variant (l, po) -> string "todo: Ppat_variant"
  | Ppat_record (pl, c) ->
      group
        (nest 2
           (lbrace ^^ break 0
           ^^ separate_map (comma ^^ break 1) (fun (_, p) -> pattern p) pl)
        ^^ ifflat empty comma ^^ break 0 ^^ rbrace)
  | Ppat_array pl -> string "todo: Ppat_array"
  | Ppat_or (p, p2) -> pattern p ^^ hardline ^^ bar ^^ space ^^ pattern p2
  | Ppat_constraint (p, c) -> string "todo: Ppat_constraint"
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
   ^^ type_kind ptype_kind)

and type_kind = function
  | Ptype_record ll ->
      (* ifflat
         (braces (separate_map (comma ^^ blank 1) label_declaration ll)) *)
      string "{"
      ^^ nest 2
           (hardline ^^ separate_map (comma ^^ hardline) label_declaration ll)
      ^^ comma ^^ break 1 ^^ string "}"
      (* braces (nest 2 (hardline ^^ separate_map (comma ^^ blank 1) label_declaration ll)) *)
  | Ptype_variant cl ->
      nest 2 (break 1 ^^ separate_map hardline constructor_declaration cl)
  | Ptype_abstract -> string "todo: ptype_abstract"
  | Ptype_open -> string "todo: Ptype_open"

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
  | Ptyp_constr (_, cl) ->
      separate_map star core_type cl ^^ string (string_of_int (List.length cl))
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
