module Comment = Reason_comment
module Layout = Reason_layout
val source_map : ?loc:Location.t -> Layout.t -> Layout.t
val ( |> ) : 'a -> ('a -> 'b) -> 'b
val ( <| ) : ('a -> 'b) -> 'a -> 'b
exception NotPossible of string
val case_not_implemented :
  string -> Ast_404.Location.t -> string * int * int -> unit
val exprDescrString : Ast_404.Parsetree.expression -> string
val commaTrail : Layout.separator
val commaSep : Layout.separator
type ruleInfoData = {
  reducePrecedence : precedence;
  shiftPrecedence : precedence;
}
and ruleCategory =
    FunctionApplication of Layout.t list
  | SpecificInfixPrecedence of ruleInfoData * resolvedRule
  | PotentiallyLowPrecedence of Layout.t
  | Simple of Layout.t
and resolvedRule =
    LayoutNode of Layout.t
  | InfixTree of string * resolvedRule * resolvedRule
and associativity = Right | Nonassoc | Left
and precedenceEntryType = TokenPrecedence | CustomPrecedence
and precedence = Token of string | Custom of string
and tokenFixity =
    AlmostSimplePrefix of string
  | UnaryPlusPrefix of string
  | UnaryMinusPrefix of string
  | UnaryNotPrefix of string
  | UnaryPostfix of string
  | Infix of string
  | Normal
type infixChain = InfixToken of string | Layout of Layout.t
val expression_extension_sugar :
  Ast_404.Parsetree.expression ->
  (string Ast_404.Asttypes.loc * Ast_404.Parsetree.expression) option
val expression_immediate_extension_sugar :
  Ast_404.Parsetree.expression ->
  string Ast_404.Asttypes.loc option * Ast_404.Parsetree.expression
val expression_not_immediate_extension_sugar :
  Ast_404.Parsetree.expression ->
  (string Ast_404.Asttypes.loc * Ast_404.Parsetree.expression) option
val add_extension_sugar :
  string -> string Ast_404.Location.loc option -> string
val longIdentSame : Ast_404.Longident.t * Ast_404.Longident.t -> bool
val trueForEachPair : 'a list -> 'b list -> ('a -> 'b -> bool) -> bool
val same_ast_modulo_varification_and_extensions :
  Ast_404.Parsetree.core_type -> Ast_404.Parsetree.core_type -> bool
val expandLocation :
  Ast_404.Location.t -> expand:int * int -> Ast_404.Location.t
type attributesPartition = {
  arityAttrs : Ast_404.Parsetree.attributes;
  docAttrs : Ast_404.Parsetree.attributes;
  stdAttrs : Ast_404.Parsetree.attributes;
  jsxAttrs : Ast_404.Parsetree.attributes;
}
val partitionAttributes :
  Ast_404.Parsetree.attribute list -> attributesPartition
val extractStdAttrs :
  Ast_404.Parsetree.attribute list -> Ast_404.Parsetree.attributes
val sequentialIfBlocks :
  Ast_404.Parsetree.expression option ->
  (Ast_404.Parsetree.expression * Ast_404.Parsetree.expression) list *
  Ast_404.Parsetree.expression option
val almost_simple_prefix_symbols : char list
val unary_minus_prefix_symbols : string list
val unary_plus_prefix_symbols : string list
val infix_symbols : char list
val operator_chars : char list
val numeric_chars : char list
val special_infix_strings : string list
val updateToken : string
val requireIndentFor : string list
val namedArgSym : string
val getPrintableUnaryIdent : string -> string
val printedStringAndFixity : string -> tokenFixity
val isSimplePrefixToken : string -> bool
val rules :
  (precedenceEntryType * (string -> associativity * bool)) list list
val without_prefixed_backslashes : string -> string
val indexOfFirstMatch :
  prec:precedence ->
  (precedenceEntryType * (string -> 'a * bool)) list list ->
  ('a * int) option
val precedenceInfo : prec:precedence -> (associativity * int) option
val isLeftAssociative : prec:precedence -> bool
val isRightAssociative : prec:precedence -> bool
val higherPrecedenceThan : precedence -> precedence -> bool
val printedStringAndFixityExpr : Ast_404.Parsetree.expression -> tokenFixity
val is_predef_option : Ast_404.Longident.t -> bool
val needs_parens : string -> bool
val needs_spaces : string -> bool
val protect_ident : Format.formatter -> string -> unit
val protect_longident :
  Format.formatter ->
  (Format.formatter -> 'a -> unit) -> 'a -> string -> unit
val longident : Format.formatter -> Ast_404.Longident.t -> unit
val orList : Ast_404.Parsetree.pattern -> Ast_404.Parsetree.pattern list
type space_formatter = (unit, Format.formatter, unit) format
val override : Ast_404.Asttypes.override_flag -> string
val type_variance : Ast_404.Asttypes.variance -> string
type construct =
    [ `cons of Ast_404.Parsetree.expression list
    | `list of Ast_404.Parsetree.expression list
    | `nil
    | `normal
    | `simple of Ast_404.Longident.t
    | `tuple ]
type sequence_kind = [ `Array | `ES6List | `List | `Tuple ]
val view_expr :
  Ast_404.Parsetree.expression ->
  [> `cons of Ast_404.Parsetree.expression list
   | `list of Ast_404.Parsetree.expression list
   | `nil
   | `normal
   | `simple of Ast_404.Longident.t
   | `tuple ]
val is_simple_list_expr : Ast_404.Parsetree.expression -> bool
val is_simple_construct : construct -> bool
val is_single_unit_construct : Ast_404.Parsetree.expression list -> bool
val pp : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
type funcReturnStyle = ReturnValOnSameLine
val detectTernary :
  Ast_404.Parsetree.case list ->
  (Ast_404.Parsetree.expression * Ast_404.Parsetree.expression) option
type funcApplicationLabelStyle =
    NeverWrapFinalItem
  | WrapFinalListyItemIfFewerThan of int
type formatSettings = {
  constructorTupleImplicitArity : bool;
  space : int;
  returnStyle : funcReturnStyle;
  listsRecordsIndent : int;
  indentWrappedPatternArgs : int;
  indentMatchCases : int;
  indentAfterLabels : int;
  trySwitchIndent : int;
  funcApplicationLabelStyle : funcApplicationLabelStyle;
  funcCurriedPatternStyle : funcApplicationLabelStyle;
  width : int;
  assumeExplicitArity : bool;
  constructorLists : string list;
}
val configuredSettings : formatSettings ref
val configure :
  width:int ->
  assumeExplicitArity:bool -> constructorLists:string list -> unit
val string_of_formatter : (Format.formatter -> 'a -> unit) -> 'a -> string
val createFormatter :
  unit ->
  < case_list : Format.formatter -> Ast_404.Parsetree.case list -> unit;
    core_type : Format.formatter -> Ast_404.Parsetree.core_type -> unit;
    expression : Format.formatter -> Ast_404.Parsetree.expression -> unit;
    pattern : Format.formatter -> Ast_404.Parsetree.pattern -> unit;
    signature : Comment.t list ->
                Format.formatter -> Ast_404.Parsetree.signature -> unit;
    structure : Comment.t list ->
                Format.formatter -> Ast_404.Parsetree.structure -> unit;
    toplevel_phrase : Format.formatter ->
                      Ast_404.Parsetree.toplevel_phrase -> unit >
val defaultSettings : formatSettings
