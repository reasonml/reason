open Ast_404.Parsetree

type 'a parser
type 'a erroneous_parser
type 'a entry_point 
type token = Reason_parser.token

val entry_implementation : structure entry_point
val entry_interface : signature entry_point
val entry_expression : expression entry_point
val entry_core_type : core_type entry_point
val entry_toplevel_phrase : toplevel_phrase entry_point
val entry_use_file : toplevel_phrase list entry_point

val initial : 'a entry_point -> Lexing.position -> 'a parser

type 'a step =
  | Intermediate of 'a parser
  | Success of 'a * Reason_lexer.invalid_docstrings
  | Error of 'a erroneous_parser

val step : 'a parser -> token Reason_lexer.positioned -> 'a step
