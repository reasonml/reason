type 'a parser

val initial :
  (Lexing.position -> 'a Reason_parser.MenhirInterpreter.checkpoint) ->
  Lexing.position -> 'a parser

type 'a step =
  | Intermediate of 'a parser
  | Success of 'a * Reason_lexer.invalid_docstrings
  | Error

val step : 'a parser -> Reason_parser.token Reason_lexer.positioned -> 'a step

(* Interface for recovery *)

val recover :
  'a Reason_parser.MenhirInterpreter.checkpoint ->
  Reason_lexer.invalid_docstrings ->
  'a parser

val recovery_env :
  'a parser ->
  'a Reason_parser.MenhirInterpreter.env * Reason_lexer.invalid_docstrings
