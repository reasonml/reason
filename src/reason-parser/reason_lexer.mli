open Reason_parser

type t
type 'a positioned = 'a * Lexing.position * Lexing.position

val init : ?insert_completion_ident:Lexing.position -> Lexing.lexbuf -> t
val token : t -> token positioned
val lexbuf : t -> Lexing.lexbuf

type comment = string * Location.t

(* Some docstrings are not accepted by the parser
   and turned into comments. *)
type invalid_docstrings
val empty_invalid_docstrings : invalid_docstrings
val add_invalid_docstring :
  string -> Lexing.position -> Lexing.position ->
  invalid_docstrings -> invalid_docstrings

val get_comments : t -> invalid_docstrings -> comment list
