open Reason_parser
 
type state
type 'a positioned = 'a * Lexing.position * Lexing.position

val init : ?insert_completion_ident:Lexing.position -> Lexing.lexbuf -> state
val token : state -> token positioned

type comment = string * Location.t

(* Some docstrings are not accepted by the parser
   and turned into comments. *)
type invalid_docstrings
val empty_invalid_docstrings : invalid_docstrings
val add_invalid_docstring : comment -> invalid_docstrings -> invalid_docstrings

val get_comments : state -> invalid_docstrings -> comment list
