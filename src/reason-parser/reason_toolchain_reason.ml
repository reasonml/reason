open Reason_toolchain_conf
open Reason_errors

module P = Reason_multi_parser
module Lexer = Reason_lexer

(* From Reason source text to OCaml AST

   1. Make a lexbuf from source text
   2. Reason_lexer:
      a. Using OCamllex:
         extract one token from stream of characters
      b. post-process token:
         - store comments separately
         - insert ES6_FUN token
         - insert completion identifier
   3. Reason_parser, using Menhir:
      A parser with explicit continuations, which take a new token and return:
      - an AST when parse succeeded
      - a new continuation if more tokens are needed
      - nothing, if the parser got stuck (token is invalid in current state)
   4. Reason_toolchain connect lexer and parser:
*)

type token = Reason_parser.token
type invalid_docstrings = Reason_lexer.invalid_docstrings

let rec loop lexer parser =
  let token = Lexer.token lexer in
  match P.step parser token with
  | P.Intermediate parser' ->
    loop lexer parser'
  | P.Error _err ->
    let _, loc_start, loc_end = token in
    let loc = {Location. loc_start; loc_end; loc_ghost = false} in
    raise_fatal_error (Parsing_error ()) loc
  | P.Success (x, docstrings) ->
    (x, docstrings)

let initial_run entry_point lexer =
  loop lexer
    (P.initial entry_point (Lexer.lexbuf lexer).Lexing.lex_curr_p)

let implementation lexer =
  initial_run Reason_parser.Incremental.implementation lexer

let interface lexer =
  initial_run Reason_parser.Incremental.interface lexer

let core_type lexer =
  initial_run Reason_parser.Incremental.parse_core_type lexer

let toplevel_phrase lexer =
  initial_run Reason_parser.Incremental.toplevel_phrase lexer

let use_file lexer =
  initial_run Reason_parser.Incremental.use_file lexer

(* Skip tokens to the end of the phrase *)
let rec skip_phrase lexer =
  try
    match Lexer.token lexer with
    | (Reason_parser.SEMI | Reason_parser.EOF), _, _ -> ()
    | _ -> skip_phrase lexer
  with Reason_error (Lexing_error ( Unterminated_comment _
                                  | Unterminated_string
                                  | Unterminated_string_in_comment _
                                  | Illegal_character _) , _ ) ->
    skip_phrase lexer

let safeguard_parsing lexbuf fn =
  try fn ()
  with
  | Reason_error _ as err
    when !Location.input_name = "//toplevel//"->
    skip_phrase (Lexer.init lexbuf);
    raise err
  | Location.Error _ as x ->
     let loc = Location.curr lexbuf in
     if !Location.input_name = "//toplevel//"
     then
       let _ = skip_phrase (Lexer.init lexbuf) in
       raise(Syntaxerr.Error(Syntaxerr.Other loc))
     else
       raise x

let format_interface_with_comments (signature, comments) formatter =
  let reason_formatter = Reason_pprint_ast.createFormatter () in
  reason_formatter#signature comments formatter signature
let format_implementation_with_comments (implementation, comments) formatter =
  let reason_formatter = Reason_pprint_ast.createFormatter () in
  reason_formatter#structure comments formatter implementation
