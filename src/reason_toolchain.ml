(***********************************************************************)
(*                                                                     *)
(*                                Reason                               *)
(*                                                                     *)
(***********************************************************************)
(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)


(* Entry points in the parser *)

(**
 * Provides a simple interface to the most common parsing entrypoints required
 * by editor/IDE toolchains, preprocessors, and pretty printers.
 *
 * The form of this entrypoint includes more than what the standard OCaml
 * toolchain (oprof/ocamldoc) expects, but is still compatible.
 *
 * [implementation_with_comments] and [interface_with_comments] includes
 * additional information (about comments) suitable for building pretty
 * printers, editor, IDE and VCS integration.
 *)

module type Toolchain = sig
  val canonical_implementation: Lexing.lexbuf -> Parsetree.structure
  val canonical_interface: Lexing.lexbuf -> Parsetree.signature
  val canonical_toplevel_phrase: Lexing.lexbuf -> Parsetree.toplevel_phrase
  val canonical_use_file: Lexing.lexbuf -> (Parsetree.toplevel_phrase list)
  val canonical_core_type: Lexing.lexbuf -> Parsetree.core_type
  val canonical_expression: Lexing.lexbuf -> Parsetree.expression
  val canonical_pattern: Lexing.lexbuf -> Parsetree.pattern
  val canonical_implementation_with_comments: Lexing.lexbuf -> (Parsetree.structure * ((String.t * Location.t) list))
  val canonical_interface_with_comments: Lexing.lexbuf -> (Parsetree.signature * ((String.t * Location.t) list))

  (* Printing *)
  val print_canonical_interface_with_comments: (String.t * Location.t) list -> Parsetree.signature -> unit
  val print_canonical_implementation_with_comments: (String.t * Location.t) list -> Parsetree.structure -> unit
end

module type Toolchain_spec = sig
  val safeguard_parsing: Lexing.lexbuf ->
    (unit -> 'a * ((String.t * Location.t) list)) ->
    'a * ((String.t * Location.t) list)

  module rec Lexer_impl: sig
    val init: unit -> unit
    val token: Lexing.lexbuf -> Parser_impl.token
    val comments: unit -> (String.t * Location.t) list
  end
  and Parser_impl: sig
    type token
    val implementation: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.structure
    val interface: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.signature
    val toplevel_phrase: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.toplevel_phrase
    val use_file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.toplevel_phrase list)
    val parse_core_type: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.core_type
    val parse_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.expression
    val parse_pattern: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Parsetree.pattern
  end

  val format_interface_with_comments: (String.t * Location.t) list -> Format.formatter -> Parsetree.signature -> unit
  val format_implementation_with_comments: (String.t * Location.t) list -> Format.formatter -> Parsetree.structure -> unit
end

module Create_parse_entrypoint
       (Toolchain_impl: Toolchain_spec)
       :(Toolchain) = struct

  let wrap_with_comments parsing_fun lexbuf =
    Toolchain_impl.safeguard_parsing lexbuf (fun () ->
      Toolchain_impl.Lexer_impl.init ();
      let ast = parsing_fun Toolchain_impl.Lexer_impl.token lexbuf in
      let comments = Toolchain_impl.Lexer_impl.comments() in (
        Parsing.clear_parser();
        (ast, comments)
      )
    )

  let wrap parsing_fun lexbuf =
    let (ast, comments) = wrap_with_comments parsing_fun lexbuf in
    ast

  let canonical_implementation = wrap Toolchain_impl.Parser_impl.implementation
  let canonical_interface = wrap Toolchain_impl.Parser_impl.interface
  let canonical_toplevel_phrase = wrap Toolchain_impl.Parser_impl.toplevel_phrase
  let canonical_use_file = wrap Toolchain_impl.Parser_impl.use_file
  let canonical_core_type = wrap Toolchain_impl.Parser_impl.parse_core_type
  let canonical_expression = wrap Toolchain_impl.Parser_impl.parse_expression
  let canonical_pattern = wrap Toolchain_impl.Parser_impl.parse_pattern
  let canonical_implementation_with_comments = wrap_with_comments Toolchain_impl.Parser_impl.implementation
  let canonical_interface_with_comments = wrap_with_comments Toolchain_impl.Parser_impl.interface

  (* Printing *)
  let print_canonical_interface_with_comments comments interface =
    Toolchain_impl.format_interface_with_comments comments Format.std_formatter interface
  let print_canonical_implementation_with_comments comments implementation =
    Toolchain_impl.format_implementation_with_comments comments Format.std_formatter implementation
end

module OCaml_syntax = struct
  module Lexer_impl = Lexer
  module Parser_impl = Parser
  (* Skip tokens to the end of the phrase *)
  (* TODO: consolidate these copy-paste skip/trys into something that works for
   * every syntax (also see [reason_util]). *)
  let rec skip_phrase lexbuf =
    try
      match Lexer_impl.token lexbuf with
        Parser_impl.SEMISEMI | Parser_impl.EOF -> ()
      | _ -> skip_phrase lexbuf
    with
      | Lexer_impl.Error (Lexer_impl.Unterminated_comment _, _)
      | Lexer_impl.Error (Lexer_impl.Unterminated_string, _)
      | Lexer_impl.Error (Lexer_impl.Unterminated_string_in_comment _, _)
      | Lexer_impl.Error (Lexer_impl.Illegal_character _, _) ->
          skip_phrase lexbuf

  let maybe_skip_phrase lexbuf =
    if Parsing.is_current_lookahead Parser_impl.SEMISEMI
    || Parsing.is_current_lookahead Parser_impl.EOF
    then ()
    else skip_phrase lexbuf

  let safeguard_parsing lexbuf fn =
    try fn ()
    with
    | Lexer_impl.Error(Lexer_impl.Illegal_character _, _) as err
      when !Location.input_name = "//toplevel//"->
        skip_phrase lexbuf;
        raise err
    | Syntaxerr.Error _ as err
      when !Location.input_name = "//toplevel//" ->
        maybe_skip_phrase lexbuf;
        raise err
    | Parsing.Parse_error | Syntaxerr.Escape_error ->
        let loc = Location.curr lexbuf in
        if !Location.input_name = "//toplevel//"
        then maybe_skip_phrase lexbuf;
        raise(Syntaxerr.Error(Syntaxerr.Other loc))

  (* Unfortunately we drop the comments because there doesn't exist an ML
   * printer that formats comments *and* line wrapping! (yet) *)
  let format_interface_with_comments comments formatter signature =
    Pprintast.signature formatter signature
  let format_implementation_with_comments comments formatter signature =
    Pprintast.structure formatter signature
end

module JS_syntax = struct
  module Lexer_impl = Reason_lexer
  module Parser_impl = Reason_parser
  (* Skip tokens to the end of the phrase *)
  let rec skip_phrase lexbuf =
    try
      match Lexer_impl.token lexbuf with
        Parser_impl.SEMISEMI | Parser_impl.EOF -> ()
      | _ -> skip_phrase lexbuf
    with
      | Lexer_impl.Error (Lexer_impl.Unterminated_comment _, _)
      | Lexer_impl.Error (Lexer_impl.Unterminated_string, _)
      | Lexer_impl.Error (Lexer_impl.Unterminated_string_in_comment _, _)
      | Lexer_impl.Error (Lexer_impl.Illegal_character _, _) -> skip_phrase lexbuf

  let maybe_skip_phrase lexbuf =
    if Parsing.is_current_lookahead Parser_impl.SEMISEMI
    || Parsing.is_current_lookahead Parser_impl.EOF
    then ()
    else skip_phrase lexbuf

  let safeguard_parsing lexbuf fn =
    try fn ()
    with
    | Lexer_impl.Error(Lexer_impl.Illegal_character _, _) as err
      when !Location.input_name = "//toplevel//"->
        skip_phrase lexbuf;
        raise err
    | Syntaxerr.Error _ as err
      when !Location.input_name = "//toplevel//" ->
        maybe_skip_phrase lexbuf;
        raise err
    | Parsing.Parse_error | Syntaxerr.Escape_error ->
        let loc = Location.curr lexbuf in
        if !Location.input_name = "//toplevel//"
        then maybe_skip_phrase lexbuf;
        raise(Syntaxerr.Error(Syntaxerr.Other loc))
    | _ as x ->
        raise x

  let format_interface_with_comments comments formatter signature =
    let reason_formatter = Reason_pprint_ast.createFormatter () in
    reason_formatter#signature comments formatter signature
  let format_implementation_with_comments comments formatter signature =
    let reason_formatter = Reason_pprint_ast.createFormatter () in
    reason_formatter#structure comments formatter signature

end


module ML = Create_parse_entrypoint (OCaml_syntax)
module JS = Create_parse_entrypoint (JS_syntax)
