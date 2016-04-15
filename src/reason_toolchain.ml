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

open Ast_helper
open Location
open Lexing

let invalidLex = "invalidCharacter.orComment.orString"
let syntax_error_str err loc =
  if !Reason_config.recoverable then
    [
      Str.mk ~loc:loc (Parsetree.Pstr_extension (Reason_utils.syntax_error_extension_node loc invalidLex, []))
    ]
  else
    raise err

let syntax_error_core_type err loc =
  if !Reason_config.recoverable then
    Typ.mk ~loc:loc (Parsetree.Ptyp_extension (Reason_utils.syntax_error_extension_node loc invalidLex))
  else
    raise err

let syntax_error_sig err loc =
  if !Reason_config.recoverable then
    [Sig.mk ~loc:loc (Parsetree.Psig_extension (Reason_utils.syntax_error_extension_node loc invalidLex, []))]
  else
    raise err

(* given a comment string and the first two characters read from a channel, check if the comment
   should be reformatted *)
let modify_comment comment c1 c2 =
  (* This handles the 4.02.3 case of docstrings (starting with `STAR`
     and `WHITESPACE`) having their strings truncated *)
  if c1 == '*' &&
     c2 == ' ' &&
     (if String.length comment >= 2
      then String.compare (String.sub comment 0 2) "* " <> 0
      else true)  (* 4.02.1 will correctly start with `* ` *)
  then "*" ^ comment
  (* This handles the 4.02.3 case of comments of the form
     `LPAREN`(`STAR`)(`STAR`)*.. returning `STAR``LPAREN``STAR`(`STAR`)*... *)
  else if c1 == '*' &&
          c2 == '*' &&
          (if String.length comment >= 3
           then String.compare (String.sub comment 0 3) "*(*" == 0
           else true) (* 4.02.3 incorrectly start with `STAR LPAREN STAR` *)
  then String.sub comment 3 (String.length comment - 4) ^ " "
  else comment

type comments = (String.t * Location.t) list

module type Toolchain = sig
  (* Parsing *)
  val canonical_core_type_with_comments: Lexing.lexbuf -> (Parsetree.core_type * comments)
  (* Note: stdin_input must be passed in as a ref because the lexer has not yet run when this is called*)
  val canonical_implementation_with_comments: ?stdin_input_ref:String.t ref -> ?filename:String.t -> Lexing.lexbuf -> (Parsetree.structure * comments)
  val canonical_interface_with_comments: ?stdin_input_ref:String.t ref -> ?filename:String.t -> Lexing.lexbuf -> (Parsetree.signature * comments)

  val canonical_core_type: Lexing.lexbuf -> Parsetree.core_type
  val canonical_implementation: Lexing.lexbuf -> Parsetree.structure
  val canonical_interface: Lexing.lexbuf -> Parsetree.signature
  val canonical_toplevel_phrase: Lexing.lexbuf -> Parsetree.toplevel_phrase
  val canonical_use_file: Lexing.lexbuf -> Parsetree.toplevel_phrase list


  (* Printing *)
  val print_canonical_interface_with_comments: (Parsetree.signature * comments) -> unit
  val print_canonical_implementation_with_comments: (Parsetree.structure * comments) -> unit

end

module type Toolchain_spec = sig
  val safeguard_parsing: Lexing.lexbuf ->
    (unit -> ('a * comments)) -> ('a * comments)

  module rec Lexer_impl: sig
    val init: unit -> unit
    val token: Lexing.lexbuf -> Parser_impl.token
    val comments: unit -> (String.t * Location.t) list
  end

  and Parser_impl: sig
    type token
  end

  val core_type: Lexing.lexbuf -> Parsetree.core_type
  val implementation: Lexing.lexbuf -> Parsetree.structure
  val interface: Lexing.lexbuf -> Parsetree.signature
  val toplevel_phrase: Lexing.lexbuf -> Parsetree.toplevel_phrase
  val use_file: Lexing.lexbuf -> Parsetree.toplevel_phrase list

  val format_interface_with_comments: (Parsetree.signature * comments) -> Format.formatter -> unit
  val format_implementation_with_comments: (Parsetree.structure * comments) -> Format.formatter -> unit
end

module Create_parse_entrypoint (Toolchain_impl: Toolchain_spec) :Toolchain = struct
  (* Note: stdin_input must be passed in as a ref because the lexing has not yet occured when wrap_with_comments
     is initially called. It is only called after safeguard_parsing is run *)
  let wrap_with_comments ?(stdin_input_ref=ref "") ?(filename="") parsing_fun lexbuf =
    Toolchain_impl.safeguard_parsing lexbuf (fun () ->
      let _ = Toolchain_impl.Lexer_impl.init () in
      let ast = parsing_fun lexbuf in
      let unmodified_comments = Toolchain_impl.Lexer_impl.comments() in
      match filename with
        | "" -> (
          match !stdin_input_ref with
            | "" ->
              (
                let _  = Parsing.clear_parser() in
                (ast, unmodified_comments)
              )
            | _ -> (
              let modified_comments =
                List.map (fun (str, loc) ->
                  let cnum = loc.loc_start.pos_cnum in
                  let char1 = String.get !stdin_input_ref (cnum + 2) in  (* ignore the leading `LPAREN STAR` *)
                  let char2 = String.get !stdin_input_ref (cnum + 3) in
                  let modified_comment = modify_comment str char1 char2 in
                  (modified_comment, loc)
                )
                unmodified_comments
              in (
                let _  = Parsing.clear_parser() in
                (ast, modified_comments)
              )
            )
        )
        | _ ->
          let file_chan = open_in filename in
          let modified_comments =
            List.map (fun (str, loc) ->
              let cnum = loc.loc_start.pos_cnum in
              seek_in file_chan (cnum + 2);  (* ignore the leading `LPAREN STAR` *)
              let char1 = input_char file_chan in
              let char2 = input_char file_chan in
              let modified_comment = modify_comment str char1 char2 in
              (modified_comment, loc)
            )
            unmodified_comments
          in
          (
            let _  = Parsing.clear_parser() in
            (ast, modified_comments)
          )
    )

  (*
   * The canonical interface/implementations (with comments) are used with
   * recovering mode for IDE integration. The parser itself likely
   * implements its own recovery, but we need to recover in the event
   * that the file couldn't even lex.
   * Note, the location reported here is broken for some lexing errors
   * (nested comments or unbalanced strings in comments) but at least we don't
   * crash the process. TODO: Report more accurate location in those cases.
   *
   * The filename is needed when converting .ml files. The channel is accessed
   * to modify the comment tokens given back by the lexer due to the discrepancies
   * between 4.02.3 and 4.02.1
   *)
  let canonical_implementation_with_comments ?(stdin_input_ref=ref "") ?(filename="") lexbuf =
    try wrap_with_comments ~stdin_input_ref:stdin_input_ref ~filename:filename Toolchain_impl.implementation lexbuf with
    | err -> (syntax_error_str err (Location.curr lexbuf), [])

  let canonical_core_type_with_comments lexbuf =
    try wrap_with_comments Toolchain_impl.core_type lexbuf with
    | err -> (syntax_error_core_type err (Location.curr lexbuf), [])

  let canonical_interface_with_comments ?(stdin_input_ref= ref "") ?(filename="") lexbuf =
    try wrap_with_comments ~stdin_input_ref:stdin_input_ref ~filename:filename Toolchain_impl.interface lexbuf with
    | err -> (syntax_error_sig err (Location.curr lexbuf), [])

  let canonical_toplevel_phrase_with_comments lexbuf =
    wrap_with_comments Toolchain_impl.toplevel_phrase lexbuf

  let canonical_use_file_with_comments lexbuf =
    wrap_with_comments Toolchain_impl.use_file lexbuf

  (** [ast_only] wraps a function to return only the ast component
   *)
  let ast_only f =
    (fun lexbuf -> lexbuf |> f |> fst)

  let canonical_implementation = ast_only canonical_implementation_with_comments

  let canonical_core_type = ast_only canonical_core_type_with_comments

  let canonical_interface = ast_only canonical_interface_with_comments

  let canonical_toplevel_phrase = ast_only canonical_toplevel_phrase_with_comments

  let canonical_use_file = ast_only canonical_use_file_with_comments

  (* Printing *)
  let print_canonical_interface_with_comments interface =
    Toolchain_impl.format_interface_with_comments interface Format.std_formatter

  let print_canonical_implementation_with_comments implementation =
    Toolchain_impl.format_implementation_with_comments implementation Format.std_formatter
end

module OCaml_syntax = struct
  module Lexer_impl = Lexer
  module Parser_impl = Parser

  let implementation = Parser.implementation Lexer.token
  let core_type = Parser.parse_core_type Lexer.token
  let interface = Parser.interface Lexer.token
  let toplevel_phrase = Parser.toplevel_phrase Lexer.token
  let use_file = Parser.use_file Lexer.token

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
  let format_interface_with_comments (signature, _) formatter =
    Pprintast.signature formatter signature
  let format_implementation_with_comments (implementation, _) formatter =
    Pprintast.structure formatter implementation
end

module JS_syntax = struct
  module I = Reason_parser.MenhirInterpreter
  module Lexer_impl = Reason_lexer
  module Parser_impl = Reason_parser

  let initial_checkpoint constructor lexbuf =
    (constructor lexbuf.lex_curr_p)

  (* [tracking_supplier] is a supplier that tracks the last token read *)
  type tracking_supplier = {
      (* The last token that was obtained from the lexer, together with its start
     and end positions. Warning: before the first call to the lexer has taken
     place, a None value is stored here. *)

      triple: (Reason_parser.token * Lexing.position * Lexing.position) option ref;

      (* A supplier function that returns one token at a time*)
      supplier: unit -> (Reason_parser.token * Lexing.position * Lexing.position)
    }

  (* [lexbuf_to_supplier] returns a supplier to be feed into Menhir's incremental API.
   * Each time the supplier is called, a new token in the lexbuf is returned.
   * If the supplier is called after an EOF is already returned, a syntax error will be raised.
   *
   * This makes sure at most one EOF token is returned by supplier, which
   * is the default behavior of ocamlyacc.
   *)
  let lexbuf_to_supplier lexbuf =
    let s = I.lexer_lexbuf_to_supplier Reason_lexer.token lexbuf in
    let eof_met = ref false in
    let supplier = fun () ->
      let (token, s, e) = s () in
      if token = Reason_parser.EOF then
        if not !eof_met then
          let _ = eof_met := true in
          (token, s, e)
        else
          raise(Syntaxerr.Error(Syntaxerr.Other (Location.curr lexbuf)))
      else
        (token, s, e)
    in
    let triple = ref None in
    {triple; supplier}

  let read {triple; supplier} =
    let t = supplier() in
    let _ = triple := Some t in
    t

  let last_token {triple; _} =
    !triple

  (* [loop_handle_yacc] mimic yacc's error handling mechanism in menhir.
     When it hits an error state, it pops up the stack until it finds a
     state when the error can be shifted or reduced.

     This is similar to Menhir's default behavior for error handling, with
     one subtle difference:
     When loop_handle_yacc recovers from the error, unlike Menhir, it doesn't
     discard the input token immediately. Instead, it restarts the parsing
     from recovered state with the original lookahead token that caused the
     error. If there is still an error, the look ahead token is then discarded.

     yacc's behavior gives us a chance to recover the following code :
     ```
     {
       let a = 1;
       Js.
     }
     ```
     , where "}" is the lookahead token that triggers an error state. With
     yacc's behavior, "}" will still be shifted once we recover from "Js.",
     giving the parser the ability to reduce the whole program to a sequence
     expression.
  *)

  let rec loop_handle_yacc supplier in_error checkpoint =

    match checkpoint with
    | I.InputNeeded _ ->
       if in_error then
         begin
           match last_token supplier with
           | Some triple ->
              (* We just recovered from the error state, try the original token again*)
              let checkpoint_with_previous_token = I.offer checkpoint triple in
              let accept_new = I.loop_test
                                 (fun _ _ -> true)
                                 checkpoint_with_previous_token
                                 false
              in
              if accept_new then
                loop_handle_yacc supplier false checkpoint_with_previous_token
              else
                (* The original token still fail to be parsed, discard *)
                loop_handle_yacc supplier false checkpoint
           | None -> assert false
         end
       else
         let triple = read supplier in
         let checkpoint = I.offer checkpoint triple in
         loop_handle_yacc supplier false checkpoint
    | I.Shifting _
      | I.AboutToReduce _ ->
       let checkpoint = I.resume checkpoint in
       loop_handle_yacc supplier in_error checkpoint
    | I.HandlingError env ->
       let checkpoint = I.resume checkpoint in
       (* Enter error state *)
       loop_handle_yacc supplier true checkpoint
    | I.Rejected ->
       begin
         match last_token supplier with
         | Some (_, s, e) ->
            let loc = {
                loc_start = s;
                loc_end = e;
                loc_ghost = false;
              } in
            raise Syntaxerr.(Error(Syntaxerr.Other loc))
         | None -> assert false
       end
    | I.Accepted v ->
       (* The parser has succeeded and produced a semantic value. *)
       v

  let implementation lexbuf =
    let cp = initial_checkpoint Reason_parser.Incremental.implementation lexbuf in
    loop_handle_yacc (lexbuf_to_supplier lexbuf) false cp

  let interface lexbuf =
    let cp = initial_checkpoint Reason_parser.Incremental.interface lexbuf in
    loop_handle_yacc (lexbuf_to_supplier lexbuf) false cp

  let core_type lexbuf =
    let cp = initial_checkpoint Reason_parser.Incremental.parse_core_type lexbuf in
    loop_handle_yacc (lexbuf_to_supplier lexbuf) false cp

  let toplevel_phrase lexbuf =
    let cp = initial_checkpoint Reason_parser.Incremental.toplevel_phrase lexbuf in
    loop_handle_yacc (lexbuf_to_supplier lexbuf) false cp

  let use_file lexbuf =
    let cp = initial_checkpoint Reason_parser.Incremental.use_file lexbuf in
    loop_handle_yacc (lexbuf_to_supplier lexbuf) false cp

  (* Skip tokens to the end of the phrase *)
  let rec skip_phrase lexbuf =
    try
      match Lexer_impl.token lexbuf with
        Parser_impl.SEMI | Parser_impl.EOF -> ()
      | _ -> skip_phrase lexbuf
    with
      | Lexer_impl.Error (Lexer_impl.Unterminated_comment _, _)
      | Lexer_impl.Error (Lexer_impl.Unterminated_string, _)
      | Lexer_impl.Error (Lexer_impl.Unterminated_string_in_comment _, _)
      | Lexer_impl.Error (Lexer_impl.Illegal_character _, _) -> skip_phrase lexbuf

  let maybe_skip_phrase lexbuf =
    if Parsing.is_current_lookahead Parser_impl.SEMI
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
    | x -> raise x

  let format_interface_with_comments (signature, comments) formatter =
    let reason_formatter = Reason_pprint_ast.createFormatter () in
    reason_formatter#signature comments formatter signature
  let format_implementation_with_comments (implementation, comments) formatter =
    let reason_formatter = Reason_pprint_ast.createFormatter () in
    reason_formatter#structure comments formatter implementation

end

module ML = Create_parse_entrypoint (OCaml_syntax)
module JS = Create_parse_entrypoint (JS_syntax)
