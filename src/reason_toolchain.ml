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
 *
 * The comments include the full text of the comment (typically in between the
 * "(*" and the "*)", as well as location information for that comment.
 *
 * WARNING: The "end" location is one greater than the actual final position!
 * (for both [associatedTextLoc] and [commentLoc]).
 *
 * Currently, the location information for comments is of the form:
 *
 *  (associatedTextLoc)
 *
 * But we should quickly change it to be of the form:
 *
 *  (associatedTextLoc, commentLoc)
 *
 * Where the [commentLoc] is the actual original location of the comment,
 * and the [associatedTextLoc] records the location in the file that the
 * comment is attached to. If [associatedTextLoc] and [commentLoc] are the
 * same, then the comment is "free floating" in that it only attaches to itself.
 * The [Reason] pretty printer will try its best to interleave those comments
 * in the containing list etc. But if [associatedTextLoc] expands beyond
 * the [commentLoc] it means the comment and the AST that is captured by
 * the [associatedTextLoc] are related - where "related" is something
 * this [reason_toolchain] decides (but in short it handles "end of line
 * comments"). Various pretty printers can decide how to preserve this
 * relatedness. Ideally, it would preserve end of line comments, but in the
 * short term, it might merely use that relatedness to correctly attach
 * end of line comments to the "top" of the AST node.
 *
 *    let lst = [
 *
 *    ];   (*    Comment    *)
 *         ----commentLoc-----
 *    ---associatedTextLoc----
 *
 *
 * Ideally that would be formatted as:
 *
 *    let lst = [
 *
 *    ];   (*    Comment    *)
 *
 * Or:
 *
 *    let lst = [ ];   (*    Comment    *)
 *
 *
 * But a shorter term solution would use that [associatedTextLoc] to at least
 * correctly attach the comment to the correct node, even if not "end of line".
 *
 *   (*    Comment    *)
 *   let lst = [ ];
 *)

open Location
open Lexing

module S = MenhirLib.General (* Streams *)

let invalidLex = "invalidCharacter.orComment.orString"
let syntax_error_str err loc =
  if !Reason_config.recoverable then
    [
      Ast_helper.Str.mk ~loc:loc (Parsetree.Pstr_extension (Syntax_util.syntax_error_extension_node loc invalidLex, []))
    ]
  else
    raise err

let syntax_error_core_type err loc =
  if !Reason_config.recoverable then
    Ast_helper.Typ.mk ~loc:loc (Parsetree.Ptyp_extension (Syntax_util.syntax_error_extension_node loc invalidLex))
  else
    raise err

let syntax_error_sig err loc =
  if !Reason_config.recoverable then
    [Ast_helper.Sig.mk ~loc:loc (Parsetree.Psig_extension (Syntax_util.syntax_error_extension_node loc invalidLex, []))]
  else
    raise err

let chan_input = ref ""

(* replaces Lexing.from_channel so we can keep track of the input for comment modification *)
let keep_from_chan chan =
  Lexing.from_function (fun buf n -> (
    (* keep input from chan in memory so that it can be used to reformat comment tokens *)
    let nchar = input chan buf 0 n in
    chan_input := !chan_input ^ Bytes.sub_string buf 0 nchar;
    nchar
  ))

let setup_lexbuf use_stdin filename =
  (* Use custom method of lexing from the channel to keep track of the input so that we can
     reformat tokens in the toolchain*)
  let lexbuf =
    match use_stdin with
      | true ->
        keep_from_chan stdin
      | false ->
        let file_chan = open_in filename in
        seek_in file_chan 0;
        keep_from_chan file_chan
  in
  Location.init lexbuf filename;
  lexbuf

(* (comment text, attachment_location, physical location) *)
type attached_comments = (String.t * Location.t * Location.t) list

module type Toolchain = sig
  (* Parsing *)
  val canonical_core_type_with_comments: Lexing.lexbuf -> (Parsetree.core_type * attached_comments)
  val canonical_implementation_with_comments: Lexing.lexbuf -> (Parsetree.structure * attached_comments)
  val canonical_interface_with_comments: Lexing.lexbuf -> (Parsetree.signature * attached_comments)

  val canonical_core_type: Lexing.lexbuf -> Parsetree.core_type
  val canonical_implementation: Lexing.lexbuf -> Parsetree.structure
  val canonical_interface: Lexing.lexbuf -> Parsetree.signature
  val canonical_toplevel_phrase: Lexing.lexbuf -> Parsetree.toplevel_phrase
  val canonical_use_file: Lexing.lexbuf -> Parsetree.toplevel_phrase list

  (* Printing *)
  val print_canonical_interface_with_comments: (Parsetree.signature * attached_comments) -> unit
  val print_canonical_implementation_with_comments: (Parsetree.structure * attached_comments) -> unit

end

module type Toolchain_spec = sig
  val safeguard_parsing: Lexing.lexbuf ->
    (unit -> ('a * attached_comments)) -> ('a * attached_comments)

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

  val format_interface_with_comments: (Parsetree.signature * attached_comments) -> Format.formatter -> unit
  val format_implementation_with_comments: (Parsetree.structure * attached_comments) -> Format.formatter -> unit
end

let line_content = Re_str.regexp "[^ \t]+"
(* We allow semicolons to be considered white space for sake of determining if
   an item is the last thing on the line. *)
let space_before_newline = Re_str.regexp "[,; \t]*$"
let new_line = Re_str.regexp "^"

module Create_parse_entrypoint (Toolchain_impl: Toolchain_spec) :Toolchain = struct
  let wrap_with_comments parsing_fun lexbuf =
    Toolchain_impl.safeguard_parsing lexbuf (fun () ->
      let _ = Toolchain_impl.Lexer_impl.init () in
      let ast = parsing_fun lexbuf in
      let unmodified_comments = Toolchain_impl.Lexer_impl.comments() in
      match !chan_input with
        | "" ->
          let _  = Parsing.clear_parser() in
          (ast, unmodified_comments |> List.map (fun (txt, phys_loc) -> (txt, phys_loc, phys_loc)))
        | _ ->
          let modified_and_attached_comments =
            List.map (fun (str, physical_loc) ->
              (* When searching for "^" regexp, returns location of newline + 1 *)
              let first_char_of_line = Re_str.search_backward new_line !chan_input physical_loc.loc_start.pos_cnum in
              let end_pos_plus_one = physical_loc.loc_end.pos_cnum in
              let comment_length = (end_pos_plus_one - physical_loc.loc_start.pos_cnum - 4) in
              (* Also, the string contents originally reported are incorrect! *)
              let original_comment_contents = String.sub !chan_input (physical_loc.loc_start.pos_cnum + 2) comment_length in
              let (com, attachment_location) =
                match Re_str.search_forward line_content !chan_input first_char_of_line
                with
                  | n ->
                    (* Recall that all end positions are actually the position of end + 1. *)
                    let one_greater_than_comment_end = end_pos_plus_one in
                    (* Re_str.string_match lets you specify a position one greater than last position *)
                    let comment_is_last_thing_on_line =
                      Re_str.string_match space_before_newline !chan_input one_greater_than_comment_end in
                    if n < physical_loc.loc_start.pos_cnum && comment_is_last_thing_on_line then (
                      original_comment_contents,
                      {physical_loc with loc_start = {physical_loc.loc_start with pos_cnum = n}}
                    )
                    else
                      (original_comment_contents, physical_loc)
                  | exception Not_found -> (original_comment_contents, physical_loc)
              in
              (com, attachment_location, physical_loc)
            )
            unmodified_comments
          in
          let _  = Parsing.clear_parser() in
          (ast, modified_and_attached_comments)
    )

  (*
   * The canonical interface/implementations (with comments) are used with
   * recovering mode for IDE integration. The parser itself likely
   * implements its own recovery, but we need to recover in the event
   * that the file couldn't even lex.
   * Note, the location reported here is broken for some lexing errors
   * (nested comments or unbalanced strings in comments) but at least we don't
   * crash the process. TODO: Report more accurate location in those cases.
   *)
  let canonical_implementation_with_comments lexbuf =
    try wrap_with_comments Toolchain_impl.implementation lexbuf with
    | err -> (syntax_error_str err (Location.curr lexbuf), [])

  let canonical_core_type_with_comments lexbuf =
    try wrap_with_comments Toolchain_impl.core_type lexbuf with
    | err -> (syntax_error_core_type err (Location.curr lexbuf), [])

  let canonical_interface_with_comments lexbuf =
    try wrap_with_comments Toolchain_impl.interface lexbuf with
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
   * every syntax (also see [syntax_util]). *)
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

      mutable last_token: (Reason_parser.token * Lexing.position * Lexing.position) option;

      (* A supplier function that returns one token at a time*)
      get_token: unit -> (Reason_parser.token * Lexing.position * Lexing.position)
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
    let get_token = fun () ->
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
    let last_token = None in
    {last_token; get_token}

  let read supplier =
    let t = supplier.get_token () in
    supplier.last_token <- Some t;
    t

  (* read last token's location from a supplier *)
  let last_token_loc supplier =
    match supplier.last_token with
    | Some (_, s, e) ->
       {
         loc_start = s;
         loc_end = e;
         loc_ghost = false;
       }
    | None -> assert false

  (* get the stack of a checkpoint *)
  let stack checkpoint =
    match checkpoint with
    | I.HandlingError env ->
       I.stack env
    | _ ->
       assert false

  (* get state number of a checkpoint *)
  let state checkpoint : int =
    match Lazy.force (stack checkpoint) with
    | S.Nil ->
       0
    | S.Cons (I.Element (s, _, _, _), _) ->
             I.number s

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
           match supplier.last_token with
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
       if !Reason_config.recoverable then
         let checkpoint = I.resume checkpoint in
         (* Enter error recovery state *)
         loop_handle_yacc supplier true checkpoint
       else
         (* If not in a recoverable state, fail early by raising a
          * customized Error object
          *)
         let loc = last_token_loc supplier in
         let state = state checkpoint in
         (* Check the error database to see what's the error message
          * associated with the current parser state
          *)
         let msg =
           try
             Reason_parser_message.message state
           with
             | Not_found -> "<UNKNOWN SYNTAX ERROR>"
         in
         let msg_with_state = Printf.sprintf "%d: %s" state msg in
         raise (Syntax_util.Error (loc, (Syntax_util.Syntax_error msg_with_state)))
    | I.Rejected ->
       begin
         let loc = last_token_loc supplier in
         raise Syntaxerr.(Error(Syntaxerr.Other loc))
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
    | Error _ as x ->
       let loc = Location.curr lexbuf in
       if !Location.input_name = "//toplevel//"
       then
         let _ = maybe_skip_phrase lexbuf in
         raise(Syntaxerr.Error(Syntaxerr.Other loc))
       else
         raise x
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
