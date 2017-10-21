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

open Migrate_parsetree
open Ast_404

open Location
open Lexing

module From_current = Convert(OCaml_current)(OCaml_404)
module To_current = Convert(OCaml_404)(OCaml_current)

module S = MenhirLib.General (* Streams *)

let invalidLex = "invalidCharacter.orComment.orString"
let syntax_error_str err loc =
    if !Reason_config.recoverable = false then
      raise err
    else
      match err with
      | Location.Error err ->
        [
          Ast_helper.Str.mk ~loc:err.loc (Parsetree.Pstr_extension (Syntax_util.syntax_error_extension_node err.loc err.msg, []))
        ]
      | _ ->
        let menhirError = Syntax_util.findMenhirErrorMessage loc in
        match menhirError with
          | Syntax_util.MenhirMessagesError errMessage ->
              [Ast_helper.Str.mk ~loc:errMessage.Syntax_util.loc (Parsetree.Pstr_extension (Syntax_util.syntax_error_extension_node errMessage.Syntax_util.loc errMessage.Syntax_util.msg, []))]
          | _ ->
              [Ast_helper.Str.mk ~loc:loc (Parsetree.Pstr_extension (Syntax_util.syntax_error_extension_node loc invalidLex, []))]

let syntax_error_core_type err loc =
  if !Reason_config.recoverable = false then
    raise err
  else
    match err with
    | Location.Error err ->
      Ast_helper.Typ.mk ~loc:err.loc (Parsetree.Ptyp_extension (Syntax_util.syntax_error_extension_node err.loc err.msg))
    | _ ->
      Ast_helper.Typ.mk ~loc:loc (Parsetree.Ptyp_extension (Syntax_util.syntax_error_extension_node loc invalidLex))

let syntax_error_sig err loc =
  if !Reason_config.recoverable = false then
    raise err
  else
    match err with
    | Location.Error err ->
      [Ast_helper.Sig.mk ~loc:err.loc (Parsetree.Psig_extension (Syntax_util.syntax_error_extension_node err.loc err.msg, []))]
    | _ ->
      [Ast_helper.Sig.mk ~loc:loc (Parsetree.Psig_extension (Syntax_util.syntax_error_extension_node loc invalidLex, []))]


let setup_lexbuf use_stdin filename =
  (* Use custom method of lexing from the channel to keep track of the input so that we can
     reformat tokens in the toolchain*)
  let lexbuf =
    match use_stdin with
      | true -> Lexing.from_channel
        stdin
      | false ->
        let file_chan = open_in filename in
        seek_in file_chan 0;
        Lexing.from_channel file_chan
  in
  Location.init lexbuf filename;
  lexbuf


module type Toolchain = sig
  (* Parsing *)
  val core_type_with_comments: Lexing.lexbuf -> (Parsetree.core_type * Reason_pprint_ast.commentWithCategory)
  val implementation_with_comments: Lexing.lexbuf -> (Parsetree.structure * Reason_pprint_ast.commentWithCategory)
  val interface_with_comments: Lexing.lexbuf -> (Parsetree.signature * Reason_pprint_ast.commentWithCategory)

  val core_type: Lexing.lexbuf -> Parsetree.core_type
  val implementation: Lexing.lexbuf -> Parsetree.structure
  val interface: Lexing.lexbuf -> Parsetree.signature
  val toplevel_phrase: Lexing.lexbuf -> Parsetree.toplevel_phrase
  val use_file: Lexing.lexbuf -> Parsetree.toplevel_phrase list

  (* Printing *)
  val print_interface_with_comments: Format.formatter -> (Parsetree.signature * Reason_pprint_ast.commentWithCategory) -> unit
  val print_implementation_with_comments: Format.formatter -> (Parsetree.structure * Reason_pprint_ast.commentWithCategory) -> unit

end

module type Toolchain_spec = sig
  val safeguard_parsing: Lexing.lexbuf ->
    (unit -> ('a * Reason_pprint_ast.commentWithCategory)) -> ('a * Reason_pprint_ast.commentWithCategory)

  type token

  module Lexer_impl: sig
    val init: unit -> unit
    val token: Lexing.lexbuf -> token
    val comments: unit -> (String.t * Location.t) list
  end

  val core_type: Lexing.lexbuf -> Parsetree.core_type
  val implementation: Lexing.lexbuf -> Parsetree.structure
  val interface: Lexing.lexbuf -> Parsetree.signature
  val toplevel_phrase: Lexing.lexbuf -> Parsetree.toplevel_phrase
  val use_file: Lexing.lexbuf -> Parsetree.toplevel_phrase list

  val format_interface_with_comments: (Parsetree.signature * Reason_pprint_ast.commentWithCategory) -> Format.formatter -> unit
  val format_implementation_with_comments: (Parsetree.structure * Reason_pprint_ast.commentWithCategory) -> Format.formatter -> unit
end

let rec left_expand_comment should_scan_prev_line source loc_start =
  if loc_start = 0 then
    (String.unsafe_get source 0, true, 0)
  else
    let c = String.unsafe_get source (loc_start - 1) in
    match c with
    | '\t' | ' ' -> left_expand_comment should_scan_prev_line source (loc_start - 1)
    | '\n' when should_scan_prev_line -> left_expand_comment should_scan_prev_line source (loc_start - 1)
    | '\n' -> (c, true, loc_start)
    | _ -> (c, false, loc_start)

let rec right_expand_comment should_scan_next_line source loc_start =
  if loc_start = String.length source then
    (String.unsafe_get source (String.length source - 1), true, String.length source)
  else
    let c = String.unsafe_get source loc_start in
    match c with
    | '\t' | ' ' -> right_expand_comment should_scan_next_line source (loc_start + 1)
    | '\n' when should_scan_next_line -> right_expand_comment should_scan_next_line source (loc_start + 1)
    | '\n' -> (c, true, loc_start)
    | _ -> (c, false, loc_start)


module Create_parse_entrypoint (Toolchain_impl: Toolchain_spec) :Toolchain = struct

  let buffer_add_lexbuf buf skip lexbuf =
    let bytes = lexbuf.Lexing.lex_buffer in
    let start = lexbuf.Lexing.lex_start_pos + skip in
    let stop = lexbuf.Lexing.lex_buffer_len in
    Buffer.add_subbytes buf bytes start (stop - start)

  let refill_buff buf refill lb =
    let skip = lb.Lexing.lex_buffer_len - lb.Lexing.lex_start_pos in
    let result = refill lb in
    buffer_add_lexbuf buf skip lb;
    result

  (* replaces Lexing.from_channel so we can keep track of the input for comment modification *)
  let keep_from_lexbuf buffer lexbuf =
    buffer_add_lexbuf buffer 0 lexbuf;
    let refill_buff = refill_buff buffer lexbuf.Lexing.refill_buff in
    {lexbuf with refill_buff}

  let wrap_with_comments parsing_fun lexbuf =
    Toolchain_impl.safeguard_parsing lexbuf (fun () ->
      let _ = Toolchain_impl.Lexer_impl.init () in
      let input_copy = Buffer.create 0 in
      let ast = parsing_fun (keep_from_lexbuf input_copy lexbuf) in
      let unmodified_comments = Toolchain_impl.Lexer_impl.comments() in
      let contents = Buffer.contents input_copy in
      Buffer.reset input_copy;
      if contents = "" then
          let _  = Parsing.clear_parser() in
          (ast, unmodified_comments |> List.map (fun (txt, phys_loc) -> (txt, Reason_pprint_ast.Regular, phys_loc)))
      else
        let modified_and_comment_with_category =
          List.map (fun (str, physical_loc) ->
              (* When searching for "^" regexp, returns location of newline + 1 *)
              let (stop_char, eol_start, virtual_start_pos) = left_expand_comment false contents physical_loc.loc_start.pos_cnum in
              let one_char_before_stop_char =
                if virtual_start_pos <= 1 then
                  ' '
                else
                  String.unsafe_get contents (virtual_start_pos - 2)
              in
              (*
               *
               * The following logic are designed for cases like:
               * | (* comment *)
               *   X => 1
               * we want to extend the comment to the next line so it can be
               * correctly attached to X
               *
               * But we don't want it to extend to next line in this case:
               *
               * true || (* comment *)
               *   fasle
               *
               *)
              let should_scan_next_line = stop_char = '|' &&
                                          (one_char_before_stop_char = ' ' ||
                                           one_char_before_stop_char = '\n' ||
                                           one_char_before_stop_char = '\t' ) in
              let (stop_char, eol_end, virtual_end_pos) = right_expand_comment should_scan_next_line contents physical_loc.loc_end.pos_cnum in
              let end_pos_plus_one = physical_loc.loc_end.pos_cnum in
              let comment_length = (end_pos_plus_one - physical_loc.loc_start.pos_cnum - 4) in
              let original_comment_contents = String.sub contents (physical_loc.loc_start.pos_cnum + 2) comment_length in
              let t = match (eol_start, eol_end) with
                | (true, true) -> Reason_pprint_ast.SingleLine
                | (false, true) -> Reason_pprint_ast.EndOfLine
                | _ -> Reason_pprint_ast.Regular
              in
              let start_pos = virtual_start_pos in
              (original_comment_contents, t,
               {physical_loc with loc_start = {physical_loc.loc_start with pos_cnum = start_pos};
                                  loc_end = {physical_loc.loc_end with pos_cnum = virtual_end_pos}})
            )
            unmodified_comments
        in
        let _  = Parsing.clear_parser() in
        (ast, modified_and_comment_with_category)
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
  let implementation_with_comments lexbuf =
    try wrap_with_comments Toolchain_impl.implementation lexbuf with
    | err -> (syntax_error_str err (Location.curr lexbuf), [])

  let core_type_with_comments lexbuf =
    try wrap_with_comments Toolchain_impl.core_type lexbuf with
    | err -> (syntax_error_core_type err (Location.curr lexbuf), [])

  let interface_with_comments lexbuf =
    try wrap_with_comments Toolchain_impl.interface lexbuf with
    | err -> (syntax_error_sig err (Location.curr lexbuf), [])

  let toplevel_phrase_with_comments lexbuf =
    wrap_with_comments Toolchain_impl.toplevel_phrase lexbuf

  let use_file_with_comments lexbuf =
    wrap_with_comments Toolchain_impl.use_file lexbuf

  (** [ast_only] wraps a function to return only the ast component
   *)
  let ast_only f =
    (fun lexbuf -> lexbuf |> f |> fst)

  let implementation = ast_only implementation_with_comments

  let core_type = ast_only core_type_with_comments

  let interface = ast_only interface_with_comments

  let toplevel_phrase = ast_only toplevel_phrase_with_comments

  let use_file = ast_only use_file_with_comments

  (* Printing *)
  let print_interface_with_comments formatter interface =
    Toolchain_impl.format_interface_with_comments interface formatter

  let print_implementation_with_comments formatter implementation =
    Toolchain_impl.format_implementation_with_comments implementation formatter
end

module OCaml_syntax = struct
  open Migrate_parsetree

  (* The OCaml parser keep doc strings in the comment list.
     To avoid duplicating comments, we need to filter comments that appear
     as doc strings is the AST out of the comment list. *)
  let doc_comments_filter () =
    let open Ast_mapper in
    let open Parsetree in
    let seen = Hashtbl.create 7 in
    let attribute mapper = function
      | ({ Location. txt = ("ocaml.doc" | "ocaml.text") },
        PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string(_text, None)); _ } , _);
                pstr_loc = loc; _ }]) as attribute ->
        (* Workaround: OCaml 4.02.3 kept an initial '*' in docstrings.
         * For other versions, we have to put the '*' back. *)
        Hashtbl.add seen loc ();
        default_mapper.attribute mapper attribute
      | attribute -> default_mapper.attribute mapper attribute
    in
    let mapper = {default_mapper with attribute} in
    let filter (_text, loc) = not (Hashtbl.mem seen loc) in
    (mapper, filter)

  module Lexer_impl = struct
    let init = Lexer.init
    let token = Lexer.token

    let filtered_comments = ref []
    let filter_comments filter =
      filtered_comments := List.filter filter (Lexer.comments ())
    let comments () = !filtered_comments
  end
  module OCaml_parser = Parser
  type token = OCaml_parser.token

  (* OCaml parser parses into compiler-libs version of Ast.
     Parsetrees are converted to Reason version on the fly. *)

  let parse_and_filter_doc_comments iter fn lexbuf=
    let it, filter = doc_comments_filter () in
    let result = fn lexbuf in
    ignore (iter it result);
    Lexer_impl.filter_comments filter;
    result


  let implementation lexbuf =
    parse_and_filter_doc_comments
      (fun it -> it.Ast_mapper.structure it)
      (fun lexbuf -> From_current.copy_structure
                       (Parser.implementation Lexer.token lexbuf))
      lexbuf

  let core_type lexbuf =
    parse_and_filter_doc_comments
      (fun it -> it.Ast_mapper.typ it)
      (fun lexbuf -> From_current.copy_core_type
                       (Parser.parse_core_type Lexer.token lexbuf))
      lexbuf

  let interface lexbuf =
    parse_and_filter_doc_comments
      (fun it -> it.Ast_mapper.signature it)
      (fun lexbuf -> From_current.copy_signature
                       (Parser.interface Lexer.token lexbuf))
      lexbuf

  let filter_toplevel_phrase it = function
    | Parsetree.Ptop_def str -> ignore (it.Ast_mapper.structure it str)
    | Parsetree.Ptop_dir _ -> ()

  let toplevel_phrase lexbuf =
    parse_and_filter_doc_comments
      filter_toplevel_phrase
      (fun lexbuf -> From_current.copy_toplevel_phrase
          (Parser.toplevel_phrase Lexer.token lexbuf))
      lexbuf

  let use_file lexbuf =
    parse_and_filter_doc_comments
      (fun it result -> List.map (filter_toplevel_phrase it) result)
      (fun lexbuf ->
        List.map
          From_current.copy_toplevel_phrase
          (Parser.use_file Lexer.token lexbuf))
      lexbuf

  (* Skip tokens to the end of the phrase *)
  (* TODO: consolidate these copy-paste skip/trys into something that works for
   * every syntax (also see [syntax_util]). *)
  let rec skip_phrase lexbuf =
    try
      match Lexer.token lexbuf with
        OCaml_parser.SEMISEMI | OCaml_parser.EOF -> ()
      | _ -> skip_phrase lexbuf
    with
      | Lexer.Error (Lexer.Unterminated_comment _, _)
      | Lexer.Error (Lexer.Unterminated_string, _)
      | Lexer.Error (Lexer.Unterminated_string_in_comment _, _)
      | Lexer.Error (Lexer.Illegal_character _, _) ->
          skip_phrase lexbuf

  let maybe_skip_phrase lexbuf =
    if Parsing.is_current_lookahead OCaml_parser.SEMISEMI
    || Parsing.is_current_lookahead OCaml_parser.EOF
    then ()
    else skip_phrase lexbuf

  let safeguard_parsing lexbuf fn =
    try fn ()
    with
    | Lexer.Error(Lexer.Illegal_character _, _) as err
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
    Pprintast.signature formatter
      (To_current.copy_signature signature)
  let format_implementation_with_comments (structure, _) formatter =
    Pprintast.structure formatter
      (To_current.copy_structure structure)
end

module Reason_syntax = struct
  module I = Reason_parser.MenhirInterpreter
  module Lexer_impl = Reason_lexer
  type token = Reason_parser.token

  (* [tracking_supplier] is a supplier that tracks the last token read *)
  type tracking_supplier = {
    (* The last token that was obtained from the lexer, together with its start
       and end positions. Warning: before the first call to the lexer has taken
       place, a None value is stored here. *)

    mutable last_token: (token * Lexing.position * Lexing.position) option;

    (* A supplier function that returns one token at a time*)
    get_token: unit -> (token * Lexing.position * Lexing.position)
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

  let rec normalize_checkpoint = function
    | I.Shifting _ | I.AboutToReduce _ as checkpoint ->
      normalize_checkpoint (I.resume checkpoint)
    | checkpoint -> checkpoint

  let offer_normalize checkpoint triple =
    normalize_checkpoint (I.offer checkpoint triple)

  let commit_invalid_docstrings = function
    | [] -> ()
    | docstrings ->
      let process_invalid_docstring (text, loc) = Reason_lexer.add_invalid_docstring text loc in
      List.iter process_invalid_docstring (List.rev docstrings)

  let rec handle_other supplier checkpoint =
    match checkpoint with
    | I.InputNeeded _ ->
      (* An input needed in the "other" case means we are recovering from an error *)
      begin match supplier.last_token with
        | None -> assert false
        | Some triple ->
          (* We just recovered from the error state, try the original token again *)
          let checkpoint_with_previous_token = I.offer checkpoint triple in
          match I.shifts checkpoint_with_previous_token with
          | None ->
            (* The original token still fail to be parsed, discard *)
            handle_inputs_needed supplier [([], checkpoint)]
          | Some env ->
            handle_inputs_needed supplier [([], checkpoint_with_previous_token)]
      end

    | I.HandlingError env when !Reason_config.recoverable ->
      let loc = last_token_loc supplier in
      begin match Syntax_util.findMenhirErrorMessage loc with
        | Syntax_util.MenhirMessagesError err -> ()
        | Syntax_util.NoMenhirMessagesError ->
          let state = state checkpoint in
          let msg =
            try Reason_parser_message.message state
            with Not_found -> "<SYNTAX ERROR>\n"
          in
          Syntax_util.add_error_message Syntax_util.{loc = loc; msg = msg};
      end;
      let checkpoint = I.resume checkpoint in
      (* Enter error recovery state *)
      handle_other supplier checkpoint

    | I.HandlingError env ->
      (* If not in a recoverable state, fail early by raising a
       * customized Error object
      *)
      let loc = last_token_loc supplier in
      let state = state checkpoint in
      (* Check the error database to see what's the error message
       * associated with the current parser state
      *)
      let msg =
        try Reason_parser_message.message state
        with Not_found -> "<UNKNOWN SYNTAX ERROR>"
      in
      let msg_with_state = Printf.sprintf "%d: %s" state msg in
      raise (Syntax_util.Error (loc, (Syntax_util.Syntax_error msg_with_state)))

    | I.Rejected ->
      let loc = last_token_loc supplier in
      raise Syntaxerr.(Error(Syntaxerr.Other loc))

    | I.Accepted v ->
      (* The parser has succeeded and produced a semantic value. *)
      v

    | I.Shifting _ | I.AboutToReduce _ ->
      handle_other supplier (normalize_checkpoint checkpoint)

  and handle_inputs_needed supplier checkpoints =
    match read supplier with
    (* ES6_FUN token marks a possible fork point *)
    | Reason_parser.ES6_FUN, _, _ as triple ->
      let process_checkpoint (invalid_docstrings, checkpoint as x) tl =
        match offer_normalize checkpoint triple with
        | I.HandlingError _ -> x :: tl
        | checkpoint' -> x :: (invalid_docstrings, checkpoint') :: tl
      in
      handle_inputs_needed supplier (List.fold_right process_checkpoint checkpoints [])

    | Reason_parser.DOCSTRING text, loc_start, loc_end as triple ->
      let process_checkpoint (invalid_docstrings, checkpoint) =
        match offer_normalize checkpoint triple with
        | I.HandlingError _ ->
          (* DOCSTRING at an invalid position: store it and add it back to comments
           * if this checkpoint is "committed"
           * TODO: print warning? *)
          let invalid_docstring = (text, { Location. loc_ghost = false; loc_start; loc_end }) in
          (invalid_docstring :: invalid_docstrings, checkpoint)
        | checkpoint' -> (invalid_docstrings, checkpoint')
      in
      handle_inputs_needed supplier (List.map process_checkpoint checkpoints)

    | triple ->
      begin match checkpoints with
        | [] -> assert false
        | [docstrings, checkpoint] ->
          begin match offer_normalize checkpoint triple with
            | I.InputNeeded _ as checkpoint' ->
              handle_inputs_needed supplier [docstrings, checkpoint']
            | checkpoint ->
              commit_invalid_docstrings docstrings;
              handle_other supplier checkpoint
          end
        | checkpoints ->
          let rec process_checkpoints inputs_needed others = function
            |  (docstrings, checkpoint) :: xs ->
              begin match offer_normalize checkpoint triple with
                | I.Accepted _ as other ->
                  commit_invalid_docstrings docstrings;
                  handle_other supplier other
                | I.InputNeeded _ as checkpoint' ->
                  process_checkpoints ((docstrings, checkpoint') :: inputs_needed) others xs
                | other -> process_checkpoints inputs_needed ((docstrings, other) :: others) xs
              end
            | [] ->
              match List.rev inputs_needed with
              | [] ->
                begin match List.rev others with
                  | (docstrings, checkpoint) :: _ ->
                    commit_invalid_docstrings docstrings;
                    handle_other supplier checkpoint
                  | [] -> assert false
                end
              | inputs_needed -> handle_inputs_needed supplier inputs_needed
          in
          process_checkpoints [] [] checkpoints
      end

  let initial_run constructor lexbuf =
    let checkpoint = constructor lexbuf.lex_curr_p in
    let supplier = lexbuf_to_supplier lexbuf in
    match normalize_checkpoint checkpoint with
    | I.InputNeeded _ as checkpoint ->
      handle_inputs_needed supplier [[], checkpoint]
    | other -> handle_other supplier other

  let implementation lexbuf =
    initial_run Reason_parser.Incremental.implementation lexbuf

  let interface lexbuf =
    initial_run Reason_parser.Incremental.interface lexbuf

  let core_type lexbuf =
    initial_run Reason_parser.Incremental.parse_core_type lexbuf

  let toplevel_phrase lexbuf =
    initial_run Reason_parser.Incremental.toplevel_phrase lexbuf

  let use_file lexbuf =
    initial_run Reason_parser.Incremental.use_file lexbuf

  (* Skip tokens to the end of the phrase *)
  let rec skip_phrase lexbuf =
    try
      match Lexer_impl.token lexbuf with
        Reason_parser.SEMI | Reason_parser.EOF -> ()
      | _ -> skip_phrase lexbuf
    with
    | Lexer_impl.Error (Lexer_impl.Unterminated_comment _, _)
    | Lexer_impl.Error (Lexer_impl.Unterminated_string, _)
    | Lexer_impl.Error (Lexer_impl.Unterminated_string_in_comment _, _)
    | Lexer_impl.Error (Lexer_impl.Illegal_character _, _) -> skip_phrase lexbuf

  let maybe_skip_phrase lexbuf =
    if Parsing.is_current_lookahead Reason_parser.SEMI
    || Parsing.is_current_lookahead Reason_parser.EOF
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
module RE = Create_parse_entrypoint (Reason_syntax)
