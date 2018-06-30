(***********************************************************************)
(*                                                                     *)
(*                                Reason                               *)
(*                                                                     *)
(***********************************************************************)
(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)



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

open Reason_toolchain_conf
open Migrate_parsetree
open Ast_408

open Location
open Lexing

module Comment = Reason_comment

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

  let extensions_of_errors errors =
    ignore (Format.flush_str_formatter () : string);
    let error_extension (err, loc) =
      Reason_errors.report_error Format.str_formatter ~loc err;
      let msg = Format.flush_str_formatter () in
      let due_to_recovery = match err with
        | Reason_errors.Parsing_error _ -> true
        | Reason_errors.Lexing_error _ -> false
        | Reason_errors.Ast_error _ -> false
      in
      if due_to_recovery then
        Reason_errors.error_extension_node_from_recovery loc msg
      else
        Reason_errors.error_extension_node loc msg
    in
    List.map error_extension errors

  let wrap_with_comments parsing_fun attach_fun lexbuf =
    let input_copy = Buffer.create 0 in
    let lexbuf = keep_from_lexbuf input_copy lexbuf in
    Toolchain_impl.safeguard_parsing lexbuf (fun () ->
      let lexer =
        let insert_completion_ident =
          !Reason_toolchain_conf.insert_completion_ident in
        Toolchain_impl.Lexer.init ?insert_completion_ident lexbuf
      in
      let ast, invalid_docstrings =
        let result =
          if !Reason_config.recoverable
          then Reason_errors.recover_non_fatal_errors
              (fun () -> parsing_fun lexer)
          else (Ok (parsing_fun lexer), [])
        in
        match result with
        | Ok x, [] -> x
        | Ok (x, ds), errors -> (attach_fun x (extensions_of_errors errors), ds)
        | Error exn, _ -> raise exn
      in
      let unmodified_comments =
        Toolchain_impl.Lexer.get_comments lexer invalid_docstrings
      in
      let contents = Buffer.contents input_copy in
      Buffer.reset input_copy;
      if contents = "" then
        let _  = Parsing.clear_parser() in
        let make_regular (text, location) =
          Comment.make ~location Comment.Regular text in
        (ast, List.map make_regular unmodified_comments)
      else
        let rec classifyAndNormalizeComments unmodified_comments =
          match unmodified_comments with
          | [] -> []
          | hd :: tl -> (
              let classifiedTail = classifyAndNormalizeComments tl in
              let (txt, physical_loc) = hd in
              (* When searching for "^" regexp, returns location of newline + 1 *)
              let (stop_char, eol_start, virtual_start_pos) =
                left_expand_comment false contents physical_loc.loc_start.pos_cnum
              in
              if Reason_syntax_util.isLineComment txt then
                let comment = Comment.make
                  ~location:physical_loc
                  (if eol_start then SingleLine else EndOfLine)
                  txt
                in
                comment :: classifiedTail
              else
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
               *   false
               *
               *)
              let should_scan_next_line = stop_char = '|' &&
                                          (one_char_before_stop_char = ' ' ||
                                           one_char_before_stop_char = '\n' ||
                                           one_char_before_stop_char = '\t' ) in
              let (_, eol_end, virtual_end_pos) = right_expand_comment should_scan_next_line contents physical_loc.loc_end.pos_cnum in
              let end_pos_plus_one = physical_loc.loc_end.pos_cnum in
              let comment_length = (end_pos_plus_one - physical_loc.loc_start.pos_cnum - 4) in
              let original_comment_contents = String.sub contents (physical_loc.loc_start.pos_cnum + 2) comment_length in
              let location = {
                physical_loc with
                loc_start = {physical_loc.loc_start with pos_cnum = virtual_start_pos};
                loc_end = {physical_loc.loc_end with pos_cnum = virtual_end_pos}
              } in
              let just_after loc' =
                loc'.loc_start.pos_cnum == location.loc_end.pos_cnum - 1 &&
                loc'.loc_start.pos_lnum == location.loc_end.pos_lnum
              in
              let category = match (eol_start, eol_end, classifiedTail) with
                | (true, true, _) -> Comment.SingleLine
                | (false, true, _) -> Comment.EndOfLine
                | (false, false, comment :: _)
                  (* End of line comment is one that has nothing but newlines or
                   * other comments its right, and has some AST to the left of it.
                   * For example, there are two end of line comments in:
                   *
                   *    | Y(int, int); /* eol1 */ /* eol2 */
                   *)
                  when Comment.category comment = Comment.EndOfLine
                    && just_after (Comment.location comment) ->
                  Comment.EndOfLine
                | _ -> Comment.Regular
              in
              let comment =
                Comment.make ~location category original_comment_contents
              in
              comment :: classifiedTail
            )
        in
        let modified_and_comment_with_category = classifyAndNormalizeComments unmodified_comments in
        let _  = Parsing.clear_parser() in
        (ast, modified_and_comment_with_category)
    )

  let default_error lexbuf err =
    if !Reason_config.recoverable then
      let loc, msg = match err with
        | Location.Error err ->
          Reason_syntax_util.split_compiler_error err
        | Reason_errors.Reason_error (e, loc) ->
          Reason_errors.report_error Format.str_formatter ~loc e;
          (loc, Format.flush_str_formatter ())
        | exn ->
          (Location.curr lexbuf, "default_error: " ^ Printexc.to_string exn)
      in
      (loc, Reason_errors.error_extension_node loc msg)
    else
      raise err

  let ignore_attach_errors x _extensions =
    (* FIXME: attach errors in AST *) x

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
    let attach impl extensions =
      (impl @ List.map Ast_helper.Str.extension extensions)
    in
    try wrap_with_comments Toolchain_impl.implementation attach lexbuf
    with err ->
      let loc, error = default_error lexbuf err in
      ([Ast_helper.Str.mk ~loc (Parsetree.Pstr_extension (error, []))], [])

  let core_type_with_comments lexbuf =
    try wrap_with_comments Toolchain_impl.core_type ignore_attach_errors lexbuf
    with err ->
      let loc, error = default_error lexbuf err in
      (Ast_helper.Typ.mk ~loc (Parsetree.Ptyp_extension error), [])

  let interface_with_comments lexbuf =
    let attach impl extensions =
      (impl @ List.map Ast_helper.Sig.extension extensions)
    in
    try wrap_with_comments Toolchain_impl.interface attach lexbuf
    with err ->
      let loc, error = default_error lexbuf err in
      ([Ast_helper.Sig.mk ~loc (Parsetree.Psig_extension (error, []))], [])

  let toplevel_phrase_with_comments lexbuf =
    wrap_with_comments
      Toolchain_impl.toplevel_phrase ignore_attach_errors lexbuf

  let use_file_with_comments lexbuf =
    wrap_with_comments Toolchain_impl.use_file ignore_attach_errors lexbuf

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

module ML = Create_parse_entrypoint (Reason_toolchain_ocaml)
module RE = Create_parse_entrypoint (Reason_toolchain_reason)
module From_current = From_current
module To_current = To_current
