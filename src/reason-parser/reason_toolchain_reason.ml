open Reason_toolchain_conf

module I = Reason_parser.MenhirInterpreter
module Lexer_impl = struct
  include Reason_lexer
  let init () = init ?insert_completion_ident:!insert_completion_ident ()
end
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

let last_token supplier =
  match supplier.last_token with
  | Some (t, _, _) -> t
  | None -> assert false

(* read last token's location from a supplier *)
let last_token_loc supplier =
  match supplier.last_token with
  | Some (_, s, e) ->
     { Location.
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

let rec normalize_checkpoint = function
  | I.Shifting _ | I.AboutToReduce _ as checkpoint ->
     normalize_checkpoint (I.resume checkpoint)
  | checkpoint -> checkpoint

(* Simply submit a token to the parser *)
let offer_normalize checkpoint triple =
  normalize_checkpoint (I.offer checkpoint triple)

let offer_normalize_many checkpoint triples =
  let rec loop acc xs = match xs with
    | [] -> Some acc
    | triple :: xs ->
       begin match normalize_checkpoint (I.offer acc triple) with
       | I.InputNeeded _ as checkpoint' -> loop checkpoint' xs
       | _ -> None
       end
  in
  loop checkpoint triples

(* Insert a semicolon before submitting a token to the parser *)
let try_inserting_semi_on = function
  | Reason_parser.LET
    | Reason_parser.TYPE
    | Reason_parser.MODULE
    | Reason_parser.OPEN
    | Reason_parser.EXCEPTION
    | Reason_parser.INCLUDE
    | Reason_parser.DOCSTRING _
    | Reason_parser.LIDENT _
    | Reason_parser.UIDENT _
    | Reason_parser.IF
    | Reason_parser.WHILE
    | Reason_parser.FOR
    | Reason_parser.SWITCH
    | Reason_parser.TRY
    | Reason_parser.ASSERT
    | Reason_parser.LAZY
    | Reason_parser.EXTERNAL
    | Reason_parser.LBRACKETAT -> true
  | _ -> false

let try_inserting_semi checkpoint ((_, pos, _) as triple) =
  match offer_normalize checkpoint (Reason_parser.SEMI, pos, pos) with
  | I.InputNeeded _ as checkpoint' ->
     Some (offer_normalize checkpoint' triple)
  | _ -> None

let try_inserting_label_on = function
  | Reason_parser.INFIXOP0 s when s.[0] == '=' ->
     let is_optional = String.length s > 1 && s.[1] == '?' in
     let idx = if is_optional then 2 else 1 in
     let operator = String.sub s idx (String.length s - idx) in
     let token = match operator with
       | "-" -> Some Reason_parser.MINUS
       | "-." -> Some Reason_parser.MINUSDOT
       | "+" -> Some Reason_parser.PLUS
       | "+." -> Some Reason_parser.PLUSDOT
       | "!" -> Some Reason_parser.BANG
       | _ -> None
     in
     token, is_optional
  | _ -> None, false

let try_inserting_equal_unary checkpoint (_, pos, _) ~optional token =
  match offer_normalize checkpoint (Reason_parser.EQUAL, pos, pos) with
  | I.InputNeeded _ as checkpoint' ->
     if optional then
       match offer_normalize checkpoint' (Reason_parser.QUESTION, pos, pos) with
       | I.InputNeeded _ as checkpoint'' ->
          Some (offer_normalize checkpoint'' (token, pos, pos))
       | _ -> None
     else
       Some (offer_normalize checkpoint' (token, pos, pos))
  | _ -> None

let try_inserting_postfix checkpoint infix ((_, pos, _) as triple) =
  (* we know that the infix was exclusively composed of '^' *)
  let rec mk_postfixes acc i =
    if i < 0 then
      acc
    else
      let triple = (Reason_parser.POSTFIXOP "^", pos, pos) in
      mk_postfixes (triple :: acc) (i - 1)
  in
  let infixes = mk_postfixes [] (String.length infix - 1) in
  match offer_normalize_many checkpoint infixes with
  | Some (I.InputNeeded _ as checkpoint') ->
     Some (offer_normalize checkpoint' triple)
  | _ -> None

(* Offer and insert a semicolon in case of failure *)
let offer_normalize checkpoint triple =
  match offer_normalize checkpoint triple with
  | I.HandlingError _ as error_checkpoint ->
     (* About to enter error state:
         if the token is the beginning of an item (LET, TYPE, ...), try
         inserting a SEMICOLON, otherwise return the checkpoint to the caller.
      *)
     let (token, _, _) = triple in
     if try_inserting_semi_on token then
       match try_inserting_semi checkpoint triple with
       | Some (I.InputNeeded _ as checkpoint') -> checkpoint'
       | Some _ | None -> error_checkpoint
     else begin match try_inserting_label_on token with
          | Some tokn, optional ->
             begin match try_inserting_equal_unary ~optional checkpoint triple tokn with
             | Some (I.InputNeeded _ as checkpoint') -> checkpoint'
             | Some _ | None -> error_checkpoint
             end
          | None, _ -> error_checkpoint
          end
  | checkpoint -> checkpoint

let commit_invalid_docstrings = function
  | [] -> ()
  | docstrings ->
     let process_invalid_docstring (text, loc) = Reason_lexer.add_invalid_docstring text loc in
     List.iter process_invalid_docstring (List.rev docstrings)

let custom_error supplier env =
  let loc = last_token_loc supplier in
  let token = match supplier.last_token with
    | Some token -> token
    | None -> assert false
  in
  let state = I.current_state_number env in
  (* Check the error database to see what's the error message
    associated with the current parser state  *)
  let msg = Reason_parser_explain.message env token in
  let msg_with_state = Printf.sprintf "%d: %s" state msg in
  raise (Reason_syntax_util.Error (loc, (Reason_syntax_util.Syntax_error msg_with_state)))

let rec handle_other supplier checkpoint =
  match checkpoint with
  | I.InputNeeded env ->
     (* An input needed in the "other" case means we are recovering from an error *)
     begin match supplier.last_token with
     | None -> assert false
     | Some triple ->
        (* We just recovered from the error state, try the original token again *)
        let checkpoint_with_previous_token = I.offer checkpoint triple in
        let checkpoint = try match I.shifts checkpoint_with_previous_token with
                             | None -> checkpoint
                             (* The original token still fail to be parsed, discard *)
                             | Some _ -> normalize_checkpoint checkpoint_with_previous_token
                         with
                         | Syntaxerr.Error _ as exn -> raise exn
                         | Syntaxerr.Escape_error -> custom_error supplier env
        in
        handle_inputs_needed supplier [([], checkpoint)]
     end

  (*| I.HandlingError env when !Reason_config.recoverable ->
     let loc = last_token_loc supplier in
     begin match Reason_syntax_util.findMenhirErrorMessage loc with
     | Reason_syntax_util.MenhirMessagesError _ -> ()
     | Reason_syntax_util.NoMenhirMessagesError ->
        let token = match supplier.last_token with
          | Some token -> token
          | None -> assert false
        in
        let msg = Reason_parser_explain.message env token in
        Reason_syntax_util.add_error_message Reason_syntax_util.{loc = loc; msg = msg};
     end;
     let checkpoint = I.resume checkpoint in
     (* Enter error recovery state *)
     handle_other supplier checkpoint*)

  | I.HandlingError env as error_checkpoint ->
     (* If not in a recoverable state, resume just enough to be able to
      * catch a nice error message above. *)
     let token = last_token supplier in
     if Hashtbl.mem Reason_lexer.reverse_keyword_table token then
       custom_error supplier env
     else
       let cp = I.resume error_checkpoint in
       handle_other supplier (normalize_checkpoint cp)
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
          (* DOCSTRING at an invalid position: store it and add it back to
           * comments if this checkpoint is "committed"
           * TODO: print warning? *)
          let invalid_docstring = (text, { Location. loc_ghost = false; loc_start; loc_end }) in
          (invalid_docstring :: invalid_docstrings, checkpoint)
       | checkpoint' -> (invalid_docstrings, checkpoint')
     in
     handle_inputs_needed supplier (List.map process_checkpoint checkpoints)

  (*
   * This catches the `foo^^` case (which the parser thinks is an error – infix
   * without 2nd operand) and inserts as many postfix ops as there are carets
   * in the operator. We catch the token here and don't actually fork checkpoints
   * because catching this in `handle_other` would be too late – we would only
   * see the token where the error was (e.g. semicolon).
   *)
  | Reason_parser.INFIXOP1 op, _, _ as triple
       when List.for_all (fun x -> x == '^') (Reason_syntax_util.explode_str op) ->
     let rec process_checkpoints inputs_needed checkpoints =
       match checkpoints with
       | [] -> handle_inputs_needed supplier inputs_needed
       | (invalid_docstrings, checkpoint) :: tl ->
          begin match offer_normalize checkpoint triple with
          | I.InputNeeded _ as checkpoint' ->
             let next_triple = read supplier in
             begin match offer_normalize checkpoint' next_triple with
             | I.HandlingError _ ->
                begin match try_inserting_postfix checkpoint op next_triple with
                | Some (I.Accepted _ as cp) -> handle_other supplier cp
                | Some cp ->
                   process_checkpoints ((invalid_docstrings, cp) :: inputs_needed) tl
                | None ->
                   process_checkpoints ((invalid_docstrings, checkpoint') :: inputs_needed) tl
                end
             | checkpoint'' ->
                process_checkpoints ((invalid_docstrings, checkpoint'') :: inputs_needed) tl
             end
          | checkpoint' ->
             process_checkpoints ((invalid_docstrings, checkpoint') :: inputs_needed) tl
          end
     in
     process_checkpoints [] checkpoints

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
  let checkpoint = constructor lexbuf.Lexing.lex_curr_p in
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
  | Location.Error _ as x ->
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
