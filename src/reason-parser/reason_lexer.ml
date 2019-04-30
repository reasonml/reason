(* Filter commnets *)

let token_with_comments =
  Reason_declarative_lexer.token

let last_comments = ref []

let rec token lexbuf =
  match token_with_comments lexbuf with
      COMMENT (s, comment_loc) ->
        last_comments := (s, comment_loc) :: !last_comments;
        token lexbuf
    | tok -> tok

let add_invalid_docstring text loc =
  let open Location in
  let rec aux = function
    | ((_, loc') as x) :: xs
      when loc'.loc_start.pos_cnum > loc.loc_start.pos_cnum ->
      x :: aux xs
    | xs -> (text, loc) :: xs
  in
  last_comments := aux !last_comments

let comments () = List.rev !last_comments

(* Routines for manipulating lexer state *)

let save_triple lexbuf tok =
  (tok, lexbuf.lex_start_p, lexbuf.lex_curr_p)

let load_triple lexbuf (tok, p1, p2) = (
  lexbuf.lex_start_p <- p1;
  lexbuf.lex_curr_p <- p2;
  tok
)

let fake_triple t (_, pos, _) =
  (t, pos, pos)

(* insert ES6_FUN *)

exception Lex_balanced_failed of (token * position * position) list *
                                 (exn * position * position) option

let closing_of = function
  | LPAREN -> RPAREN
  | LBRACE -> RBRACE
  | _ -> assert false

let inject_es6_fun = function
  | tok :: acc ->
    tok :: fake_triple ES6_FUN tok :: acc
  | _ -> assert false

let is_triggering_token = function
  | EQUALGREATER | COLON -> true
  | _ -> false

let rec lex_balanced_step closing lexbuf acc tok =
  let acc = save_triple lexbuf tok :: acc in
  match tok, closing with
  | (RPAREN, RPAREN) | (RBRACE, RBRACE) | (RBRACKET, RBRACKET) ->
    acc
  | ((RPAREN | RBRACE | RBRACKET | EOF), _) ->
    raise (Lex_balanced_failed (acc, None))
  | (( LBRACKET | LBRACKETLESS | LBRACKETGREATER
     | LBRACKETAT
     | LBRACKETPERCENT | LBRACKETPERCENTPERCENT ), _) ->
    lex_balanced closing lexbuf (lex_balanced RBRACKET lexbuf acc)
  | ((LPAREN | LBRACE), _) ->
    let rparen =
      try lex_balanced (closing_of tok) lexbuf []
      with (Lex_balanced_failed (rparen, None)) ->
        raise (Lex_balanced_failed (rparen @ acc, None))
    in
    begin match token lexbuf with
    | exception exn ->
      raise (Lex_balanced_failed (rparen @ acc, Some (save_triple lexbuf exn)))
    | tok' ->
      let acc = if is_triggering_token tok' then inject_es6_fun acc else acc in
      lex_balanced_step closing lexbuf (rparen @ acc) tok'
    end
  | ((LIDENT _ | UNDERSCORE), _) ->
    begin match token lexbuf with
    | exception exn ->
      raise (Lex_balanced_failed (acc, Some (save_triple lexbuf exn)))
    | tok' ->
      let acc = if is_triggering_token tok' then inject_es6_fun acc else acc in
      lex_balanced_step closing lexbuf acc tok'
    end
  (* `...` with a closing `}` indicates that we're definitely not in an es6_fun
   * Image the following:
   *    true ? (Update({...a, b: 1}), None) : x;
   *    true ? ({...a, b: 1}) : a;
   *    true ? (a, {...a, b: 1}) : a;
   * The lookahead_esfun is triggered initiating the lex_balanced procedure.
   * Since we now "over"-parse spread operators in pattern position (for
   * better errors), the ... pattern in ({...a, b: 1}) is now a valid path.
   * This means that the above expression `({...a, b: 1}) :` is seen as a pattern.
   * I.e. the arguments of an es6 function: (pattern) :type => expr
   * We exit here, to indicate that an expression needs to be parsed instead
   * of a pattern.
   *)
  | (DOTDOTDOT, RBRACE) -> acc
  | _ -> lex_balanced closing lexbuf acc

and lex_balanced closing lexbuf acc =
  match token lexbuf with
  | exception exn ->
    raise (Lex_balanced_failed (acc, Some (save_triple lexbuf exn)))
  | tok -> lex_balanced_step closing lexbuf acc tok

let queued_tokens = ref []
let queued_exn = ref None

let lookahead_esfun lexbuf (tok, _, _ as lparen) =
  let triple =
    match lex_balanced (closing_of tok) lexbuf [] with
    | exception (Lex_balanced_failed (tokens, exn)) ->
      queued_tokens := List.rev tokens;
      queued_exn := exn;
      lparen
    | tokens ->
      begin match token lexbuf with
        | exception exn ->
          queued_tokens := List.rev tokens;
          queued_exn := Some (save_triple lexbuf exn);
          lparen
        | token ->
          let tokens = save_triple lexbuf token :: tokens in
          if is_triggering_token token then (
            queued_tokens := lparen :: List.rev tokens;
            fake_triple ES6_FUN lparen
          ) else (
            queued_tokens := List.rev tokens;
            lparen
          )
      end
  in
  load_triple lexbuf triple

let token lexbuf =
  match !queued_tokens, !queued_exn with
  | [], Some exn ->
    queued_exn := None;
    raise (load_triple lexbuf exn)
  | [(LPAREN, _, _) as lparen], None ->
    let _ = load_triple lexbuf lparen in
    lookahead_esfun lexbuf lparen
  | [(LBRACE, _, _) as lparen], None ->
    let _ = load_triple lexbuf lparen in
    lookahead_esfun lexbuf lparen
  | [], None ->
    begin match token lexbuf with
    | LPAREN | LBRACE as tok ->
        lookahead_esfun lexbuf (save_triple lexbuf tok)
    | (LIDENT _ | UNDERSCORE) as tok ->
        let tok = save_triple lexbuf tok in
        begin match token lexbuf with
        | exception exn ->
            queued_exn := Some (save_triple lexbuf exn);
            load_triple lexbuf tok
        | tok' ->
          if is_triggering_token tok' then (
            queued_tokens := [tok; save_triple lexbuf tok'];
            load_triple lexbuf (fake_triple ES6_FUN tok)
          ) else (
            queued_tokens := [save_triple lexbuf tok'];
            load_triple lexbuf tok
          )
        end
    | token -> token
    end
  | x :: xs, _ -> queued_tokens := xs; load_triple lexbuf x

let completion_ident_offset = ref min_int
let completion_ident_pos = ref Lexing.dummy_pos

let token lexbuf =
  let space_start = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum in
  let token = token lexbuf in
  let token_start = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum in
  let token_stop = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum in
  if !completion_ident_offset > min_int &&
     space_start <= !completion_ident_offset &&
     token_stop >= !completion_ident_offset then (
    match token with
    | LIDENT _ | UIDENT _ when token_start <= !completion_ident_offset ->
        completion_ident_offset := min_int;
        token
    | _ ->
      queued_tokens := save_triple lexbuf token :: !queued_tokens;
      completion_ident_offset := min_int;
      load_triple lexbuf
        (LIDENT "_", !completion_ident_pos, !completion_ident_pos)
  ) else
    token

let init ?insert_completion_ident () =
  is_in_string := false;
  last_comments := [];
  comment_start_loc := [];
  queued_tokens := [];
  queued_exn := None;
  begin match insert_completion_ident with
    | None ->
      completion_ident_offset := min_int;
    | Some pos ->
      completion_ident_offset := pos.Lexing.pos_cnum;
      completion_ident_pos := pos
  end
