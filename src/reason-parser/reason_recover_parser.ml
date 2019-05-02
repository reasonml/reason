module M = Reason_multi_parser
module R =
  Merlin_recovery.Make(Reason_parser.MenhirInterpreter)
    (struct
      include Reason_parser_recover

      let default_value loc x =
        Default.default_loc := loc;
        default_value x

      let guide _ = false
    end)

type 'a parser =
  | Correct of 'a M.parser
  | Recovering of 'a R.candidates * Reason_lexer.invalid_docstrings

let initial entry_point position =
  Correct (M.initial entry_point position)

type 'a step =
  | Intermediate of 'a parser
  | Success of 'a * Reason_lexer.invalid_docstrings
  | Error

let step parser token =
  match parser with
  | Correct parser ->
    begin match M.step parser token with
      | M.Intermediate parser -> Intermediate (Correct parser)
      | M.Success (x, ds) -> Success (x, ds)
      | M.Error ->
        let (_, loc_start, loc_end) = token in
        let loc = {Location. loc_start; loc_end; loc_ghost = false} in
        Reason_errors.raise_error (Reason_errors.Parsing_error ()) loc;
        let env, ds = M.recovery_env parser in
        Intermediate (Recovering (R.generate env, ds))
    end
  | Recovering (candidates, ds) ->
    begin match token with
      | Reason_parser.DOCSTRING text, startp, endp ->
        let ds = Reason_lexer.add_invalid_docstring text startp endp ds in
        Intermediate (Recovering (candidates, ds))
      | _ ->
        match R.attempt candidates token with
        | `Ok (cp, _) -> Intermediate (Correct (M.recover cp ds))
        | `Fail -> Error
        | `Accept x -> Success (x, ds)
    end

