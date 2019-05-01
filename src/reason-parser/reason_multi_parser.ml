module S = Reason_single_parser

type 'a parser = 'a S.parser list
type 'a erroneous_parser = 'a S.erroneous_parser

let initial entry_point position =
  [S.initial entry_point position]

type 'a step =
  | Intermediate of 'a parser
  | Success of 'a * Reason_lexer.invalid_docstrings
  | Error of 'a erroneous_parser

let rec fork token = function
  | [] -> []
  | x :: xs ->
    begin match S.step x token with
    | S.Intermediate x' -> x :: x' :: fork token xs
    | _ -> x :: fork token xs
    end

let rec progress_successful token acc = function
  | [] -> Intermediate (List.rev acc)
  | x :: xs ->
    begin match S.step x token with
      | S.Intermediate p ->
        progress_successful token (p :: acc) xs
      | S.Error _ ->
        progress_successful token acc xs
      | S.Success (result, ds) -> Success (result, ds)
    end

let step parsers token =
  match token with
  | (Reason_parser.ES6_FUN, _, _) ->
    (* Fork case *)
    Intermediate (fork token parsers)
  | _ ->
    (* Regular case *)
    match parsers with
    | [x] ->
      (* Fast-path: One parser *)
      begin match S.step x token with
        | S.Intermediate parser -> Intermediate [parser]
        | S.Success (result, ds) -> Success (result, ds)
        | S.Error erroneous -> Error erroneous
      end
    (* Parallel parsing case *)
    | x :: xs ->
      begin match S.step x token with
        | S.Intermediate p -> progress_successful token [p] xs
        | S.Success (result, ds) -> Success (result, ds)
        | S.Error erroneous ->
          begin match progress_successful token [] xs with
            | Intermediate [] -> Error erroneous
            | result -> result
          end
      end
    (* Impossible case *)
    | [] -> assert false
