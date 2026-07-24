module S = Reason_single_parser

type 'a parser = 'a S.parser list

let initial entry_point position = [ S.initial entry_point position ]

type 'a step =
  | Intermediate of 'a parser
  | Success of 'a * Reason_lexer.invalid_docstrings
  | Error

(* When the lexer injects ES6_FUN (its lookahead saw `(...)` followed by `=>`
   or `:`), we fork: [x] continues without the token (e.g. the expression
   reading) and [x'] consumes it (the arrow-function reading). Both parsers may
   run to completion when the source is genuinely ambiguous — e.g.
   [f((x: int): int => x)], where `int => x` is also a valid arrow *type*, so
   the whole argument can be read as the constrained expression
   [(x: int) : (int => x)]. Since [step]/[progress_successful] return the
   first success in list order, the ordering here decides the winner. Put the
   arrow-function parser first: an ES6 arrow is what such code means in
   practice, and this matches how the same text already parses in let-binding
   position (where the bare-constraint reading is not grammatical). See
   reasonml/reason#2925. *)
let rec fork token = function
  | [] -> []
  | x :: xs ->
    (match S.step x token with
    | S.Intermediate x' -> x' :: x :: fork token xs
    | _ -> x :: fork token xs)

let rec progress_successful token acc = function
  | [] -> Intermediate (List.rev acc)
  | x :: xs ->
    (match S.step x token with
    | S.Intermediate p -> progress_successful token (p :: acc) xs
    | S.Error -> progress_successful token acc xs
    | S.Success (result, ds) -> Success (result, ds))

let step parsers token =
  match token with
  | Reason_parser.ES6_FUN, _, _ ->
    (* Fork case *)
    Intermediate (fork token parsers)
  | _ ->
    (* Regular case *)
    (match parsers with
    | [ x ] ->
      (* Fast-path: One parser *)
      (match S.step x token with
      | S.Intermediate parser -> Intermediate [ parser ]
      | S.Success (result, ds) -> Success (result, ds)
      | S.Error -> Error)
    (* Parallel parsing case *)
    | x :: xs ->
      (match S.step x token with
      | S.Intermediate p -> progress_successful token [ p ] xs
      | S.Success (result, ds) -> Success (result, ds)
      | S.Error ->
        (match progress_successful token [] xs with
        | Intermediate [] -> Error
        | result -> result))
    (* Impossible case *)
    | [] -> assert false)

(* Interface for recovery *)

let recover cp ds = [ S.recover cp ds ]
let recovery_env = function [] -> assert false | x :: _xs -> S.recovery_env x
