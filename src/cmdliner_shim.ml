open Cmdliner

(*
What follows is a gross violation of natural law.

Motivation: sometimes `refmt` raises exceptions, and we want to handle those
            before Cmdliner does. While we could either:
              1) wrap the entirety of the body of refmt in a try/with
                OR
              2) write a wrapper function that takes the same # of arguments,
            we decided to try this instead.
*)

type handler = exn -> string
type +'a q = Success of ('a * handler) | Exception of string

let wrap x handler = Success (x, handler)

let unwrap = function
  | Success (x, _) -> x
  | Exception m -> `Error (true, m)

(* TODO: allow the user of this module to pass in a custom exn handler *)
let app : ('a -> 'b) q -> 'a -> 'b q = fun f a ->
  let (f', handler) = match f with
    | Success x -> x
    | _ -> failwith "can't happen"
  in
  try
    Success ((f' a), handler)
  with
  | exn -> Exception (handler exn)

let load x =
    Term.app (Term.const unwrap) x

let appq : (('a -> 'b) q) Term.t -> 'a Term.t -> ('b q) Term.t = fun qtF at ->
    Term.app (Term.app (Term.const app) qtF) at

let ($) = appq
