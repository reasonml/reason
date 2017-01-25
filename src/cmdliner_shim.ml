open Cmdliner
exception Invalid_config = Printer_maker.Invalid_config

(*
What follows is a gross violation of natural law.

Motivation: sometimes `refmt` raises exceptions, and we want to handle those
            before Cmdliner does. While we could either:
              1) wrap the entirety of the body of refmt in a try/with
                OR
              2) write a wrapper function that takes the same # of arguments,
            we decided to try this instead.
*)

type +'a q = Success of 'a | Exception of string

let wrap x = Success x

let unwrap = function
  | Success x -> x
  | Exception m -> `Error (true, m)

(* TODO: allow the user of this module to pass in a custom exn handler *)
let app : ('a -> 'b) q -> 'a -> 'b q = fun f a ->
  try
    match f with
    | Success f' -> Success (f' a)
    | _ -> failwith "can't happen"
  with
  | Invalid_config m -> Exception m
  | exn -> Exception (Printexc.to_string exn)

let load x =
    Term.app (Term.const unwrap) x

let appq : (('a -> 'b) q) Term.t -> 'a Term.t -> ('b q) Term.t = fun qtF at ->
    Term.app (Term.app (Term.const app) qtF) at
