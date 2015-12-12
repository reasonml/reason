(** ocaml language extensions
    (http://caml.inria.fr/pub/docs/manual-ocaml/manual021.html)
*)

(* other integer literals *)
let i = 12l
        + 0l

let i = 12L
        + 0l

let i = 12n
        + 0n

(* range patterns *)
let f = function
  | 'a'..'z' ->
    e1
  | 'A'..'Z'
  | '0'..'9' ->
    e2

(* local modules *)
let f =
  let module M =
    F(struct end)
  in
  M.f x

(* recursive modules *)
module rec M : S =
struct
  ;;
end
and M1 : S1 =
struct
  ;;
end

(* private types *)
type t = private
    X of string
  | Y

type t = private
  { f1:t1;
    f2: t2 }

type t =
  private t'

(* local opens *)

module Res = F(X)
let _ =
  let open Res
  in
  ()

(* record shortcuts *)
let _ =
  let x = 1 and y = 2
  in
  { x;
    y
  }

let f = function
  | { x;
      y;
      _
    } ->
    ()

(* locally abstract types *)
let f = fun
  (type t)
  (x: t)
  ->
    ()

let f
    (type t)
    (x: t)
  =
  ()

(* first-class modules *)
type m =
  (module M.Sig
    with type t = 'b)
  * unit


(* module type of *)
module type S = sig
  include module type of M
end

(* signature substitution *)
module type S = sig
  include
    M0 with type t := t
  val x : unit
end

(* class overriding *)
class cl = object
  inherit!
      cl
  val! v = v
  method! m = m
end

(* GADTs *)
type _ t =
    A: int t
  | B: 'a t * 'b t -> ('a*'b) t


