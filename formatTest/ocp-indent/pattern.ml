let f = match x with
  | { x = 3 } ->
    let x = 4 in
    ()

let f = match x with
  | (X|Y) | (Z|U) -> 1
  | K -> 2

let f = match x with
  | X when foo = bar ->
    fff
  | Y when f = x
        && g = 3 ->
    z

let f () =
  match s with
  (* Parenthesized ident ? *)
  | x -> x, d
  (* Regular ident *)
  | _ -> g
;;

match x with
| X | Y -> 1
| X     ->
  2;
  3
| A -> 2
;;

let f g =
  (* haha *)
  match z with
  | Z | B _ -> x
  | A (a, _, _, b) as x ->
    let x = f a and hr = f b in
    f

let unwind_to =
  match t with KType | KModule -> true | Kblob -> false
             | _ -> true

let f x = match x with
  | A | B
  | C ->
    x
  | z -> match z with
    | _ -> function
      | x ->
        x

let fun_dep ulam = function
  | A
  | B
  | C ->
    ()

let fun_dep ulam = function
  |A
  |B|C
  |D ->
    ()

(* let _ = *)
(*   (match bla *)
(*    with bli) *)
