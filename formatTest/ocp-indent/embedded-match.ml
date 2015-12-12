let f x = function
  | A when match x with A | B -> true | _ -> false
    ->
    B
  | A -> x
  | _ -> B

let f x =
  if
    match x with
    | A -> true
  then
    1
  else
    0

let f x =
  match x with
  | A -> true
  | B ->
    false
  | exception
      Not_found ->
    false
  | C -> true
  | exception (Failure _ | Invalid_argument _) ->
    true
  | exception (A | B) | exception B.Err
  | exception C.Types.Xxx "someparam" ->
    false

exception MyExn of string
