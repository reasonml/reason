let f x = match x with
| `A -> "A"
| `B -> "B"

let f = function
| `A -> "A"
| `B -> "B"

let f = fun x -> match x with
| `A -> "A"
| `B -> "B"

let f = 
  let g x = match x with
  | `A -> "A"
  | `B -> "B"
  in
  g

let f = 
  let g = function 
  | `A -> "A"
  | `B -> "B"
  in
  g

let f = 
  let g = fun x -> match x with
  | `A -> "A"
  | `B -> "B"
  in
  g

let z =
  begin match
    x
  with
  | X -> x
  end

let config_converter =
  (fun str -> try (* just check syntax *)
     ignore (IndentConfig.update_from_string IndentConfig.default str);
     `Ok str
   with Invalid_argument s -> `Error s),
  ignore (IndentConfig.update_from_string IndentConfig.default str);
  `Ok str

let f =
  try match a
  with B -> x
  with C -> y

let g =
  try match X with
  | X -> X
  with
  | X -> Y
