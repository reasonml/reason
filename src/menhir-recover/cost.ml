type t = int

let zero = 0
let infinite = max_int
let compare : t -> t -> int = compare

let add t1 t2 =
  let result = t1 + t2 in
  if result < 0 then infinite else result

let of_int x =
  if x < 0 then invalid_arg "Cost.of_int: cost must be positive" else x

let to_int x = x
let is_infinite x = x = infinite
let arg_min f a b = if compare (f a) (f b) <= 0 then a else b
let pp = Format.pp_print_int
