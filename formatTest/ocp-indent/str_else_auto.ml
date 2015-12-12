let () =
  if true then "bla" else
  if true then "bli" else
    "blo"

let () =
  if true then "bla" else
  if true then "bli" else
    begin
      "hop"
    end

let () =
  if true then "hop" else
  if true then "hap" else
    ((); "bla")

let () =
  if
    x
  then
    y
  else k,
       w;
  z

let () =
  if x then a
  else
  let y = x / 42 in
  y

let () =
  if x then a
  else if y
  then b
  else begin
    blabla
  end;
  x

let () =
  if x then
    a
  else match y with
    | A -> x
    | B -> y

let () =
  if x then
    a
  else
  match y with
  | A -> x
  | B -> y

let () =
  if x then a else
  fun x ->
    y
