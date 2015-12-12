let x =
  try y with
    | A -> ()
    | B -> ()

let x = try y with
  | A -> 0
  | B -> 0

let x =
  try y with
      A -> 0
    | B -> 0

let x = try y with
    A -> 0
  | B -> 0

let _ =
  let x =
    try y with
      | A -> 0
      | B -> 0
  in
  let x = try y with
    | A -> 0
    | B -> 0
  in
  let x =
    try y with
        A -> 0
      | B -> 0
  in
  let x = try y with
      A -> 0
    | B -> 0
  in x
