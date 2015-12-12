(* A *)

type x = 
  (* A *)
  | Foo

  (* B *)

  | Bar

(* AA *)

(* D *)
let x = 3

module M = struct
  (* M1 *)
  let x =
    a
  (* M2 *)
  let y =
    b

  (* M3 *)
  (* M4 *)
end

let f x =
  if true then
    0
    (* comment *)
  else if false then
    1

let g x =
  if true then
    0

  (* comment *)
  else if false then
    1

let _ =
  f x
    (* bla *) y
    (* bla *) (z)

(* ending comments *)

