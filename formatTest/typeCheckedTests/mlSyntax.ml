(*
 * Testing pattern matching using ml syntax to exercise nesting of cases.
 *)


type xyz =
  | X
  | Y of int * int * int
  | Z of int * int
  | Q
  | R

let doubleBar = function
  | X | Y (_, _, _) | Z (_, _) | Q -> true
  | _ -> false

let doubleBarNested = function
  | X | Y (_, _, _) | (Z (_, _) | Q)  -> true
  | _ -> false

type bcd = B | C | D | E
type a = A of bcd
let result = match B with
  | B
  | C
  | D
  | E -> ()

let nested_match = function | A (B | C | D | E) -> 3

module EM = struct
  exception E of int * int
end

exception Ealias = EM.E
