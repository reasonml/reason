(*
 * Testing pattern matching using ml syntax to exercise nesting of cases.
 *)


let doubleBar = function
  | Empty | Leaf (_, _, _) | HashCollision (_, _) | Sealife | Munro -> true
  | _ -> false

let doubleBarNested = function
  | Empty | Leaf (_, _, _) | (HashCollision (_, _) | Sealife) | Munro -> true
  | _ -> false

