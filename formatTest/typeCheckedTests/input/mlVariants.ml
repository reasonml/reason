(* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

type polyVariantsInMl = [
  | `IntTuple of (int * int)
  | `StillAnIntTuple of (int * int)
]



let intTuple = `IntTuple (1, 2)
let stillAnIntTuple = `StillAnIntTuple (4, 5)
let sumThem = function
  | `IntTuple (x, y) -> x + y
  | `StillAnIntTuple (a, b) -> a + b

type nonrec t = A of int | B of bool

type t2 = [ `a | `b ]
type u2 = [ t2 | `c ]
let listPatternWithHash = function
  | #t2 -> 1
  | `c -> 2