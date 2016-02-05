/*
 * Testing pattern matching using ml syntax to exercise nesting of cases.
 */
type xyz =
  | X | Y of int int int | Z of int int | Q | R;

let doubleBar =
  fun | X
      | Y _ _ _
      | Z _ _
      | Q => true 
      | _ => false;

let doubleBarNested =
  fun | X
      | Y _ _ _
      | Z _ _
      | Q => true 
      | _ => false;

/* Liberal use of the Any pattern being compatible with multiple arguments  */
let doubleBarAnyPatterns =
  fun | X
      | Y _
      | Z _
      | Q => true 
      | _ => false;

let doubleBarNestedAnyPatterns =
  fun | X
      | Y _
      | Z _
      | Q => true 
      | _ => false;

type bcd = | B | C | D | E;

type a = | A of bcd;

let result =
  switch B {
    | B
    | C
    | D
    | E => ()
  };

let nested_match =
  fun | A (B | C | D | E) => 3;

let module EM = {exception E of int int;};

exception Ealias = EM.E;
type polyVariantsInMl = [
  | `IntTuple of (int, int) 
  | `StillAnIntTuple of (int, int)
];

let intTuple = `IntTuple (1, 2);

let stillAnIntTuple = `StillAnIntTuple (4, 5);

let sumThem =
  fun | `IntTuple (x, y) => x + y 
      | `StillAnIntTuple (a, b) => a + b;
