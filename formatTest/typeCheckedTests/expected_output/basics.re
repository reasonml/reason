/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
type reasonXyz =
  | X
  | Y(int, int, int)
  | Z(int, int)
  | Q
  | R;

type reasonXyzWithOf =
  | X
  | Y(int, int, int)
  | Z(int, int)
  | Q
  | R;

let reasonBarAs =
  fun
  | ((Y(_) | Z(_)) as t, _) => {
      let _ = t;
      true
    }
  | _ => false;

let reasonDoubleBar =
  fun
  | X
  | Y(_, _, _)
  | Z(_, _)
  | Q => true
  | _ => false;

let reasonDoubleBarNested =
  fun
  | X
  | Y(_, _, _)
  | Z(_, _)
  | Q => true
  | _ => false;

/* Liberal use of the Any pattern being compatible with multiple
   arguments  */
let reasonDoubleBarAnyPatterns =
  fun
  | X
  | Y(_)
  | Z(_)
  | Q => true
  | _ => false;

let reasonDoubleBarNestedAnyPatterns =
  fun
  | X
  | Y(_)
  | Z(_)
  | Q => true
  | _ => false;

let (\+) = (+);

let a = 2.0 *\* 4.0;

let (\===) = (===);

let expectedPrecendence =
  1 + 1 \=== 1 + 1 && 1 + 1 !== 1 + 1;

let expectedPrecendence =
  1 \+ 1 \=== 1 \+ 1 && 1 \+ 1 !== 1 \+ 1;

module X: {let x: x::unit? => unit => unit;} = {
  let x (::x=(), ()) => ();
};
