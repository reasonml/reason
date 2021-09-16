/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

type polyVariantsInMl = [
  | `IntTuple(int, int)
  | `StillAnIntTuple(int, int)
];

let intTuple = `IntTuple((1, 2));
let stillAnIntTuple = `StillAnIntTuple((4, 5));
let sumThem =
  fun
  | `IntTuple(x, y) => x + y
  | `StillAnIntTuple(a, b) => a + b;

type nonrec t =
  | A(int)
  | B(bool);

type s = [ | `Poly];

let x: s = `Poly;

/* There's a bug in ocaml 4.06 resulting in an extra Pexp_constraint on the `Poly,
 * duplicating the core_type.
 * https://caml.inria.fr/mantis/view.php?id=7758
 * https://caml.inria.fr/mantis/view.php?id=7344 */
let x: s = (`Poly: s);
