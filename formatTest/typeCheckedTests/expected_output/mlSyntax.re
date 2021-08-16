/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

/**
 * Testing pattern matching using ml syntax to exercise nesting of cases.
 */;
[@reason.version 3.7];

type xyz =
  | X
  | Y(int, int, int)
  | Z(int, int)
  | Q
  | R;

let doubleBar =
  fun
  | X
  | [@implicit_arity] Y(_, _, _)
  | [@implicit_arity] Z(_, _)
  | Q => true
  | _ => false;

let doubleBarNested =
  fun
  | X
  | [@implicit_arity] Y(_, _, _)
  | [@implicit_arity] Z(_, _)
  | Q => true
  | _ => false;

/* Liberal use of the Any pattern being compatible with multiple arguments  */
let doubleBarAnyPatterns =
  fun
  | X
  | Y(_)
  | Z(_)
  | Q => true
  | _ => false;

let doubleBarNestedAnyPatterns =
  fun
  | X
  | Y(_)
  | Z(_)
  | Q => true
  | _ => false;

type bcd =
  | B
  | C
  | D
  | E;
type a =
  | A(bcd);
let result =
  switch (B) {
  | B
  | C
  | D
  | E => ()
  };

let nested_match =
  fun
  | A(B | C | D | E) => 3;

let some = Some((1, 2, 3));

let (\===) = (==);

/* Test regression for https://github.com/facebook/Reason/issues/222 */
let _ = Pervasives.(==);

let structuralEquality = 1 == 1;

let physicalInequality = 1 != 2;

let referentialEquality = 2 === 2;

let referentialInequality = 2 !== 2;

let equalityInIf =
  if (1 == 1) {
    true;
  } else {
    false;
  };

let equalityWithIdentifiers =
  structuralEquality == referentialEquality;

let nestedSome = Some((1, 2, Some((1, 2, 3))));

let nestedSomeSimple = Some(Some((1, 2, 3)));

module EM = {
  /** Exception */

  exception E(int, int);
};

exception Ealias = EM.E;

let switc = "match";
let switch_ = "match";
let switch__ = "match";
let pub_ = "method";
let pub__ = "method";
let pri_ = "private";
let pri__ = "private";

external private: unit => unit;
external pri_: unit => unit;

type pub_ = int;
type pub__ = int;
