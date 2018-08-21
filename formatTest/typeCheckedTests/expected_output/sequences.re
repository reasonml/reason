/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

/**
 * Testing Sequences.
 */
let result = {
  let twenty = 20;
  let result = twenty;
  result;
};

/* Final semicolon is not required */
let result = {
  let twenty = result;
  twenty;
};
let anInt = result + 20;

let twenty = 20;

/**
 * Each of these are a sequence with a single item - they will be
 * printed in reduced form because sequences are a *parse* time construct.
 * To ensure these are parsed correctly, adding to an integer.
 */
let result = 0 + twenty;
let result = 0 + twenty;
let result = 0 + twenty;

let unitValue = ();
/* While loops/for loops merely accept a "simple expression" (which means
 * it is either a simple token or balanced with parens/braces). However,
 * the formatter ensures that the bodies are printed in "sequence" form even if
 * it's not required.
 */
while (false) {
  unitValue;
};
while (false) {
  print_string("test");
};
while (false) {
  print_string("test");
};

type myRecord = {number: int};
let x = {number: 20};
let number = 20;
/*
 * The (mild) consequence of not requiring a final semi in a sequence,
 * is that we can no longer "pun" a single field record (which would)
 * be very rare anyways.
 */
let cannotPunASingleFieldRecord = {
  number: number,
};
let fourty =
  20 + cannotPunASingleFieldRecord.number;
let thisIsASequenceNotPunedRecord = number;
let fourty = 20 + thisIsASequenceNotPunedRecord;

type recordType = {
  a: int,
  b: int,
  c: int,
};
let a = 0;
let b = 0;
let c = 0;
/* All of these will be printed as punned because they have more than one field. */
let firstFieldPunned = {a, b, c};
let sndFieldPunned = {a, b, c};
let thirdFieldPunned = {a, b, c};
let singlePunAcceptedIfExtended = {
  ...firstFieldPunned,
  a,
};

module Option = {
  let map = (x, f) =>
    switch (x) {
    | Some(x) => Some(f(x))
    | None => None
    };

  let flatMap = (x, f) =>
    switch (x) {
    | Some(x) => f(x)
    | None => None
    };

  let pair = (x, y) =>
    switch (x, y) {
    | (Some(x), Some(y)) => Some((x, y))
    | _ => None
    };
};

let _ = {
  let!Option x = Some(23)
  and!Option y = Some(5);

  Some(x + y)
};

module Async = {
  type t('value) = Js.Promise.t('value);
  let let_: (t('a), 'a => t('b)) => t('b) = Js.Promise.then_;
  let and_: (t('a), t('b)) => t(('a, 'b)) = Js.Promise.all2;
  let try_: (t('a), exn => t('a)) => t('a) = Js.Promise.catch;
  let resolve = Js.Promise.resolve;
  let reject = Js.Promise.reject;
};

let getAge = () => Async.reject(Failure("Cannot get age"));

let _ = {
  let!Async x = try!Async (getAge()) {
    | Failure(message) => Ok(23)
    | exn => raise(exn)
  };

  let!Async a = Async.resolve(2)
  and!Async b = Async.resolve(5)
  and!Async c = Async.resolve(7);
  print_endline(string_of_int(a));

  Async.resolve(a + x * b + c);
};
