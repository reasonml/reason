

let preserveThis = "preserveThis";

let x = (a) => {
  preserveThis;
};

let x = (a) => {
  /* Even with comment */
  preserveThis;
};

let res = {
  /**
   * doc comment.
   */
  if (true) {
    ()
  } else {
    ()
  };
  ();
};

let x = (a) =>
/* It would be okay for this comment to get formatted into
 * the braces but ideally it wouldn't. */
{
  preserveThis;
};

type recordType = {
  recordTypeFieldOne: int,
  recordTypeFieldTwo: string
};
let x = {
  recordTypeFieldOne: 0,
  recordTypeFieldTwo: "two",
}.recordTypeFieldOne;

/**
 * Use a pointless wrapping.
 */
let x = {
  {
    recordTypeFieldOne: 0,
    recordTypeFieldTwo: "two",
  }
}.recordTypeFieldOne;

let createRecord = () => {
  recordTypeFieldOne: 0,
  recordTypeFieldTwo: "two",
};

let x = {
  createRecord();
}.recordTypeFieldOne;


let y = [@attr] 3;

/* The attribute on the explicit braces should also be preserved */
let y = [@attr] {
  3;
};

let myFun = (.a, b, c) => a + b + c;

let result = {
  myFun(. 0, 1, 2);
};

let result = [@attr] {
  myFun(. 0, 1, 2);
};

let tmp = {
  /** On if statement */
  if (true) {
    true
  } else {
    false
  };
};

let tmp = {
  let tmp = false;
  /** On if statement */
  if (tmp) {
    true
  } else {
    false
  };
};

let tmp = {
  let tmp = false;
  /** On if statement */
  if (tmp) {
    "true"
  } else {
    "false"
  };
  "but the if statement wasn't the last";
};


module Callbacks = {
  let runThisCallback = (s, next) => {s ++ next("!"); ()};
};

let result = Callbacks.runThisCallback("hi", (s) => s ++ "concatThis!");
let result = Callbacks.runThisCallback("hi", (s) => {
  s ++ "!"
});

Callbacks.runThisCallback("hi", (s) => s ++ "concatThis!");
Callbacks.runThisCallback("hi", (s) => {
  s ++ "concatThis!"
});

Callbacks.runThisCallback("hi", (s) => {
  let s = s ++ s;
  s ++ "concatThis!"
});


let test = {
  open Callbacks;
  ();
  34;
};

/* Even though these braces aren't needed we will preserve them because they
 * were requested */
let test = {
  open Callbacks;
  open String;
  34;
};

/* You can have unecessary braces around inline opens even */
let test = {
  Callbacks.(String.(34));
};

/* Even though these braces aren't needed we will preserve them because they
 * were requested */
let test = {
  open Callbacks;
  String.("hello" ++ "!");
};

let test = {
  open Callbacks;
  [@attr]
  String.("hello" ++ "!");
};

/* Doesn't currently parse.
  let test = {
    [@attr1]
    open Callbacks;
    String.("hello" ++ "!");
  };
*/

let test = {
  Callbacks.({
    let f = 0;
    f;
  });
};

let test =
  Callbacks.({
    let f = 0;
    f;
  });
