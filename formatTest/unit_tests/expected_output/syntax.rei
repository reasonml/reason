[@reason.version 3.7];
/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

/**
 * Typically the "interface file" is where you would write a ton of
 * comments/documentation.
 */
type adders = {
  /*
   * Adds two numbers together.
   */
  addTwoNumbers: (int, int) => int,
  /*
   * Amazingly, adds *three* numbers together.
   */
  addThreeNumbers: (int, int, int) => int,
  /*
   * Tuple version of previous function.
   */
  addThreeNumbersTupled:
    ((int, int, int)) => int,
};

/**
 * Public function.
 */
let myRecordWithFunctions: adders;
/**
 * Public result.
 */
let result: int;

/* https://github.com/facebook/reason/issues/1614 */
module Event: (module type of {
  include ReactEventRe;
});

module type Event = (module type of {
  include ReactEventRe;
});

/* https://github.com/facebook/reason/issues/2169 */
let not: string => string;

let other: string => not;

include
   (module type of Bos.Cmd) with
    type t = Bos.Cmd.t;
