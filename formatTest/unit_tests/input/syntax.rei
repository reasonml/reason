/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

/**
 * Typically the "interface file" is where you would write a ton of
 * comments/documentation.
 */
type adders = {
  /*
   * Adds two numbers together.
   */
  addTwoNumbers: int => int => int,
  /*
   * Amazingly, adds *three* numbers together.
   */
  addThreeNumbers: int => int => int => int,
  /*
   * Tuple version of previous function.
   */
  addThreeNumbersTupled: (int, int, int) => int,
};

/**
 * Public function.
 */
let myRecordWithFunctions: adders;
/**
 * Public result.
 */
let result: int;
