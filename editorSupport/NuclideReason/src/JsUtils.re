/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

let _isTruthy = Js.Unsafe.js_expr {|
  function(a) {return !!a;}
|};

let isTruthy jsVal => Js.to_bool (Js.Unsafe.fun_call _isTruthy jsVal);

