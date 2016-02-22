/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

let require s => Js.Unsafe.fun_call (Js.Unsafe.js_expr "require") [|Js.Unsafe.inject (Js.string s)|];

let export s itm => Js.Unsafe.set (Js.Unsafe.js_expr "exports") s itm;

let dotCall x y z => Js.Unsafe.meth_call x y z;

