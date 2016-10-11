/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
type t = <>;

type t = < u : int, v : int >;

type t = < u : int, .. >;

type t = < u : int, .. >;

type t = < .. >;

type t = < .. >;

let (<..>) a b => a + b;

let five = 2 <..> 3;
