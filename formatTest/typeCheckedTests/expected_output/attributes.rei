/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

/**Floating comment text should be removed*/;
[@reason.version 3.7];

let test: int;

/**
 * Attributes with doc/text attributes should be stripped. They're left over from a
 * conversion from ML likely.
 * ----------------------
 */;

/**Floating doc text should be removed*/;

/**
 * #990: don't strip attributes in interface files
 */
[@bs.val]
let x: int;

type t('a);

type reactClass;

type reactElement;

[@bs.val] [@bs.module "React"]
external createClassInternalHack:
  t('classSpec) => reactClass =
  "createClass";

[@bs.send.pipe: array('a)]
external map: (. ('a => 'b)) => array('b);

[@bs.send.pipe: array('a)]
external map: (. ('a => 'b)) => array('b);

[@bs.val] [@bs.module "react"]
external createClassInternalHack:
  t('classSpec) => reactClass =
  "createClass";

[@bs.val] [@bs.module "react"] [@bs.splice]
external createCompositeElementInternalHack:
  (
    reactClass,
    t({.. reasonProps: 'props}),
    array(reactElement)
  ) =>
  reactElement =
  "createElement";

/* Record item attributes */

type t_ = {
  /** Comment attribute on record item */
  x: int,
};

type tt = {
  [@attr "on record field"]
  x: int,
};

type ttt = {
  [@attr "on record field"]
  x: [@attr "on type itself"] int,
};

type tttt = {
  /** Comment attribute on record item */
  x: int,
  [@regularAttribute "on next item"]
  y: int,
};

type ttttt = {
  [@attr "moved to first row"] [@attr]
  x: int,
};

module Foo: {
  [@someattr]
  let foo: int => int;
};
