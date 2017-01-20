/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

[@@@ocaml.text "Floating comment text should be removed"];
let test : int;

/**
 * Attributes with doc/text attributes should be stripped. They're left over from a
 * conversion from ML likely.
 * ----------------------
 */

[@@@ocaml.doc "Floating doc text should be removed"];


/**
 * #990: don't strip attributes in interface files
 */
let x: int [@@bs.val];

type t 'a;

type reactClass;

type reactElement;

external createClassInternalHack : t 'classSpec => reactClass = "createClass" [@@bs.val] [@@bs.module "React"];

external map : ('a => 'b) [@bs] => array 'b = "" [@@bs.send.pipe : array 'a];

external createClassInternalHack : t 'classSpec => reactClass =
  "createClass" [@@bs.val] [@@bs.module "react"];

external createCompositeElementInternalHack :
  reactClass => t {.. reasonProps : 'props} => array reactElement => reactElement =
  "createElement" [@@bs.val] [@@bs.module "react"] [@@bs.splice];
