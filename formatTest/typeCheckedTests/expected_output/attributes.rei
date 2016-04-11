/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
[@@@ocaml.text
  "Floating comment text should be removed"
];
let test: int;
/**
 * Attributes with doc/text attributes should be stripped. They're left over from a
 * conversion from ML likely.
 * ----------------------
 */
[@@@ocaml.doc
  "Floating doc text should be removed"
];