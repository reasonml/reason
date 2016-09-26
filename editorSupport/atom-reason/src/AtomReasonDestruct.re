/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
let destruct editor obj => {
  let range = Js.Unsafe.get obj "range";
  let replace = Js.Unsafe.get obj "replace";
  let _ = Atom.Editor.setSelectedBufferRange editor range;
  let selections = Atom.Editor.getSelections editor;
  let firstSelection = Js.Unsafe.get selections "0";
  Atom.Selection.insertText firstSelection replace;
}