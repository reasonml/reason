/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
let selectOccurrences editor::editor ranges =>
  if (Array.length ranges > 0) {
    Atom.Editor.setSelectedBufferRanges editor ranges
  };
