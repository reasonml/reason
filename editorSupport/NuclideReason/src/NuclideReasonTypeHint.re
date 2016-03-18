/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open NuclideReasonCommon;

let makeTellComand text => ["tell", "start", "end", text];

let makeTypeHintCommand (line, col) => [|
  Js.Unsafe.inject (Js.string "type"),
  Js.Unsafe.inject (Js.string "enclosing"),
  Js.Unsafe.inject (Js.string "at"),
  Js.Unsafe.inject (
    Js.Unsafe.obj [|
      /* lines (rows) are 1-based for merlin, not 0-based, like for Atom */
      ("line", Js.Unsafe.inject (Js.number_of_float (float_of_int (line + 1)))),
      ("col", Js.Unsafe.inject (Js.number_of_float (float_of_int col)))
    |]
  )
|];

/* TODO: make selection work in conjunction with expansion */
let getMerlinTypeHint editor text position => {
  let path =
    switch (Atom.Editor.getPath editor) {
    | None => "tmp.re"
    | Some path => path
    };
  let service = MerlinService.getService path;
  let promise = Atom.Promise.create (
    fun resolve reject => {
      let contextifiedTellCmd = MerlinService.contextifyStringQuery (makeTellComand text) path;
      let contextifiedTypeHintCmd = MerlinService.contextifyQuery (makeTypeHintCommand position) path;
      let afterTellText result =>
        MerlinService.runSingleCommand
          service
          path
          contextifiedTypeHintCmd
          (fun raw => resolve (MerlinServiceConvert.jsMerlinTypeHintEntryToNuclide raw));
      MerlinService.runSingleCommand service path contextifiedTellCmd afterTellText
    }
  );
  promise
};
