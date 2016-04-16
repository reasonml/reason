/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open AtomReasonCommon;

open StringUtils;

open Atom;

let fixedEnv = Js.Unsafe.js_expr "require('../lib/fixedEnv')";

/**
 * @param (Array.t string) standard output formatting of file.
 * @param int curCursorRow Current cursor row before formatting.
 * @param int curCursorColumn Current cursor column before formatting.
 * @return Nuclide.FileFormat.result
 */
let characterIndexForPositionInString stdOutLines (curCursorRow, curCursorColumn) => {
  let result = {contents: []};
  let arrLen = Array.length stdOutLines;
  let charCount = {contents: 0};
  let colCount = {contents: 0};
  let rowCount = {contents: 0};
  let finalCharCount = {contents: 0};
  for iArr in 0 to (arrLen - 1) {
    let line = stdOutLines.(iArr);
    let lineLen = String.length line;
    /* We also trim *trailing* whitespace for each line. */
    let lenNotEndingInWhiteSpace = {contents: 0};
    /* No guarantee that each line is actually a single line. */
    for chPos in 0 to (lineLen - 1) {
      let ch = line.[chPos];
      if (ch === '\n' || ch === '\r') {
        rowCount.contents = rowCount.contents + 1;
        colCount.contents = 0
      } else {
        colCount.contents = colCount.contents + 1;
        lenNotEndingInWhiteSpace.contents = lenNotEndingInWhiteSpace.contents + 1
      };
      charCount.contents = charCount.contents + 1;
      if (rowCount.contents <= curCursorRow) {
        if (colCount.contents <= curCursorColumn) {
          finalCharCount.contents = charCount.contents
        }
      }
    };
    result.contents = [line, ...result.contents]
  };
  {
    Nuclide.FileFormat.newCursor: finalCharCount.contents,
    Nuclide.FileFormat.formatted: String.concat "" (List.rev result.contents)
  }
};

let formatImpl editor subText isInterface onComplete onFailure => {
  let open Atom.JsonType;
  let stdOutLines = {contents: [||]};
  let stdErrLines = {contents: [||]};
  let fmtPath =
    switch (Atom.Config.get "AtomReason.pathToReasonfmt") {
    | JsonString pth => pth
    | _ => raise (Invalid_argument "You must setup AtomReason.pathToReasonfmt in your Atom config")
    };
  let printWidth =
    switch (Atom.Config.get "AtomReason.printWidth") {
    | JsonNum n => int_of_float n
    | Empty => 110
    | _ => raise (Invalid_argument "AtomReason.printWidth must be an integer")
    };
  let onStdOut line => stdOutLines.contents = Array.append stdOutLines.contents [|line|];
  let onStdErr line => stdErrLines.contents = Array.append stdErrLines.contents [|line|];
  let cursors = Editor.getCursors editor;
  let (origCursorRow, origCursorCol) =
    switch cursors {
    | [] => (0, 0)
    | [firstCursor, ...tl] => Atom.Cursor.getBufferPosition firstCursor
    };
  let onExit code => {
    let formatResult = characterIndexForPositionInString stdOutLines.contents (origCursorRow, origCursorCol);
    let stdErr = String.concat "\n" (Array.to_list stdErrLines.contents);
    onComplete code formatResult stdErr
  };
  let args = [
    "-print-width",
    string_of_int printWidth,
    "-use-stdin",
    "true",
    "-parse",
    "re",
    "-print",
    "re",
    "-is-interface-pp",
    isInterface ? "true" : "false"
  ];
  let proc =
    Atom.BufferedProcess.create
      options::{...Atom.Process.defaultOptions, env: fixedEnv}
      stdout::onStdOut
      stderr::onStdErr
      exit::onExit
      args::args
      fmtPath;
  let errorTitle = "AtomReason could not spawn " ^ fmtPath;
  let handleError error handle => {
    NotificationManager.addError options::{...NotificationManager.defaultOptions, detail: error} errorTitle;
    /* TODO: this doesn't type check, but sits across the border of js <-> reason so it passes. onFailure (the
       promise `onFailure`) takes in a reason string, when it reality it should take in a Js.string like the other
       locations in this file where we do `onFailure stdErr` */
    onFailure "Failure!";
    handle ()
  };
  BufferedProcess.onWillThrowError proc handleError;
  /* Underlying child process. */
  let process = BufferedProcess.process proc;
  ChildProcess.writeStdin process subText;
  ChildProcess.endStdin process
};

/**
 * A better way to restore the cursor position is:
 * - If only white space change occured before where the cursor was, place
 * cursor at *new* location after whitespce changes.
 * - As an enhancement, consider insertion/elimination of certain
 * characters/sequences in the same class as white space changes.
 * (extra/removed parens, or even "= fun").
 * - If text before cursor changed in ways beyond "whitespace" changes, fall
 * back to current behavior.
 */
let getEntireFormatting editor range notifySuccess notifyInvalid notifyInfo resolve reject => {
  let buffer = Editor.getBuffer editor;
  let text = Buffer.getText buffer;
  let subText = Buffer.getTextInRange buffer range;
  formatImpl
    editor
    subText
    (isInterface (Editor.getPath editor))
    (
      fun code (formatResult: Nuclide.FileFormat.result) stdErr => {
        if (not (code == 0.0)) {
          notifyInvalid "Syntax Error"
        } else if (formatResult.formatted \=== text) {
          notifyInfo "Already Formatted"
        } else {
          notifySuccess "Format: Success"
        };
        code == 0.0 ? resolve formatResult : reject stdErr
      }
    )
    reject
};

let getPartialFormatting editor range notifySuccess notifyInvalid notifyInfo resolve reject => {
  let buffer = Editor.getBuffer editor;
  let subText = Buffer.getTextInRange buffer range;
  formatImpl
    editor
    subText
    (isInterface (Editor.getPath editor))
    (
      fun code (formatResult: Nuclide.FileFormat.result) stdErr => {
        if (not (code == 0.0)) {
          notifyInvalid "Syntax Error"
        } else if (formatResult.formatted \=== subText) {
          notifyInfo "Already Formatted"
        } else {
          notifySuccess "Format: Success"
        };
        /* One bit of Js logic remaining in this otherwise "pure" module. */
        code == 0.0 ? resolve formatResult.formatted : reject stdErr
      }
    )
    reject
};
