/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
/* This is the js <-> reason conversion point for Nuclide-specific data structures, akin to Atom.re */
type range = ((int, int), (int, int));

let module Diagnostic = {
  type filePath = string;
  type diagnosticType = | Error | Warning;
  /**
   * A trace is a reference to another file/location related to an error.
   */
  let module Trace = {
    type t = {
      typee: [ | `Trace],
      text: option string,
      html: option string,
      filePath: string,
      range: option range
    };
  };
  let module Message = {
    type fileDiagnosticMessage = {
      scope: [ | `file],
      providerName: string,
      typee: diagnosticType,
      filePath: string,
      text: option string,
      html: option string,
      range: option range,
      trace: option (array Trace.t)
    };
    type projectDiagnosticMessage = {
      scope: [ | `project],
      providerName: string,
      typee: diagnosticType,
      text: option string,
      html: option string,
      range: option range,
      trace: option (array Trace.t)
    };
    type t =
      | FileDiagnosticMessage of fileDiagnosticMessage
      | ProjectDiagnosticMessage of projectDiagnosticMessage;
  };
};

let module FileFormat = {
  type result = {
    /* Character offset into new formatted string. */
    newCursor: int,
    formatted: string
  };
};

let module AutocompleteProviderRequest = {
  type autocompleteRequest = {editor: Atom.Editor.t, prefix: string};
};

let module Autocomplete = {
  /*
   * Type must be one of the following:
   * ----------------------------------
   * variable, constant, property, value, method, function, class, type,
   * keyword, tag, snippet, import, require
   */
  type nuclideCompletionEntryKind =
    | Variable
    | Constant
    | Property
    | Value
    | Method
    | Function
    | Class
    | Require
    | Type
    | Keyword
    | Tag
    | Snippet
    | Import;
  type nuclideCompletionEntry = {
    leftLabel: string,
    text: string,
    displayText: string,
    typee: nuclideCompletionEntryKind,
    description: string,
    replacementPrefix: string
  };
};
