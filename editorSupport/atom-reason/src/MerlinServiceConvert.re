/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
let jsMerlinPositionToAtomRange position => {
  let merlinStart = Js.Unsafe.get position "start";
  let merlinEnd = Js.Unsafe.get position "end";
  let range =
    Js.undefined === merlinStart || Js.undefined === merlinEnd ?
      Atom.Range.emptyRange :
      (
        /* lines (rows) are 1-based for merlin, not 0-based, like for Atom */
        (Js.Unsafe.get merlinStart "line" - 1, Js.Unsafe.get merlinStart "col"),
        (Js.Unsafe.get merlinEnd "line" - 1, Js.Unsafe.get merlinEnd "col")
      );
  range
};

let jsMerlinErrorToNuclideDiagnostic filePath jsMerlinError => {
  let message = Js.Unsafe.get jsMerlinError "message";
  /* One of  ("type"|"parser"|"env"|"warning"|"unkown") */
  let merlinType = Js.Unsafe.get jsMerlinError "type";
  let diagnosticType =
    Js.string "warning" === merlinType ? Nuclide.Diagnostic.Warning : Nuclide.Diagnostic.Error;
  Nuclide.Diagnostic.Message.FileDiagnosticMessage {
    Nuclide.Diagnostic.Message.scope: `file,
    providerName: "Merlin",
    typee: diagnosticType,
    filePath,
    text: Some message,
    html: None,
    range: Some (jsMerlinPositionToAtomRange jsMerlinError),
    trace: None
  }
};

let jsMerlinErrorsToNuclideDiagnostics filePath errors =>
  Array.map (jsMerlinErrorToNuclideDiagnostic filePath) (Js.to_array errors);

let stringToMerlinCompletionEntryKind s =>
  switch s {
  | "type" => Merlin.Type
  | "Type" => Merlin.Type
  | "value" => Merlin.Value
  | "Value" => Merlin.Value
  | "module" => Merlin.Module
  | "Module" => Merlin.Module
  | "constructor" => Merlin.Constructor
  | "Constructor" => Merlin.Constructor
  | _ => Merlin.Value
  };

let merlinCompletionEntryKindToNuclide k =>
  switch k {
  | Merlin.Type => Nuclide.Autocomplete.Type
  | Merlin.Value => Nuclide.Autocomplete.Value
  | Merlin.Module => Nuclide.Autocomplete.Require
  | Merlin.Constructor => Nuclide.Autocomplete.Class
  | _ => Nuclide.Autocomplete.Value
  };

let jsMerlinCompletionEntryToMerlinEntry o => {
  Merlin.desc: Js.to_string (Js.Unsafe.get o "desc"),
  info: Js.to_string (Js.Unsafe.get o "info"),
  kind: stringToMerlinCompletionEntryKind (Js.to_string (Js.Unsafe.get o "kind")),
  name: Js.to_string (Js.Unsafe.get o "name")
};

let merlinCompletionEntryToNuclide replacementPrefix e => {
  Nuclide.Autocomplete.leftLabel: e.Merlin.desc,
  /* Even though we display the type in the center (main column) */
  /* we replace at cursor with the item.name. */
  text: e.name,
  /* Item.desc is the type sig.   */
  displayText: e.Merlin.name,
  /* type is the "kind" of label to use. */
  typee: merlinCompletionEntryKindToNuclide e.kind,
  /* Include the full type in the description just in case it gets truncated in */
  /* the center column, you'll be able to see it in the description bar. */
  description: e.desc,
  replacementPrefix
};

let jsMerlinTypeHintEntryToNuclide arr => {
  let length = Js.Unsafe.get arr "length";
  if (length == 0) {
    Js.undefined
  } else {
    /* TODO: merlin gives us further type information if we expand our selection. Use it */
    let firstType = Js.Unsafe.get arr "0";
    let reasonHint = Js.to_string (Js.Unsafe.get firstType "type");
    Js.Unsafe.obj [|
      ("hint", Js.Unsafe.inject (Js.string reasonHint)),
      ("range", Js.Unsafe.inject (Atom.Range.toJs (jsMerlinPositionToAtomRange firstType)))
    |]
  }
};

let jsMerlinOccurrencesToAtom arr => Js.to_array arr |> Array.map jsMerlinPositionToAtomRange;
