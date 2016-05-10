/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
/* This exposes the modules (e.g. autocomplete, diagnostic) that do the conversion from reason data structures
   (with the help of types from nuclide.re) to js ones */
let def x d =>
  switch x {
  | None => d
  | Some s => s
  };

let module Diagnostic = {
  open Nuclide.Diagnostic;
  let diagnosticTypeToJs =
    fun
    | Error => Js.string "Error"
    | Warning => Js.string "Warning";
  let diagnosticTypeFromJs =
    fun
    | "Error" => Nuclide.Diagnostic.Error
    | "Warning" => Nuclide.Diagnostic.Warning
    | _ => raise (Invalid_argument "invalid JS Diagnostic message type");
  /* A trace is a reference to another file/location related to an error. */
  let module Trace = {
    open Nuclide.Diagnostic.Trace;
    let toJs (trace: t) => {
      let {text, html, filePath, range} = trace;
      let fields = [|
        ("type", Js.Unsafe.inject (Js.string "trace")),
        ("text", Js.Unsafe.inject (Js.string (def text ""))),
        ("filePath", Js.Unsafe.inject (Js.string filePath))
      |];
      let fields =
        switch html {
        | None => fields
        | Some h => Array.append fields [|("html", Js.Unsafe.inject h)|]
        };
      let fields =
        switch range {
        | None => fields
        | Some r => Array.append fields [|("range", Js.Unsafe.inject (Atom.Range.toJs r))|]
        };
      Js.Unsafe.obj fields
    };
  };
  let module Message = {
    open Nuclide.Diagnostic.Message;
    let toJs diagnosticMessage =>
      switch diagnosticMessage {
      | FileDiagnosticMessage {providerName, typee, filePath, text, html, range, trace} =>
        let fields = [|
          ("scope", Js.Unsafe.inject (Js.string "file")),
          ("providerName", Js.Unsafe.inject (Js.string providerName)),
          ("type", Js.Unsafe.inject (diagnosticTypeToJs typee)),
          ("filePath", Js.Unsafe.inject (Js.string filePath)),
          ("text", Js.Unsafe.inject (Js.string (def text "")))
        |];
        let fields =
          switch html {
          | None => fields
          | Some h => Array.append fields [|("html", Js.Unsafe.inject h)|]
          };
        let fields =
          switch range {
          | None => fields
          | Some r => Array.append fields [|("range", Js.Unsafe.inject (Atom.Range.toJs r))|]
          };
        let fields =
          switch trace {
          | None => fields
          | Some ts =>
            let jsTraces = Js.Unsafe.inject (Js.array (Array.map Trace.toJs ts));
            Array.append fields [|("trace", jsTraces)|]
          };
        Js.Unsafe.obj fields
      | ProjectDiagnosticMessage {providerName, typee, text, html, range, trace} =>
        let fields = [|
          ("scope", Js.Unsafe.inject (Js.string "project")),
          ("providerName", Js.Unsafe.inject (Js.string providerName)),
          ("type", Js.Unsafe.inject (diagnosticTypeToJs typee)),
          ("text", Js.Unsafe.inject (Js.string (def text "")))
        |];
        let fields =
          switch html {
          | None => fields
          | Some h => Array.append fields [|("html", Js.Unsafe.inject h)|]
          };
        let fields =
          switch range {
          | None => fields
          | Some r => Array.append fields [|("range", Js.Unsafe.inject (Atom.Range.toJs r))|]
          };
        let fields =
          switch trace {
          | None => fields
          | Some ts =>
            let jsTraces = Js.Unsafe.inject (Js.array (Array.map Trace.toJs ts));
            Array.append fields [|("trace", jsTraces)|]
          };
        Js.Unsafe.obj fields
      };
  };
};

let module FileFormat = {
  let toJs (result: Nuclide.FileFormat.result) => Js.Unsafe.obj [|
    ("newCursor", Js.Unsafe.inject (Js.number_of_float (float_of_int result.newCursor))),
    ("formatted", Js.Unsafe.inject (Js.string result.formatted))
  |];
};

let module AutocompleteProviderRequest = {
  open Nuclide.AutocompleteProviderRequest;
  let fromJs o => {
    editor: Atom.Editor.fromJs (Js.Unsafe.get o "editor"),
    prefix: Js.to_string (Js.Unsafe.get o "prefix")
  };
};

let module Autocomplete = {
  open Nuclide.Autocomplete;
  let kindToJs =
    fun
    | Value => Js.string "value"
    | Class => Js.string "class"
    | Require => Js.string "require"
    | Type => Js.string "type"
    | Variable => Js.string "variable"
    | Constant => Js.string "constant"
    | Property => Js.string "property"
    | Method => Js.string "method"
    | Function => Js.string "function"
    | Keyword => Js.string "keyword"
    | Tag => Js.string "tag"
    | Snippet => Js.string "snippet"
    | Import => Js.string "import";
  let entryToJs e => Js.Unsafe.obj [|
    ("leftLabel", Js.Unsafe.inject (Js.string e.leftLabel)),
    ("text", Js.Unsafe.inject (Js.string e.text)),
    ("displayText", Js.Unsafe.inject (Js.string e.displayText)),
    ("type", Js.Unsafe.inject (kindToJs e.typee)),
    ("description", Js.Unsafe.inject (Js.string e.description)),
    ("replacementPrefix", Js.Unsafe.inject (Js.string e.replacementPrefix))
  |];
};
