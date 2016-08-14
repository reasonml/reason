/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
let buildToken kind value => Js.Unsafe.obj [|
  ("kind", Js.Unsafe.inject (Js.string kind)),
  ("value", Js.Unsafe.inject (Js.string value))
|];

let makeTokens data => {
  let merlinKind = Js.to_string (Js.Unsafe.get data "kind");
  let name = Js.to_string (Js.Unsafe.get data "name");
  let kind =
    switch merlinKind {
    | "Value" => "val"
    | "Constructor" => "ctor"
    | "Signature" => "sig"
    | kind => String.lowercase kind
    };
  let nameToken =
    switch merlinKind {
    | "Value" => buildToken "method" name
    | "Class"
    | "Constructor"
    | "Exn"
    | "Module"
    | "Signature" => buildToken "class-name" name
    | "Type" => buildToken "type" name
    | kind => buildToken "plain" name
    };
  Js.Unsafe.inject (
    Js.array [|
      buildToken "keyword" kind,
      buildToken "whitespace" " ",
      nameToken
    |]
  )
};

let rec convertOutlines arr =>
  Js.array_map
    (
      fun data => {
        let start = MerlinServiceConvert.jsMerlinPointToAtomPoint (
          Js.Unsafe.get data "start"
        );
        let endd = MerlinServiceConvert.jsMerlinPointToAtomPoint (
          Js.Unsafe.get data "end"
        );
        let children = Js.Unsafe.get data "children";
        Js.Unsafe.obj [|
          ("tokenizedText", makeTokens data),
          ("startPosition", Atom.Point.toJs start),
          ("endPosition", Atom.Point.toJs endd),
          ("children", Js.Unsafe.inject (convertOutlines children))
        |]
      }
    )
    arr;

let getOutline path::path text::text resolve reject =>
  SuperMerlin.getOutline
    path::path
    text::text
    (
      fun result => resolve (
        Js.Unsafe.obj [|
          ("outlineTrees", Js.Unsafe.inject (convertOutlines result))
        |]
      )
    )
    reject;
