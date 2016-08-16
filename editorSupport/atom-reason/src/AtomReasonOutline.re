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

let makeTokens grammar data => {
  let merlinKind = Js.to_string (Js.Unsafe.get data "kind");
  let name = Js.to_string (Js.Unsafe.get data "name");
  let kind =
    switch merlinKind {
    | "Value" =>
      switch grammar {
      | `Reason => "let"
      | `OCaml => "val"
      }
    | "Constructor" => "ctor"
    | kind => String.lowercase kind
    };
  let nameToken =
    switch merlinKind {
    | "Value" => buildToken "method" name
    | "Class"
    | "Constructor"
    | "Exn"
    | "Module" => buildToken "class-name" name
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

let rec convertOutlines grammar entries => {
  let entries =
    List.rev_map
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
            ("tokenizedText", makeTokens grammar data),
            ("startPosition", Atom.Point.toJs start),
            ("endPosition", Atom.Point.toJs endd),
            ("children", Js.Unsafe.inject (convertOutlines grammar children))
          |]
        }
      )
      (Array.to_list (Js.to_array entries));
  Js.array (Array.of_list entries)
};

let getOutline path::path text::text grammar::grammar resolve reject => {
  let grammar =
    switch grammar {
    | "Reason" => `Reason
    | _ => `OCaml
    };
  SuperMerlin.getOutline
    path::path
    text::text
    (
      fun result => resolve (
        Js.Unsafe.obj [|
          ("outlineTrees", Js.Unsafe.inject (convertOutlines grammar result))
        |]
      )
    )
    reject
};
