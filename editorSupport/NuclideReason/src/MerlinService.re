/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
/**
 * This module wraps the JS merlin service. The inputs and outputs should be
 * Reason. The internals might use JS.
 */
type service;

let nuclideClient = Js.Unsafe.js_expr "require('nuclide/pkg/nuclide-client')";

let dotCall x y z => Js.Unsafe.meth_call x y z;

let getService filePath => {
  let nuclideOCamlPathToMerlin = Atom.Config.get "nuclide.nuclide-ocaml.pathToMerlin";
  let nuclideOCamlMerlinFlags = Atom.Config.get "nuclide.nuclide-ocaml.merlinFlags";
  let nuclideOCamlPathToMerlinOverwrite = Atom.Config.get "nuclide.nuclide-ocaml.nuclideReasonOverwroteYour_pathToMerlin";
  let nuclideOCamlMerlinFlagsOverwrite = Atom.Config.get "nuclide.nuclide-ocaml.nuclideReasonOverwroteYour_merlinFlags";
  let nuclideReasonPathToMerlin = Atom.Config.get "NuclideReason.pathToMerlin";
  let nuclideReasonMerlinFlags = Atom.Config.get "NuclideReason.merlinFlags";
  switch (nuclideOCamlPathToMerlin, nuclideOCamlPathToMerlinOverwrite) {
  | (_, Empty) =>
      Atom.Config.set
        "nuclide.nuclide-ocaml.nuclideReasonOverwroteYour_pathToMerlin" nuclideOCamlPathToMerlin
  | _ => ()
  };
  switch (nuclideOCamlMerlinFlags, nuclideOCamlMerlinFlagsOverwrite) {
  | (_, Empty) =>
      Atom.Config.set "nuclide.nuclide-ocaml.nuclideReasonOverwroteYour_merlinFlags" nuclideOCamlMerlinFlags
  | _ => ()
  };
  Atom.Config.set "nuclide.nuclide-ocaml.pathToMerlin" nuclideReasonPathToMerlin;
  Atom.Config.set "nuclide.nuclide-ocaml.merlinFlags" nuclideReasonMerlinFlags;
  Js.Unsafe.meth_call
    nuclideClient
    "getServiceByNuclideUri"
    [|Js.Unsafe.inject (Js.string "MerlinService"), Js.Unsafe.inject (Js.string filePath)|]
};

/**
 * Will turn the array into a jsArray.
 */
let runSingleCommand (service: service) (filePath: string) jsCmd andThen => {
  let onResolve result => andThen result;
  let runSingleCommandPromise =
    dotCall service "runSingleCommand" [|Js.Unsafe.inject (Js.string filePath), jsCmd|];
  dotCall runSingleCommandPromise "then" [|Js.Unsafe.inject (Js.wrap_callback onResolve)|]
};

let contextifyStringQuery cmdList filePath => Js.Unsafe.obj [|
  ("query", Js.Unsafe.inject (Js.array (Array.map Js.string (Array.of_list cmdList)))),
  ("context", Js.Unsafe.inject (Js.array [|Js.string "auto", Js.string filePath|]))
|];

let contextifyQuery cmdArray filePath => Js.Unsafe.obj [|
  ("query", Js.Unsafe.inject (Js.array cmdArray)),
  ("context", Js.Unsafe.inject (Js.array [|Js.string "auto", Js.string filePath|]))
|];

let complete (service: service) (filePath: string) (line: int) (col: int) (prefix: string) andThen => {
  let lineCol = Js.Unsafe.inject (
    Js.Unsafe.obj [|("line", Js.Unsafe.inject (line + 1)), ("col", Js.Unsafe.inject (col + 1))|]
  );
  let completeCommand = [|
    Js.Unsafe.inject (Js.string "complete"),
    Js.Unsafe.inject (Js.string "prefix"),
    Js.Unsafe.inject (Js.string prefix),
    Js.Unsafe.inject (Js.string "at"),
    lineCol
  |];
  let onResolve result =>
    if (Js.Opt.test (Js.Opt.return result)) {
      let lengthValue = Js.Unsafe.get result "length";
      let jsEntries =
        Js.Opt.case lengthValue (fun () => Js.Unsafe.get result "entries") (fun jsLen => result);
      let res = Array.to_list (
        Array.map MerlinServiceConvert.jsMerlinCompletionEntryToMerlinEntry (Js.to_array jsEntries)
      );
      Js.array (Array.of_list (andThen res))
    } else {
      Js.array [||]
    };
  /** TODO: We should first check if the MerlinService could be found and if not, provide
    * troubleshooting information. */
  runSingleCommand service filePath (contextifyQuery completeCommand filePath) onResolve
};
