/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
/**
 * This module wraps the JS merlin service.
 * The inputs and outputs should be Reason. The internals might use JS.
 */
type service;

let nuclideClient = Js.Unsafe.js_expr "require('nuclide/pkg/nuclide/client')";

let dotCall x y z => Js.Unsafe.meth_call x y z;

let getService filePath =>
  Js.Unsafe.meth_call
    nuclideClient
    "getServiceByNuclideUri"
    [|Js.Unsafe.inject (Js.string "MerlinService"), Js.Unsafe.inject (Js.string filePath)|];

/**
 * Will turn the array into a jsArray.
 */
let runSingleCommand (service: service) (filePath: string) jsCmd andThen => {
  let onResolve result => andThen result;
  let runSingleCommandPromise =
    dotCall service "runSingleCommand" [|Js.Unsafe.inject (Js.string filePath), jsCmd|];
  dotCall runSingleCommandPromise "then" [|Js.Unsafe.inject (Js.wrap_callback onResolve)|]
};

let complete (service: service) (filePath: string) (line: int) (col: int) (prefix: string) andThen => {
  let onResolve result =>
    if (Js.Opt.test (Js.Opt.return output)) {
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
  let runSingleCommandPromise =
    dotCall
      service
      "complete"
      [|
        Js.Unsafe.inject (Js.string filePath),
        Js.Unsafe.inject line,
        Js.Unsafe.inject col,
        Js.Unsafe.inject (Js.string prefix)
      |];
  /** TODO: We should first check if the MerlinService could be found and if not, provide
    * troubleshooting information. */
  dotCall runSingleCommandPromise "then" [|Js.Unsafe.inject (Js.wrap_callback onResolve)|]
};

let contextifyQuery jsCmd filePath => Js.Unsafe.obj [|
  ("query", jsCmd),
  ("context", Js.Unsafe.inject (Js.array [|Js.string "auto", Js.string filePath|]))
|];

