/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
let goToLocation' = Js.Unsafe.js_expr {|
  function(res) {
    // TODO: rewrite this in reason.
    var goToLocation = require('nuclide/pkg/nuclide-atom-helpers').goToLocation;
    if (typeof res === "string") {
      console.error(res);
      return;
    }
    goToLocation(res.file, res.pos.line - 1, res.pos.col);
  }
|};

let goToLocation result => Js.Unsafe.fun_call goToLocation' [|result|];

let getMerlinLocation editor::editor range::range => {
  let path = AtomReasonCommon.path editor;
  let extension = AtomReasonCommon.isInterface (Some path) ? "mli" : "ml";
  let text = Atom.Buffer.getText editor;
  let (startPosition, _) = range;
  SuperMerlin.locate
    path::path
    text::text
    extension::extension
    position::startPosition
    (fun successResult => goToLocation successResult)
    /* TODO: use this */
    (fun _ => ())
};
