/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
/* This is lifted from nuclide source, from the helper function of the same name. */
let goToLocation' = Js.Unsafe.js_expr {|
  function() {
    function goToLocation(file, line, column, center) {
      center = center == null ? true : center;
      return atom.workspace.open(file, {
        initialLine: line,
        initialColumn: column,
        searchAllPanes: true,
      })
      .then(function(editor) {
        if (center) {
          return editor.scrollToBufferPosition([line, column], {center: true});
        }
        return editor;
      });
    }

    return function(res) {
      // TODO: rewrite this in reason.
      if (typeof res === "string") {
        console.error(res);
        return;
      }
      goToLocation(res.file, res.pos.line - 1, res.pos.col);
    }
  }()
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
