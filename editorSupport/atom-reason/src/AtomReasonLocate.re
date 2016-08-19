/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
let notiflyer = Js.Unsafe.js_expr "require('../lib/Notiflyer')";

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
      goToLocation(res.file, res.pos.line - 1, res.pos.col);
    }
  }()
|};

let goToLocation result => Js.Unsafe.fun_call goToLocation' [|Js.Unsafe.inject result|];

let locateAndGoToLocation path::path text::text extension::extension range::range resolve reject => {
  let (startPosition, endPosition) = range;
  if (startPosition == endPosition) {
    /* Sometimes we're hovering over an empty line. */
    resolve Js.null
  } else {
    SuperMerlin.locate
      path::path
      text::text
      extension::extension
      position::startPosition
      (
        fun result =>
          switch (MerlinServiceConvert.jsMerlinLocateToEntry result) {
          | Merlin.InvalidIdentifier =>
            /* Invalid identifier... let's not show that in the Notiflyer. */
            resolve Js.null
          | Merlin.OtherError err =>
            let failCallback = Js.wrap_callback (
              fun () =>
                Js.Unsafe.meth_call notiflyer "showInfoBar" [|Js.Unsafe.inject @@ Js.string err|]
            );
            resolve (
              Js.Unsafe.obj [|
                ("range", Atom.Range.toJs range),
                ("callback", Js.Unsafe.inject failCallback)
              |]
            )
          | a =>
            Js.Unsafe.meth_call notiflyer "clearFeedbackBar" [||];
            let successJumpToLocationCallback = Js.wrap_callback (fun () => goToLocation result);
            resolve (
              Js.Unsafe.obj [|
                ("range", Atom.Range.toJs range),
                ("callback", Js.Unsafe.inject successJumpToLocationCallback)
              |]
            )
          }
      )
      reject
  }
};
