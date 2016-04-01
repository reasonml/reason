/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
/*
 * Ideally, most of NuclideReason would be in pure Reason, and the parts that need to
 * to convert to/from JS would be at the edges (Index.re, and MerlinService.re).
 */
open NuclideReasonCommon;

/* These exports will be visible to main.js, under NuclideReason */
export
  "getDiagnostics"
  (
    Js.wrap_callback (
      fun text path onComplete onFailure => {
        let path = Js.to_string path;
        let text = Js.to_string text;
        let onComplete arr =>
          Js.Unsafe.fun_call
            onComplete [|Js.Unsafe.inject (Js.array (Array.map NuclideJs.Diagnostic.Message.toJs arr))|];
        let onFailure str => Js.Unsafe.fun_call onComplete [|Js.Unsafe.inject (Js.string str)|];
        NuclideReasonDiagnostics.getMerlinDiagnostics text path onComplete onFailure
      }
    )
  );

let getFormatting f => Js.wrap_callback (
  fun jsEditor jsRange jsNotifySuccess jsNotifyInvalid jsNotifyInfo => {
    let editor = Atom.Editor.fromJs jsEditor;
    let range = Atom.Range.fromJs jsRange;
    let notifySuccess msg => Js.Unsafe.fun_call jsNotifySuccess [|Js.Unsafe.inject (Js.string msg)|];
    let notifyInvalid msg => Js.Unsafe.fun_call jsNotifyInvalid [|Js.Unsafe.inject (Js.string msg)|];
    let notifyInfo msg => Js.Unsafe.fun_call jsNotifyInfo [|Js.Unsafe.inject (Js.string msg)|];
    let promise = f editor range notifySuccess notifyInvalid notifyInfo;
    Atom.Promise.toJs promise
  }
);

export "getEntireFormatting" (getFormatting NuclideReasonFormat.getEntireFormatting);

export "getPartialFormatting" (getFormatting NuclideReasonFormat.getPartialFormatting);

export
  "getNuclideJsAutocompleteSuggestions"
  (
    Js.wrap_callback (
      fun r => NuclideReasonAutoCompleteProvider.getNuclideJsAutocompleteSuggestions (
        NuclideJs.AutocompleteProviderRequest.fromJs r
      )
    )
  );

export
  "getNuclideJsTypeHint"
  (
    Js.wrap_callback (
      fun editor position => {
        /* TODO: make selection work in conjunction with expansion */
        /* TODO: currently gets the first type hint. The rest are the types of the expanded scope. Will use
        them one day. */
        let position = Atom.Point.fromJs position;
        let text = Atom.Buffer.getText (Atom.Editor.getBuffer editor);
        /*
         * This is likely slightly broken. If you open a file without a file name, but
         * later save it to a different location on disk, you likely will not pick up the
         * right merlin path.
         */
        let path =
          switch (Atom.Editor.getPath editor) {
          | None => "tmp.re"
          | Some path => path
          };
        /* This returns a js promise. */
        SuperMerlin.getTypeHint path::path text::text position::position
      }
    )
  );
