/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
/*
 * Ideally, most of AtomReason would be in pure Reason, and the parts that need to
 * to convert to/from JS would be at the edges (Index.re, and SuperMerlin.re).
 */
open AtomReasonCommon;

/* These exports will be visible to main.js, under AtomReason */
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
        /* TODO: this has never been used. */
        let onFailure str => Js.Unsafe.fun_call onComplete [|Js.Unsafe.inject (Js.string str)|];
        AtomReasonDiagnostics.getMerlinDiagnostics text path onComplete onFailure
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

export "getEntireFormatting" (getFormatting AtomReasonFormat.getEntireFormatting);

export "getPartialFormatting" (getFormatting AtomReasonFormat.getPartialFormatting);

/*
 * This is likely slightly broken. If you open a file without a file name, but
 * later save it to a different location on disk, you likely will not pick up the
 * right merlin path.
 */
let path editor =>
  switch (Atom.Editor.getPath editor) {
  | None => "tmp.re"
  | Some path => path
  };

export
  "getNuclideJsAutocompleteSuggestions"
  (
    Js.wrap_callback (
      fun request => {
        let request = NuclideJs.AutocompleteProviderRequest.fromJs request;
        let editor = request.Nuclide.AutocompleteProviderRequest.editor;
        let prefix = request.prefix;
        let text = Atom.Buffer.getText (Atom.Editor.getBuffer editor);
        let (line, col) as position = Atom.Cursor.getBufferPosition (List.hd (Atom.Editor.getCursors editor));
        /**
         * The default prefix at something like `Printf.[cursor]` is just the dot. Compute
         * `linePrefix` so that ocamlmerlin gets more context. Compute `replacementPrefix`
         * to make sure that the existing dot doesn't get clobbered when autocompleting.
         */
        let linePrefix = String.sub (Atom.Editor.lineTextForBufferRow editor line) 0 col;
        let linePrefix =
          String.length linePrefix === 0 ?
            linePrefix :
            {
              let regex = Js.Unsafe.js_expr {|/([ |\t\[\](){}<>,+*\/-])/|};
              let lst = StringUtils.split linePrefix by::regex;
              let len = List.length lst;
              len > 0 ? List.nth lst (len - 1) : linePrefix
            };
        if (String.length (String.trim linePrefix) === 0 || String.length (String.trim prefix) === 0) {
          Js.Unsafe.js_expr {|new Promise(function(resolve) {resolve([]);})|}
        } else {
          let replacementPrefix =
            if (String.contains prefix '.' && String.index prefix '.' === 0) {
              String.sub prefix 1 (String.length prefix - 1)
            } else {
              prefix
            };
          let res =
            SuperMerlin.getAutoCompleteSuggestions
              path::(path editor) text::text position::position prefix::linePrefix;
          Js.Unsafe.meth_call
            res
            "then"
            [|
              Js.Unsafe.inject (
                Js.wrap_callback (
                  fun result => {
                    let resultRe =
                      Js.Unsafe.get result "entries" |>
                        Js.to_array |>
                        Array.to_list |>
                        List.map MerlinServiceConvert.jsMerlinCompletionEntryToMerlinEntry;
                    resultRe |>
                      List.map (fun entry => entry.Merlin.desc) |>
                      OcamlTypeToReasonType.formatMany |>
                      List.map2 (fun entry reasonType => {...entry, Merlin.desc: reasonType}) resultRe |>
                      List.map (MerlinServiceConvert.merlinCompletionEntryToNuclide replacementPrefix) |>
                      List.map NuclideJs.Autocomplete.entryToJs |>
                      Array.of_list |>
                      Js.array
                  }
                )
              )
            |]
        }
      }
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
        /* This returns a js promise. */
        let res = SuperMerlin.getTypeHint path::(path editor) text::text position::position;
        /* This also returns a js promise. */
        Js.Unsafe.meth_call
          res
          "then"
          [|
            Js.Unsafe.inject (
              Js.wrap_callback (fun result => MerlinServiceConvert.jsMerlinTypeHintEntryToNuclide result)
            )
          |]
      }
    )
  );

export
  "getLocation"
  (
    Js.wrap_callback (
      fun editor _ range => {
        /* TODO: put the non-conversion logic somewhere else. */
        let callback = Js.wrap_callback (
          fun () => {
            let (startPosition, _) = Atom.Range.fromJs range;
            let text = Atom.Buffer.getText (Atom.Editor.getBuffer editor);
            let p = path editor;
            let extension =
              if (Filename.check_suffix p "re") {
                "ml"
              } else {
                "mli"
              };
            /* This returns a js promise. */
            let res =
              SuperMerlin.locate path::(path editor) text::text extension::extension position::startPosition;
            Js.Unsafe.meth_call
              res
              "then"
              [|
                Js.Unsafe.inject (
                  Js.wrap_callback (
                    fun result => {
                      let f = Js.Unsafe.js_expr {|
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
                      Js.Unsafe.fun_call f [|result|]
                    }
                  )
                )
              |]
          }
        );
        Js.Unsafe.obj [|("range", Js.Unsafe.inject range), ("callback", Js.Unsafe.inject callback)|]
      }
    )
  );
