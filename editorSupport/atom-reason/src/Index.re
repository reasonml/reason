/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
/*
 * Ideally, most of atom-reason would be in pure Reason, and the parts that need to
 * to convert to/from JS would be at the edges (Index.re, and SuperMerlin.re).
 */
open AtomReasonCommon;

/* These exports will be visible to main.js, under AtomReason */
export
  "getDiagnostics"
  (
    Js.wrap_callback (
      fun jsEditor => Atom.Promise.createFakePromise (
        fun resolve reject =>
          AtomReasonDiagnostics.getMerlinDiagnostics
            /* editor::(Atom.Editor.fromJs jsEditor) */
            editor::(Atom.Editor.fromJs jsEditor)
            (
              fun successResult => resolve (
                Js.array (Array.map NuclideJs.Diagnostic.Message.toJs successResult)
              )
            )
            /* TODO: this has never been used. */
            reject
      )
    )
  );

export
  "getEntireFormatting"
  (
    Js.wrap_callback (
      fun jsEditor jsRange jsNotifySuccess jsNotifyInvalid jsNotifyInfo => {
        let editor = Atom.Editor.fromJs jsEditor;
        let range = Atom.Range.fromJs jsRange;
        let notifySuccess msg =>
          Js.Unsafe.fun_call jsNotifySuccess [|Js.Unsafe.inject (Js.string msg)|];
        let notifyInvalid msg =>
          Js.Unsafe.fun_call jsNotifyInvalid [|Js.Unsafe.inject (Js.string msg)|];
        let notifyInfo msg => Js.Unsafe.fun_call jsNotifyInfo [|Js.Unsafe.inject (Js.string msg)|];
        Atom.Promise.createFakePromise (
          fun resolve reject =>
            AtomReasonFormat.getEntireFormatting
              editor::editor
              range::range
              notifySuccess::notifySuccess
              notifyInvalid::notifyInvalid
              notifyInfo::notifyInfo
              (fun successResult => resolve (NuclideJs.FileFormat.toJs successResult))
              (fun rejectedMsg => reject (Js.string rejectedMsg))
        )
      }
    )
  );

export
  "getPartialFormatting"
  (
    Js.wrap_callback (
      fun jsEditor jsRange jsNotifySuccess jsNotifyInvalid jsNotifyInfo => {
        let editor = Atom.Editor.fromJs jsEditor;
        let range = Atom.Range.fromJs jsRange;
        let notifySuccess msg =>
          Js.Unsafe.fun_call jsNotifySuccess [|Js.Unsafe.inject (Js.string msg)|];
        let notifyInvalid msg =>
          Js.Unsafe.fun_call jsNotifyInvalid [|Js.Unsafe.inject (Js.string msg)|];
        let notifyInfo msg => Js.Unsafe.fun_call jsNotifyInfo [|Js.Unsafe.inject (Js.string msg)|];
        Atom.Promise.createFakePromise (
          fun resolve reject =>
            AtomReasonFormat.getPartialFormatting
              editor::editor
              range::range
              notifySuccess::notifySuccess
              notifyInvalid::notifyInvalid
              notifyInfo::notifyInfo
              (fun successResult => resolve (Js.string successResult))
              (fun rejectedMsg => reject (Js.string rejectedMsg))
        )
      }
    )
  );

export
  "getNuclideJsAutocompleteSuggestions"
  (
    Js.wrap_callback (
      fun request => {
        let request = NuclideJs.AutocompleteProviderRequest.fromJs request;
        let editor = request.Nuclide.AutocompleteProviderRequest.editor;
        let prefix = request.prefix;
        let text = Atom.Buffer.getText (Atom.Editor.getBuffer editor);
        let (line, col) as position = Atom.Cursor.getBufferPosition (
          List.hd (Atom.Editor.getCursors editor)
        );

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
        if (
          String.length (String.trim linePrefix) === 0 || String.length (String.trim prefix) === 0
        ) {
          Atom.Promise.createFakePromise (fun resolve reject => resolve (Js.array [||]))
        } else {
          let replacementPrefix =
            if (String.contains prefix '.' && String.index prefix '.' === 0) {
              String.sub prefix 1 (String.length prefix - 1)
            } else {
              prefix
            };
          Atom.Promise.createFakePromise (
            fun resolve reject =>
              SuperMerlin.getAutoCompleteSuggestions
                path::(path editor)
                text::text
                position::position
                prefix::linePrefix
                (
                  fun result => {
                    let resultRe =
                      Js.Unsafe.get result "entries" |> Js.to_array |> Array.to_list |>
                      List.map MerlinServiceConvert.jsMerlinCompletionEntryToMerlinEntry;
                    resultRe |>
                    List.map (
                      MerlinServiceConvert.merlinCompletionEntryToNuclide replacementPrefix
                    ) |>
                    List.map NuclideJs.Autocomplete.entryToJs |> Array.of_list |> Js.array |> resolve
                  }
                )
                /* TODO: NOT ALWAYS STRING MIGHT BE ERROR FRMATTER */
                reject
          )
        }
      }
    )
  );

export
  "getNuclideJsTypeHint"
  (
    Js.wrap_callback (
      fun jsEditor position => {
        /* TODO: make selection work in conjunction with expansion */
        /* TODO: currently gets the first type hint. The rest are the types of the expanded scope. Will use
           them one day. */
        let position = Atom.Point.fromJs position;
        let text = Atom.Buffer.getText (Atom.Editor.getBuffer jsEditor);
        Atom.Promise.createFakePromise (
          fun resolve reject =>
            SuperMerlin.getTypeHint
              path::(path jsEditor)
              text::text
              position::position
              (fun result => resolve (MerlinServiceConvert.jsMerlinTypeHintEntryToNuclide result))
              (fun rejectedMsg => reject (Js.string rejectedMsg))
        )
      }
    )
  );

export
  "getLocation"
  (
    Js.wrap_callback (
      fun jsEditor _ range => Atom.Promise.createFakePromise (
        fun resolve reject => {
          let path' = path jsEditor;
          let text = Atom.Buffer.getText (Atom.Editor.getBuffer jsEditor);
          let extension = isInterface (Some path') ? "mli" : "ml";
          AtomReasonLocate.locateAndGoToLocation
            path::path'
            text::text
            extension::extension
            range::(Atom.Range.fromJs range)
            resolve
            reject
        }
      )
    )
  );

export
  "selectOccurrences"
  (
    Js.wrap_callback (
      fun jsEditor => {
        let editor = Atom.Editor.fromJs jsEditor;
        let position = Atom.Cursor.getBufferPosition (List.hd (Atom.Editor.getCursors editor));
        let text = Atom.Buffer.getText (Atom.Editor.getBuffer jsEditor);
        SuperMerlin.getOccurrences
          path::(path jsEditor)
          text::text
          position::position
          (
            fun result =>
              MerlinServiceConvert.jsMerlinOccurrencesToAtom result |>
              AtomReasonOccurrences.selectOccurrences editor::editor
          )
          /* TODO: do something here. */
          (fun rejectedMsg => ())
      }
    )
  );

export
  "destruct"
  (
    Js.wrap_callback (
      fun jsEditor startPosition endPosition => Atom.Promise.createFakePromise (
        fun resolve reject => {
          let editor = Atom.Editor.fromJs jsEditor;
          let position1 = Atom.Point.fromJs startPosition;
          let position2 = Atom.Point.fromJs endPosition;
          let text = Atom.Buffer.getText (Atom.Editor.getBuffer jsEditor);
          SuperMerlin.destruct
            path::(path jsEditor)
            text::text
            startPosition::position1
            endPosition::position2
            (
              fun result => {
                let _ =
                  AtomReasonDestruct.destruct
                    editor (MerlinServiceConvert.jsMerlinDestructToNuclide result);
                resolve ()
              }
            )
            /* merlin complains when it can't perform case analysis, ignoring that here */
            (fun rejectedMsg => resolve ())
        }
      )
    )
  );

export
  "getOutline"
  (
    Js.wrap_callback (
      fun jsEditor => Atom.Promise.createFakePromise (
        fun resolve reject => {
          let path' = path jsEditor;
          let text = Atom.Buffer.getText (Atom.Editor.getBuffer jsEditor);
          let grammar = Atom.Grammar.name (Atom.Editor.getGrammar jsEditor);
          AtomReasonOutline.getOutline path::path' text::text grammar::grammar resolve reject
        }
      )
    )
  );
