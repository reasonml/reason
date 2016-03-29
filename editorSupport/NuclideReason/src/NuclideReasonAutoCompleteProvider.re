/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open StringUtils;

let makeTellCommand text => ["tell", "start", "end", text];

let getNuclideJsAutocompleteSuggestions request => {
  let editor = request.Nuclide.AutocompleteProviderRequest.editor;
  let prefix = request.prefix;
  let text = Atom.Buffer.getText (Atom.Editor.getBuffer editor);
  let (line, col) = Atom.Cursor.getBufferPosition (List.hd (Atom.Editor.getCursors editor));
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
        let regex = Js.Unsafe.js_expr {|/([ \t\[\](){}<>,+*\/-])/|};
        let lst = StringUtils.split linePrefix by::regex;
        let len = List.length lst;
        len > 0 ? List.nth lst (len - 1) : linePrefix
      };
  if (String.length (String.trim linePrefix) === 0 || String.length (String.trim prefix) === 0) {
    []
  } else {
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
    let merlinService = MerlinService.getService path;
    let replacementPrefix =
      String.contains prefix '.' && String.index prefix '.' === 0 ?
        String.sub prefix 1 (String.length prefix - 1) : prefix;
    let onOutput output => {
      Merlin.(
        output
        |> List.map (fun entry => entry.desc)
        |> OcamlTypeToReasonType.formatMany
        |> List.map2 (fun entry reasonType => {...entry, desc: reasonType}) output
        |> List.map (MerlinServiceConvert.merlinCompletionEntryToNuclide replacementPrefix)
        |> List.map NuclideJs.Autocomplete.entryToJs
      )
    };
    let contextifiedTellCmd = MerlinService.contextifyStringQuery (makeTellCommand text) path;
    MerlinService.runSingleCommand merlinService path contextifiedTellCmd
      (fun result => MerlinService.complete merlinService path line col linePrefix onOutput)
  }
};
