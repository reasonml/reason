/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
open StringUtils;

let descriptionPrefixRegex = Js.Unsafe.js_expr {|/(let\s*\S*\s*:|type\s*\S*\s*=\s*)/|};

let getReasonifyConfig () => Js.Unsafe.js_expr {|
  {
    showSourcePreviews: false,
    showFileHeaders: false,
    pathToReasonfmt: atom.config.get('NuclideReason.pathToReasonfmt'),
    errorPreviewExpand: 5,
    warningPreviewExpand: 2, indentCode: 4,
    indentHuman: 0,
    niceifyModuleAliases: true,
    renderHtml: true,
    columns: 90
  }
|};

/* Sometimes there's an error because it's not as simple as creating a single mli file. */
/* TODO: Log this to improve tooling. For now, we can just fall back to
 * using the cache on an individual result level.
 */
let fmtErrMsg = "Could not format types for completion: This happens occasionally but report if you see it happen commonly:linePrefix:";

let reasonify = Js.Unsafe.js_expr "require('../Reasonify')";

/**
 * Until reimplemented in ML, Reasonify is written in JS and this `Reasonify`
 * module should expose a Reason API, hiding the JS types.
 */
let module Reasonify = {
  let niceifyType (str: string, config) :string => Js.to_string (
    Js.string (
      Js.Unsafe.meth_call
        reasonify "niceifyType" [|Js.Unsafe.inject (Js.string str), Js.Unsafe.inject config|]
    )
  );
  let reasonifyManySignatureItems (strList: list string) (width: int) config => {
    let jsArr = Js.array (Array.map Js.string (Array.of_list strList));
    let arr = Js.to_array (
      Js.Unsafe.meth_call
        reasonify
        "reasonifyManySignatureItems"
        [|Js.Unsafe.inject jsArr, Js.Unsafe.inject width, Js.Unsafe.inject config|]
    );
    Array.to_list (Array.map Js.to_string arr)
  };
};

/*
 * Merlin response format:
 *
 *  Module entries look like this:
 *  ----------------------------
 *  desc: ""
 *  info: ""
 *  kind: "Module"
 *  name: "ExampleMod"
 *
 *  Value entries look like this.
 *  ----------------------------
 *  desc: "val map : ('a -> 'b) -> 'a list -> 'b list"
 *  info: ""
 *  kind: "Value"
 *  name: "map"
 *
 *  Type entries look like this.
 *  ----------------------------
 *  desc: "type reexportedObscureType =↵ M_CommonMLExampleDependency__ExampleMod.reexportedObscureType"
 *  info: ""
 *  kind: "Type"
 *  name: "reexportedObscureType"
 */
let module MerlinResponseFormatter = {
  open Merlin;
  let formatCache = Hashtbl.create 7;
  let rec needsLookup outputEntries =>
    switch outputEntries {
      | [] => false
      | [hd, ...tl] => {
          let desc = hd.desc;
          String.compare desc "" != 0 && not (Hashtbl.mem formatCache hd.desc) || needsLookup tl
        }
    };
  let trimReasonifiedDescription (desc: string) => {
    /* Conveniently this won't match types of the form: type t 'a = list 'a; */
    let withoutPrefix = String.trim (StringUtils.replace desc descriptionPrefixRegex "");
    /*desc.replace(descriptionPrefixRegex, function(x) {return "";}).trim();*/
    let lastIndex = String.length withoutPrefix - 1;
    let ret = withoutPrefix.[lastIndex] == ';' ? String.sub withoutPrefix 0 lastIndex : withoutPrefix;
    String.trim ret
  };
  /**
   * Normalizes Merlin response entries into formatted, pretty printed
   * names/descriptions.
   */
  let normalizeCompletionItems (outputEntries, linePrefix) => {
    let needsLookup = needsLookup outputEntries;
    let definitelyOldMerlin = {contents: false};
    let definitelyNewMerlin = {contents: false};
    if needsLookup {
      let config = getReasonifyConfig ();
      let getNiceOutputEntry entry => {
        let nameColon = entry.name ^ ":";
        definitelyNewMerlin.contents <-
          definitelyNewMerlin.contents ||
            entry.kind == Value &&
              not (startsWith entry.desc "val") && not (startsWith entry.desc "external") ||
            entry.kind == Constructor && not (startsWith entry.desc nameColon);
        definitelyOldMerlin.contents <-
          definitelyOldMerlin.contents ||
            not definitelyNewMerlin.contents ||
            (entry.kind == Value && startsWith entry.desc "val" || startsWith entry.desc "external") ||
            entry.kind == Constructor && startsWith entry.desc (entry.name ^ " :");
        /* Special case crashes formatter */
        if (entry.desc === "type 'a list = [] | :: of 'a * 'a list") {
          "type 'a list = list"
        } else if (
          entry.kind == Value
        ) {
          definitelyNewMerlin.contents ?
            /* New merlin has no "val" prefix, insert type. */
            Reasonify.niceifyType ("type t = " ^ entry.desc, config) :
            /* Else, old merlin already has a "val: or external:" */
            Reasonify.niceifyType (entry.desc, config)
        } else if (
          entry.kind == Constructor
        ) {
          if
            /* Old merlin lists constructor descriptions as:
               Constructor : int * int */
            definitelyOldMerlin.contents {
            Reasonify.niceifyType ("type t = | " ^ entry.desc, config)
          } else {
            /* Else, it's just contains a type description */
            Reasonify.niceifyType (
              "type t = " ^ entry.desc,
              config
            )
          }
        } else {
          Reasonify.niceifyType (entry.desc, config)
        }
      };
      let niceOutputEntriesWithFixedTypeConstructors = List.map getNiceOutputEntry outputEntries;
      let newReasonifiedCompletionDescriptions =
        try (Reasonify.reasonifyManySignatureItems niceOutputEntriesWithFixedTypeConstructors 999 config) {
          | _ => {
              Console.log (fmtErrMsg ^ linePrefix);
              niceOutputEntriesWithFixedTypeConstructors
            }
        };
      let cacheEntry entry newReasonifiedCompletionDescription =>
        if (not (entry.desc === "") && not (Hashtbl.mem formatCache entry.desc)) {
          Hashtbl.add formatCache entry.desc (trimReasonifiedDescription newReasonifiedCompletionDescription)
        };
      List.iter2 cacheEntry outputEntries newReasonifiedCompletionDescriptions
    };
    let getFormattedCompletion entry => {
      let desc = Hashtbl.mem formatCache entry.desc ? Hashtbl.find formatCache entry.desc : entry.desc;
      {desc, info: entry.info, kind: entry.kind, name: entry.name}
    };
    List.map getFormattedCompletion outputEntries
  };
};

let getNuclideJsAutocompleteSuggestions request => {
  let editor = request.Nuclide.AutocompleteProviderRequest.editor;
  let prefix = request.prefix;
  let (line, col) = Atom.Cursor.getBufferPosition (List.hd (Atom.Editor.getCursors editor));
  /**
   * The default prefix at something like `Printf.[cursor]` is just the dot. Compute
   * `linePrefix` so that ocamlmerlin gets more context. Compute `replacementPrefix`
   * to make sure that the existing dot doesn't get clobbered when autocompleting.
   */
  let linePrefix = String.sub (Atom.Editor.lineTextForBufferRow editor line) 0 col;
  let linePrefix =
    String.length linePrefix == 0 ?
      linePrefix :
      {
        let regex = Js.Unsafe.js_expr {|/([ \t\[\](){}<>,+*\/-])/|};
        let lst = StringUtils.split linePrefix regex;
        let len = List.length lst;
        len > 0 ? List.nth lst (len - 1) : linePrefix
      };
  if (String.length (String.trim linePrefix) == 0 || String.length (String.trim prefix) == 0) {
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
      String.contains prefix '.' && String.index prefix '.' == 0 ?
        String.sub prefix 1 (String.length prefix - 1) : prefix;
    let onOutput output => {
      let normalizedCompletionItems = MerlinResponseFormatter.normalizeCompletionItems (output, linePrefix);
      let nuclideJsEntries =
        List.map

          (MerlinServiceConvert.merlinCompletionEntryToNuclide replacementPrefix) normalizedCompletionItems;
      List.map NuclideJs.Autocomplete.entryToJs nuclideJsEntries
    };
    MerlinService.complete merlinService path line col linePrefix onOutput
  }
};

