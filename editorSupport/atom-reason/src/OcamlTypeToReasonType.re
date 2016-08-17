/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
/* This is a somewhat stop-gap measure to convert ocaml type strings to reason ones by shelling out to the
   utility we assume is installed. */
let childProcess = Js.Unsafe.js_expr "require('child_process')";

/* Shelling out each type to pretty-print is expensive. We'll cache each result to avoid shelling out so much.
   We'll also batch all the (uncached) types we need to shell out into a list and make the terminal command
   take them and print them all at once. */
let formatCache = Hashtbl.create 20;

/* Rarely used helper that isn't in the stdlib for totally understandable reasons. */
let insert at::at item::item l => {
  let result = {contents: []};
  let l = {contents: l};
  for i in 0 to (List.length l.contents) {
    if (i == at) {
      result.contents = [item, ...result.contents]
    } else {
      result.contents = [List.hd l.contents, ...result.contents];
      l.contents = List.tl l.contents
    }
  };
  List.rev result.contents
};

/* Shell out to the terminal, get result back. */
let refmttype ocamlTypes =>
  if (ocamlTypes == []) {
    []
  } else {
    /* Special handling. According to our subsequent logic [] maps to "" then back to [""], which violates
       the invariant that n input items maps to n outputs. */
    let refmttypePath =
      Atom.JsonType.(
        switch (Atom.Config.get "atom-reason.pathToRefmttype") {
        | JsonString str => str
        | _ => raise (Invalid_argument "refmttypePath went wrong.")
        }
      );
    /* Concatenating with special, recognized separator that can't be present in what we need to format. */
    let cmd = refmttypePath ^ " \"" ^ String.concat "\\\"" ocamlTypes ^ "\"";
    Js.Unsafe.meth_call
      childProcess
      "execSync"
      [|
        Js.Unsafe.inject (Js.string cmd)
      |] |>
      Js.to_string |>
      /* Output printed types are one type per line. */
      StringUtils.split by::(Js.Unsafe.js_expr {|/\n/|}) |>
      /* Restore the escaped line breaks. */
      List.map Scanf.unescaped
  };

let formatOne ocamlType =>
  try (Hashtbl.find formatCache ocamlType) {
  | Not_found =>
    let output = List.hd (refmttype [ocamlType]);
    Hashtbl.add formatCache ocamlType output;
    output
  };

/* More elaborate logic on batch formatting + shelling out a single time, instead of iterating through each
   type and call `formatOne` (which equates to n shell out command, which is expensive). */
let formatMany ocamlTypes => {
  let indices = {contents: []};
  let formattedFromCache = {contents: []};
  let unformatted = {contents: []};
  /* We'll split the ocamlTypes to format into two lists: the ones that are already cached and the ones that
     need to be shelled out. The shell command itself takes a list of types to format, so the logic here gets a
     bit complex. */
  List.iteri
    (
      fun i ocamlType =>
        try {
          let result = Hashtbl.find formatCache ocamlType;
          formattedFromCache.contents = [result, ...formattedFromCache.contents]
        } {
        | Not_found =>
          /* Indices are for bookkepping so that we can later merge (in the correct order)
             formattedFromCache with the shelled result from unformatted. */
          indices.contents = [i, ...indices.contents];
          unformatted.contents = [ocamlType, ...unformatted.contents]
        }
    )
    ocamlTypes;
  indices.contents = List.rev indices.contents;
  formattedFromCache.contents = List.rev formattedFromCache.contents;
  unformatted.contents = List.rev unformatted.contents;
  let returnedResults = {contents: refmttype unformatted.contents};
  /* Mergining cached and uncached now */
  assert (List.length returnedResults.contents == List.length unformatted.contents);
  List.iteri
    (
      fun _ index => {
        let item = List.hd returnedResults.contents;
        Hashtbl.add formatCache (List.nth ocamlTypes index) item;
        formattedFromCache.contents = insert at::index item::item formattedFromCache.contents;
        returnedResults.contents = List.tl returnedResults.contents
      }
    )
    indices.contents;
  formattedFromCache.contents
};
