/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
let require s =>
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "require") [|Js.Unsafe.inject (Js.string s)|];

let export s itm => Js.Unsafe.set (Js.Unsafe.js_expr "exports") s itm;

let dotCall x y z => Js.Unsafe.meth_call x y z;

/* TODO: some files are temporary and/or don't have the right extension but the right syntax selected (e.g.
   source.reason). We should use that to detect syntax rather than a file extension. */
let extensionWithoutDot filePath => {
  let indexAfterlastDot =
    try (String.rindex filePath '.' + 1) {
    | Not_found =>
      raise (
        Invalid_argument "AtomReasonCommon.extensionWithoutDot: file path doesn't contain an extension."
      )
    };
  String.sub filePath indexAfterlastDot (String.length filePath - indexAfterlastDot)
};

let isInterface maybeFilePath => {
  let ext =
    switch maybeFilePath {
    | Some filePath => extensionWithoutDot filePath
    | None => "re"
    };
  String.compare "rei" ext === 0
};

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
