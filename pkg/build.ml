#!/usr/bin/env ocaml
(* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "reason" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    (* The .mllib *)
    Pkg.lib ~exts:Exts.library "src/reason";
    Pkg.lib ~exts:[".cmo"] "src/reason_toploop";
    Pkg.lib ~exts:[".cmx"; ".o"] "src/reasonbuild";
    Pkg.lib ~cond:(Env.bool "utop") ~exts:[".cmo"] "src/reason_utop";
    Pkg.bin ~auto:true "src/reasonfmt_impl" ~dst:"reasonfmt";
    Pkg.bin  "src/reasonfmt_merlin_impl.sh" ~dst:"reasonfmt_merlin";
    Pkg.bin  "src/reopt.sh" ~dst:"reopt";
    Pkg.bin  "src/rebuild.sh" ~dst:"rebuild";
    Pkg.bin  "src/rtop.sh" ~dst:"rtop";
    Pkg.bin  "src/reup.sh" ~dst:"reup";
    Pkg.bin  "src/rtop_init.ml" ~dst:"rtop_init.ml";
    Pkg.bin "_reasonbuild/_build/myocamlbuild" ~dst:"reasonbuild";
    Pkg.bin  ~auto:true "src/reason_error_reporter" ~dst:"refmterr";
    Pkg.bin  ~auto:true "src/reason_format_type" ~dst:"refmttype";
    Pkg.share "editorSupport/emacs/reasonfmt.el" ~dst:"../emacs/site-lisp/reasonfmt.el";
    Pkg.share "editorSupport/emacs/reason-mode.el" ~dst:"../emacs/site-lisp/reason-mode.el";
    Pkg.doc "README.md";
    Pkg.doc "LICENSE.txt";
    Pkg.doc "CHANGELOG.md";
  ]
