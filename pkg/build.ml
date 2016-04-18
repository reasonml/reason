#!/usr/bin/env ocaml
(* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)
#directory "pkg"
#use "topkg.ml"
let trace = try let _ = Sys.getenv "trace" in true with | Not_found -> false

let menhir_options = "'menhir --fixed-exception --table " ^ (if trace then "--trace" else "") ^ " '"
let menhir_command = "-menhir " ^ menhir_options

(* ; "-menhir 'menhir --trace'" *)
let () =
  Pkg.describe "reason" ~builder:(`OCamlbuild ["-use-menhir"; menhir_command ]) [
    Pkg.lib "pkg/META";
    (* The .mllib *)
    (* So much redundancy - this should be implicit in the mllib! *)
    Pkg.lib ~exts:Exts.library "src/reason";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext ".cmi"; `Ext ".cmt";`Ext ".mli"] "src/reason_parser";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext ".cmi";] "src/reason_lexer";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext ".cmi"; `Ext ".cmt"] "src/reason_pprint_ast";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext ".cmi"; `Ext ".cmt"] "src/reason_oprint";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext  ".cmi"; `Ext ".cmt"] "src/reason_config";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext  ".cmi"; `Ext ".cmt"] "src/reason_utils";
    Pkg.lib ~exts:[`Ext ".cmo"] "src/reason_toploop";
    Pkg.lib ~exts:[`Ext ".cmx"; `Ext ".o"] "src/reasonbuild";
    Pkg.lib ~cond:(Env.bool "utop") ~exts:[`Ext ".cmo"] "src/reason_utop";
    Pkg.bin ~auto:true "src/reasonfmt_impl" ~dst:"reasonfmt";
    Pkg.bin ~auto:true "src/ocamlmerlin_reason" ~dst:"ocamlmerlin-reason";
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
    (* AtomReason *)
    (* Unfortunately we have to specificy each individual file *)
    Pkg.share "editorSupport/AtomReason/package.json" ~dst:"editorSupport/AtomReason/package.json";
    Pkg.share "editorSupport/AtomReason/README.md" ~dst:"editorSupport/AtomReason/README.md";
    Pkg.share "editorSupport/AtomReason/output_byte_debug_js/app.js" ~dst:"editorSupport/AtomReason/output_byte_debug_js/app.js";
    Pkg.share "editorSupport/AtomReason/output_byte_debug_js/app.map" ~dst:"editorSupport/AtomReason/output_byte_debug_js/app.map";

    Pkg.share "editorSupport/AtomReason/lib/main.js" ~dst:"editorSupport/AtomReason/lib/main.js";
    Pkg.share "editorSupport/AtomReason/lib/ReasonDiagnosticsProvider.js" ~dst:"editorSupport/AtomReason/lib/ReasonDiagnosticsProvider.js";
    Pkg.share "editorSupport/AtomReason/lib/constants.js" ~dst:"editorSupport/AtomReason/lib/constants.js";
    Pkg.share "editorSupport/AtomReason/lib/Notiflyer.js" ~dst:"editorSupport/AtomReason/lib/Notiflyer.js";
    Pkg.share "editorSupport/AtomReason/lib/fixedEnv.js" ~dst:"editorSupport/AtomReason/lib/fixedEnv.js";
    Pkg.share "editorSupport/AtomReason/lib/environment-helpers.js" ~dst:"editorSupport/AtomReason/lib/environment-helpers.js";

    Pkg.share "editorSupport/AtomReason/Reasonify/formatErrorMessages.js" ~dst:"editorSupport/AtomReason/Reasonify/formatErrorMessages.js";
    Pkg.share "editorSupport/AtomReason/Reasonify/index.js" ~dst:"editorSupport/AtomReason/Reasonify/index.js";

    Pkg.share "editorSupport/AtomReason/styles/status-bar.less" ~dst:"editorSupport/AtomReason/styles/status-bar.less";
    Pkg.share "editorSupport/AtomReason/styles/type-hint.less" ~dst:"editorSupport/AtomReason/styles/type-hint.less";

    (* language-reason *)
    Pkg.share "editorSupport/language-reason/package.json" ~dst:"editorSupport/language-reason/package.json";
    Pkg.share "editorSupport/language-reason/grammars/reason.cson" ~dst:"editorSupport/language-reason/grammars/reason.cson";
    Pkg.share "editorSupport/language-reason/settings/reason.cson" ~dst:"editorSupport/language-reason/settings/reason.cson";
    Pkg.share "editorSupport/language-reason/styles/language-reason.less" ~dst:"editorSupport/language-reason/styles/language-reason.less";
    Pkg.share "editorSupport/language-reason/snippets/reason.cson" ~dst:"editorSupport/language-reason/snippets/reason.cson";
    Pkg.share "editorSupport/language-reason/lib/language-reason-view.coffee" ~dst:"editorSupport/language-reason/lib/language-reason-view.coffee";
    Pkg.share "editorSupport/language-reason/lib/language-reason.coffee" ~dst:"editorSupport/language-reason/lib/language-reason.coffee";
    Pkg.share "editorSupport/language-reason/menus/language-reason.cson" ~dst:"editorSupport/language-reason/menus/language-reason.cson";
    Pkg.share "editorSupport/language-reason/README.md" ~dst:"editorSupport/language-reason/README.md";
    Pkg.share "editorSupport/language-reason/LICENSE.md" ~dst:"editorSupport/language-reason/LICENSE.md";
    Pkg.share "editorSupport/language-reason/LICENSE.grammar" ~dst:"editorSupport/language-reason/LICENSE.grammar";
    Pkg.doc "README.md";
    Pkg.doc "LICENSE.txt";
    Pkg.doc "CHANGELOG.md";
  ]
