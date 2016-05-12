#!/usr/bin/env ocaml
(* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)
#directory "pkg"
#use "topkg.ml"
let trace = try let _ = Sys.getenv "trace" in true with | Not_found -> false

let menhir_options = "'menhir --fixed-exception --table " ^ (if trace then "--trace" else "") ^ " '"
let menhir_command = "-menhir " ^ menhir_options

(* ; "-menhir 'menhir --trace'" *)
let () =
  Pkg.describe "reason" ~builder:(`OCamlbuild ["-use-menhir"; menhir_command; "-cflags -I,+ocamldoc"]) [
    Pkg.lib "pkg/META";
    (* The .mllib *)
    (* So much redundancy - this should be implicit in the mllib! *)
    Pkg.lib ~exts:Exts.library "src/reason";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext ".cmi"; `Ext ".cmt";`Ext ".mli"] "src/reason_parser";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext ".cmi";] "src/reason_lexer";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext ".cmi"; `Ext ".cmt"] "src/reason_pprint_ast";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext ".cmi"; `Ext ".cmt"; `Ext ".cmxs"] "src/reason_oprint";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext  ".cmi"; `Ext ".cmt"] "src/reason_config";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext  ".cmi"; `Ext ".cmt"] "src/reason_utils";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext  ".cmi"; `Ext ".cmt"; `Ext ".cmxs"] "src/redoc_html";
    Pkg.lib ~exts:Exts.library "src/reasondoc";
    Pkg.lib ~exts:[`Ext ".cmo"] "src/reason_toploop";
    Pkg.lib ~exts:[`Ext ".cmx"; `Ext ".o"] "src/reasonbuild";
    Pkg.lib ~cond:(Env.bool "utop") ~exts:[`Ext ".cmo"] "src/reason_utop";
    Pkg.bin ~auto:true "src/refmt_impl" ~dst:"refmt";
    Pkg.bin ~auto:true "src/ocamlmerlin_reason" ~dst:"ocamlmerlin-reason";
    Pkg.bin  "src/refmt_merlin_impl.sh" ~dst:"refmt_merlin";
    Pkg.bin  "src/reopt.sh" ~dst:"reopt";
    Pkg.bin  "src/rebuild.sh" ~dst:"rebuild";
    Pkg.bin  "src/rtop.sh" ~dst:"rtop";
    Pkg.bin  "src/redoc.sh" ~dst:"redoc";
    Pkg.bin  "src/reup.sh" ~dst:"reup";
    Pkg.bin  "src/rtop_init.ml" ~dst:"rtop_init.ml";
    Pkg.bin "_reasonbuild/_build/myocamlbuild" ~dst:"reasonbuild";
    Pkg.bin  ~auto:true "src/reason_error_reporter" ~dst:"refmterr";
    Pkg.bin  ~auto:true "src/reason_format_type" ~dst:"refmttype";
    Pkg.share "editorSupport/emacs/refmt.el" ~dst:"../emacs/site-lisp/refmt.el";
    Pkg.share "editorSupport/emacs/reason-mode.el" ~dst:"../emacs/site-lisp/reason-mode.el";
    (* atom-reason *)
    (* Unfortunately we have to specificy each individual file *)
    Pkg.share "editorSupport/atom-reason/package.json" ~dst:"editorSupport/atom-reason/package.json";
    Pkg.share "editorSupport/atom-reason/README.md" ~dst:"editorSupport/atom-reason/README.md";
    Pkg.share "editorSupport/atom-reason/output_byte_debug_js/app.js" ~dst:"editorSupport/atom-reason/output_byte_debug_js/app.js";
    Pkg.share "editorSupport/atom-reason/output_byte_debug_js/app.map" ~dst:"editorSupport/atom-reason/output_byte_debug_js/app.map";

    Pkg.share "editorSupport/atom-reason/lib/main.js" ~dst:"editorSupport/atom-reason/lib/main.js";
    Pkg.share "editorSupport/atom-reason/lib/ReasonDiagnosticsProvider.js" ~dst:"editorSupport/atom-reason/lib/ReasonDiagnosticsProvider.js";
    Pkg.share "editorSupport/atom-reason/lib/constants.js" ~dst:"editorSupport/atom-reason/lib/constants.js";
    Pkg.share "editorSupport/atom-reason/lib/Notiflyer.js" ~dst:"editorSupport/atom-reason/lib/Notiflyer.js";
    Pkg.share "editorSupport/atom-reason/lib/fixedEnv.js" ~dst:"editorSupport/atom-reason/lib/fixedEnv.js";
    Pkg.share "editorSupport/atom-reason/lib/environment-helpers.js" ~dst:"editorSupport/atom-reason/lib/environment-helpers.js";

    Pkg.share "editorSupport/atom-reason/Reasonify/formatErrorMessages.js" ~dst:"editorSupport/atom-reason/Reasonify/formatErrorMessages.js";
    Pkg.share "editorSupport/atom-reason/Reasonify/index.js" ~dst:"editorSupport/atom-reason/Reasonify/index.js";

    Pkg.share "editorSupport/atom-reason/styles/status-bar.less" ~dst:"editorSupport/atom-reason/styles/status-bar.less";
    Pkg.share "editorSupport/atom-reason/styles/type-hint.less" ~dst:"editorSupport/atom-reason/styles/type-hint.less";

    Pkg.share "editorSupport/atom-reason/keymaps/atom-reason.json" ~dst:"editorSupport/atom-reason/keymaps/atom-reason.json";

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
    Pkg.share "editorSupport/language-reason/LICENSE.grammar" ~dst:"editorSupport/language-reason/LICENSE.grammar";

    (* VimReason *)
    Pkg.share "editorSupport/VimReason/after/syntax/reason.vim" ~dst:"editorSupport/VimReason/after/syntax/reason.vim";
    Pkg.share "editorSupport/VimReason/autoload/reason.vim" ~dst:"editorSupport/VimReason/autoload/reason.vim";
    Pkg.share "editorSupport/VimReason/autoload/refmt.vim" ~dst:"editorSupport/VimReason/autoload/refmt.vim";
    Pkg.share "editorSupport/VimReason/doc/reason.txt" ~dst:"editorSupport/VimReason/doc/reason.txt";
    Pkg.share "editorSupport/VimReason/ftdetect/reason.vim" ~dst:"editorSupport/VimReason/ftdetect/reason.vim";
    Pkg.share "editorSupport/VimReason/ftplugin/reason.vim" ~dst:"editorSupport/VimReason/ftplugin/reason.vim";
    Pkg.share "editorSupport/VimReason/indent/reason.vim" ~dst:"editorSupport/VimReason/indent/reason.vim";
    Pkg.share "editorSupport/VimReason/LICENSE" ~dst:"editorSupport/VimReason/LICENSE";
    Pkg.share "editorSupport/VimReason/plugin/reason.vim" ~dst:"editorSupport/VimReason/plugin/reason.vim";
    Pkg.share "editorSupport/VimReason/README.md" ~dst:"editorSupport/VimReason/README.md";
    Pkg.share "editorSupport/VimReason/syntax/reason.vim" ~dst:"editorSupport/VimReason/syntax/reason.vim";
    Pkg.share "editorSupport/VimReason/syntax_checkers/reason/reasonc.vim" ~dst:"editorSupport/VimReason/syntax_checkers/reason/reasonc.vim";


    Pkg.doc "README.md";
    Pkg.doc "LICENSE.txt";
    Pkg.doc "CHANGELOG.md";
  ]
