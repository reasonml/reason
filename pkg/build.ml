#!/usr/bin/env ocaml
(* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)
#directory "pkg"
#use "topkg.ml"
let trace = try let _ = Sys.getenv "trace" in true with | Not_found -> false

let menhir_options = "'menhir --strict --unused-tokens --fixed-exception --table " ^ (if trace then "--trace" else "") ^ " '"
let menhir_command = "-menhir " ^ menhir_options

(* ; "-menhir 'menhir --trace'" *)
let () =

  Pkg.describe "reason" ~builder:(`OCamlbuild ["-use-menhir"; menhir_command; "-cflags -I,+ocamldoc"]) [
    Pkg.lib "pkg/META";
    (* The .mllib *)
    (* Our job is to generate reason.cma, but depending on whether or not
     * `utop` is available, we'll select an `.mllib` to compile as
     * `reason.cma`.
     *)
    Pkg.lib ~cond:(Env.bool "utop") ~exts:Exts.library "src/reason" ~dst:"reason";
    Pkg.lib ~cond:(not (Env.bool "utop")) ~exts:Exts.library "src/reason_without_utop" ~dst:"reason";
    (* But then regardless of if we have `utop` installed - still compile a
       library when the use case demands that there be no `utop` *)
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext ".cmi"; `Ext ".cmt";`Ext ".mli"] "src/reason_parser";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext ".cmi";] "src/reason_lexer";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext ".cmi"; `Ext ".cmt"] "src/reason_pprint_ast";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext ".cmi"; `Ext ".cmt"; `Ext ".cmxs"] "src/reason_oprint";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext  ".cmi"; `Ext ".cmt"] "src/reason_config";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext  ".cmi"; `Ext ".cmt"] "src/reason_util";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext  ".cmi"; `Ext ".cmt"] "src/reason_parser_message";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext  ".cmi"; `Ext ".cmt"] "src/reason_toolchain";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext  ".cmi"; `Ext ".cmt"] "src/syntax_util";
    Pkg.lib ~exts:[`Ext ".cmo"; `Ext ".cmx";`Ext  ".cmi"; `Ext ".cmt"; `Ext ".cmxs"] "src/redoc_html";
    Pkg.lib ~exts:Exts.library "src/reasondoc";
    Pkg.lib ~exts:[`Ext ".cmo"] "src/reason_toploop";
    Pkg.lib ~exts:[`Ext ".cmx"; `Ext ".o"] "src/reasonbuild";
    Pkg.lib ~cond:(Env.bool "utop") ~exts:[`Ext ".cmo"] "src/reason_utop";
    Pkg.bin ~auto:true "src/refmt_impl" ~dst:"refmt";
    Pkg.bin ~auto:true "src/tmfer_impl" ~dst:"tmfer";
    Pkg.bin ~auto:true "src/ocamlmerlin_reason" ~dst:"ocamlmerlin-reason";
    Pkg.bin  "src/refmt_merlin_impl.sh" ~dst:"refmt_merlin";
    Pkg.bin  "src/reopt.sh" ~dst:"reopt";
    Pkg.bin  "src/rec.sh" ~dst:"rec";
    Pkg.bin  "src/share.sh" ~dst:"share";
    Pkg.bin  "src/rebuild.sh" ~dst:"rebuild";
    Pkg.bin  "src/rtop.sh" ~dst:"rtop";
    Pkg.bin  "src/redoc.sh" ~dst:"redoc";
    Pkg.bin  "src/reup.sh" ~dst:"reup";
    Pkg.bin  "src/rtop_init.ml" ~dst:"rtop_init.ml";
    Pkg.bin "_reasonbuild/_build/myocamlbuild" ~dst:"reasonbuild";
    Pkg.bin  ~auto:true "src/reason_format_type" ~dst:"refmttype";
    Pkg.bin  ~auto:true "src/reactjs_jsx_ppx" ~dst:"reactjs_jsx_ppx";
    Pkg.share "editorSupport/emacs/refmt.el" ~dst:"../emacs/site-lisp/refmt.el";
    Pkg.share "editorSupport/emacs/reason-mode.el" ~dst:"../emacs/site-lisp/reason-mode.el";

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
