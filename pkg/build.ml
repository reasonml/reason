(* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

open Topkg

let menhir_options =
  let trace = try let _ = Sys.getenv "trace" in "--trace" with | Not_found -> "" in
  "menhir --strict --unused-tokens --fixed-exception --table " ^ trace

let utop = Conf.(key "utop" bool ~absent:false)
let native = Conf.(key "native" bool ~absent:false)
let native_dynlink = Conf.(key "native-dynlink" bool ~absent:false)
let _ = native
let _ = native_dynlink

let () =

  let cmd c os files =
    let ocamlbuild = Conf.tool "ocamlbuild" os in
    OS.Cmd.run @@ Cmd.(ocamlbuild % "-use-ocamlfind"
                                  % "-use-menhir"
                                  %% (v "-menhir" % menhir_options)
                                  % "-cflags"
                                  % "-I,+ocamldoc"
                                  %% (v "-I" % "vendor/cmdliner")
                                  %% (v "-I" % "vendor/easy_format")
                                  %% (v "-I" % "vendor/ppx_deriving")
                                  %% of_list files)
  in
  let build = Pkg.build ~cmd () in
  let doc = Pkg.doc ~built:false in
  Pkg.describe "reason" ~build ~change_logs:[] ~licenses:[] ~readmes:[] @@ fun c ->
  Ok [
    Pkg.lib "pkg/META";
    (* The .mllib *)
    (* Our job is to generate reason.cma, but depending on whether or not
     * `utop` is available, we'll select an `.mllib` to compile as
     * `reason.cma`.
     *)
    Pkg.lib ~cond:(Conf.value c utop) ~exts:Exts.library "src/reason" ~dst:"reason";
    Pkg.lib ~cond:(not (Conf.value c utop)) ~exts:Exts.library "src/reason_without_utop" ~dst:"reason";
    (* But then regardless of if we have `utop` installed - still compile a
       library when the use case demands that there be no `utop` *)
    Pkg.lib ~exts:(Exts.exts [ ".cmo"; ".cmx";".cmi"; ".cmt";".mli"]) "src/reason_parser";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi";]) "src/reason_lexer";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "src/reason_pprint_ast";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"; ".cmxs"]) "src/reason_oprint";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "src/reason_config";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "src/reason_util";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "src/reason_parser_message";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "src/reason_toolchain";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "src/syntax_util";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"; ".cmxs"]) "src/redoc_html";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "vendor/cmdliner/cmdliner";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "vendor/easy_format/easy_format";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "vendor/ppx_deriving/ppx_deriving";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "vendor/ppx_deriving/ppx_deriving_show";
    Pkg.lib ~exts:Exts.library "src/reasondoc";
    Pkg.lib ~exts:(Exts.exts [".cmo"]) "src/reason_toploop";
    Pkg.lib ~exts:(Exts.exts [".cmx"; ".o"]) "src/reasonbuild";
    Pkg.lib ~cond:(Conf.value c utop) ~exts:(Exts.exts [".cmo"]) "src/reason_utop";
    Pkg.bin ~auto:true "src/refmt_impl" ~dst:"refmt";
    Pkg.bin ~auto:true "src/ocamlmerlin_reason" ~dst:"ocamlmerlin-reason";
    Pkg.bin ~auto:false "src/refmt_merlin_impl.sh" ~dst:"refmt_merlin";
    Pkg.bin ~auto:false "src/reopt.sh" ~dst:"reopt";
    Pkg.bin ~auto:false "src/rec.sh" ~dst:"rec";
    Pkg.bin ~auto:false "src/share.sh" ~dst:"share";
    Pkg.bin ~auto:false "src/rebuild.sh" ~dst:"rebuild";
    Pkg.bin ~auto:false "src/rtop.sh" ~dst:"rtop";
    Pkg.bin ~auto:false "src/redoc.sh" ~dst:"redoc";
    Pkg.bin ~auto:false "src/reup.sh" ~dst:"reup";
    Pkg.bin ~auto:false "src/rtop_init.ml" ~dst:"rtop_init.ml";
    Pkg.bin ~auto:false "_reasonbuild/_build/myocamlbuild" ~dst:"reasonbuild";
    Pkg.bin "src/reason_format_type" ~dst:"refmttype";
    Pkg.bin "src/reactjs_jsx_ppx" ~dst:"reactjs_jsx_ppx";
    Pkg.bin "src/ppx_react" ~dst:"ppx_react";
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

    doc "README.md";
    doc "LICENSE.txt";
  ]
