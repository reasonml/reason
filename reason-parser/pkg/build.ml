(* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

open Topkg

let menhir_options =
  let trace = try let _ = Sys.getenv "trace" in "--trace" with | Not_found -> "" in
  "menhir --strict --unused-tokens --fixed-exception --table " ^ trace

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
                                  %% (v "-I" % "vendor/easy_format")
                                  %% (v "-I" % "vendor/ppx_deriving")
                                  %% of_list files)
  in
  let build = Pkg.build ~cmd () in
  Pkg.describe "reason_parser" ~build ~change_logs:[] ~licenses:[] ~readmes:[] @@ fun c ->
  Ok [
    Pkg.lib "pkg/META";
    (* The .mllib *)
    (* Our job is to generate reason.cma, but depending on whether or not
     * `utop` is available, we'll select an `.mllib` to compile as
     * `reason.cma`.
     *)
    Pkg.lib ~exts:Exts.library "src/reason_parser" ~dst:"reason_parser";
    (* But then regardless of if we have `utop` installed - still compile a
       library when the use case demands that there be no `utop` *)
    Pkg.lib ~exts:(Exts.exts [ ".cmo"; ".cmx";".cmi"; ".cmt";".mli"]) "src/reason_parser";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi";]) "src/reason_lexer";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "src/reason_config";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "src/reason_util";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "src/reason_parser_message";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "src/reason_toolchain";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "src/syntax_util";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "vendor/ppx_deriving/ppx_deriving";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx";".cmi"; ".cmt"]) "vendor/ppx_deriving/ppx_deriving_show";
    Pkg.bin ~auto:true "src/ocamlmerlin_reason" ~dst:"ocamlmerlin-reason";
  ]
