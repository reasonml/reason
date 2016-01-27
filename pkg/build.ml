#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "reason" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    (* The .mllib *)
    Pkg.lib ~exts:Exts.library "src/reason";
    Pkg.lib ~exts:[".cmo"] "src/reason_toploop";
    Pkg.lib ~cond:(Env.bool "utop") ~exts:[".cmo"] "src/reason_utop";
    Pkg.bin ~auto:true "src/reasonfmt_impl" ~dst:"reasonfmt";
    Pkg.bin  "src/reasonfmt_merlin_impl.sh" ~dst:"reasonfmt_merlin";
    Pkg.bin  "src/rtop.sh" ~dst:"rtop";
    Pkg.bin  "src/rtop_init.ml" ~dst:"rtop_init.ml";
    Pkg.doc "README.md";
    Pkg.doc "LICENSE.txt";
    Pkg.doc "CHANGELOG.md";
  ]
