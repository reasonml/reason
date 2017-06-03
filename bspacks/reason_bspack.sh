#!/bin/sh

# This is only an example packing workflow!

# preludeStr="module Ppx_deriving_show = struct let sig_of_type ~options ~path decl = [] let str_of_type ~options ~path typ = [] end module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result"
# menhirSuggestedLib=`menhir --suggest-menhirLib`

# reasonTargetDir=~/.esy/store-3.x.x/_build/opam_alpha__slash__reason-1.13.5-92dff378e19035480a3809f50558ac9b51778906
# ppxToolsVersionedTargetDir=~/.esy/store-3.x.x/_build/opam_alpha__slash__ppx___tools___versioned-5.0.0-f6cc53982be397d21440d9c55c42b0050e891de4
# reasonParserTargetDir=~/.esy/store-3.x.x/_build/opam_alpha__slash__reason_parser-1.13.5-d46d6ccdf7b2d6d5318e466d39a7adbe0ce547b2
# resultTargetDir=~/.esy/store-3.x.x/_build/opam_alpha__slash__result-1.2.0-a35bca36a11ab96b24a91513f2667472b5141bbd
# # Notice the _build/default/src
# ocamlMigrateParseTreeTargetDir=./omp

# origDir="."

# ./node_modules/bs-platform/bin/bspack.exe \
#   -bs-exclude-I ppx_deriving \
#   -bs-exclude-I ppx_deriving_show \
#   -bs-exclude-I Ppx_deriving_runtime \
#   -prelude-str "$preludeStr" \
#   -I "$menhirSuggestedLib" \
#   -bs-main Refmt_impl \
#   -I "$ppxToolsVersionedTargetDir" \
#   -I "$reasonTargetDir" \
#   -I "$reasonTargetDir/_build/src" \
#   -I "$reasonTargetDir/vendor/cmdliner" \
#   -I "$reasonParserTargetDir/_build/src" \
#   -I "$reasonParserTargetDir/vendor/easy_format" \
#   -I "$ocamlMigrateParseTreeTargetDir" \
#   -o "$origDir/refmt_main.ml"

# ./node_modules/bs-platform/bin/bspack.exe \
#   -bs-exclude-I ppx_deriving \
#   -bs-exclude-I ppx_deriving_show \
#   -bs-exclude-I Ppx_deriving_runtime \
#   -prelude-str "$preludeStr" \
#   -I "$menhirSuggestedLib" -bs-main Reactjs_jsx_ppx \
#   -I "$ppxToolsVersionedTargetDir" \
#   -I "$reasonTargetDir" \
#   -I "$reasonTargetDir/_build/src" \
#   -I "$reasonTargetDir/vendor/cmdliner" \
#   -I "$reasonParserTargetDir/_build/src" \
#   -I "$reasonParserTargetDir/vendor/easy_format" \
#   -I "$ocamlMigrateParseTreeTargetDir" \
#   -o "$origDir/reactjs_ppx.ml"

# ocamlopt -no-alias-deps -linkall -I +compiler-libs ocamlcommon.cmxa "refmt_main.ml" -o "refmt_main.out"
# ocamlopt -no-alias-deps -linkall -I +compiler-libs ocamlcommon.cmxa  "reactjs_ppx.ml" -o "reactjs_ppx.out"
