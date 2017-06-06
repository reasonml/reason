ppxDerivingAndResultStub="module Ppx_deriving_show = struct let sig_of_type ~options ~path decl = [] let str_of_type ~options ~path typ = [] end module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result"
resultStub="module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result"

menhirSuggestedLib=`menhir --suggest-menhirLib`

ppxToolsVersionedTargetDir=~/.esy/store-3.x.x/_build/opam_alpha__slash__ppx___tools___versioned-5.0.0-f6cc53982be397d21440d9c55c42b0050e891de4
# copied over from ~/.esy/store-3.x.x/_build/opam_alpha__slash__ocaml_migrate_parsetree-0.7.0-399416de9584af5975cdc304d32e2b716377916f/_build/default/src/
# and modified according to the README
ocamlMigrateParseTreeTargetDir=./omp
reasonTargetDir=../
reasonParserTargetDir=../reason-parser

make -C ../reason-parser
make -C ../

./node_modules/bs-platform/bin/bspack.exe \
  -bs-exclude-I ppx_deriving \
  -bs-exclude-I ppx_deriving_show \
  -bs-exclude-I Ppx_deriving_runtime \
  -bs-main Refmt_impl \
  -prelude-str "$ppxDerivingAndResultStub" \
  -I "$menhirSuggestedLib" \
  -I "$ppxToolsVersionedTargetDir" \
  -I "$reasonTargetDir" \
  -I "$reasonTargetDir/_build/src" \
  -I "$reasonTargetDir/vendor/cmdliner" \
  -I "$reasonParserTargetDir/_build/src" \
  -I "$reasonParserTargetDir/vendor/easy_format" \
  -I "$ocamlMigrateParseTreeTargetDir" \
  -o "./refmt_main.ml"

./node_modules/bs-platform/bin/bspack.exe \
  -bs-main Reactjs_jsx_ppx \
  -prelude-str "$resultStub" \
  -I "$reasonTargetDir/_build/src" \
  -I "$ocamlMigrateParseTreeTargetDir" \
  -o "./reactjs_ppx.ml"

./node_modules/bs-platform/bin/bspack.exe \
  -bs-main Reactjs_jsx_ppx_2 \
  -prelude-str "$resultStub" \
  -I "$reasonTargetDir/_build/src" \
  -I "$ocamlMigrateParseTreeTargetDir" \
  -o "./reactjs_ppx_2.ml"

# finally, actually compile all 3 files
ocamlopt -no-alias-deps -I +compiler-libs ocamlcommon.cmxa "refmt_main.ml" -o "refmt_main.out"
ocamlopt -no-alias-deps -I +compiler-libs ocamlcommon.cmxa  "reactjs_ppx.ml" -o "reactjs_ppx.out"
ocamlopt -no-alias-deps -I +compiler-libs ocamlcommon.cmxa  "reactjs_ppx_2.ml" -o "reactjs_ppx_2.out"
