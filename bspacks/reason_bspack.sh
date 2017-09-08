# The bspacked version of refmt doesn't use the Ppx_deriving_show used in Reason for `--add-printers`, which generates runtime printing for data structures. The refmt used by BuckleScript doesn't need that (just use `Js.log`).

# Because OCaml 4.02 doesn't come with the `Result` module, it also needed stubbing out.

ppxDerivingAndResultStub="module Ppx_deriving_show = struct let sig_of_type ~options ~path decl = [] let str_of_type ~options ~path typ = [] end module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result"

menhirSuggestedLib=`menhir --suggest-menhirLib`

# copied over from ~/.esy/store-3.x.x/_build/opam_alpha__slash__ocaml_migrate_parsetree-0.7.0-399416de9584af5975cdc304d32e2b716377916f/_build/default/src/
# and modified according to the README
ocamlMigrateParseTreeTargetDir=./omp/_build/default/src
reasonTargetDir=../

# clean some artifacts
rm -rf ./refmt_main.*

# rebuild the project in case it was stale
make clean -C ../
make -C ../

../node_modules/bs-platform/bin/bspack.exe \
  -bs-main Refmt_impl \
  -prelude-str "$ppxDerivingAndResultStub" \
  -I "$menhirSuggestedLib" \
  -I "$reasonTargetDir" \
  -I "$reasonTargetDir/_build/src" \
  -I "$reasonTargetDir/vendor/cmdliner" \
  -I "$reasonTargetDir/vendor/easy_format" \
  -I "$ocamlMigrateParseTreeTargetDir" \
  -bs-MD \
  -o "./refmt_main.ml"

# finally, test the bundling by compiling the file and verifying everything compiles & dependencies aren't missing
# the `-no-alias-deps` flag is important. Not sure why...
ocamlopt -no-alias-deps -I +compiler-libs ocamlcommon.cmxa "refmt_main.ml" -o "refmt_main.out"
