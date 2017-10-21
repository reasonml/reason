# this script does 2 independent things:
# - pack the whole repo into a single refmt file for vendoring into bucklescript
# - generate the js version of refmt for web usage

echo "**This script is switching you to ocaml 4.02.3 for the subsequent bspacking. Please switch back to your own version afterward. Thanks!**\n"
opam switch 4.02.3
eval $(opam config env)

# Because OCaml 4.02 doesn't come with the `Result` module, it also needed stubbing out.
resultStub="module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result"

menhirSuggestedLib=`menhir --suggest-menhirLib`

# generated from the script ./setupOmp.sh
ocamlMigrateParseTreeTargetDir=./omp/_build/default/src
reasonTargetDir=../
buildDir=./build

REFMT_BINARY="$buildDir/refmt_binary"
REFMT_API="$buildDir/refmt_api"
REFMT_API_ENTRY="$buildDir/refmt_api_entry"
REFMT_API_FINAL="$buildDir/refmt_api_final"
REFMT_CLOSURE="$buildDir/refmt"
REFMT_PRE_CLOSURE="$buildDir/refmt_pre_closure"

# clean some artifacts
rm -rf $buildDir
mkdir $buildDir

# rebuild the project in case it was stale
make clean -C ../
make -C ../

# =============
# last step for the first task , we're done generating the single file that'll
# be coped over to bucklescript. On BS' side, it'll use a single compile command
# to turn this into a binary, like in
# https://github.com/BuckleScript/bucklescript/blob/2ad2310f18567aa13030cdf32adb007d297ee717/jscomp/bin/Makefile#L29
# =============
../node_modules/bs-platform/bin/bspack.exe \
  -main-export Refmt_impl \
  -prelude-str "$resultStub" \
  -I "$menhirSuggestedLib" \
  -I "$reasonTargetDir" \
  -I "$reasonTargetDir/_build/src" \
  -I "$reasonTargetDir/vendor/cmdliner" \
  -I "$reasonTargetDir/vendor/easy_format" \
  -I "$ocamlMigrateParseTreeTargetDir" \
  -bs-MD \
  -o "$REFMT_BINARY.ml"

# =============
# Now, second task. Packing the repo again but with a new entry file, for JS
# consumption
# =============

# this one is left here as an intermediate file for the subsequent steps. We
# disregard the usual entry point that is refmt_impl above (which takes care of
# terminal flags parsing, etc.) and swap it with a new entry point, refmtJsApi (see below)
../node_modules/bs-platform/bin/bspack.exe \
  -bs-main Reason_toolchain \
  -prelude-str "$resultStub" \
  -I "$menhirSuggestedLib" \
  -I "$reasonTargetDir" \
  -I "$reasonTargetDir/_build/src" \
  -I "$reasonTargetDir/vendor/cmdliner" \
  -I "$reasonTargetDir/vendor/easy_format" \
  -I "$ocamlMigrateParseTreeTargetDir" \
  -bs-MD \
  -o "$REFMT_API.ml"

# the `-no-alias-deps` flag is important. Not sure why...
# remove warning 40 caused by ocaml-migrate-parsetree
ocamlc -g -no-alias-deps -w -40 -I +compiler-libs ocamlcommon.cma "$REFMT_API.ml" -o "$REFMT_API.byte"

# compile refmtJsApi as the final entry file
ocamlfind ocamlc -bin-annot -g -w -30 -w -40 -package js_of_ocaml,ocaml-migrate-parsetree -o "$REFMT_API_ENTRY" -I $buildDir -c -impl ./refmtJsApi.ml
# link them together into the final bytecode
ocamlfind ocamlc -linkpkg -package js_of_ocaml,ocaml-migrate-parsetree -g -o "$REFMT_API_FINAL.byte" "$REFMT_API.cmo" "$REFMT_API_ENTRY.cmo"
# # use js_of_ocaml to take the compiled bytecode and turn it into a js file
js_of_ocaml.exe --source-map --no-inline --debug-info --pretty --linkall +weak.js +toplevel.js --opt 3 --disable strict -o "$REFMT_PRE_CLOSURE.js" "$REFMT_API_FINAL.byte"
# # use closure compiler to minify the final file!
java -jar ./closure-compiler/closure-compiler-v20170910.jar --create_source_map "$REFMT_CLOSURE.map" --language_in ECMASCRIPT6 --compilation_level SIMPLE "$REFMT_PRE_CLOSURE.js" > "$REFMT_CLOSURE.js"

# small integration test to check that the process went well
node ./testRefmtJs.js
