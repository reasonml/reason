# exit if anything goes wrong
set -e

# this script does 2 independent things:
# - pack the whole repo into a single refmt file for vendoring into bucklescript
# - generate the js version of refmt for web usage

THIS_SCRIPT_DIR="$(cd "$( dirname "$0" )" && pwd)"

command -v dune || { echo "Dune not found. If you're using esy, do 'esy ./reason_bspack.sh'"; exit 1; }

# echo "**This script is switching you to ocaml 4.02.3 for the subsequent bspacking. Please switch back to your own version afterward. Thanks!**\n"
# opam switch 4.02.3
# eval $(opam config env)
# Because OCaml 4.02 doesn't come with the `Result` module, it also needed stubbing out.
resultStub="module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result"

menhirSuggestedLib=`menhir --suggest-menhirLib`

# generated from the script ./downloadSomeDependencies.sh
# ocamlMigrateParseTreeTargetDir="$THIS_SCRIPT_DIR/ocaml-migrate-parsetree/_build/default/src"
ocamlMigrateParseTreeTargetDir=`cat _get_root | bash`/_build/default/src
rm _get_root
cd -

reasonTargetDir="$THIS_SCRIPT_DIR/.."
buildDir="$THIS_SCRIPT_DIR/build"

REFMT_BINARY="$buildDir/refmt_binary"
REFMT_API="$buildDir/refmt_api"
REFMT_API_ENTRY="$buildDir/refmt_api_entry"
REFMT_API_FINAL="$buildDir/refmt_api_final"
REFMT_PRE_CLOSURE="$buildDir/refmt_pre_closure"

REFMT_CLOSURE="$reasonTargetDir/refmt"

# clean some artifacts
rm -f "$REFMT_CLOSURE.*"
rm -rf $buildDir
mkdir $buildDir

pushd $THIS_SCRIPT_DIR
# rebuild the project in case it was stale
make clean -C ../
make pre_release -C ../
make build -C ../

# Get OMP source from esy
mkdir $THIS_SCRIPT_DIR/build/omp
cp $ocamlMigrateParseTreeTargetDir/*.ml $THIS_SCRIPT_DIR/build/omp
for i in $(ls build/omp/*.pp.ml); do
newname=$(basename $i | sed 's/\.pp\././')
target=${THIS_SCRIPT_DIR}/build/omp/${newname}
mv $i ${target}
done;
ocamlMigrateParseTreeTargetDir=$THIS_SCRIPT_DIR/build/omp

# Build ourselves a bspack.exe
if [ ! -f $THIS_SCRIPT_DIR/bspack.exe ]; then
  echo "👉 building bspack.exe"
  cd $THIS_SCRIPT_DIR/bin
  ocamlopt -g -w -40-30-3 ./ext_basic_hash_stubs.c unix.cmxa ./bspack.ml -o ../bspack.exe
  cd -
fi

build_js_api () {
  echo "👉 bspacking the js api"
  set -x
  # =============
  # Now, second task. Packing the repo again but with a new entry file, for JS
  # consumption
  # =============

  # this one is left here as an intermediate file for the subsequent steps. We
  # disregard the usual entry point that is refmt_impl above (which takes care of
  # terminal flags parsing, etc.) and swap it with a new entry point, refmtJsApi (see below)
  ./bspack.exe \
    -bs-main Reason_toolchain \
    -prelude-str "$resultStub" \
    -I "$menhirSuggestedLib" \
    -I "$reasonTargetDir/_build/default/src/ppx/"                               \
    -I "$reasonTargetDir/_build/default/src/reason-merlin/"                     \
    -I "$reasonTargetDir/_build/default/src/reason-parser/"                     \
    -I "$reasonTargetDir/_build/default/src/reason-parser/vendor/easy_format/"  \
    -I "$reasonTargetDir/_build/default/src/reason-parser/vendor/cmdliner/"     \
    -I "$reasonTargetDir/_build/default/src/reason-parser-tests/"               \
    -I "$reasonTargetDir/_build/default/src/refmt/"                             \
    -I "$reasonTargetDir/_build/default/src/refmttype/"                         \
    -I "$ocamlMigrateParseTreeTargetDir" \
    -bs-MD \
    -o "$REFMT_API.ml"

  # the `-no-alias-deps` flag is important. Not sure why...
  # remove warning 40 caused by ocaml-migrate-parsetree
  ocamlc -g -no-alias-deps -w -40-3 -I +compiler-libs ocamlcommon.cma "$REFMT_API.ml" -o "$REFMT_API.byte"

  cd jsoo
  # compile refmtJsApi as the final entry file
  esy ocamlfind ocamlc -bin-annot -g -w -30-3-40 -package js_of_ocaml,ocaml-migrate-parsetree -o "$REFMT_API_ENTRY" -I $buildDir -c -impl ../refmtJsApi.ml
  # link them together into the final bytecode
  esy ocamlfind ocamlc -linkpkg -package js_of_ocaml,ocaml-migrate-parsetree -g -o "$REFMT_API_FINAL.byte" "$REFMT_API.cmo" "$REFMT_API_ENTRY.cmo"
  # # use js_of_ocaml to take the compiled bytecode and turn it into a js file
  esy js_of_ocaml --source-map --debug-info --pretty --linkall +weak.js +toplevel.js --opt 3 --disable strict -o "$REFMT_PRE_CLOSURE.js" "$REFMT_API_FINAL.byte"
  cd ..

  # # use closure compiler to minify the final file!
  java -jar ./closure-compiler/closure-compiler-v20170910.jar --create_source_map "$REFMT_CLOSURE.map" --language_in ECMASCRIPT6 --compilation_level SIMPLE "$REFMT_PRE_CLOSURE.js" > "$REFMT_CLOSURE.js"

  # for the js bundle
  node ./testRefmtJs.js
  echo
  echo "✅ finished building refmt js api"
}

build_refmt () {
  echo "👉 bspacking refmt"

  # =============
  # last step for the first task , we're done generating the single file that'll
  # be coped over to bucklescript. On BS' side, it'll use a single compile command
  # to turn this into a binary, like in
  # https://github.com/BuckleScript/bucklescript/blob/2ad2310f18567aa13030cdf32adb007d297ee717/jscomp/bin/Makefile#L29
  # =============
  ./bspack.exe \
    -main-export Refmt_impl \
    -prelude-str "$resultStub" \
    -I "$menhirSuggestedLib" \
    -I "$reasonTargetDir" \
    -I "$reasonTargetDir/_build/default/src/ppx/"                               \
    -I "$reasonTargetDir/_build/default/src/reason-merlin/"                     \
    -I "$reasonTargetDir/_build/default/src/reason-parser/"                     \
    -I "$reasonTargetDir/_build/default/src/reason-parser/vendor/easy_format/"  \
    -I "$reasonTargetDir/_build/default/src/reason-parser/vendor/cmdliner/"     \
    -I "$reasonTargetDir/_build/default/src/reason-parser-tests/"               \
    -I "$reasonTargetDir/_build/default/src/refmt/"                             \
    -I "$reasonTargetDir/_build/default/src/refmttype/"                         \
    -I "$ocamlMigrateParseTreeTargetDir" \
    -bs-MD \
    -o "$REFMT_BINARY.ml"

  echo "👉 compiling refmt"
  # build REFMT_BINARY into an actual binary too. For testing purposes at the end
  ocamlc -g -no-alias-deps -w -40-3 -I +compiler-libs ocamlcommon.cma "$REFMT_BINARY.ml" -o "$REFMT_BINARY.byte"
  echo "👉 opt compiling refmt"
  ocamlopt -g -no-alias-deps -w -40-3 -I +compiler-libs ocamlcommon.cmxa "$REFMT_BINARY.ml" -o "$REFMT_BINARY.exe"

  # small integration test to check that the process went well
  # for the native binary
  echo "let f = (a) => 1" | "$REFMT_BINARY.byte" --parse re --print ml
  echo "✅ finished building refmt binary"
}

# build_refmt
build_js_api
