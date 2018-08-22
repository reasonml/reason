# exit if anything goes wrong
set -e

# this script does 2 independent things:
# - pack the whole repo into a single refmt file for vendoring into bucklescript
# - generate the js version of refmt for web usage

THIS_SCRIPT_DIR="$(cd "$( dirname "$0" )" && pwd)"

cd $THIS_SCRIPT_DIR

# Install & build menhir, omp
esy i
esy b

# Because OCaml 4.02 doesn't come with the `Result` module, it also needed stubbing out.
resultStub="module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result"

menhirSuggestedLib=`esy menhir --suggest-menhirLib`

reasonTargetDir="$THIS_SCRIPT_DIR/.."
buildDir="$THIS_SCRIPT_DIR/build"

REFMT_BINARY="$buildDir/refmt_binary"
REFMT_API="$buildDir/refmt_api"
REFMT_API_ENTRY="$buildDir/refmt_api_entry"
REFMT_API_FINAL="$buildDir/refmt_api_final"
REFMT_PRE_CLOSURE="$buildDir/refmt_pre_closure"

REFMT_CLOSURE="$reasonTargetDir/refmt"

ocamlMigrateParseTreeTargetDir=$THIS_SCRIPT_DIR/build/omp

# clean some artifacts
rm -f "$REFMT_CLOSURE.*"
rm -rf $buildDir
mkdir $buildDir

pushd $THIS_SCRIPT_DIR

build_reason_402 () {
  # rebuild the project in case it was stale
  cd ..
  # We need 4.02 for js_of_ocaml (it has a stack overflow otherwise :/)
  sed -i '' 's/"ocaml": "~4.6.0"/"ocaml": "~4.2.3004"/' ./esy.json
  if [ -z "$version" ];then
    export version=$(grep version ./esy.json | sed -E 's/.+([0-9]\.[0-9]\.[0-9]).+/\1/')-$(date +%Y.%m.%d)
  fi
  make pre_release
  esy install
  esy build
  git checkout esy.json esy.lock
  cd -
}

# Get OMP source from esy
get_omp () {
  mkdir $ocamlMigrateParseTreeTargetDir

  ompSource=`esy show-omp-dir`/_build/default/src

  cp $ompSource/*.ml $THIS_SCRIPT_DIR/build/omp
  for i in $(ls build/omp/*.pp.ml); do
  newname=$(basename $i | sed 's/\.pp\././')
  target=${THIS_SCRIPT_DIR}/build/omp/${newname}
  mv $i ${target}
  done;
}

build_bspack () {
  # Build ourselves a bspack.exe if we haven't yet
  if [ ! -f $THIS_SCRIPT_DIR/bspack.exe ]; then
    echo "👉 building bspack.exe"
    cd $THIS_SCRIPT_DIR/bin
    esy ocamlopt -g -w -40-30-3 ./ext_basic_hash_stubs.c unix.cmxa ./bspack.ml -o ../bspack.exe
    cd -
  fi
}

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
  esy ocamlc -g -no-alias-deps -w -40-3 -I +compiler-libs ocamlcommon.cma "$REFMT_API.ml" -o "$REFMT_API.byte"

  # compile refmtJsApi as the final entry file
  esy ocamlfind ocamlc -bin-annot -g -w -30-3-40 -package js_of_ocaml,ocaml-migrate-parsetree -o "$REFMT_API_ENTRY" -I $buildDir -c -impl ./refmtJsApi.ml
  # link them together into the final bytecode
  esy ocamlfind ocamlc -linkpkg -package js_of_ocaml,ocaml-migrate-parsetree -g -o "$REFMT_API_FINAL.byte" "$REFMT_API.cmo" "$REFMT_API_ENTRY.cmo"
  # # use js_of_ocaml to take the compiled bytecode and turn it into a js file
  esy js_of_ocaml --source-map --debug-info --pretty --linkall +weak.js +toplevel.js --opt 3 --disable strict -o "$REFMT_PRE_CLOSURE.js" "$REFMT_API_FINAL.byte"

  # Grab the closure compiler if needed
  CLOSURE_COMPILER_DIR="$THIS_SCRIPT_DIR/closure-compiler"
  if [ ! -d $CLOSURE_COMPILER_DIR ]; then
    mkdir -p $CLOSURE_COMPILER_DIR
    pushd $CLOSURE_COMPILER_DIR
    curl -O http://dl.google.com/closure-compiler/compiler-20170910.tar.gz
    tar -xzf compiler-20170910.tar.gz
    popd
  fi

  # use closure compiler to minify the final file!
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
  esy ocamlc -g -no-alias-deps -w -40-3 -I +compiler-libs ocamlcommon.cma "$REFMT_BINARY.ml" -o "$REFMT_BINARY.byte"
  echo "👉 opt compiling refmt"
  esy ocamlopt -g -no-alias-deps -w -40-3 -I +compiler-libs ocamlcommon.cmxa "$REFMT_BINARY.ml" -o "$REFMT_BINARY.exe"

  # small integration test to check that the process went well
  # for the native binary
  echo "let f = (a) => 1" | "$REFMT_BINARY.byte" --parse re --print ml
  echo "✅ finished building refmt binary"
}

build_reason_402
build_bspack
get_omp
build_refmt
build_js_api
