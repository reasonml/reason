#!/bin/bash

# exit if anything goes wrong
set -e

# this script does 2 independent things:
# - pack the whole repo into a single refmt file for vendoring into bucklescript
# - generate the js version of refmt for web usage

THIS_SCRIPT_DIR="$(cd "$( dirname "$0" )" && pwd)"

cd $THIS_SCRIPT_DIR

reasonRootDir="$THIS_SCRIPT_DIR/.."

pushd $THIS_SCRIPT_DIR

setup_env () {
  export buildDir="$THIS_SCRIPT_DIR/build/$SANDBOX"
  # for native
  export REFMT_BINARY="$buildDir/refmt_binary"
  export REFMT_API="$buildDir/refmt_api"
  # for js
  export REFMT_CLOSURE="$buildDir/refmt"
  export REFMT_API_ENTRY="$buildDir/refmt_api_entry"
  export REFMT_API_FINAL="$buildDir/refmt_api_final"
  export REFMT_PRE_CLOSURE="$buildDir/refmt_pre_closure"

  export ppxDeriversTargetDir="$buildDir/ppx_derivers"
  export ocamlMigrateParseTreeTargetDir="$buildDir/omp"
  export outputDir="$THIS_SCRIPT_DIR/output/$SANDBOX"
  export ESY="esy @$SANDBOX"

  # clean some artifacts
  rm -rf $buildDir
  mkdir -p $buildDir

  rm -rf $outputDir
  mkdir -p $outputDir

  # Install & build all bspacks deps: jsoo, menhir, omp, ppx_derivers
  $ESY

  # Because OCaml 4.02 doesn't come with the `Result` module, it also needed stubbing out.
  export resultStub="module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result"

  export menhirSuggestedLib=`$ESY menhir --suggest-menhirLib`
}

build_reason () {
  cd ..
  if [ -z "$version" ];then
    export version=$(grep version -m i ./esy.json | sed -E 's/.+([0-9]\.[0-9]\.[0-9]).+/\1/')-$(date +%Y.%m.%d)
  fi
  make pre_release
  $ESY
  reasonEsyTargetDir=`$ESY echo '#{self.target_dir}'`
  cd -
}

# Get ppx_derivers source from esy
get_ppx_derivers () {
  mkdir -p $ppxDeriversTargetDir

  ppxDeriversSource=`$ESY show-ppx_derivers-dir`/_build/default/src

  cp $ppxDeriversSource/*.ml $ppxDeriversTargetDir
  cp $ppxDeriversSource/*.mli $ppxDeriversTargetDir
}

# Get OMP source from esy
get_omp () {
  mkdir -p $ocamlMigrateParseTreeTargetDir

  ompSource=`$ESY show-omp-dir`/_build/default/src

  cp $ompSource/*.ml $ocamlMigrateParseTreeTargetDir
  for i in $(ls $ocamlMigrateParseTreeTargetDir/*.pp.ml); do
  newname=$(basename $i | sed 's/\.pp\././')
  target=$ocamlMigrateParseTreeTargetDir/${newname}
  mv $i ${target}
  done;
}

build_bspack () {
  cd $THIS_SCRIPT_DIR/bin

  bspack_src="bspack.ml"
  bspack_url="https://raw.githubusercontent.com/BuckleScript/bucklescript/975bb776c2bda1e870522718b50463c8d4f58576/lib/4.02.3/unstable/bspack.ml"

  # Get bspack source first, if not there yet
  # (TODO: remove once bspack package is in opam)
  if ! [ -f "$bspack_src" ]; then
    echo "👉 downloading bspack from BuckleScript repo"
    curl -o "$bspack_src" "$bspack_url"
  fi

  # Build ourselves a bspack.exe if we haven't yet
  if [ ! -f $THIS_SCRIPT_DIR/bspack.exe ]; then
    echo "👉 building bspack.exe"
    $ESY ocamlopt -g -w -40-30-3 ./ext_basic_hash_stubs.c unix.cmxa "$bspack_src" -o ../bspack.exe
  fi

  cd -
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
    -I "$reasonEsyTargetDir" \
    -I "$reasonEsyTargetDir/default/src/ppx/"                               \
    -I "$reasonEsyTargetDir/default/src/reason-merlin/"                     \
    -I "$reasonEsyTargetDir/default/src/reason-parser/"                     \
    -I "$reasonEsyTargetDir/default/src/reason-parser/vendor/easy_format/"  \
    -I "$reasonEsyTargetDir/default/src/reason-parser/vendor/cmdliner/"     \
    -I "$reasonEsyTargetDir/default/src/refmt/"                             \
    -I "$reasonEsyTargetDir/default/src/refmttype/"                         \
    -I "$ppxDeriversTargetDir" \
    -I "$ocamlMigrateParseTreeTargetDir" \
    -bs-MD \
    -o "$REFMT_BINARY.ml"

	# This hack is required since the emitted code by bspack somehow adds 	
	sed -i'.bak' -e 's/Migrate_parsetree__Ast_404/Migrate_parsetree.Ast_404/' $REFMT_BINARY.ml
  cp $REFMT_BINARY.ml $outputDir
  
  echo "👉 compiling refmt"
  # build REFMT_BINARY into an actual binary too. For testing purposes at the end
  $ESY ocamlc -g -no-alias-deps -w -40-3 -I +compiler-libs ocamlcommon.cma "$REFMT_BINARY.ml" -o "$REFMT_BINARY.byte"
  cp $REFMT_BINARY.byte $outputDir

  echo "👉 opt compiling refmt"
  $ESY ocamlopt -g -no-alias-deps -w -40-3 -I +compiler-libs ocamlcommon.cmxa "$REFMT_BINARY.ml" -o "$REFMT_BINARY.exe"
  cp $REFMT_BINARY.exe $outputDir

  # small integration test to check that the process went well
  # for the native binary
  echo "let f = (a) => 1" | "$REFMT_BINARY.byte" --parse re --print ml
  echo "✅ finished building refmt binary"
}

run() {
  setup_env
  build_reason
  build_bspack
  get_ppx_derivers
  get_omp
  build_refmt
}

SANDBOX="4023"
run
SANDBOX="4061"
run
