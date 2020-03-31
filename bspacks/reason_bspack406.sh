# NOTE:
# --------
# This script is derived from the original `./reason_bspack.sh`
# which is based on the 4.02 based BuckleScript bspack version. The script also
# does way more than this file, such as creating an closure-optimized refmt.js
# file.
#
# What this file is about:
# -----
# For BuckleScript v6 and above (based on 4.06),
# we want to be able to build the `refmt_api` and `refmt_binary` build
# artifacts by leveraging an up to date bspack version.
#
# bspack itself is not vendored in bs-platform anymore, so the user of this
# script has to provide the bspack binary themselves (most likely as a local
# build from the bucklescript repo)
#
# We use the env variable BSPACK_EXE to populate the bspack path.
#
# Example Usage:
# -------
# cd bspacks/
# BSPACK_EXE=~/Projects/bucklescript/jscomp/bin/bspack.exe bash reason_bspack406.sh

# exit if anything goes wrong
set -xeo pipefail

# this script does 2 independent things:
# - pack the whole repo into a single refmt file for vendoring into bucklescript
# - generate the js version of refmt for web usage

THIS_SCRIPT_DIR="$(cd "$( dirname "$0" )" && pwd)"

# Automatically read the version from reason.json, so that dune builds the right Package.ml file (version, git_version, etc.)
export version=$(cat ../reason.json \
  | grep version \
  | head -1 \
  | awk -F: '{ print $2 }' \
  | sed 's/[",]//g')

echo "**This script is switching you to ocaml 4.06.1 for the subsequent bspacking. Please switch back to your own version afterward. Thanks!**\n"
opam switch 4.06.1
eval $(opam config env)


if [ -z ${BSPACK_EXE+x} ]; then
  echo "Missing env variable 'BSPACK_EXE'"
  echo "Example Usage:"
  echo "BSPACK_EXE=~/bucklescript/jscomp/bin/bspack.exe bash reason_bspack406.sh"
  exit 1
fi

echo "Using bspack located at '${BSPACK_EXE}'..."

# Because OCaml 4.06 doesn't come with the `Result` module, it also needed stubbing out.
resultStub="module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result"

menhirSuggestedLib=`menhir --suggest-menhirLib`

# generated from the script ./downloadSomeDependencies.sh
ocamlMigrateParseTreeTargetDir="$THIS_SCRIPT_DIR/ocaml-migrate-parsetree/_build/default"
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

# =============
# last step for the first task , we're done generating the single file that'll
# be coped over to bucklescript. On BS' side, it'll use a single compile command
# to turn this into a binary, like in
# https://github.com/BuckleScript/bucklescript/blob/2ad2310f18567aa13030cdf32adb007d297ee717/jscomp/bin/Makefile#L29
# =============
$BSPACK_EXE \
  -main-export Refmt_impl \
  -prelude-str "$resultStub" \
  -I "$menhirSuggestedLib" \
  -I "$reasonTargetDir" \
  -I "$reasonTargetDir/_build/default/src/ppx/"                               \
  -I "$reasonTargetDir/_build/default/src/reason-merlin/"                     \
  -I "$reasonTargetDir/_build/default/src/reason-parser/"                     \
  -I "$reasonTargetDir/_build/default/src/reason-parser/vendor/easy_format/"  \
  -I "$reasonTargetDir/_build/default/src/reason-parser/vendor/cmdliner/"     \
  -I "$reasonTargetDir/_build/default/src/refmt/"                             \
  -I "$reasonTargetDir/_build/default/src/refmttype/"                         \
  -I "$ocamlMigrateParseTreeTargetDir/src" \
  -I "$ocamlMigrateParseTreeTargetDir/ppx_derivers/src" \
  -bs-MD \
  -o "$REFMT_BINARY.ml"

# =============
# Now, second task. Packing the repo again but with a new entry file, for JS
# consumption
# =============

# this one is left here as an intermediate file for the subsequent steps. We
# disregard the usual entry point that is refmt_impl above (which takes care of
# terminal flags parsing, etc.) and swap it with a new entry point, refmtJsApi (see below)
$BSPACK_EXE \
  -bs-main Reason_toolchain \
  -prelude-str "$resultStub" \
  -prelude "$reasonTargetDir/_build/default/src/refmt/package.ml" \
  -I "$menhirSuggestedLib" \
  -I "$reasonTargetDir/_build/default/src/ppx/"                               \
  -I "$reasonTargetDir/_build/default/src/reason-merlin/"                     \
  -I "$reasonTargetDir/_build/default/src/reason-parser/"                     \
  -I "$reasonTargetDir/_build/default/src/reason-parser/vendor/easy_format/"  \
  -I "$reasonTargetDir/_build/default/src/reason-parser/vendor/cmdliner/"     \
  -I "$reasonTargetDir/_build/default/src/refmt/"                             \
  -I "$reasonTargetDir/_build/default/src/refmttype/"                         \
  -I "$ocamlMigrateParseTreeTargetDir/src" \
  -I "$ocamlMigrateParseTreeTargetDir/ppx_derivers/src" \
  -bs-MD \
  -o "$REFMT_API.ml"


# This hack is required since the emitted code by bspack somehow adds
sed -i'.bak' -e 's/Migrate_parsetree__Ast_408/Migrate_parsetree.Ast_408/' build/*.ml

# the `-no-alias-deps` flag is important. Not sure why...
# remove warning 40 caused by ocaml-migrate-parsetree
ocamlc -g -no-alias-deps -w -40 -I +compiler-libs ocamlcommon.cma "$REFMT_API.ml" -o "$REFMT_API.byte"

# build REFMT_BINARY into an actual binary too. For testing purposes at the end
ocamlc -g -no-alias-deps -w -40 -I +compiler-libs ocamlcommon.cma "$REFMT_BINARY.ml" -o "$REFMT_BINARY.byte"
