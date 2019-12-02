# Legacy Note:
# This script is used for older 4.02 based bspacking processes.
#
# Use reason_bspack406.sh for bspacking refmt_api and refmt_binary for
# BuckleScript v6 and above (OCaml 4.06 based)!

# exit if anything goes wrong
set -e

# this script does 2 independent things:
# - pack the whole repo into a single refmt file for vendoring into bucklescript
# - generate the js version of refmt for web usage

THIS_SCRIPT_DIR="$(cd "$( dirname "$0" )" && pwd)"

echo "**This script is switching you to ocaml 4.02.3 for the subsequent bspacking. Please switch back to your own version afterward. Thanks!**\n"
opam switch 4.02.3
eval $(opam config env)

# Because OCaml 4.02 doesn't come with the `Result` module, it also needed stubbing out.
resultStub="module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result"

menhirSuggestedLib=`menhir --suggest-menhirLib`

# generated from the script ./downloadSomeDependencies.sh
ocamlMigrateParseTreeTargetDir="$THIS_SCRIPT_DIR/ocaml-migrate-parsetree/_build/default/src"
reasonTargetDir="$THIS_SCRIPT_DIR/.."
buildDir="$THIS_SCRIPT_DIR/build"

REFMT_BINARY="$buildDir/refmt_binary"

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
../node_modules/bs-platform/bin/bspack.exe \
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
  -I "$ocamlMigrateParseTreeTargetDir" \
  -bs-MD \
  -o "$REFMT_BINARY.ml"

# build REFMT_BINARY into an actual binary too. For testing purposes at the end
ocamlc -g -no-alias-deps -w -40 -I +compiler-libs ocamlcommon.cma "$REFMT_BINARY.ml" -o "$REFMT_BINARY.byte"

# small integration test to check that the process went well
# for the native binary
echo "let f = (a) => 1" | "$REFMT_BINARY.byte" --parse re --print ml