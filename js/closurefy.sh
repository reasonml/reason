#!/bin/bash
set -xeo pipefail

THIS_SCRIPT_DIR="$(cd "$( dirname "$0" )" && pwd)"

# download google closure compiler if needed
CLOSURE_COMPILER_DIR="$THIS_SCRIPT_DIR/closure-compiler"
if [ ! -d $CLOSURE_COMPILER_DIR ]; then
  mkdir -p $CLOSURE_COMPILER_DIR
  pushd $CLOSURE_COMPILER_DIR
  curl -O http://dl.google.com/closure-compiler/compiler-20170910.tar.gz
  tar -xzf compiler-20170910.tar.gz
  popd
fi

ESY_TARGET_DIR=`esy @jsoo echo '#{self.target_dir}'`
JSOO_FILE="$ESY_TARGET_DIR/default/js/refmt.bc.js"
OUTPUT="$THIS_SCRIPT_DIR/../refmt"

# # use closure compiler to minify the final file!
java -jar $THIS_SCRIPT_DIR/closure-compiler/closure-compiler-v20170910.jar --create_source_map "$OUTPUT.map" --language_in ECMASCRIPT6 --compilation_level SIMPLE "$JSOO_FILE" > "$OUTPUT.js"
