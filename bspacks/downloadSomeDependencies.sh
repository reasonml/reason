THIS_SCRIPT_DIR="$(cd "$( dirname "$0" )" && pwd)"

# =============
# download google closure compiler
# =============
CLOSURE_COMPILER_DIR="$THIS_SCRIPT_DIR/closure-compiler"

mkdir -p $CLOSURE_COMPILER_DIR
pushd $CLOSURE_COMPILER_DIR
curl -O http://dl.google.com/closure-compiler/compiler-20170910.tar.gz
tar -xzf compiler-20170910.tar.gz

popd
