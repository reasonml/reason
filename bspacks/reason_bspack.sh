#!/bin/zsh

# Prerequisite: have `menhir` and `ocamlopt` available.

# call the makefile of the root directory
echo "* Build Reason itself"
cd $(dirname "$0")
echo "Now in $(pwd)"
[[ -f ../_build/sanitize.sh ]] && ../_build/sanitize.sh
make -C ..

echo "* Cleaning before packing"
rm -f refmt_main.ml
rm -f reactjs_ppx.ml

echo "copy over migrate-parsetree & postprocessing"
PARSETREE_DIR=./migrate_parsetree_pp_src
rm -rf ${PARSETREE_DIR}
mkdir -p ${PARSETREE_DIR}
pushd ../node_modules/ocaml-migrate-parsetree-actual/ && make && popd
for i in $(find ../node_modules/ocaml-migrate-parsetree-actual/_build/default/src/*.pp.ml*); do
  cp "$i" "${PARSETREE_DIR}/`basename $i`"
  echo cp "$i" "${PARSETREE_DIR}/`basename $i`"
done
for i in $(find ${PARSETREE_DIR}/*.pp.ml); do
  mv "$i" "${PARSETREE_DIR}/`basename $i .pp.ml`.ml"
  echo mv "$i" "${PARSETREE_DIR}/`basename $i .pp.ml`.ml"
done
for i in $(find ${PARSETREE_DIR}/*.pp.mli); do
  mv "$i" "${PARSETREE_DIR}/`basename $i .pp.mli`.mli"
  echo mv "$i" "${PARSETREE_DIR}/`basename $i .pp.mli`.mli"
done

# Result isn't available in 4.02.3. We'll exposed it below, but we'll have to qualify it
sed -i '' "s/Ok/Result.Ok/g" ${PARSETREE_DIR}/migrate_parsetree_ast_io.ml
sed -i '' "s/Error/Result.Error/g" ${PARSETREE_DIR}/migrate_parsetree_ast_io.ml
sed -i '' "s/result/Result.result/g" ${PARSETREE_DIR}/migrate_parsetree_ast_io.mli

echo "copy over ppx_deriving & preprocessing"
DERIVING_DIR=./ppx_deriving_ppx_src
rm -rf ${DERIVING_DIR}
mkdir -p ${DERIVING_DIR}

ocamlfind ppx_tools_versioned/ppx_metaquot_404 ../node_modules/reason-parser-actual/vendor/ppx_deriving/ppx_deriving.ml > ${DERIVING_DIR}/ppx_deriving.ml
ocamlfind ppx_tools_versioned/ppx_metaquot_404 ../node_modules/reason-parser-actual/vendor/ppx_deriving/ppx_deriving_show.ml > ${DERIVING_DIR}/ppx_deriving_show.ml

echo "* Packing refmt"
./node_modules/bs-platform/bin/bspack.exe -bs-main Refmt_impl \
  -I `menhir --suggest-menhirLib` \
  -I ../_build/src \
  -I ../_build \
  -I ../node_modules/reason-parser-actual/_build/src \
  -I ../node_modules/reason-parser-actual/_build \
  -I ../vendor/cmdliner \
  -I ../node_modules/reason-parser-actual/vendor/easy_format \
  -I ../node_modules/result-actual \
  -I ../node_modules/ppx_tools_versioned-actual \
  -I ${PARSETREE_DIR} \
  -I ${DERIVING_DIR} \
  -o refmt_main.ml

echo "* Packing reactjs_ppx"
./node_modules/bs-platform/bin/bspack.exe -bs-main Reactjs_jsx_ppx \
  -I `menhir --suggest-menhirLib` \
  -I ../_build/src \
  -I ../node_modules/reason-parser-actual/_build/src \
  -I ../vendor/cmdliner \
  -I ../node_modules/reason-parser-actual/vendor/easy_format \
  -I ../node_modules/result-actual \
  -I ../node_modules/ppx_tools_versioned-actual \
  -I ${PARSETREE_DIR} \
  -I ${DERIVING_DIR} \
  -o reactjs_ppx.ml

# to compile into binaries: https://github.com/bloomberg/bucklescript/blob/b515a95c5a5740d59cf7eaa9c4fd46863598197f/jscomp/bin/Makefile#L29-L33
# you only need to compile these to test that the bundling worked. Otherwise,
# just copy paste the *.ml files to BuckleScript and submit a PR. Their
# makefiles containing the same compiling instructions will take care of
# compiling to binaries.
ocamlopt.opt -I +compiler-libs ocamlcommon.cmxa refmt_main.mli refmt_main.ml -o refmt.exe
ocamlopt.opt -I +compiler-libs ocamlcommon.cmxa reactjs_ppx.mli reactjs_ppx.ml -o reactjs_jsx_ppx.exe
