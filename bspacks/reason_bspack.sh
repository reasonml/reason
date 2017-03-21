#!/bin/zsh

# Prerequisite: have `menhir` and `ocamlopt` available.

# call the makefile of the root directory
echo "* Build Reason itself"
cd $(dirname "$0")
[[ -f ../_build/sanitize.sh ]] && ../_build/sanitize.sh
make -C ..

echo "* Cleaning before packing"
rm -f refmt_main.ml
rm -f reactjs_ppx.ml

echo "copy over migrate-parsetree & postprocessing"
rm -rf ./migrate_parsetree_pp_src
mkdir -p ./migrate_parsetree_pp_src
pushd ../node_modules/ocaml-migrate-parsetree-actual/ && make && popd
for i in $(find ../node_modules/ocaml-migrate-parsetree-actual/_build/default/src/*.pp.ml*); do
  cp $i ./migrate_parsetree_pp_src/`basename $i`
done
for i in $(find migrate_parsetree_pp_src/*.pp.ml); do
  mv $i "${i%%.*}".ml
done
for i in $(find migrate_parsetree_pp_src/*.pp.mli); do
  mv $i "${i%%.*}".mli
done

# Result isn't available in 4.02.3. We'll exposed it below, but we'll have to qualify it
sed -i '' "s/Ok/Result.Ok/g" migrate_parsetree_pp_src/migrate_parsetree_ast_io.ml
sed -i '' "s/Error/Result.Error/g" migrate_parsetree_pp_src/migrate_parsetree_ast_io.ml
sed -i '' "s/result/Result.result/g" migrate_parsetree_pp_src/migrate_parsetree_ast_io.mli

echo "copy over ppx_deriving & preprocessing"
rm -rf ./ppx_deriving_ppx_src
mkdir -p ./ppx_deriving_ppx_src
cp -r ../vendor/ppx_deriving/ ./ppx_deriving_ppx_src
OCAMLFIND_IGNORE_DUPS_IN=$(ocamlfind query compiler-libs) ocamlfind ocamlopt -c -g -safe-string -package ppx_tools_versioned.metaquot_404 -package ocaml-migrate-parsetree -dsource ../vendor/ppx_deriving/ppx_deriving.ml 2> ./ppx_deriving_ppx_src/ppx_deriving.ml
OCAMLFIND_IGNORE_DUPS_IN=$(ocamlfind query compiler-libs) ocamlfind ocamlopt -c -g -safe-string -package ppx_tools_versioned.metaquot_404 -package ocaml-migrate-parsetree -I ./ppx_deriving_ppx_src -dsource ../vendor/ppx_deriving/ppx_deriving_show.ml 2> ./ppx_deriving_ppx_src/ppx_deriving_show.ml

echo "* Packing refmt"
./node_modules/bs-platform/bin/bspack.exe \
  -I `menhir --suggest-menhirLib` -bs-main Refmt_impl \
  -I ../_build/src \
  -I ../_build \
  -I ../vendor/cmdliner \
  -I ../vendor/easy_format \
  -I ./ppx_deriving_ppx_src/ \
  -I ../node_modules/ppx_tools_versioned-actual \
  -I ../node_modules/result-actual \
  -I migrate_parsetree_pp_src \
  -o refmt_main.ml

echo "* Packing reactjs_ppx"
./node_modules/bs-platform/bin/bspack.exe \
  -I `menhir --suggest-menhirLib` -bs-main Reactjs_jsx_ppx \
  -I ../_build/src \
  -I ../vendor/cmdliner \
  -I ../vendor/easy_format/ \
  -I ./ppx_deriving_ppx_src/ \
  -I ../node_modules/ppx_tools_versioned-actual \
  -I ../node_modules/result-actual \
  -I migrate_parsetree_pp_src \
  -o reactjs_ppx.ml

# to compile into binaries: https://github.com/bloomberg/bucklescript/blob/b515a95c5a5740d59cf7eaa9c4fd46863598197f/jscomp/bin/Makefile#L29-L33
# you only need to compile these to test that the bundling worked. Otherwise,
# just copy paste the *.ml files to BuckleScript and submit a PR. Their
# makefiles containing the same compiling instructions will take care of
# compiling to binaries.
ocamlopt.opt -I +compiler-libs ocamlcommon.cmxa refmt_main.mli refmt_main.ml -o refmt.exe
ocamlopt.opt -I +compiler-libs ocamlcommon.cmxa reactjs_ppx.mli reactjs_ppx.ml -o reactjs_jsx_ppx.exe
