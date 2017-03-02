#!/bin/sh

# Prerequisite: have `menhir` and `ocamlopt` available.

echo "* Building bspack itself"
cd $(dirname "$0")/bspack_source
# this is copied over from https://github.com/bloomberg/bucklescript/blob/6c64a8f675c7a30767b78b07bfbc10b31fd7a4a8/jscomp/bin/Makefile#L41
ocamlopt.opt -g -w -40-30 ext_basic_hash_stubs.c unix.cmxa config_bspack.mli config_bspack.ml bspack.mli bspack.ml -o bspack.exe

cd ../

# call the makefile of the root directory 
echo "* Build Reason itself"
make -C .. || true

echo "* Cleaning before packing"
rm -f refmt_main.ml
rm -f reactjs_ppx.ml

echo "* Packing refmt"
./bspack_source/bspack.exe -I `menhir --suggest-menhirLib` -bs-main Refmt_impl -I ../_build/src -I ../_build -I ../vendor/cmdliner -I ../vendor/easy_format -I ../vendor/ppx_deriving -I ../node_modules/result-actual -o refmt_main.ml

echo "* Packing reactjs_ppx"
./bspack_source/bspack.exe -I `menhir --suggest-menhirLib` -bs-main Reactjs_jsx_ppx -I ../_build/src -I ../vendor/cmdliner -I ../vendor/easy_format/ -I ../vendor/ppx_deriving/ -o reactjs_ppx.ml


# to compile into binaries: https://github.com/bloomberg/bucklescript/blob/b515a95c5a5740d59cf7eaa9c4fd46863598197f/jscomp/bin/Makefile#L29-L33

# ocamlopt.opt -I +compiler-libs ocamlcommon.cmxa refmt_main.mli refmt_main.ml -o refmt.exe
# ocamlopt.opt -I +compiler-libs ocamlcommon.cmxa reactjs_ppx.mli reactjs_ppx.ml -o reactjs_jsx_ppx.exe

# you only need to compile them to test that the bundling worked. Otherwise, just copy paste the *.ml files to BuckleScript and submit a PR. Their makefile will take care of compiling to binaries.
