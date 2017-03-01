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
./bspack_source/bspack.exe -I `menhir --suggest-menhirLib` -bs-main Refmt_impl -I ../_build/src -I ../vendor/cmdliner -I ../vendor/easy_format/ -I ../vendor/ppx_deriving/ -o refmt_main.ml

echo "* Packing reactjs_ppx"
./bspack_source/bspack.exe -I `menhir --suggest-menhirLib` -bs-main Reactjs_jsx_ppx -I ../_build/src -I ../vendor/cmdliner -I ../vendor/easy_format/ -I ../vendor/ppx_deriving/ -o reactjs_ppx.ml
