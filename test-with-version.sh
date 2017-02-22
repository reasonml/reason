#!/usr/bin/env bash

OCAML_VERSION="${1}"

make clean
opam switch "$OCAML_VERSION"
opam update
opam pin add -y reason .
eval `opam config env`
make test
