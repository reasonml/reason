#!/usr/bin/env bash

export OCAML_VERSION="${1}"

make clean
opam switch "${OCAML_VERSION}"
eval `opam config env`
opam update
opam pin add -y reason .
make test
git diff --exit-code
