#!/usr/bin/env bash

set -e

export OCAML_VERSION="${1}"

make clean-for-ci
opam switch "${OCAML_VERSION}"
eval `opam config env`
opam update
opam install -y jbuilder
# Our constraints are wrong I believe. We need this version.
opam install -y menhir.20170712
opam pin add -y reason .
make test
git diff --exit-code
