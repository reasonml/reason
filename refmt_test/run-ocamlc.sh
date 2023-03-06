#!/usr/bin/env bash

set -eu

unameOut="$(uname -s)"

case "${unameOut}" in
    MINGW*)     REFMT_PATH=$(cygpath --mixed --absolute $(which refmt.exe));;
    *)          REFMT_PATH="../src/refmt/refmt_impl.exe"
esac

ocamlc -c -pp "$REFMT_PATH --print binary" "$@"
