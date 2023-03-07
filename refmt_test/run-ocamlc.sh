#!/usr/bin/env bash

set -eu

unameOut="$(uname -s)"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

case "${unameOut}" in
    MINGW*)     REFMT_PATH=$(cygpath --mixed --absolute $(which refmt.exe));;
    *)          REFMT_PATH="$SCRIPT_DIR/../src/refmt/refmt_impl.exe"
esac

ocamlc -c -pp "$REFMT_PATH --print binary" "$@"
