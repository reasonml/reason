#!/usr/bin/env bash
# Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

# rec is a decorator for ocamlc that passes arguments through to ocamlc directly
# If it detects the invocation is to build myocamlbuild, it modifies the commandline arguments
# by adding reasonbuild.cmx

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [[ -f $DIR/share.sh ]];
then
    . "$DIR/share.sh"
fi

if [[ -f $DIR/share ]];
then
    . "$DIR/share"
fi

if [ -z "$OCAMLC" ];
then
    OCAMLC="ocamlc.opt"
fi

# use OCAMLC that's passed in by rebuild
$OCAMLC "$@"
