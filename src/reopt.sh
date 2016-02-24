#!/usr/bin/env bash
# Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

# reopt is a decorator for ocamlopt that passes arguments through to ocamlopt directly
# If it detects the invocation is to build myocamlbuild, it modifies the commandline arguments
# by adding reasonbuild.cmx

MY_OCAML_BUILD="-o myocamlbuild"

echo "$REASON_BUILD_DIR"
if [ -z "$REASON_BUILD_DIR" ];
then
    REASON_BUILD_DIR=$(ocamlfind query reason)
fi

if [ -z "$REASON_BUILD_DIR" ];
then
    echo "Couldn't find Reason"
    exit 1
fi

# Expand special subtitions like '~'
eval REASON_BUILD_DIR=$REASON_BUILD_DIR

echo "${OCAML_BUILD}"
if [[ "${@: -2}" = "${MY_OCAML_BUILD}" ]];
then
    # Link reason build rules
    set -- "${@:1:$#-3}" "$REASON_BUILD_DIR/reasonbuild.cmx" "${@: -3}"
fi
echo $OCAMLOPT $@

# use OCAMLOPT that's passed in by rebuild
$OCAMLOPT $@
