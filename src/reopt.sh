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

if [ -z "$OCAMLOPT" ];
then
    OCAMLOPT="ocamlopt.opt"
fi


# Expand special subtitions like '~'
eval REASON_BUILD_DIR=$REASON_BUILD_DIR

if [[ "${@: -2}" = "${MY_OCAML_BUILD}" ]];
then
    #
    # Remove "unix.cmxa" as myocamlbuild is already linked
    # with the library
    #
    # See https://github.com/facebook/Reason/issues/133
    #
    UNIXIDX=-1
    i=1
    for var in "$@"
    do
        if [[ $var =~ "unix.cmxa" ]];
        then
            #
            # If there is already an unix.cmxa linked, remove the second one
            #
            if [[ $UNIXIDX -ne -1 ]];
            then
                set -- "${@:1:i-1}" "${@: i+1}"
            fi
            UNIXIDX=$i
        fi
        i=$i+1
    done



    # Link reason build rules
    set -- "${@:1:$#-3}" "$REASON_BUILD_DIR/reasonbuild.cmx" "${@: -3}"
fi
echo $OCAMLOPT $@

# use OCAMLOPT that's passed in by rebuild
$OCAMLOPT "$@"
