#!/usr/bin/env bash

# share logic between rec and reopt to set up rebuild configs

MY_OCAML_BUILD="-o myocamlbuild"

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
