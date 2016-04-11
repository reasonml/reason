#!/usr/bin/env bash
# Copyright (c) 2016-present, Facebook, Inc. All rights reserved.

# rebuild is a reasonbuild wrapper that builds reason files
# it calls into ocamlbuild, telling it to call a special
# command 'reopt' which links custom reasson build
# rules.


DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

REOPT=""

if [[ -f $DIR/reopt.sh ]];
then
    REOPT="$DIR/reopt.sh"
fi

if [[ -f $DIR/reopt ]];
then
    REOPT="$DIR/reopt"
fi

if [ -z "$REOPT" ];
then
    echo "Couldn't find reopt"
    exit 1
fi

# Since we need to override -ocamlopt, we parse the user passed-in -ocamlopt
# here and rebuild it in reopt

OCAMLOPTIDX=-1

# find ocamlopt in argument list
i=1
for var in "$@"
do
    if [[ $var = "-ocamlopt" ]];
    then
        OCAMLOPTIDX=$i
    fi
    i=$i+1
done

# found ocamlopt, parsing
OCAMLOPT="ocamlopt.opt"
if [[ $OCAMLOPTIDX -ne -1 ]];
then
    # The argument after "-ocamlopt" will be parsed into reopt as ocamlopt to be used
    VALUEIDX=$((OCAMLOPTIDX+1))
    OCAMLOPT=${!VALUEIDX}
    # Remove the parsed argument out of argument list
    set -- "${@:1:OCAMLOPTIDX-1}" "${@: VALUEIDX+1}"
fi

# pass OCAMLOPT as an environment variable
reasonbuild -ocamlopt "env OCAMLOPT=\"$OCAMLOPT\" $REOPT" "$@"
