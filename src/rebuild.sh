#!/usr/bin/env bash
# Copyright (c) 2016-present, Facebook, Inc. All rights reserved.

# rebuild is a reasonbuild wrapper that builds reason files
# it calls into ocamlbuild, telling it to call a special
# command 'reopt' and 'rec' which links custom reasson build
# rules.


DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

REOPT=""

REC=""

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

if [[ -f $DIR/rec.sh ]];
then
    REC="$DIR/rec.sh"
fi

if [[ -f $DIR/rec ]];
then
    REC="$DIR/rec"
fi

if [ -z "$REC" ];
then
    echo "Couldn't find rec"
    exit 1
fi

# Since we need to override -ocamlopt and -ocaml, we parse the user passed-in
#  -ocamlopt and -ocamlc here and rebuild it in reopt and rec

OCAMLOPTIDX=-1
OCAMLCIDX=-1
USEOCAMLFIND=-1

# find ocamlopt in argument list
i=1
for var in "$@"
do
    if [[ $var = "-ocamlopt" ]];
    then
        OCAMLOPTIDX=$i
    fi

    if [[ $var = "-ocamlc" ]];
    then
        OCAMLCIDX=$i
    fi

    if [[ $var = "-use-ocamlfind" ]];
    then
        USEOCAMLFIND=1
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

# found ocamlc, parsing
OCAMLC="ocamlc.opt"
if [[ $OCAMLCIDX -ne -1 ]];
then
    # The argument after "-ocamlc" will be parsed into rec as ocamlc to be used
    VALUEIDX=$((OCAMLCIDX+1))
    OCAMLC=${!VALUEIDX}
    # Remove the parsed argument out of argument list
    set -- "${@:1:OCAMLCIDX-1}" "${@: VALUEIDX+1}"
fi

if [[ $USEOCAMLFIND -ne -1 ]];
then
    env OCAMLFIND_COMMANDS="ocamlopt=$REOPT ocamlc=$REC" reasonbuild "$@"
else
   # pass OCAMLOPT as an environment variable
   reasonbuild -ocamlopt "env OCAMLOPT=\"$OCAMLOPT\" $REOPT" -ocamlc "env OCAMLC=\"$OCAMLC\" $REC" "$@"
fi
