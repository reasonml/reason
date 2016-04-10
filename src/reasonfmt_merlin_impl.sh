#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

REASON_FMT=""

if [[ -f $DIR/reasonfmt_impl.native ]];
then
    REASON_FMT="$DIR/reasonfmt_impl.native"
fi

if [[ -f $DIR/reasonfmt ]];
then
    REASON_FMT="$DIR/reasonfmt"
fi

if [[ -z $REASON_FMT  ]];
then
    >&2 echo "Couldn't find reasonfmt"
    exit 1
fi

$REASON_FMT $@ -recoverable true -parse re
