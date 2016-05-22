#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

REFMT=""

if [[ -f $DIR/refmt_impl.native ]];
then
    REFMT="$DIR/refmt_impl.native"
fi

if [[ -f $DIR/refmt ]];
then
    REFMT="$DIR/refmt"
fi

if [[ -z $REFMT  ]];
then
    >&2 echo "Couldn't find refmt"
    exit 1
fi

$REFMT $@ -recoverable true -parse re
