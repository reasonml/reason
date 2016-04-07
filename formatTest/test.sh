#!/bin/bash
# Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

WARNING='\033[0;31m'
INFO='\033[0;32m'
RESET='\033[0m'

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

REASONFMT="$DIR/../reasonfmt_impl.native"

TEST_DIR=$(mktemp -d -t reason_test)

if [[ -f REASONFMT ]];
then
    echo "Cannot find reasonfmt at $REASONFMT" 1>&2
    exit 1;
fi

UNIT_TEST_INPUT=$DIR/unit_tests/input

UNIT_TEST_OUTPUT=$TEST_DIR/unit_tests/output

UNIT_TEST_EXPECTED_OUTPUT=$DIR/unit_tests/expected_output


TYPE_TEST_INPUT=$DIR/typeCheckedTests

TYPE_TEST_OUTPUT=$TEST_DIR/typeCheckedTests/output

function info() {
    printf "${INFO}$1${RESET}\n"
}

function output() {
    printf "$1\n"
}

function warning() {
    printf "${WARNING}$1${RESET}\n"
}

function setup_test_dir() {
    echo "Setting up test dir at $UNIT_TEST_EXPECTED_OUTPUT"
    mkdir -p $UNIT_TEST_OUTPUT
    mkdir -p $TYPE_TEST_OUTPUT
}

setup_test_dir

function unit_test() {
    FILE=$1
    INPUT=$2
    OUTPUT=$3
    EXPECTED_OUTPUT=$4

    info "=============="
    echo "Unit testing $FILE"
    info "Generating output:"
    echo " '$REASONFMT -parse re -print re $INPUT/$FILE 2>&1 > $OUTPUT/$FILE'"
    $REASONFMT -parse re -print re $INPUT/$FILE 2>&1 > $OUTPUT/$FILE

    info "Comparing results:"
    echo " diff $OUTPUT/$FILE $EXPECTED_OUTPUT/$FILE"

    diff --unchanged-line-format="" --new-line-format=":%dn: %L" --old-line-format=":%dn: %L" $OUTPUT/$FILE $EXPECTED_OUTPUT/$FILE

    if ! [[ $? -eq 0 ]]; then
        warning "TEST FAILED\n"
        info "${INFO}$OUTPUT/$FILE${RESET}\n"
        echo "doesn't match expected output"
        info "${INFO}$EXPECTED_OUTPUT/$FILE${RESET}"
        exit 1
    fi

    info "PASS"
    info "=============="
    echo
}

function idempotent_test() {
    FILE=$1
    INPUT=$2
    OUTPUT=$3

    info "=============="
    echo "Property testing $FILE"
    info "Generating output:"
    echo " '$REASONFMT -parse re -print re $INPUT/$FILE 2>&1 > $OUTPUT/$FILE'"
    $REASONFMT -parse re -print re $INPUT/$FILE 2>&1 > $OUTPUT/$FILE

    info "Generating output again:"
    echo "$REASONFMT -parse re -print re $OUTPUT/$FILE 2>&1 > $OUTPUT/$FILE.formatted"
    $REASONFMT -parse re -print re $OUTPUT/$FILE 2>&1 > $OUTPUT/$FILE.formatted

    diff --unchanged-line-format="" --new-line-format=":%dn: %L" --old-line-format=":%dn: %L" $OUTPUT/$FILE $OUTPUT/$FILE.formatted
    if ! [[ $? -eq 0 ]]; then
        warning "TEST FAILED\n"
        info "${INFO}$OUTPUT/$FILE${RESET}\n"
        echo "is not same as"
        info "${INFO}$EXPECTED_OUTPUT/$FILE${RESET}"
        exit 1
    fi

    info "PASS"
    info "=============="
    echo
}

function typecheck_test() {
    FILE=$1
    INPUT=$2
    OUTPUT=$3

    info "=============="
    echo "Typecheck testing $FILE"
    if [ "$(basename $FILE)" != "$(basename $FILE .ml)" ]; then
        REFILE="$(basename $FILE .ml).re"
        info "Converting $FILE to $REFILE:"
        echo "$REASONFMT -heuristics-file $INPUT/arity.txt -parse ml -print re $INPUT/$FILE 2>&1 > $OUTPUT/$REFILE"
        $REASONFMT -heuristics-file $INPUT/arity.txt -parse ml -print re $INPUT/$FILE 2>&1 > $OUTPUT/$REFILE
        if ! [[ $? -eq 0 ]]; then
            warning "TEST FAILED\n"
            exit 1
        fi
        FILE=$REFILE
    else
        info "Formatting:"
        echo "$REASONFMT -parse re -print re $INPUT/$FILE 2>&1 > $OUTPUT/$REFILE"
        $REASONFMT -parse re -print re $INPUT/$FILE 2>&1 > $OUTPUT/$FILE
        if ! [[ $? -eq 0 ]]; then
            warning "TEST FAILED\n"
            exit 1
        fi
    fi

    info "Compiling:"
    echo "ocamlc -c -pp $REASONFMT -intf-suffix .rei -impl $OUTPUT/$FILE"
    ocamlc -c -pp $REASONFMT -intf-suffix .rei -impl "$OUTPUT/$FILE"
    if ! [[ $? -eq 0 ]]; then
        warning "TEST FAILED\n"
        exit 1
    fi

    info "PASS"
    info "============="
    echo
}


cd $UNIT_TEST_INPUT && find . -type f -name "*.re" | while read file; do
        unit_test $file $UNIT_TEST_INPUT $UNIT_TEST_OUTPUT $UNIT_TEST_EXPECTED_OUTPUT
        idempotent_test $file $UNIT_TEST_INPUT $UNIT_TEST_OUTPUT $UNIT_TEST_EXPECTED_OUTPUT
done

cd $TYPE_TEST_INPUT && find . -type f \( -name "*.re" -or -name "*.ml" \) | while read file; do
        typecheck_test $file $TYPE_TEST_INPUT $TYPE_TEST_OUTPUT
done


if [ -z "$KEEP" ]; then
    info "Removing up $TEST_DIR (set env keep=1 to keep build directory)"
    rm -rf $TEST_DIR
fi

exit 0
