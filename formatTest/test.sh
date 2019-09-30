#!/usr/bin/env bash
# Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

set -eu

WARNING='\033[0;31m'
SUCCESS='\033[0;32m'
NOTICE='\033[0;33m'
INFO=''
DEBUG=''
RESET='\033[0m'
VERBOSE=${VERBOSE:-}
OCAML_VERSION=`echo $(ocaml -version) | egrep -o '[0-9]+.[0-9]+.[0-9]+' | head -1`
OCAML_VERSION=${OCAML_VERSION:-"4.02.3"}

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

REFMT="$DIR/../_build/install/default/bin/refmt"
OPRINT_TEST_BIN="$DIR/../_build/default/src/reason-parser-tests/testOprint.exe"

if [[ -f REFMT ]];
then
    echo "Cannot find refmt at $REFMT" 1>&2
    exit 1;
fi

# for better visual diffing in the terminal, try https://github.com/jeffkaufman/icdiff
if hash icdiff 2> /dev/null; then
    DIFF="icdiff"
elif hash git 2> /dev/null; then
    DIFF="git --no-pager diff --no-index"
else
    DIFF="eval diff --unchanged-line-format='' --new-line-format=':%dn: %L' --old-line-format=':%dn: %L'"
fi

OPRINT_TEST_INPUT=$DIR/oprintTests/input

OPRINT_TEST_OUTPUT=$DIR/oprintTests/actual_output

OPRINT_TEST_EXPECTED_OUTPUT=$DIR/oprintTests/expected_output

OPRINT_TEST_INTF_OUTPUT=$DIR/oprintTests/intf_output


FAILED_TESTS=$DIR/failed_tests

function info() {
    printf "${INFO}$1${RESET}\n"
}

function debug() {
    if [ ! -z "$VERBOSE" ]; then
        printf "${DEBUG}$1${RESET}\n"
    fi
}

function notice() {
    printf "${NOTICE}$1${RESET}\n"
}

function success() {
    printf "${SUCCESS}$1${RESET}"
}

function output() {
    printf "$1\n"
}

function warning() {
    printf "${WARNING}$1${RESET}\n"
}

function version() {
    echo "$@" | awk -F . '{ printf("%03d%03d%03d\n", $1, $2, $3); }';
}

function setup_test_dir() {
    echo "Setting up test dirs actual_output alongside the tests' expected_output"
    mkdir -p $OPRINT_TEST_OUTPUT $OPRINT_TEST_INTF_OUTPUT
    touch $FAILED_TESTS
}

setup_test_dir

set +e

function stdin_test() {
    INPUT_FILE=$1
    OUTPUT_FILE=$2
    EXPECTED_OUTPUT_FILE=$3
    # explicitly pass in heuristics file because idempotent tests read from output directory
    HEURISTICS_FILE=$4
    if [[ -z "${5-}" ]]; then
      EXTRA_FLAGS='--interface false --parse re'
    else
      EXTRA_FLAGS="$5"
    fi

    FILENAME=$(basename $INPUT_FILE)
    FILEEXT="${FILENAME##*.}"

    if [[ $FILEEXT = "re" ]]; then
      cat $INPUT_FILE | $REFMT $EXTRA_FLAGS --print-width 50 --print re 2>&1 > $OUTPUT_FILE
    elif [[ $FILEEXT = "rei" ]]; then
      cat $INPUT_FILE | $REFMT --interface true --print-width 50 --parse re --print re 2>&1 > $OUTPUT_FILE
    elif [[ $FILEEXT = "ml" ]]; then
      cat $INPUT_FILE | $REFMT --heuristics-file $HEURISTICS_FILE --interface false --print-width 50 --parse ml --print re 2>&1 > $OUTPUT_FILE
    elif [[ $FILEEXT = "mli" ]]; then
      cat $INPUT_FILE | $REFMT --heuristics-file $HEURISTICS_FILE --interface true --print-width 50 --parse ml --print re 2>&1 > $OUTPUT_FILE
    else
      warning "  ⊘ FAILED --use-stdin \n"
      info "  Cannot determine default implementation parser for extension ${FILEEXT}"
      return 1
    fi

    if ! [[ $? -eq 0 ]]; then
        warning "  ⊘ FAILED --use-stdin \n"
        info "  There was an error when testing --use-stdin"
        info "  for input file $INPUT_FILE"
        info "  and output file $OUTPUT_FILE${RESET}"
        echo ""
        return 1
    fi

    debug "  Comparing --use-stdin results:  diff $EXPECTED_OUTPUT_FILE $OUTPUT_FILE"
    $DIFF $EXPECTED_OUTPUT_FILE $OUTPUT_FILE

    if ! [[ $? -eq 0 ]]; then
        warning "  ⊘ FAILED --use-stdin \n"
        info "  ${INFO}$OUTPUT_FILE${RESET}"
        info "  doesn't match expected output"
        info "  ${INFO}$EXPECTED_OUTPUT_FILE${RESET}"
        echo ""
        return 1
    fi
    return 0
}

function oprint_test() {
    FILE=$1
    INPUT=$2
    OUTPUT=$3
    EXPECTED_OUTPUT=$4
    INTF_OUTPUT=$5

    FILENAME=$(basename $FILE)
    FILEEXT="${FILENAME##*.}"


    info "Outcome Printer Test: $FILE"

    debug "  'cat $FILE | $OPRINT_TEST_BIN $INPUT/$FILE 2>&1 > $OUTPUT/$FILE'"
    cat $INPUT/$FILE | $OPRINT_TEST_BIN $INPUT/$FILE 2>&1 > $OUTPUT/$FILE

    debug "  'cp $OUTPUT/$FILE $INTF_OUTPUT/$(basename $FILE .re).rei"
    cp $OUTPUT/$FILE $INTF_OUTPUT/$(basename $FILE .re).rei


    OFILE="${FILE}"
    VERSION_SPECIFIC_FILE="${FILE}.${OCAML_VERSION}"
    if [ -f "${EXPECTED_OUTPUT}/${VERSION_SPECIFIC_FILE}" ]; then
        echo "Found test file specific to version ${OCAML_VERSION}..."
        OFILE="${VERSION_SPECIFIC_FILE}"
    fi

    debug "  Comparing results:  diff $EXPECTED_OUTPUT/$OFILE $OUTPUT/$FILE"

    $DIFF $EXPECTED_OUTPUT/$OFILE $OUTPUT/$FILE

    if ! [[ $? -eq 0 ]]; then
        warning "  ⊘ FAILED\n"
        info "  ${INFO}$OUTPUT/$FILE${RESET}"
        info "  doesn't match expected output"
        info "  ${INFO}$EXPECTED_OUTPUT/$OFILE${RESET}"
        info ""
        info "  To approve the changes run:"
        info "    cp $OUTPUT/$FILE $EXPECTED_OUTPUT/$OFILE"
        echo ""
        return 1
    fi

    if ! [[ $? -eq 0 ]]; then
      return 1
    else
      success "  ☑ PASS"
      echo
    fi
}

cd $OPRINT_TEST_INPUT && find . -type f \( -name "*.re*" -or -name "*.ml*" \) | while read file; do
        oprint_test $file $OPRINT_TEST_INPUT $OPRINT_TEST_OUTPUT $OPRINT_TEST_EXPECTED_OUTPUT $OPRINT_TEST_INTF_OUTPUT
        if ! [[ $? -eq 0 ]]; then
            echo "$file -- failed oprint_test" >> $FAILED_TESTS
        fi

        oprint_test $file $OPRINT_TEST_OUTPUT $OPRINT_TEST_INTF_OUTPUT $OPRINT_TEST_EXPECTED_OUTPUT '-i true --parse re'
        if ! [[ $? -eq 0 ]]; then
            echo "$file -- failed oprint_test" >> $FAILED_TESTS
        fi
done

if [[ -s $FAILED_TESTS ]]; then
  warning "Failed tests:"
  cat $FAILED_TESTS
  exit 1
fi

exit 0
