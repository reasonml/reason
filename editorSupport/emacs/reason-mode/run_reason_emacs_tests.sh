#!/bin/sh
# Copyright 2014 The Rust Project Developers. See the COPYRIGHT
# file at the top-level directory of this distribution and at
# http://rust-lang.org/COPYRIGHT.
# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
# <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
# option. This file may not be copied, modified, or distributed
# except according to those terms.
#
# This runs the test for emacs rust-mode.
# Either $EMACS must be set, or it must be possible to find emacs via PATH.

if [ -z "$EMACS" ]; then
    EMACS=emacs
fi

$EMACS --batch || {
   echo "You must set EMACS to a program that runs emacs."
   exit 1
}

$( $EMACS -batch > /dev/null 2>&1 ) || {
    echo "Your emacs command ($EMACS) does not run properly."
    exit 2
};

$( $EMACS -batch --eval "(require 'ert)" > /dev/null 2>&1 ) || {
    echo 'You must install the `ert` dependency; see README.md'
    exit 3
};

warnings="$( $EMACS -Q -batch -f batch-byte-compile rust-mode.el 2>&1 | grep -v '^Wrote ' )"
if [ -n "$warnings" ]; then
    echo "Byte-compilation failed:"
    echo "$warnings"
    exit 4
else
    echo "Byte-compilation passed."
fi

$EMACS -batch -l rust-mode.el -l rust-mode-tests.el -f ert-run-tests-batch-and-exit
