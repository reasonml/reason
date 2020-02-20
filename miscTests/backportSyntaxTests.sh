#!/bin/bash
set -e

echo "Testing the backport of 'letop'"
testPath="./miscTests/backport_syntax_tests"
input=$testPath/basic.re
output=$testPath/basic.bin
binary=$testPath/bin

./_build/install/default/bin/refmt $input --print binary > $output
ocamlc -impl $output -o $binary
$binary
rm $output $binary $testPath/basic.cm*
