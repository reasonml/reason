#!/bin/bash
# Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
# http://sc.tamu.edu/help/general/unix/redirection.html
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./testUtils.re 2>&1 | sed -e 's/ *$//g' >./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./basicStructures.re 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./variants.re 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./polymorphism.re 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./wrappingTest.re 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./modules.re 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -intf ./syntax.rei 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./syntax.re 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re

function idempotent_test() {
    ../reasonfmt_impl.native -print-width 50 -parse re -print re ./$1 2>&1 >./$1.formatted
    ../reasonfmt_impl.native -print-width 50 -parse re -print re ./$1.formatted 2>&1 >./$1.formatted.formatted

    diff --unchanged-line-format="" --new-line-format=":%dn: %L" --old-line-format=":%dn: %L" ./$1.formatted.formatted ./$1.formatted
    if ! [[ $? -eq 0 ]]; then
        echo "$1.re is not idempotent" 1>&2
    fi
}

../reasonfmt_impl.native -print-width 50 -print re ./testUtils.re 2>&1 >./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./basicStructures.re 2>&1 >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./if.re 2>&1 >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./polymorphism.re 2>&1 >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./modules.re 2>&1 >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./variants.re 2>&1 >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./wrappingTest.re 2>&1 >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./syntax.re 2>&1 >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./infix.re 2>&1  >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./trailingSpaces.re 2>&1 >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./features403.re 2>&1 >>./formatOutput.re

../reasonfmt_impl.native -print-width 50 -print re ./escapesInStrings.re 2>&1 >>./formatOutput.re

idempotent_test ./formatOutput.re

../reasonfmt_impl.native -print-width 50 -print re ./wrappingTest.rei 2>&1 >./formatOutput.rei
../reasonfmt_impl.native -print-width 50 -print re ./syntax.rei 2>&1 >>./formatOutput.rei

touch ./customMLFormatOutput.re

echo "" > ./customMLFormatOutput.re

shopt -s nullglob # prevent the variable 'file' from being set to "./customMLFiles/*.ml"
for file in ./customMLFiles/*.ml
do
  ../reasonfmt_impl.native -print-width 50 -parse ml -print re "$file" 2>&1 >> ./customMLFormatOutput.re
done

idempotent_test ./customMLFormatOutput.re


for file in ./typeCheckedTests/*.re
do
  ocamlc -c -pp ../reasonfmt_impl.native -intf-suffix .rei -impl "$file"
  ../reasonfmt_impl.native -print-width 50 -print re "$file" 2>&1 >>./formatOutput.re
done

# Print them for the record (so we can detect changes caused by diffs)
../reasonfmt_impl.native -print-width 50 -parse ml -print re ./typeCheckedTests/mlSyntax.ml 2>&1 >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -parse ml -print re ./typeCheckedTests/mlVariants.ml 2>&1 >>./formatOutput.re
# Then print them to individual files for compiling.
../reasonfmt_impl.native -parse ml -print re ./typeCheckedTests/mlSyntax.ml > ./typeCheckedTests/mlSyntax.re
ocamlc -c -pp ../reasonfmt_impl.native -intf-suffix .rei -impl ./typeCheckedTests/mlSyntax.re
../reasonfmt_impl.native -parse ml -print re ./typeCheckedTests/mlVariants.ml > ./typeCheckedTests/mlVariants.re
ocamlc -c -pp ../reasonfmt_impl.native -intf-suffix .rei -impl ./typeCheckedTests/mlVariants.re

../reasonfmt_impl.native -heuristics-file ./typeCheckedTests/arity.txt -assume-explicit-arity -parse ml -print re ./typeCheckedTests/arityConversion.ml > ./typeCheckedTests/arityConversion.re
ocamlc -c -pp ../reasonfmt_impl.native -intf-suffix .rei -impl ./typeCheckedTests/arityConversion.re 2>&1 | ../reason_error_reporter.native

# Remove the generated .re version too
rm ./typeCheckedTests/mlSyntax.re
rm ./typeCheckedTests/mlVariants.re

rm ./typeCheckedTests/*.cmi
rm ./typeCheckedTests/*.cmo
rm ./*.formatted
