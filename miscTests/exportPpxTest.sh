#!/bin/bash

set +e
testPath="miscTests/export_tests"

for name in $testPath/*.ml
do
  base=$(basename "$name")
  base="${base%.ml}"
  echo ">> Checking ${base}"
  ocamlc -dsource -ppx ./ppx_export.native $name -o test.native &> ${name}.actual
  if [[ "$?" = "0" ]]; then
    echo "OK"
  else
    cat ${name}.actual
    rm ${name}.actual
  fi
  rm -rf miscTests/export_tests/${base}.cm*
done

# Expected failures
for name in $testPath/invalid/*.ml
do
  base=$(basename "$name")
  base="${base%.ml}"
  echo ">> Checking ${base}"
  ocamlc -dsource -ppx ./ppx_export.native $name -o test.native 2> ${name}.err > ${name}.actual
  if [[ "$?" = "0" ]]; then
    echo "Expected failure?"
    cat ${name}.err
    cat ${name}.actual
  else
    echo "OK"
  fi
  rm ${name}.err
  rm -rf miscTests/export_tests/invalid/${base}.cm*
done

# ocamlc -dsource -ppx ./ppx_export.native miscTests/export_tests/test.ml -o test.native
# rm -rf miscTests/export_tests/*.ml*
