# Tests the jsx ppx for ReasonReact. Call `jsxPpxTest.sh update` to update the
# expected results

echo "Testing reactjs @JSX ppx..."

testPath="miscTests/reactjs_jsx_ppx_tests"

tempFile="temp_jsx_test.ml"

# for better visual diffing in the terminal, try https://github.com/jeffkaufman/icdiff
if hash icdiff 2>/dev/null; then
  DIFF="icdiff"
else
  DIFF="diff -u"
fi

for version in v2 v3
do
  for i in {1..3}
  do
    test="$testPath/test$i.re"
    expected="$testPath/expected_${version}_${i}.re"
    actual="$testPath/actual_${version}_${i}.re"

    # for each test, we're gonna use ocamlc and the ppx to dump the post-ppx ocaml
    # file somewhere

    ocamlc -dsource -ppx "./reactjs_jsx_ppx_${version}.native" \
      -pp "./refmt_impl.native --print binary" -impl $test \
      2> $tempFile

    # if there's an Error/Fatal error, bail early
    if grep -q "rror: " $tempFile; then
      echo "\033[0;31mV2 behavior $i: error in output after the ppx ran. Here's the content:\033[m"
      cat $tempFile
      rm $tempFile
      exit 1
    fi
    # no error
    ./refmt_impl.native --print-width 100 --parse ml --print re $tempFile > $actual

    rm $tempFile

    # if this script's called with the argument `update`, update the expected
    # result instead.
    if [[ $1 = "update" ]]; then
      cat $actual > $expected && echo "\033[0;32mUpdated tests\033[m"
    fi

    if [[ "$(cat $expected)" = "$(cat $actual)" ]]; then
      echo "\033[0;32m$version behavior $i: ok\033[m"
    else
      echo "\033[0;31m$version behavior $i: wrong\033[m"
      # show the error diff
      $DIFF $expected $actual
      exit 1
    fi
  done
done
