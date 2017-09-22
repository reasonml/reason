echo "Testing reactjs @JSX ppx..."

testPath="miscTests/reactjs_jsx_ppx_tests"

tempFile="temp_jsx_test.ml"

for i in {1..3}
do
  test="$testPath/test$i.re"

  expected=`cat $testPath/expected${i}_newBehavior.re`

  # for each test, we're gonna use ocamlc and the ppx to dump the post-ppx ocaml
  # file somewhere

  ocamlc -dsource -ppx "./reactjs_jsx_ppx_2.native" \
    -pp "./refmt_impl.native --print binary" -impl $test \
    2> $tempFile

  # if there's an error, bail early
  if grep -q "Error:" $tempFile; then
    echo "\033[0;31mV2 behavior $i: error in output after the ppx ran. Here's the content:\033[m"
    cat $tempFile
    rm $tempFile
    exit 1
  fi

  # no error
  ./refmt_impl.native --print-width 100 --parse ml --print re $tempFile \
    > $testPath/actual${i}_newBehavior.re

  rm $tempFile

  # uncomment the line below to write override expected with actual (i.e. update the tests)

  # cat $testPath/actual${i}_newBehavior.re > \
  #   $testPath/expected${i}_newBehavior.re && \
  #   echo "\033[0;32mUpdated tests\033[m" && exit 0

  actual=`cat $testPath/actual${i}_newBehavior.re`

  if [[ "$expected" = "$actual" ]]; then
    echo "\033[0;32mV2 behavior $i: ok\033[m"
  else
    echo "\033[0;31mV2 behavior $i: wrong\033[m"
    # show the error
    diff -u $testPath/expected${i}_newBehavior.re $testPath/actual${i}_newBehavior.re
    # icdiff $testPath/expected${i}_newBehavior.re $testPath/actual${i}_newBehavior.re
    exit 1
  fi
done
