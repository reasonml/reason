#set -x
echo "Testing reactjs @JSX ppx..."

testPath="miscTests/reactjs_jsx_ppx_tests"

for i in {1..3}
do
  test="$testPath/test$i.re"

  expected=`cat $testPath/expected$i.re`

  ocamlc -dsource -ppx "./reactjs_jsx_ppx.native" -pp "./refmt_impl.native --print binary" -impl $test \
    2>&1 | sed '$ d' | sed '$ d' | \
    ./refmt_impl.native --parse ml --print re --interface false \
    > $testPath/actual${i}.re
  # remove the last two lines. It's noise about command failure and changes at
  # every run because the temporary file name in the error message changes

  actual=`cat $testPath/actual$i.re`

  if [[ "$expected" = "$actual" ]]; then
    echo "V1 behavior $i: ok"
  else
    echo "V1 behavior $i: wrong"
    # show the error
    diff -u $testPath/expected$i.re $testPath/actual$i.re
    exit 1
  fi
done

for i in {1..3}
do
  test="$testPath/test$i.re"

  expected=`cat $testPath/expected${i}_newBehavior.re`

  ocamlc -dsource -ppx "./reactjs_jsx_ppx_2.native" -pp "./refmt_impl.native --print binary" -impl $test \
    2>&1 | sed '$ d' | sed '$ d' | \
    ./refmt_impl.native --parse ml --print re --interface false \
    > $testPath/actual${i}_newBehavior.re
  # remove the last two lines. It's noise about command failure and changes at
  # every run because the temporary file name in the error message changes

  actual=`cat $testPath/actual${i}_newBehavior.re`

  if [[ "$expected" = "$actual" ]]; then
    echo "V2 behavior $i: ok"
  else
    echo "V2 behavior $i: wrong"
    # show the error
    diff -u $testPath/expected${i}_newBehavior.re $testPath/actual${i}_newBehavior.re
    exit 1
  fi
done
