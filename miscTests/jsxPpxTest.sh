echo "Testing reactjs @JSX ppx..."

testPath="miscTests/reactjs_jsx_ppx_tests"

# used to have more than one tests. Keep the loop for now
for i in 1
do
  test="$testPath/test$i.re"

  expected=`cat $testPath/expected$i.re`

  ocamlc -dsource -ppx ./reactjs_jsx_ppx.native -pp ./refmt_impl.native -impl $test \
    2>&1 | sed '$ d' | sed '$ d' | \
    ./refmt_impl.native -use-stdin true -parse ml -print re -is-interface-pp false \
    > $testPath/actual${i}.re
    # remove the last two lines. It's noise about command failure and changes at
    # every run because the temporary file name in the error message changes

  actual=`cat $testPath/actual$i.re`

  if [[ "$expected" = "$actual" ]]; then
    echo "OK"
  else
    echo "Wrong"
    # show the error
    diff -u $testPath/expected$i.re $testPath/actual$i.re
    exit 1
  fi
done
