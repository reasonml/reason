echo "Testing reactjs @JSX ppx..."

testPath="miscTests/reactjs_jsx_ppx_tests"

for i in 1 2 3 4 5
do
  test="$testPath/test$i.re"

  expected=`cat $testPath/expected$i.txt`

  ocamlc -dsource -ppx ./reactjs_jsx_ppx.native -pp ./refmt_impl.native -impl $test \
    2>&1| sed '$ d' | sed '$ d' > $testPath/actual${i}.txt
    # remove the last two lines. It's noise about command failure and changes at
    # every run because the temporary file name in the error message changes

  actual=`cat $testPath/actual$i.txt`

  if [[ "$expected" = "$actual" ]]; then
    echo "OK"
  else
    echo "Wrong"
    # show the error
    diff -u $testPath/expected$i.txt $testPath/actual$i.txt
    exit 1
  fi
done
