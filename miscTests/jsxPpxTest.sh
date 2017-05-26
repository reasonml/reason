#set -x
echo "Testing reactjs @JSX ppx..."

testPath="miscTests/reactjs_jsx_ppx_tests"

ppx[1]="./reactjs_jsx_ppx.native"
ppx[2]="./reasonreact_jsx_ppx.native"

# used to have more than one tests. Keep the loop for now
for i in {1..2}
do
  test="$testPath/test.re"

  expected=`cat $testPath/expected$i.re`

  ocamlc -dsource -ppx ${ppx[$i]} -pp "./refmt_impl.native --print binary" -impl $test \
    2>&1 | sed '$ d' | sed '$ d' | \
    ./refmt_impl.native --parse ml --print re --interface false \
    > $testPath/actual${i}.re
    # remove the last two lines. It's noise about command failure and changes at
    # every run because the temporary file name in the error message changes

  actual=`cat $testPath/actual$i.re`

  if [[ "$expected" = "$actual" ]]; then
    echo "OK$i"
  else
    echo "Wrong$1"
    # show the error
    diff -u $testPath/expected$i.re $testPath/actual$i.re
    exit 1
  fi
done
