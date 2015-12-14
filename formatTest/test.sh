# http://sc.tamu.edu/help/general/unix/redirection.html
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./testUtils.re 2>&1 | sed -e 's/ *$//g' >./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./basicStructures.re 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./variants.re 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./polymorphism.re 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./wrappingTest.re 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./modules.re 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -intf ./syntax.rei 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re
# ocamlc -c -pp reasonfmt -w -32-27-26-11 -dsource -I . -impl ./syntax.re 2>&1 | sed -e 's/ *$//g' >>./formatOutput.re


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
../reasonfmt_impl.native -print-width 50 -print re ./escapesInStrings.re 2>&1 >>./formatOutput.re

../reasonfmt_impl.native -print-width 50 -print re ./wrappingTest.rei 2>&1 >./formatOutput.rei
../reasonfmt_impl.native -print-width 50 -print re ./syntax.rei 2>&1 >>./formatOutput.rei

touch ./ocpFormatOutput.re

echo "" > ./ocpFormatOutput.re
for file in ./ocp-indent/*
do
  ../reasonfmt_impl.native -print-width 50 -parse ml -print re "$file" 2>&1 >>./ocpFormatOutput.re
done

for file in ./typeCheckedTests/*.re
do
  ocamlc -c -pp ../reasonfmt_impl.native -intf-suffix rei -impl "$file"
  ../reasonfmt_impl.native -print-width 50 -print re "$file" 2>&1 >>./formatOutput.re
done

../reasonfmt_impl.native -print-width 50 -parse ml -print re ./typeCheckedTests/mlSyntax.ml 2>&1 >>./formatOutput.re
../reasonfmt_impl.native -parse ml -print re ./typeCheckedTests/mlSyntax.ml > ./typeCheckedTests/mlSyntax.re
ocamlc -c -pp ../reasonfmt_impl.native -intf-suffix rei -impl ./typeCheckedTests/mlSyntax.re
# Remove the generated .re version too
rm ./typeCheckedTests/mlSyntax.re

rm ./typeCheckedTests/*.cmi
rm ./typeCheckedTests/*.cmo
