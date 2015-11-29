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
../reasonfmt_impl.native -print-width 50 -print re ./syntax.rei 2>&1 >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./syntax.re 2>&1 >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./infix.re 2>&1  >>./formatOutput.re
../reasonfmt_impl.native -print-width 50 -print re ./trailingSpaces.re 2>&1 >>./formatOutput.re

../reasonfmt_impl.native -print-width 50 -print re ./wrappingTest.rei 2>&1 >./formatOutput.rei



# Let's start creating formatting test cases that must type check.
# Errors in parsing/printing often are caught via the type system.
ocamlc -c -pp ../reasonfmt_impl.native -intf-suffix rei -impl ./typeCheckedTests/sequences.re
rm ./typeCheckedTests/sequences.cmi
rm ./typeCheckedTests/sequences.cmo
../reasonfmt_impl.native -print-width 50 -print re ./typeCheckedTests/sequences.re 2>&1 >>./formatOutput.re

ocamlc -c -pp ../reasonfmt_impl.native -intf-suffix rei -impl ./typeCheckedTests/mutation.re
rm ./typeCheckedTests/mutation.cmi
rm ./typeCheckedTests/mutation.cmo
../reasonfmt_impl.native -print-width 50 -print re ./typeCheckedTests/mutation.re 2>&1 >>./formatOutput.re

# Parse the ml syntax, print it to the formatOutput.re.
../reasonfmt_impl.native -print-width 50 -parse ml -print re ./typeCheckedTests/mlSyntax.ml 2>&1 >>./formatOutput.re
# Also generate a separate .re file so we can type check it.
../reasonfmt_impl.native -parse ml -print re ./typeCheckedTests/mlSyntax.ml > ./typeCheckedTests/mlSyntax.re
ocamlc -c -pp ../reasonfmt_impl.native -intf-suffix rei -impl ./typeCheckedTests/mlSyntax.re
rm ./typeCheckedTests/mlSyntax.cmi
rm ./typeCheckedTests/mlSyntax.cmo
# Remove the generated .re version too
rm ./typeCheckedTests/mlSyntax.re
