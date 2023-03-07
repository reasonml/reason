Type-check basics
  $ export REFMT="$(readlink -f ./../../src/refmt/refmt_impl.exe)"

  $ $REFMT --version
  Reason 3.7.0 @ b66ed1b

  $ ocamlc -c -pp "$REFMT --print binary" -intf-suffix .rei -impl input.re

  $ ocamlopt -c -pp "./../../src/refmt/refmt_impl.exe --print binary" -intf-suffix .rei -impl input.re
