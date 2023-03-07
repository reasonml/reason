Type-check basics
  $ $REFMT --version
  Reason 3.7.0 @ b66ed1b

  $ ocamlc -c -pp "$REFMT --print binary" -intf-suffix .rei -impl input.re
