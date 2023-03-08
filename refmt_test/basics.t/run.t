Type-check basics
  $ refmt_impl --version
  Reason 3.7.0 @ b66ed1b

  $ ocamlc -c -pp "refmt_impl --print binary" -intf-suffix .rei -impl input.re
