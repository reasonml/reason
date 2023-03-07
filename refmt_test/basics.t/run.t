Type-check basics
  $ ocamlc -c -pp "./../../src/refmt/refmt_impl.exe --print binary" -intf-suffix .rei -impl input.re

  $ ocamlopt -c -pp "./../../src/refmt/refmt_impl.exe --print binary" -intf-suffix .rei -impl input.re
