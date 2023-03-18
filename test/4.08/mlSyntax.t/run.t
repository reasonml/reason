Format basic
  $ refmt --print re ./input.ml > ./formatted.re

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re
  File "formatted.re", line 71, characters 8-23:
  71 | let _ = Pervasives.(==);
               ^^^^^^^^^^^^^^^
  Alert deprecated: module Stdlib.Pervasives
  Use Stdlib instead.
  
  If you need to stay compatible with OCaml < 4.07, you can use the 
  stdlib-shims library: https://github.com/ocaml/stdlib-shims

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
