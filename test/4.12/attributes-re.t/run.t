Format basic
  $ refmt --print re ./input.re > ./formatted.re

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re
  File "formatted.re", line 503, characters 4-10:
  503 |     concat;
            ^^^^^^
  Warning 10 [non-unit-statement]: this expression should have type unit.

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
