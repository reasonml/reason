Format basic
  $ refmt --print re ./input.ml > ./formatted.re

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re
  File "formatted.re", line 10, characters 2-5:
  10 |   | _ => ();
         ^^^
  Warning 11 [redundant-case]: this match case is unused.

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
