Format basic
  $ refmt --print re ./input.re > ./formatted.re
  refmt: FILENAMES… arguments: no './input.re' file
  Usage: refmt [OPTION]… [FILENAMES]…
  Try 'refmt --help' for more information.
  [1]

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
  0a1
  > 
  [1]
