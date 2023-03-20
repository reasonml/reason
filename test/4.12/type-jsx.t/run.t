Format basic
  $ refmt --print re ./input.re > ./formatted.re

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re
  File "formatted.re", line 460, characters 23-26:
  460 |   <Optional1 required=?zzz />;
                               ^^^
  Warning 43 [nonoptional-label]: the label required is not optional.

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
