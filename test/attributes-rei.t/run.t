Format basic
  $ refmt --print re ./input.rei > ./formatted.rei

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf formatted.rei

Format the formatted file back
  $ refmt --print re ./formatted.rei > ./formatted_back.rei

Ensure idempotency: first format and second format are the same
  $ diff formatted.rei formatted_back.rei
