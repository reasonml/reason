Format issue #1829 - Conjunctive polymorphic variant type prints and parses weirdly
  $ refmt ./input.ml | tee formatted.re
  type t = [ | `Something(int) &(string)];

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

