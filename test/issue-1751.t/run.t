Format issue #1751 - Refmt should remap record field names like it does identifiers
  $ refmt ./input.ml | tee formatted.re
  type t = {pub_: int};

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

