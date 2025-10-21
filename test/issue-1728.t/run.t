Format issue #1728 - Negative numbers print unnecessary parens
  $ refmt ./input.re | tee formatted.re
  ((-10), (-10))

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

