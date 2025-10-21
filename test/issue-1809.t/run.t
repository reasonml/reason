Format issue #1809 - make(module I1) prints as make((module I1))
  $ refmt ./input.re | tee formatted.re
  make((module I1))

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

