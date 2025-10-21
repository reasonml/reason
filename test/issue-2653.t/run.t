Format issue #2653 - refmt removes unremovable parentheses
  $ refmt ./input.re | tee formatted.re
  !:<p />

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

