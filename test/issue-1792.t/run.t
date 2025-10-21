Format issue #1792 - Attribute attachment not preserved in refmt
  $ refmt ./input.re | tee formatted.re
  [@attr]
  x + [@attr2] y;
  
  [@attr] x + [@attr2] y;

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

