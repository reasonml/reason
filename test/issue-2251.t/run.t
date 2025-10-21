Format issue #2251 - Support annotations inside function definitions
  $ refmt ./input.re | tee formatted.re
  [@genType]
  let renameABunch = ~pad =>
    [@genType.as "xRenamed"]
    ((~x) => [@genType.as "yRenamed"] ((~y) => pad + x + y));

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

