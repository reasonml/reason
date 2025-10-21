Format issue #2001 - Trailing comma removed by refmt in record with trailing comment
  $ refmt ./input.re | tee formatted.re
  type t = {
    x: int, /* A value */
    y: int /* Another value */
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

