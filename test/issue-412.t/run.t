Format issue #412 - Wrap strings using backslash escape
  $ refmt ./input.re | tee formatted.re
  let x = "helllllllloooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo";

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

