Format issue #920 - refmt comment reordering
  $ refmt ./input.re | tee formatted.re
  switch ("") {
  | a => a /*
    | 2 => c */ /*
    | 1 => b */
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

