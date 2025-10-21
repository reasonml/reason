Format issue #2318 - Comment above line influences line width calculation
  $ refmt ./input.re | tee formatted.re
  /* do nothing here as target path is going to be checked at some point
   */
  return((
    prevpath,
    prevmtime,
  ));

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

