Format issue #2029 - Comment interleaving wrong before JSX
  $ refmt ./input.re | tee formatted.re
  <div
    className
    /* foo */
  />;

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

