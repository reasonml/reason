Format issue #773 - JS object sugar comment interleaving problem
  $ refmt ./input.re | tee formatted.re
  {
    "myKey":
      1
      /* foo */
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

