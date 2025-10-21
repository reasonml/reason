Format issue #2065 - Comments move inside of list literal after longer comment
  $ refmt ./input.re | tee formatted.re
  let _ = [
    /* The should be before the list ........................................ */
    1,
    2,
    3,
  ];

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

