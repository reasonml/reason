Format issue #1626 - Unnecessary breaks and indentation in function with many parameters
  $ refmt ./input.re | tee formatted.re
  let f =
      (
        someVariable1,
        someVariable2,
        someVariable3,
        someVariable4,
        someVariable5,
        someVariable6,
      ) => 1;

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

