Format issue #1961 - Refmt doesn't line break for long function arguments + single body item
  $ refmt ./input.re | tee formatted.re
  let f =
      (
        someVariable1,
        someVariable2,
        someVariable3,
        someVariable4,
        someVle5,
      ) => "hiiiiiiiiiiiiiiiiiiiiiiiiiii";

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

