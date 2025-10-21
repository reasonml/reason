Format issue #2424 - Weird formatting of record member access following function call
  $ refmt ./input.re | tee formatted.re
  expect.option(
    Some(
      "a very very very very very very very very very very long line",
    ),
  ).
    toBeSome();

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

