Format issue #1619 - Unnecessary breaks on multi-line functions with explicit type
  $ refmt ./input.re | tee formatted.re
  let f: int => int =
    x => {
      ();
      x;
    };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

