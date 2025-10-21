Format issue #1818 - Bad formatting for callback
  $ refmt ./input.re | tee formatted.re
  Fooooooooooooooooooo.baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa(
    baaaz,
    quuuuux => something(here),
  );

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

