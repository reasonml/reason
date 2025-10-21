Format issue #1603 - Comment at start of block causes next line to wrap
  $ refmt ./input.re | tee formatted.re
  let thing = b =>
    if (b) {
      /*
         long comment
         long comment
         long comment
         long comment
         long comment
       */
      String.concat(
        ",",
        ["a", "b"],
      );
    } else {
      "";
    };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

