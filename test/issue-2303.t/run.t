Format issue #2303 - Annotated functions
  $ refmt ./input.re | tee formatted.re
  type t = {x: int => int};
  
  let _ = {
    x:
      [@log]
      (
        x => {
          Js.log(x);
          x;
        }
      ),
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

