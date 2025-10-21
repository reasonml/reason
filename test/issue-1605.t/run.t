Format issue #1605 - refmt indent bug with inline first-class modules
  $ refmt ./input.re | tee formatted.re
  let anonymousModule: (module MyModule) =
    (module
     {
       let f1 = a => true;
       let f2 = b => 1;
     });

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

