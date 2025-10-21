Format issue #1566 - indentation with ## #= and anonymous function
  $ refmt ./input.re | tee formatted.re
  great##runAllTests
  #= (
       () => {
         test1();
         test2();
       }
     );

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

