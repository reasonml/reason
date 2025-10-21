Format issue #1987 - Ternary formatting
  $ refmt ./input.re | tee formatted.re
  booleanCheck ? trueResult : falseResult;
  
  booleanCheck1
    ? trueResult1
    : booleanCheck2
        ? trueResult2
        : booleanCheck3 ? trueResult3 : fallbackResult;

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

