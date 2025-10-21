Format issue #2492 - Underscore application with infix operator produces invalid syntax
  $ refmt ./input.re | tee formatted.re
  let divBy2 = _ / 2;

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re
  File "./formatted.re", line 1, characters 13-14:
  Error: Syntax error
  [1]

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
  diff: formatted_back.re: No such file or directory
  [1]

