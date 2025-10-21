Format issue #2334 - unhygienic expansion and unintended normalization in refmt
  $ refmt ./input.re | tee formatted.re
  let f = (x, p) => p(x);
  let g = (a, b) => a + b;
  
  let __x = 42;
  
  Js.log(7->f(g(_, __x)));
  
  7->f(__x => g(__x, foo));

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

