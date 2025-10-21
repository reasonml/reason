Format issue #2445 - Invalid code generated from valid OCaml interface code
  $ refmt ./input.ml | tee formatted.re
  [@a [@b] external bar: string = ]
  let foo: int;

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re
  File "./formatted.re", line 1, characters 32-33:
  Error: Syntax error
  [1]

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
  diff: formatted_back.re: No such file or directory
  [1]

