Format issue #416 - Converting warning-50-clean OCaml code results in misplaced doc comments
  $ refmt ./input.ml | tee formatted.re
  /** doc comment */
  let foo: int;

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

