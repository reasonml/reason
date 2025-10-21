Format issue #1923 - ocamldoc (**/**) comment reformats badly
  $ refmt ./input.ml | tee formatted.re
  /**/**/

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re
  File "./formatted.re", line 1, characters 0-7:
  Error: Comment not terminated
  [1]

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
  diff: formatted_back.re: No such file or directory
  [1]

