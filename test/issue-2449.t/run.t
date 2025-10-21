Format issue #2449 - unknown error at "fun%expr" within "switch"
  $ refmt ./input.re | tee formatted.re
  File "./input.re", line 4, characters 4-9:
  Error: Unknown error: please file an issue at github.com/facebook/reason
  [1]

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re
  refmt: FILENAMES… arguments: no './formatted.re' file
  Usage: refmt [OPTION]… [FILENAMES]…
  Try 'refmt --help' for more information.
  [1]

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
  diff: formatted.re: No such file or directory
  diff: formatted_back.re: No such file or directory
  [1]

