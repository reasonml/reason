Format issue #1781 - Single comment at end of block prints strangely
  $ refmt ./input.re | tee formatted.re
  for (i in 0 to 1) {
    print_newline();
              /* comment */
  }

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

