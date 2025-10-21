Format issue #1712 - Unnecessary parens wrapping for switch case followed by lambda
  $ refmt ./input.re | tee formatted.re
  switch (foo) {
  | Bar => (a => 1)
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

