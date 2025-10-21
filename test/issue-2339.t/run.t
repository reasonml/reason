Format issue #2339 - Refmt favors multiple lines for short tuples and lists
  $ refmt ./input.re | tee formatted.re
  switch (value) {
  | Some(VeryVeryVery(Loooooong(PatternMatch(_)))) => [
      123,
      456789,
      0,
    ]
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

