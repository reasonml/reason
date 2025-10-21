Format issue #2285 - sometimes new lines are unnecessary in switch cases
  $ refmt ./input.re | tee formatted.re
  let isVowel = theChar =>
    switch (theChar) {
    | 'a'
    | 'e'
    | 'i'
    | 'o'
    | 'u'
    | 'y' => true
    | _ => false
    };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

