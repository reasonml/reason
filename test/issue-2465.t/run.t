Format issue #2465 - refmt: keep new line characters when using infix operators
  $ refmt ./input.re | tee formatted.re
  thing
  |> doFirstThing
  |> doSecondThing
  |> doThirdThing
  |> doFourthThing;
  
  thing
  >>= doFirstThing
  >>= doSecondThing
  >>= doThirdThing
  >>= doFourthThing;

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

