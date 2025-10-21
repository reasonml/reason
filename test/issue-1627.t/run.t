Format issue #1627 - Incorrect indentation when right side of pipe operator is multi-line
  $ refmt ./input.re | tee formatted.re
  m
  |> StringMap.add("a", 1)
  |> StringMap.fold_right(
       (el, m) => StringMap.add(el, 1, m),
       ["someReallyLongString"],
     )
  |> StringMap.add("h", 1);

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

