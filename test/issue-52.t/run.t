Format issue #52 - Nested OR patterns aren't preserved
  $ refmt ./input.re | tee formatted.re
  let reasonDoubleBarNested =
    fun
    | X
    | Y(_, _, _)
    | Z(_, _)
    | Q => true
    | _ => false;

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

