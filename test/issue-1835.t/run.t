Format issue #1835 - Placement of closing > in JSX
  $ refmt ./input.re | tee formatted.re
  <Component prop1 prop2>
    {"child" |> ReasonReact.stringToElement}
  </Component>

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

