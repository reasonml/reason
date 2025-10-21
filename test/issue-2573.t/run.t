Format issue #2573 - refmt produces invalid code in JSX with fast pipe and field access
  $ refmt ./input.re | tee formatted.re
  let out = <div> f(x).value->React.string </div>;

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

