Format issue #1100 - Refmting multi-line ppx strings
  $ refmt ./input.re | tee formatted.re
  let compose: _ => unit
  [@@ocaml.deprecated
    {|
  Use the |> as an infix operator to chain the
  result of one function into another:
  
  `compose(f, g, h)(x)`
  in JS goes to
  `x |> h |> g |> f`
  in Reason.
  |}
  ];

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

