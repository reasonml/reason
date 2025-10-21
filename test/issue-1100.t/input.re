let compose:
  _ => unit

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

