Print error for type reserved keyword
  $ cat >input.re <<EOF
  > let foo = { foo: "bar", type: "qux" };
  > EOF

  $ refmt ./input.re
  File "./input.re", line 1, characters 24-28:
  Error: type is a reserved keyword, it cannot be used as an identifier. Try `type_` or `_type` instead
  [1]
