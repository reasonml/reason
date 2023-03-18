Print error for type reserved keyword
  $ cat >input.re <<EOF
  > type x = { type: string };
  > EOF

  $ refmt ./input.re
  Error: type is a reserved keyword, it cannot be used as an identifier. Try `type_` or `_type` instead
  
  [1]



