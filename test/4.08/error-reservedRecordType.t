Print error for type reserved keyword
  $ cat >input.re <<EOF
  > type x = { type: string };
  > EOF

  $ refmt ./input.re
  File "./input.re", line 1, characters 11-15:
  1 | type x = { type: string };
                 ^^^^
  Error: type is a reserved keyword, it cannot be used as an identifier. Try `type_` or `_type` instead
  
  [1]



