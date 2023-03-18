Force error for type ./input.re
  $ cat >input.re <<EOF
  > try (bad);
  > EOF

  $ refmt ./input.re
  File "./input.re", line 1, characters 9-10:
  1 | try (bad);
               ^
  Error: Syntax error
  
  [1]
