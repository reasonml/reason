Force error for try catch
  $ cat >input.re <<EOF
  > try (bad);
  > EOF

  $ refmt ./input.re
  File "./input.re", line 1, characters 9-10:
  Error: Syntax error
  [1]
