Force error for try catch
  $ cat >input.re <<EOF
  > try (bad);
  > EOF

  $ refmt ./input.re
  Error: Syntax error
  
  [1]
