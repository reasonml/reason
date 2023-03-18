Print error for lowercase module
  $ cat >input.re <<EOF
  > module lowercase = {};
  > EOF

  $ refmt ./input.re
  Error: Module names must start with an uppercase letter.
  
  [1]
