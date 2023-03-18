Print error for lowercase module
  $ cat >input.re <<EOF
  > module rec lowercase = {};
  > EOF

  $ refmt ./input.re
  File "./input.re", line 1, characters 0-2:
  Error: Module names must start with an uppercase letter.
  
  [1]
