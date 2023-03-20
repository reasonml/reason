Print error for lowercase module
  $ cat >input.re <<EOF
  > module lowercase = {};
  > EOF

  $ refmt ./input.re
  File "./input.re", line 1, characters 7-16:
  1 | module lowercase = {};
             ^^^^^^^^^
  Error: Module names must start with an uppercase letter.
  
  [1]
