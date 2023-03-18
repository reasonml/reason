Print error for lowercase module
  $ cat >input.re <<EOF
  > module rec lowercase = {};
  > EOF

  $ refmt ./input.re
  File "./input.re", line 1, characters 11-20:
  1 | module rec lowercase = {};
                 ^^^^^^^^^
  Error: Module names must start with an uppercase letter.
  
  [1]
