Format ./input.re
  $ cat >input.re <<EOF
  > /* this is an unterminated comment
  > EOF

  $ refmt ./input.re
  File "./input.re", line 1, characters 0-2:
  Error: Comment not terminated
  
  [1]
