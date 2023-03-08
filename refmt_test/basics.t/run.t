Type-check basics
  $ refmt_impl --version
  Reason 3.7.0 @ b66ed1b

  $ ocamlc -c -pp "refmt_impl --print binary" -intf-suffix .rei -impl input.re

$ refmt --heuristics-file $INPUT/arity.txt  --print re $INPUT/$FILE 2>&1 > $OUTPUT/$REFILE
$ refmt  --print re $OUTPUT/$FILE 2>&1 > $OUTPUT/$FILE.formatted
