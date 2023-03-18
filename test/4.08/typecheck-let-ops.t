  $ cat > input.ml <<EOF
  > open struct
  >   type t = string
  > end
  > 
  > let (let+) x f = List.map f x
  > 
  > let (and+) = List.map2 (fun x y -> x,y)
  > 
  > let x =
  >   let+ x = [2]
  >   and+ y = [3]
  >   in
  >   x, y
  > 
  > let y =
  >   let+ x = [2] in
  >   x
  > EOF

Format basic
  $ refmt --print re ./input.ml > ./formatted.re

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
