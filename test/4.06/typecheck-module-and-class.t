  $ cat > input.ml <<EOF
  > module EM = struct
  >   (** Exception *)
  >   exception E of int * int
  > end
  > 
  > (* Pcl_open *)
  > class x = let open EM in object (self) end
  > 
  > module OM = struct
  >   type t
  > end
  > 
  > class y = let open EM in let open OM in object (self) end
  > 
  > module type S = sig
  >   type t = private ..
  >   type t += Foo
  > end
  > EOF

Format basic
  $ refmt --print re ./input.ml > ./formatted.re

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
