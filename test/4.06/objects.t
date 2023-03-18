  $ cat > input.re <<EOF
  > /* Oinherit (https://github.com/ocaml/ocaml/pull/1118) */
  > type t = {
  >   .
  >   a: string
  > };
  > 
  > type t1 = {
  >   .
  >   n: string,
  >   ...t,
  > };
  > 
  > type t2('a) = {
  >   ..
  >   o: string,
  >   ...t,
  > } as 'a;
  > 
  > /* Pcl_open, Pcty_open (https://github.com/ocaml/ocaml/pull/1249) */
  > module EM = {
  >   type t;
  > };
  > 
  > module OM = {
  >   type t;
  > };
  > 
  > class x = {
  >   open EM;
  >   as self;
  > };
  > 
  > class y = {
  >   open EM;
  >   open OM;
  >   as self;
  > };
  > EOF

Format basic
  $ refmt --print re ./input.re > ./formatted.re

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
