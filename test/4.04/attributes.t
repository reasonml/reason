  $ cat > input.re <<EOF
  > /* Pexp_letexception with attributes */
  > let () = {
  >   [@attribute] exception E;
  >   raise(E)
  > };
  > 
  > /** Different payloads **/
  > 
  > /* Empty signature */
  > 
  > [@haha: ]
  > let x = 5;
  > 
  > /* signature_item */
  > [@haha: let x : option(int)]
  > let x = 5;
  > 
  > /* Signature */
  > [@haha: type t; let x : option(t)]
  > let x = 5;
  > EOF

Format basic
  $ refmt --print re ./input.re > ./formatted.re

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
