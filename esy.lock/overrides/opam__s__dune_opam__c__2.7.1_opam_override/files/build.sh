#!/bin/bash

SECONDARY_CONF="$OCAMLFIND_SECONDARY_PREFIX/lib/findlib.conf.d/ocaml-secondary-compiler.conf" 

if test -f $SECONDARY_CONF; then
   export OCAMLFIND_CONF=$SECONDARY_CONF;
fi

env -u OCAMLLIB ocaml bootstrap.ml
./dune.exe build -p dune --profile dune-bootstrap
