#!/usr/bin/env bash

# redoc is an ocamldoc wrapper for producing html docs in Reason syntax

REASON_DIR=`ocamlfind query reason`

ocamlfind ocamldoc -g $REASON_DIR/reasondoc.cma $@
