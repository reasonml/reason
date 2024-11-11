Test raw identifiers in Reason syntax

  $ refmt ./input.re | tee input2.re
  let \#let = 2;
  let \#let = \#let
  and \#and = \#let;
  
  /* labeled arguments */
  let \#let = (~\#let) => {
    \#let;
  };
  
  let \#let = (~\#let: \#let=\#let) => {
    \#let;
  };
  
  /* Types */
  type \#type = \#type;
  
  module type \#module = \#module;
  
  class \#class = class \#class;
  
  class type \#class = \#class;
  
  type x = [ | `\#module];
  type y = [ \#module];
  let x = `\#module;
  
  external \#external: unit => unit = "external";
  
  type \#rec = {
    \#type,
    \#module: module_,
  };
  
  let \#rec = {
    \#type,
    \#module: module_,
  };
  
  let true = x => x;
  
  let \#true = x => x;

Check idempotency

  $ refmt ./input2.re > out.re

