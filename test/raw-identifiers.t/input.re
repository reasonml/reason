
let \#let = 2;
let \#let = \#let
and \#and = \#let;

/* labeled arguments */
let \#let = (~\#let) => {
  \#let;
};

let \#let = (~\#let: \#let = \#let) => {
  \#let;
};

/* Types */
type \#type = \#type;

module type \#module = \#module;

class \#class = \#class;

class type \#class = \#class;

type x = [ | `\#module ]
type y = [ | \#module ]
let x = `\#module

external \#external: unit => unit = "external";

type \#rec = {
  \#type: \#type,
  \#module: module_
};

let \#rec = {
  \#type: \#type,
  \#module: module_
}

let true = x => x;

let \#true = x => x;

