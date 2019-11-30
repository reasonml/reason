/* Type names (supported with PR#2342) */
module T = {
  type pub_ = unit;
};

/* Value names (already supported) */
module V = {
  let pub_ = ();
};

/* Record fields */
module R = {
  type r = {pub_: int};

  let foo = {pub_: 4};
};

/* Class names and instance variables */
module C = {
  class pub_ = {
    as _;
  };

  class c = {
    as _;
    val pub_ = 0;
    pub pub_ = () => ();
  };

  class c' = {
    as _;
    inherit class pub_;
    val! pub_ = 1;
  };
};

/* Class types */
module Ct = {
  class type pub_ = {
    val pub_: unit => unit;
  };
};

/* Virtual */
module Cv = {
  class virtual pub_ = {
    as _;
  };
};

/* Object methods */
module O = {
  let o = {as _; pub pub_ = ()};
};

/* Function parameter labels */
module L = {
  let f = (~pub_) => ignore(pub_);
};

/* Module types */
module type pub_ = {};

/* Polymorphic variants (probably ok as-is?) */
module P = {
  type t = [ | `pub_];
};

type method = string;

type foo = {method};
