module Foo = {
  type t = {name: string};
};

let foo = (Foo.{name}) => ();
