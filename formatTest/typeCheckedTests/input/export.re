[@@bs.val] export external exportExternal1 : int => int => int = "exportExternal";
export external exportExternal2 : int => int => int = "exportExternal";

[@@type] export type x = int;

type x = int
    and export y = string;

type attr = ..;

type attr += Str of string;

[@@foo] export type attr += | Point int int;

export exception E int int;

[@@foo]
export let module Js = {
  type t 'a;
};

[@@foo]
export let module rec Js2 = {
  type t 'a;
} and export Bar = {
  type t;
};

export module type HasTT = {
  type tt;
};

export module type Foo;

export class firstRecursiveClass init => {
 val v = init;
} and export firstRecursiveClass2 init => {
    val v2 = init;
};

[@@structureItem]
export class type addablePointClassType = {
  pub x: int;
  pub y: int;
  pub add:
    addablePointClassType =>
    addablePointClassType =>
    int
}
[@@structureItem]
and export anotherClassType = {
  pub foo: int;
  pub bar: int
};

export [@@@foo];