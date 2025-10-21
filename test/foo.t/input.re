let x1 = () => 1;
let x2 = (a) => 1;
let x3 = (a: int, b) => 1;
let x4 = ((a,b)) => 1;
let x5 = fun (a,b): int => 1;
let x6 = (~x, ~y) => 1;
let x7 = (~x: int, ~y: string) => 1;
let x8 = (~x=5, ~y=?, ~z: option(string)=?, ()) => 1;

type a = int;
type b = float;
type c = string;
type t1 = (a) => b;
type t2 = (a, b) => c;
type t3 = ((a, b)) => c;
type t4 = (~x: int, ~y: string) => c;
type t5 = (~x: a=?) => b;
type tf = (int => int) => string;
type tNested2 = ((int => int)) => string;
type tNested3 = ((int => int) => int) => string;
type tNested4 = (int, int) => string;
type tNested5 = ((int, int)) => string;
type tNested7 = array(list(int) => string);

type t6 = int;
type t7('a) = list('a);
type t8('a, 'b) = (list('a), 'b);
type t9 = t8(string, int);

class type restricted_point_type = {
  pub get_x: int;
  pub bump: unit;
};
class type t10('a) = {
  pub thing: 'a;
};
class type t11('a, 'b) = {
  pub thing: ('a, list('b))
};

module MyFirstModule = {
  let x = 0;
  type i = int
  and n = string;
};

module type HasTT = {
  type tt;
};

module SubModule: HasTT = {
  type tt = int;
};

module type HasEmbeddedHasTT = {
  module SubModuleThatHasTT = SubModule;
};

module type HasPolyType = {type t('a);};
module type HasDoublePoly = {type m('b, 'c);};

module type HasDestructivelySubstitutedPolyType =
  HasPolyType with type t('a) := list('a);

module type HasDestructivelySubstitutedSubPolyModule = {
  /* Cannot perform destructive substitution on submodules! */
  /* module X: HasPolyType with type t := list (int, int); */
  module X: HasDestructivelySubstitutedPolyType;
};

module type HasSubPolyModule = {
  /* Cannot perform destructive substitution on submodules! */
  /* module X: HasPolyType with type t := list (int, int); */
  module X: HasPolyType;
};

module EmbedsSubPolyModule: HasSubPolyModule = {
  module X = {
    type t('a) = list('a);
  };
};

module InliningSig: {let x: int; let y:int;} = {
  let x = 10;
  let y = 20;
};

module MyFunctor = fun (M: HasTT) => {
  type reexportedTT = M.tt;
  let someValue = 1000;
};

module MyFunctorResult = MyFunctor ({type tt = string;});
module type ASig = {let a:int;};
module type BSig = {let b:int;};
module CurriedSugar (A:ASig, B:BSig) {
  let result = A.a + B.b;
};

type withThreeFields = {
  name: string,
  age: int,
  occupation: string
};

let testRecord = {
  name: "joe",
  age: 20,
  occupation: "engineer"
};

let makeRecordBase () {name: "Joe", age: 30, occupation: "Engineer"};


type t =
  | A
  | B(int)
  | C(int, int)
  | D((int, int));

type foo = {x:int};
let result = Some {x:1};


type tt1 = A(int) | B(bool, string);

type tt2 = A(int) | B((bool, string));

type tt3 = [ `A(int) | `B(bool, string) | `C];

type tt4 = [ `A(int) | `B((bool, string)) | `C];

let (==) = 0;

let (===) = 0;

let (!=) = 0;

let (!==) = 0;

type foobar(_) = | Foo('a): foobar(unit);

type expr(_) =
  | Int(int): expr(int)
  | String(string): expr(string)
  | Pair('a, 'b): expr(('a, 'b));

let eval = (type a, e: expr(a)): a =>
  switch (e) {
  | Int(n) => n
  | String(s) => s
  | Pair(x, y) => (x, y)
  };

type point = | Point({x: int, y: int});
type person = | Person({name: string, age: int}) | Anonymous;

type covariant(+'a) = list('a);
type contravariant(-'a) = 'a => unit;
type mixed(+'a, -'b) = 'a => 'b => unit;

type open_variant = [> `A | `B(int)];
type open_with_values = [> `Red | `Blue(string) | `Green(int, int)];

exception GenericError('a);

exception ParseError('a, 'b): exn;

type exn +=
  | CustomError('a): exn;

let with_extension = [%test "payload"];
let with_complex = [%derive.show { x: 1 }];
[%%toplevel_ext "payload"];

let%foo x = 1;

/* Test attributes rendering */
[@deprecated "Use new_module instead"]
module OldModule = { let x = 1; };

[@inline]
let inline_func = (x) => x + 1;

[@warning "-27"]
module type IgnoreUnused = { let unused: int; };

let%lwt x = 1;

let x = switch%platform () {
  | Server => 1
  | Client => 2
};

[@custom_attr];

[@custom_attr]
include SomeModule;

let rec foo = (a, b) => a + b
and bar = (a, b) => foo(a - b);
