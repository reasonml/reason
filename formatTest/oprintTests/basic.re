/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
let run = () => "Basic Structures";

let endOfRangeMustBeSimple = (a, b) => 10;
let theSame = (a, b, c) => 20;

type zed = {bar: ref(int)};
let foo = ref(ref({bar: ref(10)}));

module MyFirstModule = {
  let x = 0;
  let y = x + x;
  let a = 2
  and b = 3;
  type i = int
  and n = string;
};

module type MySecondModuleType = {
  type someType = int;
  let x: int;
  let y: int;
};

module MyLocalModule = {
  type i = int;
  let x:i = 10;
};

module MyLocalModule2: MySecondModuleType = {
  type someType = int;
  let x:someType = 10;
  let y:someType = 20;
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
  /*
   * Comment inside of signature.
   */
  let x = 10;
  /* Inline comment inside signature. */
  let y = 20;
};

module MyFunctor = fun (M: HasTT) => {
  type reexportedTT = M.tt;
  /* Inline comment inside module. */
  /** Following special comment inside module. */
  let someValue = 1000;
};

/* Notice how
   - Functors no longer require parens around argument.
   - A final semicolon is required for module structures.
   - We should eliminate both those requirements. See action items 13-14 at the
   bottom of this file. [Actually, forgiving the trailing SEMI might not be
   such a great idea].
   */
module MyFunctorResult = MyFunctor ({type tt = string;});

module type ASig = {let a:int;};
module type BSig = {let b:int;};

module CurriedSugar (A:ASig, B:BSig) {
  let result = A.a + B.b;
};


let addValues = fun (a:int, b:int) => {
  a + b;
};

let myFunction = fun (a : int, b : int) : int => a + b;

let functionReturnValueType (i:int, s:string): (int) => int = fun(x) => x + 1;

let curriedFormOne (i:int, s:string) = s ++ string_of_int(i);

let curriedFormTwo (i:int, x:int) :(int, int) = (i, x);
/* let nonCurriedFormTwo = fun (i:int, x:int) (:(int, int)) => (i, x); */

let curriedFormThree (i:int, (a:int, b:int):(int, int)) :(int, int, int) = (i, a, b);

type myFuncType = (int, int) => int;

let myFunc: myFuncType = fun (a,b) => a + b;

let funcWithTypeLocallyAbstractTypes (type atype, type btype, a, b, c: (atype, btype) => unit) = c(a,b);

type firstNamedArgShouldBeGroupedInParens =
    (~first: (int) => int, ~second: int) => int;
type allParensCanBeRemoved =
    (~first: int) => ((~second: int) => ((~third: int) => int));
type firstTwoShouldBeGroupedAndFirstThree =
    (~first: ((int) => int) => int) => int;
type firstNamedArgShouldBeGroupedInParensOpt =
    (~first: ((int) => int)=?, ~second: list(int)=?) => int;

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