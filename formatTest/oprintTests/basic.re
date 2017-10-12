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

module LookNoParensNeeded = MyFunctor {type tt = string;};

module type SigResult = {let result:int;};

module type ASig = {let a:int;};
module type BSig = {let b:int;};
module AMod = {let a = 10;};
module BMod = {let b = 10;};

module CurriedSugar (A:ASig, B:BSig) {
  let result = A.a + B.b;
};