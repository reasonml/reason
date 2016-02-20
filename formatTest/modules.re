/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

let run = fun () => {
  TestUtils.printSection "Modules";
};



/**
 * Modules:
 * ----------------------------------------------------------------------------
 * Modules accomplish the following:
 * - Organization of code and data.
 * - Basis for separate compilation and fast compile times.
 * - Enabled higher order type abstractions that are capable of modelling very
 * sophisticated typing relationships between various parts of your code.
 *
 * Modules are much like records, but much more powerful. Much of modules'
 * powers are derived from the fact that they are primarily built up and
 * organized at compilation time, instead of runtime. It is this constraint
 * that allows modules to be more powerful than simple records.
 *
 * There are some important ways that modules differ from records:
 *
 * - Fields are lexically scoped: In the following record: `{x:1, y: x + x}`,
 * the reference to `x` does not refer to the record field `x`. This is because
 * records fields do not form lexical scopes for the evaluation of other record
 * values. Modules, on the other hand do allow fields to reference other
 * fields. Appropriately, each field is introduced via the keyword `let`.
 */

let module MyFirstModule = {
  let x = 0;
  let y = x + x;
};

let result = MyFirstModule.x + MyFirstModule.y;

/**
 * - A module is introduced with the `let module` phrase.
 * - A module *must* have a capital letter as its first character.
 * - The exported fields of a module must be listed within `{}` braces and each
 * exported value binding is specified via a `let` keyword.
 */

/**
 * Another way that modules are more powerful than records, is that they may
 * also export types.
 */
let module MySecondModule = {
  type someType = int;
  let x = 0;
  let y = x + x;
};

let myInt:MySecondModule.someType = 100;

/** Module signatures:
 * ----------------------------------------------------------------------------
 * Not only may modules export types, but modules *themselves* can be described
 * by types via the `module type` phrase. We call these module types
 * "signatures". For example, `MySecondModule` has the following `module type`
 * signature:
 */
module type MySecondModuleType = {
  type someType = int;
  let x: int;
  let y: int;
};

/**
 * Much like how you can ensure that a value is compatible with a specific
 * type:

 let myTestVal: int = 10;

 * You can also perform the same type of annotation to ensure that you have
 * written code that matches your understanding. For example, `MySecondModule`
 * could have been written as:

 let module MySecondModule: MySecondModuleType = {
 type someType = int;
 let x = 0;
 let y = x + x;
 };
 */

/**
 * - Modules may be artificially "constrained" so that users of a module see
 * fewer details than are actually present.
 * - Modules may be combined, merged, and transformed at compile time in ways
 * that
 * - Because they are more powerful, they may not be passed around at runtime
 * as easily as records.
 *
 * Some additioal benefits to using modules:
 * - Modules are a very elegant way to organize large packages of code.
 * - Modules are the unit of compilation. Minimal recompilation changes
 * - Modules can help you achieve higher degrees of polymorphism than the core
 * language.
 */

let opensAModuleLocally = {
  let module MyLocalModule = {
    type i = int;
    let x:i = 10;
  };
  /* Notice how local modules names may be used twice and are shadowed */
  let module MyLocalModule: MySecondModuleType = {
    type someType = int;
    let x:someType = 10;
    let y:someType = 20;
  };
  let tmp = MyLocalModule.x + 22;
  tmp + 30;
};

module type HasTT = {
  type tt;
};

let module SubModule: HasTT = {
  type tt = int;
};

module type HasEmbeddedHasTT = {
  let module SubModuleThatHasTT = SubModule;
};

module type HasPolyType = {type t 'a;};

module type HasDestructivelySubstitutedPolyType =
  HasPolyType with type t 'a := list 'a;

module type HasDestructivelySubstitutedSubPolyModule = {
  /* Cannot perform destructive substitution on submodules! */
  /* module X: HasPolyType with type t := list (int, int); */
  let module X: HasDestructivelySubstitutedPolyType;
};
module type HasSubPolyModule = {
  /* Cannot perform destructive substitution on submodules! */
  /* module X: HasPolyType with type t := list (int, int); */
  let module X: HasPolyType;
};

let module EmbedsSubPolyModule: HasSubPolyModule = {
  let module X = {
    type t 'a = list 'a;
  };
};

let module EmbedsDestructivelySubstitutedPolyModule: HasDestructivelySubstitutedSubPolyModule = {
  let module X = {
    type t = list (int, int);
  };
};

module type HasMultiPolyType = {
  type substituteThis 'a 'b;
  type substituteThat 'a 'b;
};

module type HasDestructivelySubstitutedMultiPolyType = (
  HasMultiPolyType with
type substituteThis 'a 'b := Hashtbl.t 'a 'b and
type substituteThat 'a 'b := Hashtbl.t 'a 'b
);


let module InliningSig: {let x: int; let y:int;} = {
  /*
   * Comment inside of signature.
   */
  let x = 10;
  /* Inline comment inside signature. */
  let y = 20;
};

let module MyFunctor = functor (M: HasTT) => {
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
let module MyFunctorResult = MyFunctor ({type tt = string;});

let module LookNoParensNeeded = MyFunctor {type tt = string;};

module type SigResult = {let result:int;};

module type ASig = {let a:int;};
module type BSig = {let b:int;};
let module AMod = {let a = 10;};
let module BMod = {let b = 10;};

let module CurriedSugar (A:ASig) (B:BSig) => {
  let result = A.a + B.b;
};


/* Right now [CurriedSuperSugar] is parsed as being indistinguishable from
   the above.

   let module CurriedSuperSugar (A:ASig) (B:BSig): SigResult => ({
   let result = A.a + B.b;
   }: SigResult);

   SugarML unifies the functor and function syntax. The same currying/annotation
   patterns are available for both module/functors and value/functions.

   /* Not supported in OCaml OR SugarML */
   let x = (a:foo) :bar => baz;
   module x = functor (A:Foo) :Bar => Baz;

   /* Supported in both OCaml and SugarML */
   let x (a:foo) :bar => baz;
   module x (A:Foo) :Bar => Baz;

   */
let module CurriedSugarWithReturnType (A:ASig) (B:BSig): SigResult => {
  let result = A.a + B.b;
};

/* This is parsed as being equivalent to the above example */
let module CurriedSugarWithAnnotatedReturnVal (A:ASig) (B:BSig) => ({
  let result = A.a + B.b;
}: SigResult);

let module CurriedNoSugar = functor (A:ASig) => functor (B:BSig) => {
  let result = A.a + B.b;
};

let letsTryThatSyntaxInLocalModuleBindings () => {
  let module CurriedSugarWithReturnType (A:ASig) (B:BSig): SigResult => {
    let result = A.a + B.b;
  };
  let module CurriedSugarWithAnnotatedReturnVal (A:ASig) (B:BSig) => ({
    let result = A.a + B.b;
  }: SigResult);

  let module CurriedNoSugar = functor (A:ASig) => functor (B:BSig) => {
    let result = A.a + B.b;
  };

  /*
   * The following doesn't work in OCaml (LocalModule (struct end)).x isn't even
   * parsed!
   *
   * let thisDoesntWorkInOCaml () =
   * let module LocalModule(A:sig end) = struct let x = 10 end in
   * let module Out = (LocalModule (struct end)) in
   * let outVal = (LocalModule (struct end)).x in
   * let res = Out.x in
   * res;;
   */

  let module TempModule = CurriedNoSugar AMod BMod;
  let module TempModule2 = CurriedSugarWithAnnotatedReturnVal AMod BMod;
  TempModule.result + TempModule2.result;
};



module type EmptySig = {};
let module MakeAModule (X:EmptySig) => {let a = 10;};
let module CurriedSugarFunctorResult = CurriedSugar AMod BMod;
let module CurriedSugarFunctorResultInline = CurriedSugar {let a=10;} {let b=10;};
let module CurriedNoSugarFunctorResult = CurriedNoSugar AMod BMod;
let module CurriedNoSugarFunctorResultInline = CurriedNoSugar {let a=10;} {let b=10;};

let module ResultFromNonSimpleFunctorArg = CurriedNoSugar (MakeAModule {}) BMod;


/* TODO: Functor type signatures should more resemble value signatures */
let curriedFunc: int=>int=>int = fun a b => a + b;
module type FunctorType =  ASig => BSig => SigResult;
/* Which is sugar for:*/
module type FunctorType2 = (_:ASig) => (_:BSig) => SigResult;

/* Just for compability with existing OCaml ASTs you can put something other
* than an underscore */
module type FunctorType3 = (Blah:ASig) => (ThisIsIgnored:BSig) => SigResult;

/* The actual functors themselves now have curried sugar (which the pretty
 * printer will enforce as well */
/* The following: */
let module CurriedSugarWithAnnotation2: ASig => BSig => SigResult =
  functor (A:ASig) => functor (B:BSig) => {let result = A.a + B.b;};

/* Becomes: */
let module CurriedSugarWithAnnotation: ASig => BSig => SigResult =
  functor (A:ASig) (B:BSig) => {let result = A.a + B.b;};


/* "functors" that are not in sugar curried form cannot annotate a return type
 * for now, so we settle for: */
let module CurriedSugarWithAnnotationAndReturnAnnotated: ASig => BSig => SigResult =
  functor (A:ASig) (B:BSig) => ({let result = A.a + B.b;}: SigResult);

let module ReturnsAFunctor (A:ASig) (B:BSig): (ASig => BSig => SigResult) =>
  functor (A:ASig) (B:BSig) => {
    let result = 10;
  };

let module ReturnsSigResult (A:ASig) (B:BSig): SigResult => {
  let result = 10;
};

let module ReturnsAFunctor2 (A:ASig) (B:BSig): (ASig => BSig => SigResult) =>
  functor (A:ASig) (B:BSig) => {let result = 10;};

/*
 * Recursive modules.
 * TODO: Test [Psig_recmodule]
 */
let module rec A : {
  type t = Leaf of string | Node of ASet.t;
  let compare: t => t => int;
} = {
  type t = Leaf of string | Node of ASet.t;
  let compare t1 t2 => switch (t1, t2) {
    | (Leaf s1, Leaf s2) => Pervasives.compare s1 s2
    | (Leaf _, Node _) => 1
    | (Node _, Leaf _) => -1
    | (Node n1, Node n2) => ASet.compare n1 n2
  };
}
and ASet: Set.S with type elt = A.t = Set.Make A;


/*
 * How recursive modules appear in signatures.
 */
module type HasRecursiveModules = {
  let module rec A: {
    type t = | Leaf of string | Node of ASet.t;
    let compare: t => t => int;
  }
  and ASet: Set.S with type elt = A.t;
};


/* From http://stackoverflow.com/questions/1986374/higher-order-type-constructors-and-functors-in-ocaml */
module type Type = {type t;};
let module Char = {type t = char;};
let module List (X:Type) => {type t = list X.t;};
let module Maybe (X:Type) => {type t = option X.t;};
let module Id (X:Type) => X;
let module Compose (F:Type=>Type) (G:Type=>Type) (X:Type) => F(G(X));
let l : Compose(List)(Maybe)(Char).t = [Some 'a'];
let module Example2 (F:Type=>Type) (X:Type) => {
  let iso (a:Compose(Id)(F)(X).t): F(X).t => a;
};

Printf.printf "\nModules And Functors: %n\n" (CurriedNoSugarFunctorResultInline.result);

/* We would have: */
/* let module CurriedSugarWithAnnotation: ASig => BSig => SigResult =
 functor (A:ASig) (B:BSig) => {let result = A.a + B.b;;
 */

/*
 let module Typeahead = React.Create {
 type props = {initialCount: int};
 type state = {count: int};
 let getInitialState props => {count: 10};
 let render {props, state} =>
 <div>
 <span><BigBox></span>
 </div>;
 };
 */

include YourLib.CreateComponent {
  type thing = blahblahblah;
  type state = unit;
  let getInitialState _ => ();
  let myValue = {
    recordField: "hello"
  };
};


module type HasInt = {let x: int;};

let module MyModule = {let x = 10;};

let myFirstClass = (module MyModule : HasInt);

let myFirstClassWillBeFormattedAs: (module HasInt) = (module MyModule);

let acceptsAndUnpacksFirstClass (module M : HasInt) => M.x + M.x;

let acceptsAndUnpacksFirstClass ((module M) : (module HasInt)) => M.x + M.x;

let module SecondClass = (val myFirstClass);

let module SecondClass2 = (val (module MyModule: HasInt));

let p = SecondClass.x;

/* Opening Modules */
let module M = {
  let module Inner = {};
};

let module N = {
open M;
let z = { let open M; 34; };
let y = 44;
};

open M;
open M.Inner;
open M;

