

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

mod MyFirstModule = {
  let x = 0;
  let y = x + x;
};

let result = MyFirstModule.x + MyFirstModule.y;

/**
 * - A module is introduced with the `mod` phrase.
 * - A module *must* have a capital letter as its first character.
 * - The exported fields of a module must be listed within `{}` braces and each
 * exported value binding is specified via a `let` keyword.
 */

/**
 * Another way that modules are more powerful than records, is that they may
 * also export types.
 */
mod MySecondModule = {
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
mod type MySecondModuleType = {
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

 mod MySecondModule: MySecondModuleType = {
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
  mod MyLocalModule = {
    type i = int;
    let x:i = 10;
  };
  /* Notice how local modules names may be used twice and are shadowed */
  mod MyLocalModule: MySecondModuleType = {
    type someType = int;
    let x:someType = 10;
    let y:someType = 20;
  };
  let tmp = MyLocalModule.x + 22;
  tmp + 30;
};

mod type HasTT = {
  type tt;
};

mod SubModule: HasTT = {
  type tt = int;
};

mod type HasEmbeddedHasTT = {
  mod SubModuleThatHasTT = SubModule;
};

mod type HasPolyType = {type t 'a;};

mod type HasDestructivelySubstitutedPolyType =
  HasPolyType with type t 'a := list 'a;

mod type HasDestructivelySubstitutedSubPolyModule = {
  /* Cannot perform destructive substitution on submodules! */
  /* module X: HasPolyType with type t := list (int, int); */
  mod X: HasDestructivelySubstitutedPolyType;
};
mod type HasSubPolyModule = {
  /* Cannot perform destructive substitution on submodules! */
  /* module X: HasPolyType with type t := list (int, int); */
  mod X: HasPolyType;
};

mod EmbedsSubPolyModule: HasSubPolyModule = {
  mod X = {
    type t 'a = list 'a;
  };
};

mod EmbedsDestructivelySubstitutedPolyModule: HasDestructivelySubstitutedSubPolyModule = {
  mod X = {
    type t = list (int, int);
  };
};

mod type HasMultiPolyType = {
  type substituteThis 'a 'b;
  type substituteThat 'a 'b;
};

mod type HasDestructivelySubstitutedMultiPolyType = (
  HasMultiPolyType with
type substituteThis 'a 'b := Hashtbl.t 'a 'b and
type substituteThat 'a 'b := Hashtbl.t 'a 'b
);


mod InliningSig: {let x: int; let y:int;} = {
  /*
   * Comment inside of signature.
   */
  let x = 10;
  /* Inline comment inside signature. */
  let y = 20;
};

mod MyFunctor = functor (M: HasTT) => {
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
mod MyFunctorResult = MyFunctor ({type tt = string;});

mod LookNoParensNeeded = MyFunctor {type tt = string;};

mod type SigResult = {let result:int;};

mod type ASig = {let a:int;};
mod type BSig = {let b:int;};
mod AMod = {let a = 10;};
mod BMod = {let b = 10;};

mod CurriedSugar (A:ASig) (B:BSig) => {
  let result = A.a + B.b;
};


/* Right now [CurriedSuperSugar] is parsed as being indistinguishable from
   the above.

   mod CurriedSuperSugar (A:ASig) (B:BSig): SigResult => ({
   let result = A.a + B.b;
   }: SigResult);

   SugarML unifies the functor and function syntax. The same currying/annotation
   patterns are available for both module/functors and value/functions.

   /* Not supported in OCaml OR SugarML */
   let x = (a:foo) :bar => baz;
   mod x = functor (A:Foo) :Bar => Baz;

   /* Supported in both OCaml and SugarML */
   let x (a:foo) :bar => baz;
   mod x (A:Foo) :Bar => Baz;

   */
mod CurriedSugarWithReturnType (A:ASig) (B:BSig): SigResult => {
  let result = A.a + B.b;
};

/* This is parsed as being equivalent to the above example */
mod CurriedSugarWithAnnotatedReturnVal (A:ASig) (B:BSig) => ({
  let result = A.a + B.b;
}: SigResult);

mod CurriedNoSugar = functor (A:ASig) => functor (B:BSig) => {
  let result = A.a + B.b;
};

let letsTryThatSyntaxInLocalModuleBindings () => {
  mod CurriedSugarWithReturnType (A:ASig) (B:BSig): SigResult => {
    let result = A.a + B.b;
  };
  mod CurriedSugarWithAnnotatedReturnVal (A:ASig) (B:BSig) => ({
    let result = A.a + B.b;
  }: SigResult);

  mod CurriedNoSugar = functor (A:ASig) => functor (B:BSig) => {
    let result = A.a + B.b;
  };

  /*
   * The following doesn't work in OCaml (LocalModule (struct end)).x isn't even
   * parsed!
   *
   * let thisDoesntWorkInOCaml () =
   * mod LocalModule(A:sig end) = struct let x = 10 end in
   * mod Out = (LocalModule (struct end)) in
   * let outVal = (LocalModule (struct end)).x in
   * let res = Out.x in
   * res;;
   */

  mod TempModule = CurriedNoSugar AMod BMod;
  mod TempModule2 = CurriedSugarWithAnnotatedReturnVal AMod BMod;
  TempModule.result + TempModule2.result;
};



mod type EmptySig = {};
mod MakeAModule (X:EmptySig) => {let a = 10;};
mod CurriedSugarFunctorResult = CurriedSugar AMod BMod;
mod CurriedSugarFunctorResultInline = CurriedSugar {let a=10;} {let b=10;};
mod CurriedNoSugarFunctorResult = CurriedNoSugar AMod BMod;
mod CurriedNoSugarFunctorResultInline = CurriedNoSugar {let a=10;} {let b=10;};

mod ResultFromNonSimpleFunctorArg = CurriedNoSugar (MakeAModule {}) BMod;


/* TODO: Functor type signatures should more resemble value signatures */
let curriedFunc: int=>int=>int = fun a b => a + b;
mod type FunctorType =  ASig => BSig => SigResult;
/* Which is sugar for:*/
mod type FunctorType2 = (_:ASig) => (_:BSig) => SigResult;

/* Just for compability with existing OCaml ASTs you can put something other
* than an underscore */
mod type FunctorType3 = (Blah:ASig) => (ThisIsIgnored:BSig) => SigResult;

/* The actual functors themselves now have curried sugar (which the pretty
 * printer will enforce as well */
/* The following: */
mod CurriedSugarWithAnnotation2: ASig => BSig => SigResult =
  functor (A:ASig) => functor (B:BSig) => {let result = A.a + B.b;};

/* Becomes: */
mod CurriedSugarWithAnnotation: ASig => BSig => SigResult =
  functor (A:ASig) (B:BSig) => {let result = A.a + B.b;};


/* "functors" that are not in sugar curried form cannot annotate a return type
 * for now, so we settle for: */
mod CurriedSugarWithAnnotationAndReturnAnnotated: ASig => BSig => SigResult =
  functor (A:ASig) (B:BSig) => ({let result = A.a + B.b;}: SigResult);

mod ReturnsAFunctor (A:ASig) (B:BSig): (ASig => BSig => SigResult) =>
  functor (A:ASig) (B:BSig) => {
    let result = 10;
  };

mod ReturnsSigResult (A:ASig) (B:BSig): SigResult => {
  let result = 10;
};

mod ReturnsAFunctor2 (A:ASig) (B:BSig): (ASig => BSig => SigResult) =>
  functor (A:ASig) (B:BSig) => {let result = 10;};

/*
 * Recursive modules.
 * TODO: Test [Psig_recmodule]
 */
mod rec A : {
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
mod type HasRecursiveModules = {
  mod rec A: {
    type t = | Leaf of string | Node of ASet.t;
    let compare: t => t => int;
  }
  and ASet: Set.S with type elt = A.t;
};


/* From http://stackoverflow.com/questions/1986374/higher-order-type-constructors-and-functors-in-ocaml */
mod type Type = {type t;};
mod Char = {type t = char;};
mod List (X:Type) => {type t = list X.t;};
mod Maybe (X:Type) => {type t = option X.t;};
mod Id (X:Type) => X;
mod Compose (F:Type=>Type) (G:Type=>Type) (X:Type) => F(G(X));
let l : Compose(List)(Maybe)(Char).t = [Some 'a'];
mod Example2 (F:Type=>Type) (X:Type) => {
  let iso (a:Compose(Id)(F)(X).t): F(X).t => a;
};

Printf.printf "\nModules And Functors: %n\n" (CurriedNoSugarFunctorResultInline.result);

/* We would have: */
/* mod CurriedSugarWithAnnotation: ASig => BSig => SigResult =
 functor (A:ASig) (B:BSig) => {let result = A.a + B.b;;
 */

/*
 mod Typeahead = React.Create {
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


mod type HasInt = {let x: int;};

mod MyModule = {let x = 10;};

let myFirstClass = (mod MyModule : HasInt);

let myFirstClassWillBeFormattedAs: (mod HasInt) = (mod MyModule);

let acceptsAndUnpacksFirstClass (mod M : HasInt) => M.x + M.x;

let acceptsAndUnpacksFirstClass ((mod M) : (mod HasInt)) => M.x + M.x;

mod SecondClass = (val myFirstClass);

mod SecondClass2 = (val (mod MyModule: HasInt));

let p = SecondClass.x;

/* Opening Modules */
mod M = {
  mod Inner = {};
};

mod N = {
open M;
let z = { let open M; 34; };
let y = 44;
};

open M;
open M.Inner;
open M;
