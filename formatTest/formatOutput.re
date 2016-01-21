let printSection s => {
  print_string "\n";
  print_string s;
  print_string "\n---------------------\n"
};

let printLn s => print_string (s ^ "\n");
let run () => TestUtils.printSection "Basic Structures";

while something {
  print_string "You're in a while loop";
  print_newline ()
};

for i in 0 to 5 {
  print_int i;
  print_newline ();
  for i in 10 to 0 {
    print_string "Counting in reverse direction";
    print_newline ()
  }
};

for i in 
    0 to 
    (endOfRangeMustBeSimple expr soWrap) {
  print_int i;
  print_newline ();
  for i in 
      (theSame isTrue ofThe startOfRange) to 
      0 {
    print_string "Counting in reverse direction";
    print_newline ()
  }
};

let x = !foo.bar;

let x = !foo#bar;

let x = !(!foo).bar;

let x = !(!foo)#bar;

/* Prefix operator */
let x = !!foo.bar;

let x = !!foo#bar;

let x = !~foo.bar;

let x = !~foo#bar;

let noParensNeeded = !blah.foo.bar;

let parensNeededAroundFirst = (!blah).foo.bar;

let parensNeededAroundSecond = (!blah.foo).bar;

let noParensNeeded = !blah#foo#bar;

let parensNeededAroundFirst = (!blah)#foo#bar;

let parensNeededAroundSecond = (!blah#foo)#bar;

/* The following two have an error in formatting! We must check for
 * *consecutive* prefix operators and either space separate or guard in
 * parens. */
let x = !!foo.bar;

let x = !!foo#bar;

/* Comments */
/*Below is an empty comment*/
/**/
/**                            IF
 *============================================================================
 */
let something =
  if self.ext.logSuccess {
    print_string "Did tap";
    print_newline ()
  };

let logTapSuccess self =>
  if self.ext.logSuccess {
    print_string "Did tap";
    print_newline ()
  } else {
    ()
  };

let logTapSuccess self =>
  if self.ext.logSuccess {
    print_string "Did tap";
    print_newline ()
  };

let loop appTime frameTime => {
  if hasSetup.contents {
    setupScene ();
    renderIntoTop ();
    hasSetup.contents <- true
  };
  process appTime frameTime
};

/* These parens should be kept around the entire last if/then/else */
if something {
  if somethingElse {
    ()
  } else {
    "blah"
  }
};

/* These parens should be kept around just the last if/then*/
if something {
  if somethingElse {
    ()
  } else {
    "blah"
  }
};

/* Parens should be generated to wrap the entire final if then else.
 * To test that it's being parsed correclty, should print "one". */
if true {
  if true {
    print_string "one"
  } else {
    print_string "two"
  }
};

/* Should print two */
if true {
  if false {
    print_string "one"
  } else {
    print_string "two"
  }
};

/* Should not print */
if false {
  if true {
    print_string "one"
  } else {
    print_string "two"
  }
};

/* Should wrap (if a > b then a else b).
 * printer(
 */
let printIfFirstArgGreater = true;

let result =
  if printIfFirstArgGreater {
    fun a b =>
      if (a > b) {
        print_string "a > b"
      } else {
        print_string "b >= a"
      }
  } else if (
    fun a b =>
      if (a > b) {
        print_string "b < a"
      } else {
        print_string "a <= b"
      }
  ) {
    print_string "That could never possibly type check";
    print_newline ()
  };

let myRecord = {
  nestedRecord: {
    anotherNestedRecord:
      fun instaComp displayRect =>
        if (
          Graphics.cgRectIntersectsWithSlop
            defaultCompositeTimerRectSlop 
            instaComp.relativeRect 
            displayRect
        ) {
          IoEligible
        } else {
          IoInelibleButTryComposition
        }
  }
};

if printIfFirstArgGreater {
  fun a b =>
    if (a > b) {
      print_string "a > b"
    }
} else {
  fun a b =>
    if (a > b) {
      print_string "b < a"
    }
};

/* Should Be Parsed As: Cleary a type error, but at least the parsing makes that clear */
if printIfFirstArgGreater {
  fun a b =>
    if (a > b) {
      print_string "a > b"
    } else {
      fun a b =>
        if (a > b) {
          print_string "b < a"
        }
    }
};

fun a b =>
  if (a > b) {
    print_string "a > b"
  };

/* What you probably wanted was: */
if printIfFirstArgGreater {
  fun a b =>
    if (a > b) {
      print_string "a > b"
    }
} else {
  fun a b =>
    if (a > b) {
      print_string "b < a"
    }
};

/* Mutative if statement: Not used to evaluate to something. */
if (10 < 100) {
  let msg = "If there was any doubt, 10 is in fact less than 100.";
  print_string msg
} else {
  let msg = "All bets are off.";
  print_string msg
};

if (10 < 100) {
  print_string "If there was any doubt, 10 is in fact less than 100."
} else {
  print_string "All bets are off."
};

/**                            TYPE CONSTRAINTS
 *============================================================================
*/
let x: int = 10;

let x: int = 10;

let x: int = 10;

let x: int = (10: int);

/* let (x:int) = (10:string); */
/* let (x:string) = ("hello":int); */
/**                            TUPLES
 *============================================================================
 */
/* In SugarML, types look like the data they model! Tuples are no exception. */
type pairOfInts = (int, int);

let letBindingWithTypeConstraint: int = 10;

let (tupleItem: int, withTypeConstraint: int) = (
  10, 
  20
);

/* To make sure that tuple field annotations are annotating the entire field */
let _dummyFunc x => 10;

let annotatingFuncApplication = (
  _dummyFunc "a": int, 
  _dummyFunc "a": int
);

/* Pretty printer might stick the [int] at the label. */
let annotatingSingleFuncApplication: int = _dummyFunc "a";

/* So lets try a place where it won't */
let annotatingSingleFuncApplication = {
  /* Commenting a let binding. */
  let a = 100;
  /* Commenting another let binding. */
  let int = 200;
  /*
   * This demonstrates why named arguments cannot simply have the form (func
   * arg:val) - it is indistinguishable from a type constraint.
  */
  2 + (_dummyFunc a: int)
};

let (
  tupleItem: int, 
  constrainedWithoutGrouping: int
) = (
  10, 
  20
);

let (tupleItem, withOutsideTypeConstraint): (
  int, 
  int
) = (
  10, 
  20
);

/**                        Immutable Lists
 * ============================================================================
 */
/* Anatomy:        -Head-      --------- Tail---------  nil: You can't see nil */
let x: list int = [1, 2, 3, 4, 5, 6, 7, 8, 9];

let hd = "appendedToHead";

let tl = ["listTo", "append", "to"];

/* To push *one* and only *one* item to the front of a list - use [hd, ...tl] */
let result: list string = [hd, ...tl];

/* Is the same as writing */
let result: list string = [
  "appendedToHead", 
  "listTo", 
  "append", 
  "to"
];

/* To operate on lists, use pattern matching */
let rec size =
  fun | [] => 0 
      | [hd, ...tl] => 1 + size tl;

/* Optimize for tail recursion */
let rec size soFar lst =>
  switch lst {
    | [] => 0
    | [hd, ...tl] => size (soFar + 1) tl
  };

let nestedMatch lstLst =>
  switch lstLst {
    | [hd, ...tl] when false => 10
    | [hd, ...tl] =>
        switch tl {
          | [] => 0 + 0
          | [tlHd, ...tlTl] => 0 + 1
        }
    | [] => 0
  };

let nestedMatchWithWhen lstLst =>
  switch lstLst {
    | [hd, ...tl] when false => 10
    | [hd, ...tl] when true =>
        switch tl {
          | [] when false => 0 + 0
          | [] when true => 0 + 0
          | [tlHd, ...tlTl] => 0 + 1
        }
    | [] => 0
  };

/**
 * Aliasing with "as" during matches.
 */
type mine = | MyThing of int | YourThing of int;

/*
 * SugarML parses "as" aliases differently than OCaml.
*/
let ppp =
  switch (MyThing 20) {
    | MyThing x as ppp
    | YourThing x as ppp => ppp
  };

let MyThing _ as ppp | YourThing _ as ppp = ppp;

/*
 * in order to achieve the previous example in ocaml, you would have to group
 * as:
 */
let ppp =
  switch (MyThing 20) {
    | MyThing x as ppp
    | YourThing x as ppp => ppp
  };

let MyThing _ as ppp | YourThing _ as ppp = ppp;

/*
 * But this isn't needed in Reason because OR patterns have much lower
 * precedence - they should be pretty printed in the same way.
 */
/* TODO: */
/* let rec nestedMatch lstLst => match lstLst with { */
/*   hd::tl: match tl with { */
/*     []: 0 + 0, */
/*     tlHd::tlTl: 0 + 1, */
/*   }, */
/*   []: 0 */
/* }; */
/*  */
/**                               ARRAYS
 * ============================================================================
 * Arrays are weird looking. Usually you want lists because they support pattern
 * matching - that's why they have nicer syntax - to entice you. But if you want
 * random access and better control over memory layout, use arrays.
 */
let emptyArray = [||];

let arrayWithOne = [|10|];

let arrayWithTwo = [|10, 10|];

let secondItem = arrayWithTwo.(1);

/* Getting And Setting: Yeah, we should really change this */
/* Get an array item at index 1 */
let secondItem = arrayWithTwo.(1);

/* Set an array item at index 1 */
arrayWithTwo.(1) <- 300;

/**
 *                                STRINGS
 *  ============================================================================
 *  The language supports mutating strings, but that should not be depended upon.
 */
let myString = "asdf";

myString.[2] <- '9';

/* Replacing a character: I could do without this sugar */
/*                           FUNCTIONS
 *=============================================================================
 */
/*                           TYPE ANNOTATIONS
 * =============================================================================
 */
let one = 900;

let two = 10000;

/* Tuple expressions can be annotated without additional paren wrapping */
let myTuple = (one: int, two: int);

type myTupleType = (int, int);

let myTuple: myTupleType = myTuple;

/* Anything *outside* of a tuple, must still be annotated within parens. */
let myTuple: myTupleType = (one: int, two: int);

/* Now functions that accept a single argument being a tuple look familiar */
let addValues (a: int, b: int) => a + b;

/* Impossible to annotate return values of fun lambdas - just like in OCaml */
let addValues (a: int, b: int) => a + b;

let functionReturnValueType 
    (i: int, s: string) 
    :(int => int) =>
  fun x => x + 1;

let curriedFormOne (i: int, s: string) =>
  s ^ string_of_int i;

let curriedFormTwo (i: int, x: int) :(int, int) => (
  i, 
  x
);

/* let nonCurriedFormTwo = fun (i:int, x:int) (:(int, int)) => (i, x); */
let curriedFormThree 
    (i: int, (a: int, b: int): (int, int)) 
    :(int, int, int) => (
  i, 
  a, 
  b
);

/* let nonCurriedFormThree = fun (i:int, (a:int, b:int):(int, int)) (:(int, int, int)) => (i, a, b);  */
/** TODO: But this, however doesn't work.
 *  let (myCurriedFunc: int => int) a => a;
 *  Note: This is likely because only "simple patterns" are accepted as constraints
 *  in let bindings - that may be easy to change.
 */
type myFuncType = (int, int) => int;

let myFunc: myFuncType = fun (a, b) => a + b;

let funcWithTypeLocallyAbstractTypes 
    (type atype) 
    (type btype) 
    a 
    b 
    (c: atype => btype => unit) =>
  c a b;

/**
 * Records:
 *=============================================================================
 */
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

let anotherRecord = {
  ...testRecord, 
  name: "joe++", 
  age: testRecord.age + 10
};

let makeRecordBase () => {
  name: "Joe", 
  age: 30, 
  occupation: "Engineer"
};

let anotherRecord = {
  /* These parens should be evaporated. */ 
  ...makeRecordBase (), 
  name: "joe++", 
  age: testRecord.age + 10
};

let anotherRecord = {
  /* Comments should be correctly placed before ... expression */ 
  ...makeRecordBase (), 
  /* Comment after record extension */ 
  name: "joe++", 
  age: testRecord.age + 10
};

let anotherRecord = {
  /* Currently, type annotations must be wrapped in parens - that's easy to improve */ 
  ...(makeRecordBase (): withThreeFields), 
  name: "joe++", 
  age: testRecord.age + 10
};

let anotherRecord = {
  /* This is meaningless, sure */ 
  ...someArray.[0] <- 20, 
  name: "joe++", 
  age: testRecord.age + 10
};

let anotherRecord = {
  ...SomeReally.longFunctionCall {
    passingRecordField: 0, 
    andThisOtherRecordField: 10
  }, 
  name: "joe++", 
  age: testRecord.age + 10
};

let anotherRecord = {
  ...
    SomeReally.longFunctionCall
      withArguments (thatWrap: bool), 
  name: "joe++", 
  age: testRecord.age + 10
};

let anotherRecord = {
  ...
    SomeReally.longFunctionCall
      withArg 
      [
        "and", 
        "final", 
        "list", 
        "that", 
        "should", 
        "break"
      ], 
  name: "joe++", 
  age: testRecord.age + 10
};
let logTapSuccess self =>
  if (self > other) {
    print_string "Did tap";
    print_newline ()
  } else {
    ()
  };

let something =
  if self.ext.logSuccess {
    print_string "Did tap";
    print_newline ()
  };

let logTapSuccess self =>
  if self.ext.logSuccess {
    print_string "Did tap";
    print_newline ()
  } else {
    ()
  };

if (
  if x {
    true
  } else {
    false
  }
) {
  true
} else {
  false
};

/* Parens are required around if if it's an argument - this is the same as before. */
if (
  callSomeFunction (
    if true {
      true
    } else {
      false
    }
  )
) {
  true
} else {
  false
};

/* Notice that to do something strange, your code must *look* strange. */
/* That's generally a good thing */
if callSomeFunction {
  if true {
    true
  }
} else {
  false
};

if (
  callSomeFunction {
    thisIsAnArgument;
    notTheControlFlow
  }
) {
  thisIsTheControlFlow
};

/* The braces around the test conditions of if statements are not required.
 * The only requirement is that the test conditions be "simple".
 * The "then" body only need be simple but the parser will print it as a
 * sequence (this is a lossless process - nothing changes about the AST).
 *
 * The else body doesn't even need to be simple (hence the nesting of else if),
 * but the printer will print it inside a simple sequence, unless it
 * can make it prettier by nesting the else ifs.
 *
 */
if printIfFirstArgGreater {
  simpleThen
} else {
  thisDoesnt even have2 be simple
};

if (
  if x {
    true
  } else {
    false
  }
) {
  ()
} else {
  ()
};

/**                            TERNARY
 *============================================================================
 */
let ternaryResult =
  something ?
    callThisFunction withThisArg : thatResult;

let annotatedTernary = true && (
  something ? true : false: bool
);

let annotatedBranch = true && (
  something ? (true: bool) : false: bool
);

/* The following should be... */
let whatShouldThisBeParsedAs =
  something ?
    callThisFunction withThisArg : 
    trailingTest ? true : false;

/* ... it should be parsed as */
let whatShouldThisBeParsedAs =
  something ?
    callThisFunction withThisArg : 
    trailingTest ? true : false;

/* Should *not* be parsed as */
let whatShouldThisBeParsedAs =
  (
    something ?
      callThisFunction withThisArg : trailingTest
  ) ?
    true : false;

let ternaryResult =
  aaaaaa ?
    bbbbbbb : 
    ccccc ? ddddddd : eeeee ? fffffff : ggggg;

/* Should be parsed as: */
let ternaryResult =
  aaaaaa ?
    bbbbbbb : 
    ccccc ? ddddddd : eeeee ? fffffff : ggggg;

let ternaryResult =
  /* The first Parens *must* be preserved! */
  (x ? y : z) ?
    bbbbbbb : 
    ccccccc ? ddddddd : eeeeeee ? fffffff : ggggg;

let ternaryResult =
  aaaaaaa ?
    bbbbbbb : 
    /* The second Parens *must* be preserved! */  
    (x ? y : z) ?
      ddddddd : eeeeeee ? fffffff : ggggg;

let ternaryResult =
  aaaaaaa ?
    bbbbbbb : 
    x ?
      y : 
      z ?
        ddddddd : 
        /* The final parent don't need to be preserved */  
        eeeeeee ? fffffff : x ? y : z;

let addOne x => x + 1;

let result =
  addOne 0 + 0 > 1 ?
    print_string "this wont print" : 
    print_string "this will";

/*
 * Should be parsed as:
 */
let result =
  addOne 0 + 0 > 1 ?
    print_string "this wont print" : 
    print_string "this will";

/*
 * Try shouldn't be aliased as ternary!
 */
let res =
  try something {
    | true => "hi"
    | false => "bye"
  };

/*
 * Many levels of if elseif should be formatted very nicely.
 */
let result =
  if something {
    Console.log "First Branch"
  } else if anotherThing {
    Console.log "Second Branch"
  } else if yetAnotherThing {
    Console.log "Third Branch"
  } else {
    Console.log "Final Case"
  };

/*
 * Ternaries are simply switch statements on true/false. It's nice that there
 * is a distinction between if and switch (even though if could have just been
 * sugar on top of switch) because it allows us to use switching on true/false
 * as yet another pun for if/then that should be *preserved* as being distinct
 * from if/then (the ternary).
 */
let res = someExpression ? "true" : "false";

let pngSuffix =
  pixRation > 1 ?
    "@" ^ string_of_int pixRation ^ "x.png" : 
    ".png";
let run () => TestUtils.printSection "Polymorphism";

type myType 'a = list 'a;

type myTwoParamType 'a 'b = ('a, 'b);

type myTupleType = (int, int);

type myPolymorphicTupleType 'a = ('a, 'a);

type intListTranformer = list int => list int;

type x = list (int, string);

let module HoldsAType = {
  type hasPrime 'a 'b 'c =
    Hashtbl.t (list 'a) (list 'b);
};

type myType2 =
  myTwoParamType (myType (int => int)) int => int;

/* Confusing because => looks like part
                                     of the return type signature. */
let myFunc 
    (a: int => int) 
    (b: int => int) 
    :myType int => [
  a 20 + b 30
];

let myFunc 
    (a: int => int) 
    (b: int => int) 
    :(myType int => myType int) =>
  fun lst => lst;

let certainlyRequiresWrapping:
  option (Mod.handler p re, Mod.Types.handler) => 
  option
    (
      Mod.touch props (props, state) resource, 
      (list Mod.t, list Mod.t)
    ) => 
  list (Mod.update props (props, state) resource) => 
  list (Mod.update props (props, state) resource) =
  ();

/* Because of the confusion in the last two examples, I believe we should
 switch back to the `=` based syntax.

   let add a b = a + b;

   Pexp_function printing:

   Decide on either:

     let add Some (Hearts n) = n + n
       | add Some (Diamonds n) = 0
       | add Some (Spades n) = 0
       | add None = 0
       | _ = 0

   Or:
     let add = x => match x with
       | Some (Hearts n) => n + n
       | Some (Diamonds n) => 0
       | Some (Spades n) => 0
       | None => 0
       | _ => 0

     let add =
       | Some (Hearts n) => n + n
       | Some (Diamonds n) => 0
       | Some (Spades n) => 0
       | None => 0
       | _ => 0

   let myFunc = (a:int) (b:int) => a + b;

 */
/* Fringe features */
/*
  /* This parses, but doesn't type check */
  let module TryExtendingType = {type t = Hello of string;};
  type TryExtendingType.t += LookANewExtension of string;
*/let run () => TestUtils.printSection "Modules";

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

let myInt: MySecondModule.someType = 100;

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
    let x: i = 10;
  };
  /* Notice how local modules names may be used twice and are shadowed */
  let module MyLocalModule: MySecondModuleType = {
    type someType = int;
    let x: someType = 10;
    let y: someType = 20;
  };
  let tmp = MyLocalModule.x + 22;
  tmp + 30
};

module type HasTT = {type tt;};

let module SubModule: HasTT = {type tt = int;};

module type HasEmbeddedHasTT = {
  let module SubModuleThatHasTT = SubModule;
};

module type HasPolyType = {type t 'a;};

module type HasDestructivelySubstitutedPolyType =
  HasPolyType with type t 'a := list 'a;

module type HasDestructivelySubstitutedSubPolyModule = {
  /* Cannot perform destructive substitution on submodules! */ 
  /* module X: HasPolyType with type t := list (int, int); */ 
  let module X:
    HasDestructivelySubstitutedPolyType;
};

module type HasSubPolyModule = {
  /* Cannot perform destructive substitution on submodules! */ 
  /* module X: HasPolyType with type t := list (int, int); */ 
  let module X: HasPolyType;
};

let module EmbedsSubPolyModule: HasSubPolyModule = {
  let module X = {type t 'a = list 'a;};
};

let module
  EmbedsDestructivelySubstitutedPolyModule:
  HasDestructivelySubstitutedSubPolyModule = {
  let module X = {type t = list (int, int);};
};

module type HasMultiPolyType = {
  type substituteThis 'a 'b; 
  type substituteThat 'a 'b;
};

module type HasDestructivelySubstitutedMultiPolyType =
  HasMultiPolyType with
    type substituteThis 'a 'b := Hashtbl.t 'a 'b and 
    type substituteThat 'a 'b := Hashtbl.t 'a 'b;

let module InliningSig: {let x: int; let y: int;} = {
  /*
   * Comment inside of signature.
   */
  let x = 10;
  /* Inline comment inside signature. */
  let y = 20;
};

let module MyFunctor (M: HasTT) => {
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
let module MyFunctorResult = MyFunctor {
  type tt = string;
};

let module LookNoParensNeeded = MyFunctor {
  type tt = string;
};

module type SigResult = {let result: int;};

module type ASig = {let a: int;};

module type BSig = {let b: int;};

let module AMod = {
  let a = 10;
};

let module BMod = {
  let b = 10;
};

let module CurriedSugar (A: ASig) (B: BSig) => {
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
let module CurriedSugarWithReturnType 
           (A: ASig) 
           (B: BSig) 
           :SigResult => {
  let result = A.a + B.b;
};

/* This is parsed as being equivalent to the above example */
let module CurriedSugarWithAnnotatedReturnVal 
           (A: ASig) 
           (B: BSig) 
           :SigResult => {
  let result = A.a + B.b;
};

let module CurriedNoSugar (A: ASig) (B: BSig) => {
  let result = A.a + B.b;
};

let letsTryThatSyntaxInLocalModuleBindings () => {
  let module CurriedSugarWithReturnType 
             (A: ASig) 
             (B: BSig) 
             :SigResult => {
    let result = A.a + B.b;
  };
  let module CurriedSugarWithAnnotatedReturnVal 
             (A: ASig) 
             (B: BSig) 
             :SigResult => {
    let result = A.a + B.b;
  };
  let module CurriedNoSugar (A: ASig) (B: BSig) => {
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
  let module TempModule =
    CurriedNoSugar AMod BMod;
  let module TempModule2 =
    CurriedSugarWithAnnotatedReturnVal AMod BMod;
  TempModule.result + TempModule2.result
};

module type EmptySig = {};

let module MakeAModule (X: EmptySig) => {
  let a = 10;
};

let module CurriedSugarFunctorResult =
  CurriedSugar AMod BMod;

let module CurriedSugarFunctorResultInline =
  CurriedSugar
    {
      let a = 10;
    } 
    {
      let b = 10;
    };

let module CurriedNoSugarFunctorResult =
  CurriedNoSugar AMod BMod;

let module CurriedNoSugarFunctorResultInline =
  CurriedNoSugar
    {
      let a = 10;
    } 
    {
      let b = 10;
    };

let module ResultFromNonSimpleFunctorArg =
  CurriedNoSugar (MakeAModule {}) BMod;

/* TODO: Functor type signatures should more resemble value signatures */
let curriedFunc: int => int => int =
  fun a b => a + b;

module type FunctorType =
  ASig => BSig => SigResult;

/* Which is sugar for:*/
module type FunctorType2 =
  ASig => BSig => SigResult;

/* Just for compability with existing OCaml ASTs you can put something other
* than an underscore */
module type FunctorType3 =
  (Blah: ASig) => 
  (ThisIsIgnored: BSig) => 
  SigResult;

/* The actual functors themselves now have curried sugar (which the pretty
 * printer will enforce as well */
/* The following: */
let module CurriedSugarWithAnnotation2:
  ASig => BSig => SigResult =
  functor (A: ASig) (B: BSig) => {
    let result = A.a + B.b;
  };

/* Becomes: */
let module CurriedSugarWithAnnotation:
  ASig => BSig => SigResult =
  functor (A: ASig) (B: BSig) => {
    let result = A.a + B.b;
  };

/* "functors" that are not in sugar curried form cannot annotate a return type
 * for now, so we settle for: */
let module
  CurriedSugarWithAnnotationAndReturnAnnotated:
  ASig => BSig => SigResult =
  functor (A: ASig) (B: BSig) => (
    {
      let result = A.a + B.b;
    }:
      SigResult
  );

let module ReturnsAFunctor 
           (A: ASig) 
           (B: BSig) 
           :(ASig => BSig => SigResult) =>
  functor (A: ASig) (B: BSig) => {
    let result = 10;
  };

let module ReturnsSigResult 
           (A: ASig) 
           (B: BSig) 
           :SigResult => {
  let result = 10;
};

let module ReturnsAFunctor2 
           (A: ASig) 
           (B: BSig) 
           :(ASig => BSig => SigResult) =>
  functor (A: ASig) (B: BSig) => {
    let result = 10;
  };

/*
 * Recursive modules.
 * TODO: Test [Psig_recmodule]
 */
let module rec A: {
  type t = | Leaf of string | Node of ASet.t; 
  let compare: t => t => int;
} = {
  type t = | Leaf of string | Node of ASet.t;
  let compare t1 t2 =>
    switch (t1, t2) {
      | (Leaf s1, Leaf s2) =>
          Pervasives.compare s1 s2
      | (Leaf _, Node _) => 1
      | (Node _, Leaf _) => (-1)
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

let module List (X: Type) => {type t = list X.t;};

let module Maybe (X: Type) => {
  type t = option X.t;
};

let module Id (X: Type) => X;

let module Compose 
           (F: Type => Type) 
           (G: Type => Type) 
           (X: Type) => F (
  G X
);

let l: Compose(List)(Maybe)(Char).t = [Some 'a'];

let module Example2 (F: Type => Type) (X: Type) => {
  let iso (a: Compose(Id)(F)(X).t) :F(X).t => a;
};

Printf.printf
  "\nModules And Functors: %n\n" 
  CurriedNoSugarFunctorResultInline.result;

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
  let myValue = {recordField: "hello"};
};

module type HasInt = {let x: int;};

let module MyModule = {
  let x = 10;
};

let myFirstClass: (module HasInt) =
  (module MyModule);

let myFirstClassWillBeFormattedAs: (module HasInt) =
  (module MyModule);

let acceptsAndUnpacksFirstClass 
    ((module M): (module HasInt)) => M.x + M.x;

let acceptsAndUnpacksFirstClass 
    ((module M): (module HasInt)) => M.x + M.x;

let module SecondClass = (val myFirstClass);

let module SecondClass2 = (
  val ((module MyModule): (module HasInt))
);

let p = SecondClass.x;

/* Opening Modules */
let module M = {let module Inner = {};};

let module N = {
  open M;
  let z = {
    let open M;
    34
  };
  let y = 44;
};

open M;

open M.Inner;

open M;
let module LocalModule = {
  type accessedThroughModule =
    | AccessedThroughModule;
  type accessedThroughModuleWithArg =
    | AccessedThroughModuleWith of int 
    | AccessedThroughModuleWithTwo of int int;
};

type notTupleVariant =
  | NotActuallyATuple of int int;

type notTupleVariantExtraParens =
  | NotActuallyATuple2 of int int;

type simpleTupleVariant =
  | SimpleActuallyATuple of (int, int);

type tupleVariant =
  | ActuallyATuple of (int, int);

let intTuple = (20, 20);

let notTupled: notTupleVariant =
  NotActuallyATuple 10 10;

/* Doesn't work because we've correctly annotated parse tree nodes with explicit_arity! */
/* let notTupled: notTupleVariant = NotActuallyATuple (10, 10); */
let funcOnNotActuallyATuple 
    (NotActuallyATuple x y) => x + y;

/* let funcOnNotActuallyATuple (NotActuallyATuple (x, y)) => x + y; */
/* let notTupled: notTupleVariant = NotActuallyATuple intTuple; /*Doesn't work! */ */
/* At least the above acts as proof that there *is* a distinction that is
honored. */
let simpleTupled: simpleTupleVariant =
  SimpleActuallyATuple (10, 10);

let simpleTupled: simpleTupleVariant =
  SimpleActuallyATuple intTuple;

/*Works! */
let NotActuallyATuple x y =
  NotActuallyATuple 10 20;

/* Doesn't work because we've correctly annotated parse tree nodes with explicit_arity! */
/* let unfortunatelyThisStillWorks: simpleTupleVariant = SimpleActuallyATuple 10 10; */
let yesTupled: tupleVariant =
  ActuallyATuple (10, 10);

let yesTupled: tupleVariant =
  ActuallyATuple (10, 10);

let yesTupled: tupleVariant =
  ActuallyATuple intTuple;

type threeForms =
  | FormOne of int | FormTwo of int | FormThree;

let doesntCareWhichForm x =>
  switch x {
    | FormOne q
    | FormTwo q => 10
    | FormThree => 20
  };

let doesntCareWhichFormAs x =>
  switch x {
    | FormOne q as ppp
    | FormTwo q as ppp => 10
    | FormThree => 20
  };

type colorList1 = [
  otherThingInheritedFrom 
  | `Red 
  | `Black
];

type colorList = [<
  | `Red of (int, int) &int 
  | `Black of &(int, int) &int 
  | `Blue 
  > `Red `Black
];

1 + doesntCareWhichForm (FormOne 10);

1 + doesntCareWhichForm (FormTwo 10);

1 + doesntCareWhichForm FormThree;

/* Destructured matching at function definition */
let accessDeeply 
    LocalModule.AccessedThroughModule => 10;

let accessDeeplyWithArg 
    (
      LocalModule.AccessedThroughModuleWith x | 
      LocalModule.AccessedThroughModuleWithTwo
        _ x
    ) => x;

/* Destructured matching *not* at function definition */
let accessDeeply x =>
  switch x {
    | LocalModule.AccessedThroughModule => 10
    | _ => 0
  };

let accessDeeplyWithArg x =>
  switch x {
    | LocalModule.AccessedThroughModuleWith x => 10
    | _ => 0
  };

/* In OCaml's syntax, to capture the wrapped data, you do:
 *
 *   let myFunc x = function | `Blah (p as retVal) -> retVal`
 *
 * In OCaml's syntax, to capture the entire pattern you do:
 *
 *   let myFunc x = function | `Blah p as retVal -> retVal`
 */
let accessDeeply x =>
  switch x {
    | LocalModule.AccessedThroughModule as ppp => 1
  };

let accessDeeplyWithArg x =>
  switch x {
    | LocalModule.AccessedThroughModuleWith (
        x as retVal
      ) =>
        retVal + 1
    | LocalModule.AccessedThroughModuleWithTwo
        (x as retVal1) (y as retVal2) =>
        retVal1 + retVal2 + 1
  };

/* Just to show that by default `as` captures much less aggresively */
let rec accessDeeplyWithArgRecursive x count =>
  switch x {
    | LocalModule.AccessedThroughModuleWith x as entirePattern =>
        /* It captures the whole pattern */
        if (count > 0) {
          0
        } else {
          accessDeeplyWithArgRecursive
            entirePattern (count - 1)
        }
    | LocalModule.AccessedThroughModuleWithTwo
        x y as entirePattern =>
        /* It captures the whole pattern */
        if (count > 0) {
          0
        } else {
          accessDeeplyWithArgRecursive
            entirePattern (count - 1)
        }
  };

accessDeeplyWithArgRecursive
  (LocalModule.AccessedThroughModuleWith 10) 10;

let run () => {
  TestUtils.printSection "Variants";
  Printf.printf "%d %d \n" x y
};

type combination 'a =
  | HeresTwoConstructorArguments of int int;

/** But then how do we parse matches in function arguments? */
/* We must require parenthesis around construction matching in function args only*/
let howWouldWeMatchFunctionArgs 
    (HeresTwoConstructorArguments x y) => x + y;

/* How would we annotate said arg? */
let howWouldWeMatchFunctionArgs 
    (
      HeresTwoConstructorArguments x y:
        combination 'wat
    ) => x + y;

let matchingTwoCurriedConstructorsInTuple x =>
  switch x {
    | (
        HeresTwoConstructorArguments x y, 
        HeresTwoConstructorArguments a b
      ) =>
        x + y + a + b
  };

type twoCurriedConstructors =
  | TwoCombos of
      (combination int) (combination int);

let matchingTwoCurriedConstructorInConstructor x =>
  switch x {
    | TwoCombos
        (HeresTwoConstructorArguments x y) 
        (HeresTwoConstructorArguments a b) =>
        a + b + x + y
  };

type twoCurriedConstructorsPolyMorphic 'a =
  | TwoCombos of
      (combination 'a) (combination 'a);

/* Matching records */
type pointRecord = {x: int, y: int};

type alsoHasARecord =
  | Blah | AlsoHasARecord of int int pointRecord;

let result =
  switch (AlsoHasARecord 10 10 {x: 10, y: 20}) {
    | Blah => 1000
    | AlsoHasARecord a b {x, y} => a + b + x + y
  };

let rec commentPolymorphicCases: 
  'a .
  option 'a => int
 =
  fun | Some a => 1 
      /* Comment on one */ 
      | None => 0;

let thisWontCompileButLetsSeeHowItFormats =
  switch something {
    | Zero
    | One => 10
  };

let thisWontCompileButLetsSeeHowItFormats =
  fun | Zero
      | One _ _ _ => 10 
      | Two => 20;

/* Comment on two */
/**
 * GADTs.
 */
type term _ =
  | Int of int :term int 
  | Add :term (int => int => int) 
  | App of (term ('b => 'a)) (term 'b) :term 'a;

let rec eval: type a. term a => a =
  fun | Int n => n 
      /* a = int */ 
      | Add => (fun x y => x + y) 
      /* a = int => int => int */ 
      | App f x => (eval f) (eval x);

let rec eval: type a. term a => a =
  fun x =>
    switch x {
      | Int n => n
      /* a = int */
      | Add => (fun x y => x + y)
      /* a = int => int => int */
      | App f x => (eval f) (eval x)
    };

/* eval called at types (b=>a) and b for fresh b */
let evalArg = App (App Add (Int 1)) (Int 1);

let two = eval (App (App Add (Int 1)) (Int 1));

type someVariant =
  | Purple of int | Yellow of int;

let Purple x | Yellow x =
  switch (Yellow 100, Purple 101) {
    | (Yellow y, Purple p) => Yellow (p + y)
    | (Purple p, Yellow y) => Purple (y + p)
    | (Purple p, Purple y) => Yellow (y + p)
    | (Yellow p, Yellow y) => Purple (y + p)
  };

type tuples =
  | Zero 
  | One of int 
  | Two of int int 
  | OneTuple of (int, int);

let myTuple = OneTuple (20, 30);

let res =
  switch myTuple {
    | Two x y =>
        try (Two x y) {
          | One => "hi"
          | Two => "bye"
        }
    | One =>
        switch One {
          | One => "hi"
          | _ => "bye"
        }
  };

/* FIXME type somePolyVariant = [ `Purple of int | `Yellow of int]; */
let ylw = `Yellow (100, 100);

let prp = `Purple (101, 100);

let res =
  switch (ylw, prp) {
    | (`Yellow (y, y2), `Purple (p, p2)) =>
        `Yellow (p + y, 0)
    | (`Purple (p, p2), `Yellow (y, y2)) =>
        `Purple (y + p, 0)
    | (`Purple (p, p2), `Purple (y, y2)) =>
        `Yellow (y + p, 0)
    | (`Yellow (p, p2), `Yellow (y, y2)) =>
        `Purple (y + p, 0)
  };

let ylw = `Yellow 100;

let prp = `Purple 101;

let res =
  switch (ylw, prp) {
    | (`Yellow y, `Purple p) => `Yellow (p + y)
    | (`Purple p, `Yellow y) => `Purple (y + p)
    | (`Purple p, `Purple y) => `Yellow (y + p)
    | (`Yellow p, `Yellow y) => `Purple (y + p)
  };

/*
 * Now try polymorphic variants with *actual* tuples.
 * You'll notice that these become indistinguishable from multiple constructor
 * args! explicit_arity doesn't work on polymorphic variants!
 *
 * Way to resolve this (should also work for non-polymorphic variants):
 *
 * If you see *one* simple expr list that is a tuple, generate:
 *    Pexp_tuple (Pexp_tuple ..))
 *
 * If you see *one* simple expr list that is *not* a tuple, generate:
 *    Pexp..
 *
 * If you see *multiple* simple exprs, generate:
 *    Pexp_tuple..
 *
 * Though, I'm not sure this will even work.
 */
let ylw = `Yellow (100, 100);

let prp = `Purple (101, 101);

let res =
  switch (ylw, prp) {
    | (`Yellow (y, y2), `Purple (p, p2)) =>
        `Yellow (p + y, 0)
    | (`Purple (p, p2), `Yellow (y, y2)) =>
        `Purple (y + p, 0)
    | (`Purple (p, p2), `Purple (y, y2)) =>
        `Yellow (y + p, 0)
    | (`Yellow (p, p2), `Yellow (y, y2)) =>
        `Purple (y + p, 0)
  };

let rec atLeastOneFlushableChildAndNoWipNoPending 
        composition 
        atPriority =>
  switch composition {
    | [] => false
    | [hd, ...tl] =>
        switch hd {
          | OpaqueGraph {
              lifecycle: Reconciled (_, [])
            } =>
              atLeastOneFlushableChildAndNoWipNoPending
                tl atPriority
          | OpaqueGraph {
              lifecycle:
                ReconciledFlushable (
                  priority, 
                  _, 
                  _, 
                  _, 
                  _, 
                  _
                )
            }
          | OpaqueGraph {
              lifecycle:
                NeverReconciledFlushable (
                  priority, 
                  _, 
                  _, 
                  _, 
                  _
                )
            }
              when priority == AtPriority =>
              noWipNoPending tl atPriority
          | SuperLongNameThatWontBreakByItselfSoWhenWillHaveToBreak
              when
                priority ==
                  AtPrasldkfjalsdfjasdlfalsdkf =>
              noWipNoPending tl atPriority
          | _ => false
        }
  };

/*
 * When pretty printed, this appears to be multi-argument constructors.
 */
let prp = `Purple (101, 101);

let res =
  switch prp {
    | `Yellow (y, y2) => `Yellow (y2 + y, 0)
    | `Purple (p, p2) => `Purple (p2 + p, 0)
  };

/*
 * Testing explicit arity.
 */
let rec map f =>
  fun | Node None m =>
          Node None (M.map (map f) m) 
      | Node LongModule.Path.None m =>
          Node None (M.map (map f) m) 
      | Node (LongModule.Path.Some v) m =>
          Node (Some (f v)) (M.map (map f) m);

let myFunc x y None => "asdf";

let rec map f =>
  fun | Node None m =>
          Node None (M.map (map f) m) 
      | Node LongModule.Path.None m =>
          LongModule.Path.Node
            LongModule.Path.None 
            (M.map (map f) m) 
      | Node (LongModule.Path.Some v) m =>
          LongModule.Path.Node
            (LongModule.Path.Some (f v)) 
            (M.map (map f) m);

let myFunc x y LongModule.Path.None => "asdf";

let listPatternMembersNeedntBeSimple x =>
  switch x {
    | [] => ()
    | [Blah x y, Foo a b, ...rest] => ()
    | [Blah x y, Bar a b, ...rest] => ()
    | _ => ()
  };

let listTailPatternNeedntBeSimple x =>
  switch x {
    | [] => ()
    /* Although this would never typecheck! */
    | [Blah x y, Foo a b, ...Something x] => ()
    | _ => ()
  };

let listPatternMayEvenIncludeAliases x =>
  switch x {
    | [] => ()
    /* Although this would never typecheck! */
    | [
        Blah x y as head, 
        Foo a b as head2, 
        ...Something x as tail
      ] =>
        ()
    | _ => ()
  };
/* Run the formatting pretty printer with width 50 */
/*
 * Testing infix wrapping
 */
let reallyLongIdent = 100;

let andYetAnotherReallyLongIdent = 30;

let something =
  reallyLongIdent +
    andYetAnotherReallyLongIdent + 
    reallyLongIdent;

let something =
  /* Hopefully */
  reallyLongIdent +
    /* It will indent like this */  
    andYetAnotherReallyLongIdent + 
    /* And no further */  
    reallyLongIdent;

/*
 * Even though the precedence of the operators are different, no
 * "simplification" grouping is needed.
 */
let testPrintingPrecedence =
  reallyLongIdent +
    reallyLongIdent * andYetAnotherReallyLongIdent + 
    reallyLongIdent;

let testPrintingPrecedence =
  reallyLongIdent +
    /*
     * In this case, grouping of the right expression is needed because the
     * right side of the infix operator is of *lower* precedence than STAR.
     */  
    reallyLongIdent * (
      reallyLongIdent + andYetAnotherReallyLongIdent
    ) + 
    reallyLongIdent * 10;

let testPrintingPrecedence =
  reallyLongIdent +
    /*
     * In this case, grouping of the right expression is needed because the
     * right side of the infix operator is of *lower* precedence than STAR.
     */  
    reallyLongIdent * (
      reallyLongIdent + andYetAnotherReallyLongIdent
    ) + 
    reallyLongIdent;

let add x y => x + y;

let testPrintingPrecedence =
  reallyLongIdent +
    /*
     * In this case, grouping of the right expression is needed because the
     * right side isn't even infix at all.
     */  
    reallyLongIdent *
      add
        reallyLongIdent 
        andYetAnotherReallyLongIdent + 
    reallyLongIdent;

/*
 * Test wrapping every form of named arguments where various parts are
 * commented.
 */
let a = 10;

let b = 20;

/*A*/
let named /* a::a */ a::a /* b::b */ b::b => /* a + b */ a + b;

/*B*/
let namedAlias 
    /* a::aa */ 
    a::aa 
    /* b::bb */ 
    b::bb => /* aa + bb */ aa + bb;

/*C*/
let namedAnnot 
    /* a::(a: option int) */ 
    a::(a: option int) 
    /* b::(b: option int) */ 
    b::(b: option int) =>
  /* 20 */
  20;

/*D*/
let namedAliasAnnot 
    /* a::(aa: option int) */ 
    a::(aa: option int) 
    /* b::(bb: option int) */ 
    b::(bb: option int) =>
  /* 20 */
  20;

/*E*/
let optional 
    /* a::a=? */ 
    a::a=? 
    /* b::b=? */ 
    b::b=? 
    /* () */ 
    () =>
  /* 10 */
  10;

/*F*/
let optionalAlias 
    /* a::aa */ 
    a::aa=? 
    /* ?b:bb */ 
    b::bb=? 
    /* () */ 
    () =>
  /* 10 */
  10;

/*G*/
let optionalAnnot 
    /* a::(a: option int)=? */ 
    a::(a: option int)=? 
    /* ?b:(b: option int) */ 
    b::(b: option int)=? 
    /* () */ 
    () =>
  /* 10 */
  10;

/*H*/
let optionalAliasAnnot 
    /* a::(aa: option int)=? */ 
    a::(aa: option int)=? 
    /* b::(bb: option int)=? */ 
    b::(bb: option int)=? 
    /* () => */ 
    () =>
  /* 10 */
  10;

/*I: This one is really annoying? Where's the visual label?*/
let defOptional 
    /* a::a=10 */ 
    a::a=10 
    /* b::b=10 */ 
    b::b=10 
    /* () => */ 
    () =>
  /* 10 */
  10;

/*J*/
let defOptionalAlias 
    /* a::aa=10 */ 
    a::aa=10 
    /* b::bb=10 */ 
    b::bb=10 
    /* () => */ 
    () =>
  /* 10; */
  10;

/*K*/
let defOptionalAnnot 
    /* a::(a:int)=10 */ 
    a::(a: int)=10 
    /* b::(b:int)=10 */ 
    b::(b: int)=10 
    /* () => */ 
    () =>
  /* 10; */
  10;

/*L*/
let defOptionalAliasAnnot 
    /* a::(aa:int)=10 */ 
    a::(aa: int)=10 
    /* b::(bb:int)=10 */ 
    b::(bb: int)=10 
    /* () => */ 
    () =>
  /* 10; */
  10;

/* Invoking them */
named /* a::a */ a::a /* b::b; */ b::b;

named /* a::a */ a::a /* b::b; */ b::b;

optional /* a::a */ a::a /* b::b; */ b::b;

optional /* a::a */ a::a /* b::b; */ b::b;

let explictlyPassed =
  /* optional */
  optional
    /* a::? */ 
    /* None */ 
    a::?None 
    /* b::? */ 
    /* None; */ 
    b::?None;

let a = None;

let explictlyPassed =
  /* optional */
  optional
    /* a::? */ 
    a::?a 
    /* b::? */ 
    /* None; */ 
    b::?None;

let complex_default 
    callback::callback=(fun k d => 4) 
    x => 3;

let myList =
  /*CommentAfterEqualBefore1 */
  [1, 2, 3];

let myList = [
  1, 
  /*CommentAfterOneBeforeCons */ 
  2, 
  3
];

let myList = [
  1, 
  2, 
  /*CommentAfterTwoBeforeCons */ 
  3
];

let myList = [
  1, 
  2, 
  /*CommentAfterConsBeforeThree */ 
  3
];

let myList = [1, 2, 3];

/*CommentAfterThreeBeforeCons */
let myList = [
  1, 
  2, 
  3, 
  .../*CommentAfterConsBeforeAppendedTo */myList
];

let myList = [3, 4, 5];

let simpleListPattern x =>
  switch x {
    | [1, 2, 3] => 0
    | _ => 0
  };

type blahType = string;

let x: blahType = "asdf";

type nameAge = {age: int, name: string};

type hasABunch = {
  /*
   * Field comment
   */ 
  fieldOne: int, 
  fieldtwo: list int, 
  fieldThree: list string, 
  fieldFour: nameAge
};

/* Comment at bottom of record type def */
type functionsInARecord = {
  adder: int => int, 
  minuser: int => int
};

let myFunctionsInARecord = {
  adder: fun x => x, 
  minuser: fun x => x
};

let myFunctionsInARecordThatMustWrap = {
  /* Desired wrapping */ 
  adder:
    fun reallyLongArgument => reallyLongArgument, 
  minuser:
    fun anotherReallyLongArgument => anotherReallyLongArgument
};

/* Comment at bottom of record */
type twoArgFunctionsInARecord = {
  adder: int => int => int, 
  minuser: int => int => int
};

let myFunctionsInARecordThatMustWrap = {
  /* Desired wrapping */ 
  adder:
    fun reallyLongArgument 
        anotherReallyLongArgument => reallyLongArgument, 
  minuser:
    fun reallyLongArgument 
        anotherReallyLongArgument => reallyLongArgument + anotherReallyLongArgument
};

type threeArgFunctionsInARecord = {
  adder: int => int => int => int, 
  minuser: int => int => int => int
};

let myFunctionsInARecordThatMustWrap = {
  /* Desired wrapping */ 
  adder:
    fun /* Even if you have a comment before fun */ 
        reallyLongArgument 
        /* Or before the first arg */ 
        anotherReallyLongArgument 
        yetAnotherReallyLongArgument => reallyLongArgument, 
  minuser:
    fun reallyLongArgument 
        anotherReallyLongArgument 
        anotherReallyLongArgument => reallyLongArgument + anotherReallyLongArgument
};

let oneArgShouldWrapToAlignWith 
    theFunctionNameBinding => theFunctionNameBinding;

let twoArgsShouldWrapToAlignWith 
    firstArgHere 
    secondArgThere => secondArgThere;

let rec oneArgShouldWrapToAlignWith 
        theFunctionNameBinding => theFunctionNameBinding;

let rec twoArgsShouldWrapToAlignWith 
        firstArgHere 
        secondArgThere => secondArgThere;

let secondArgShouldWrap 
    pointLess 
    (a, b, c, d, e, f, g, h) =>
  pointLess + a + b + c + d + e;

/* Now check that one and two args both indent the same when applying */
let reallyReallyLongVarName = "hello";

let result = oneArgShouldWrapToAlignWith reallyReallyLongVarName;

let result =
  twoArgsShouldWrapToAlignWith
    reallyReallyLongVarName 
    reallyReallyLongVarName;

let justReturn x => x;

/* With default formatting settings: Two arguments are special cased in
   function application "justReturn hasABunch" */
let acceptsTwoThings 
    (nameAge: nameAge) 
    (hasABunch: hasABunch) => justReturn hasABunch;

/*
  Ideally, we'd allow "acceptsTwoThings {age, name}" on the first line, then
  wrapping the final argument across multiple, but that is difficult to tell
  the formatter "if the final argument cannot fit", but everything else can,
  then only wrap the final argument with open faced braces.  It's possible, but
  not a v1 feature of wrapping.
 */
let result =
  acceptsTwoThings
    {age: 20, name: "a"} 
    {
      fieldOne: 10, 
      fieldtwo: [10, 20], 
      fieldThree: ["one", "two"], 
      fieldFour: {age: 20, name: "joe"}
    };

let howDoesInfixOperatorsWrapWhenYouMustWrapQuestionMark 
    x 
    y 
    z =>
  x + y + z;

let howDoesInfixOperatorsWrapWhenYouMustWrapQuestionMark 
    x 
    y => x + y;

let reallyHowDoesInfixOperatorsWrapWhenYouMustWrapQuestionMark 
    x 
    y 
    z =>
  x + y + z;

let reallyHowDoesInfixOperatorsWrapWhenYouMustWrapQuestionMark 
    x 
    y => x + y;

let reallyLongFunctionNameThatJustConcats a =>
  String.concat "-" a;

let seeHowLongValuesWrap = {
  age: 30, 
  name: reallyLongFunctionNameThatJustConcats [
    "one", 
    "two", 
    "two", 
    "two", 
    "two", 
    "two", 
    "two"
  ]
};

/*
/--Everything up to the arrow is label left--\  /-The return is label right-\
                          /-append => to last-\
/-----------------------\ /--------------------\ */
let onlyReturnWraps (a, b, c, d, e, f) => (
  a, 
  b, 
  c, 
  d, 
  e, 
  f
);

let bothArgsWrapAndIndent 
    (a, b, c, d, e, f) 
    (h, i, j, k, l, m) => (
  a, 
  b, 
  c, 
  d, 
  e, 
  f
);

let result = onlyReturnWraps (
  10, 
  11, 
  12, 
  13, 
  14, 
  15
);

let result =
  bothArgsWrapAndIndent
    (10, 11, 12, 13, 14, 15) 
    (10, 11, 12, 13, 14, 15);

type sixteenTuple = (
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int
);

/* Nothing annotated */
let echoTuple 
    (
      a, 
      b, 
      c, 
      d, 
      e, 
      f, 
      g, 
      h, 
      i, 
      j, 
      k, 
      l, 
      m, 
      n, 
      o, 
      p
    ) => (
  a, 
  b, 
  c, 
  d, 
  e, 
  f, 
  g, 
  h, 
  i, 
  j, 
  k, 
  l, 
  m, 
  n, 
  o, 
  p
);

/* Nothing annotated fun */
let echoTuple 
    (
      a, 
      b, 
      c, 
      d, 
      e, 
      f, 
      g, 
      h, 
      i, 
      j, 
      k, 
      l, 
      m, 
      n, 
      o, 
      p
    ) => (
  a, 
  b, 
  c, 
  d, 
  e, 
  f, 
  g, 
  h, 
  i, 
  j, 
  k, 
  l, 
  m, 
  n, 
  o, 
  p
);

let echoTheEchoer 
    (x: sixteenTuple => sixteenTuple) 
    :(sixteenTuple => sixteenTuple) => x;

/* Nothing annotated fun, passed to func */
echoTheEchoer (
  fun (
        a, 
        b, 
        c, 
        d, 
        e, 
        f, 
        g, 
        h, 
        i, 
        j, 
        k, 
        l, 
        m, 
        n, 
        o, 
        p
      ) => (
    a, 
    b, 
    c, 
    d, 
    e, 
    f, 
    g, 
    h, 
    i, 
    j, 
    k, 
    l, 
    m, 
    n, 
    o, 
    p
  )
);

/* Argument annotated */
let echoTuple 
    (
      (
        a, 
        b, 
        c, 
        d, 
        e, 
        f, 
        g, 
        h, 
        i, 
        j, 
        k, 
        l, 
        m, 
        n, 
        o, 
        p
      ): sixteenTuple
    ) => (
  a, 
  b, 
  c, 
  d, 
  e, 
  f, 
  g, 
  h, 
  i, 
  j, 
  k, 
  l, 
  m, 
  n, 
  o, 
  p
);

/* Argument annotated fun */
let echoTuple 
    (
      (
        a, 
        b, 
        c, 
        d, 
        e, 
        f, 
        g, 
        h, 
        i, 
        j, 
        k, 
        l, 
        m, 
        n, 
        o, 
        p
      ): sixteenTuple
    ) => (
  a, 
  b, 
  c, 
  d, 
  e, 
  f, 
  g, 
  h, 
  i, 
  j, 
  k, 
  l, 
  m, 
  n, 
  o, 
  p
);

/* Argument annotated, return type annotated */
let echoTuple 
    (
      (
        a, 
        b, 
        c, 
        d, 
        e, 
        f, 
        g, 
        h, 
        i, 
        j, 
        k, 
        l, 
        m, 
        n, 
        o, 
        p
      ): sixteenTuple
    ) 
    :sixteenTuple => (
  a, 
  b, 
  c, 
  d, 
  e, 
  f, 
  g, 
  h, 
  i, 
  j, 
  k, 
  l, 
  m, 
  n, 
  o, 
  p
);

/* Desired formatting if first line fits within margin */
let makeTuple a b c d e f g h i j k l m n o p => (
  a, 
  b, 
  c, 
  d, 
  e, 
  f, 
  g, 
  h, 
  i, 
  j, 
  k, 
  l, 
  m, 
  n, 
  o, 
  p
);

/* Desired formatting if first line fits within margin (70) */
let (
  a, 
  b, 
  c, 
  d, 
  e, 
  f, 
  g, 
  h, 
  i, 
  j, 
  k, 
  l, 
  m, 
  n, 
  o, 
  p
) =
  makeTuple 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;

/* Annotated version */
let (
  a, 
  b, 
  c, 
  d, 
  e, 
  f, 
  g, 
  h, 
  i, 
  j, 
  k, 
  l, 
  m, 
  n, 
  o, 
  p
): sixteenTuple =
  makeTuple 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;

/* Annotated inline */
let x: (
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int
) =
  makeTuple 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;

let (
  a, 
  b, 
  c, 
  d, 
  e, 
  f, 
  g, 
  h, 
  i, 
  j, 
  k, 
  l, 
  m, 
  n, 
  o, 
  p
) = echoTuple (
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0
);

/* Annotated version */
let (
  a, 
  b, 
  c, 
  d, 
  e, 
  f, 
  g, 
  h, 
  i, 
  j, 
  k, 
  l, 
  m, 
  n, 
  o, 
  p
): sixteenTuple = echoTuple (
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0
);

/* Annotated inline */
let x: (
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int
) = echoTuple (
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0
);

/* Desired formatting if pattern does not fit, arguments do (margin 70) */
/* Destructured */
let (
  axx, 
  bxx, 
  cxx, 
  dxx, 
  exx, 
  fxx, 
  gxx, 
  hxx, 
  ixx, 
  jxx, 
  kxx, 
  lxx, 
  mxx, 
  nxx, 
  oxx, 
  pxx
) =
  makeTuple 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;

/* Annotated */
/* Destructured */
let (
  axx, 
  bxx, 
  cxx, 
  dxx, 
  exx, 
  fxx, 
  gxx, 
  hxx, 
  ixx, 
  jxx, 
  kxx, 
  lxx, 
  mxx, 
  nxx, 
  oxx, 
  pxx
): sixteenTuple =
  makeTuple 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;

/* Annotated */
/* Destructured */
/* Inline */
let (
  axx, 
  bxx, 
  cxx, 
  dxx, 
  exx, 
  fxx, 
  gxx, 
  hxx, 
  ixx, 
  jxx, 
  kxx, 
  lxx, 
  mxx, 
  nxx, 
  oxx, 
  pxx
): (
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int
) =
  makeTuple 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;

/* Not-Destructured */
let someResult =
  makeTuple 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;

/* Annotated */
/* Not-Destructured */
let someResult: sixteenTuple =
  makeTuple 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;

/* Annotated */
/* Not-Destructured */
/* Inline */
let someResult: (
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int
) =
  makeTuple 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0;

/* Destructured */
let (
  axx, 
  bxx, 
  cxx, 
  dxx, 
  exx, 
  fxx, 
  gxx, 
  hxx, 
  ixx, 
  jxx, 
  kxx, 
  lxx, 
  mxx, 
  nxx, 
  oxx, 
  pxx
) = echoTuple (
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0
);

/* Annotated */
let (
  axx, 
  bxx, 
  cxx, 
  dxx, 
  exx, 
  fxx, 
  gxx, 
  hxx, 
  ixx, 
  jxx, 
  kxx, 
  lxx, 
  mxx, 
  nxx, 
  oxx, 
  pxx
): sixteenTuple = echoTuple (
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0
);

/* Annotated Inline */
let (
  axx, 
  bxx, 
  cxx, 
  dxx, 
  exx, 
  fxx, 
  gxx, 
  hxx, 
  ixx, 
  jxx, 
  kxx, 
  lxx, 
  mxx, 
  nxx, 
  oxx, 
  pxx
): (
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int
) = echoTuple (
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0
);

/* Not-Destructured */
let someResult = echoTuple (
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0
);

/* Annotated */
/* Not-Destructured */
let someResult: sixteenTuple = echoTuple (
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0
);

/* Annotated Inline */
/* Not-Destructured */
let someResult: (
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int
) = echoTuple (
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0, 
  0
);

/* Desired formatting if neither fit on one line (margin 70) */
/* Destructured */
let (
  axx, 
  bxx, 
  cxx, 
  dxx, 
  exx, 
  fxx, 
  gxx, 
  hxx, 
  ixx, 
  jxx, 
  kxx, 
  lxx, 
  mxx, 
  nxx, 
  oxx, 
  pxx
) =
  makeTuple
    axx 
    bxx 
    cxx 
    dxx 
    exx 
    fxx 
    gxx 
    hxx 
    ixx 
    jxx 
    kxx 
    lxx 
    mxx 
    nxx 
    oxx 
    pxx;

/* Annoted */
let (
  axx, 
  bxx, 
  cxx, 
  dxx, 
  exx, 
  fxx, 
  gxx, 
  hxx, 
  ixx, 
  jxx, 
  kxx, 
  lxx, 
  mxx, 
  nxx, 
  oxx, 
  pxx
): sixteenTuple =
  makeTuple
    axx 
    bxx 
    cxx 
    dxx 
    exx 
    fxx 
    gxx 
    hxx 
    ixx 
    jxx 
    kxx 
    lxx 
    mxx 
    nxx 
    oxx 
    pxx;

/* Annoted inline */
let (
  axx, 
  bxx, 
  cxx, 
  dxx, 
  exx, 
  fxx, 
  gxx, 
  hxx, 
  ixx, 
  jxx, 
  kxx, 
  lxx, 
  mxx, 
  nxx, 
  oxx, 
  pxx
): (
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int
) =
  makeTuple
    axx 
    bxx 
    cxx 
    dxx 
    exx 
    fxx 
    gxx 
    hxx 
    ixx 
    jxx 
    kxx 
    lxx 
    mxx 
    nxx 
    oxx 
    pxx;

/* Not-Destructured */
let someResult =
  makeTuple
    axx 
    bxx 
    cxx 
    dxx 
    exx 
    fxx 
    gxx 
    hxx 
    ixx 
    jxx 
    kxx 
    lxx 
    mxx 
    nxx 
    oxx 
    pxx;

/* Not-Destructured */
/* Annoted */
let someResult: sixteenTuple =
  makeTuple
    axx 
    bxx 
    cxx 
    dxx 
    exx 
    fxx 
    gxx 
    hxx 
    ixx 
    jxx 
    kxx 
    lxx 
    mxx 
    nxx 
    oxx 
    pxx;

/* Not-Destructured */
/* Annoted inline */
let someResult: (
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int
) =
  makeTuple
    axx 
    bxx 
    cxx 
    dxx 
    exx 
    fxx 
    gxx 
    hxx 
    ixx 
    jxx 
    kxx 
    lxx 
    mxx 
    nxx 
    oxx 
    pxx;

/* Desired formatting if neither fit on one line (margin 70) */
/* Destructured */
let (
  axx, 
  bxx, 
  cxx, 
  dxx, 
  exx, 
  fxx, 
  gxx, 
  hxx, 
  ixx, 
  jxx, 
  kxx, 
  lxx, 
  mxx, 
  nxx, 
  oxx, 
  pxx
) = echoTuple (
  1000, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10
);

/* Annoted */
/* Destructured */
let (
  axx, 
  bxx, 
  cxx, 
  dxx, 
  exx, 
  fxx, 
  gxx, 
  hxx, 
  ixx, 
  jxx, 
  kxx, 
  lxx, 
  mxx, 
  nxx, 
  oxx, 
  pxx
): sixteenTuple = echoTuple (
  1000, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10
);

/* Annoted Inline */
/* Destructured */
let (
  axx, 
  bxx, 
  cxx, 
  dxx, 
  exx, 
  fxx, 
  gxx, 
  hxx, 
  ixx, 
  jxx, 
  kxx, 
  lxx, 
  mxx, 
  nxx, 
  oxx, 
  pxx
): (
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int
) = echoTuple (
  1000, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10
);

/* Desired formatting if neither fit on one line (margin 70) */
/* Not-Destructured */
let someResult = echoTuple (
  1000, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10
);

/* Annoted */
/* Not-Destructured */
let someResult: sixteenTuple = echoTuple (
  1000, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10
);

/* Annoted Inline */
/* Not-Destructured */
let someResult: (
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int
) = echoTuple (
  1000, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10, 
  10
);

let someResult: (
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int, 
  int
) = someResult;

/* This shouldn't be broken onto its own newline: @see ensureSingleTokenSticksToLabel */
type sevenStrings = (
  string, 
  string, 
  string, 
  string, 
  string, 
  string, 
  string
);

let (only, the, type_, should, have, to_, wrap) = (
  "only", 
  "the", 
  "type", 
  "should", 
  "have", 
  "to", 
  "wrap"
);

let (only, the, type_, should, have, to_, wrap): sevenStrings = (
  "only", 
  "the", 
  "type", 
  "should", 
  "have", 
  "to", 
  "wrap"
);

let
  ifTheNameIsReallyLongTheTypeAndValueShouldBothWrap: (
  string, 
  string, 
  string, 
  string, 
  string, 
  string, 
  string
) = (
  "only", 
  "the", 
  "type", 
  "should", 
  "have", 
  "to", 
  "wrap"
);

let (the, type_, and_, value, should, both, wrap): (
  string, 
  string, 
  string, 
  string, 
  string, 
  string, 
  string
) = (
  "but", 
  "the", 
  "destructured", 
  "assignment", 
  "should", 
  "not", 
  "wrap"
);

let myPolyFunc: 'a .'a => 'a = fun o => o;

let myNonPolyFunc: 'a => 'a = fun o => o;

let locallyAbstractFunc (type a) (input: a) => input;

let locallyAbstractFuncNotSugared 
    (type a) 
    (input: a) => input;

let locallyAbstractFuncAnnotated: type a. a => a =
  fun (type a) (input: a) => input;

/*
  Examples of how long versions of these should be wrapped: df stands for
  "desired formatting" when the function binding itself must wrap.
 */
let df_myPolyFunc: 'a .'a => 'a = fun o => o;

let df_myNonPolyFunc: 'a => 'a = fun o => o;

type nameBlahType = {nameBlah: int};

let myFunc 
    firstArg::firstArg 
    another::another 
    fl::fl => {
  nameBlah: 10
};

type inputEchoRecord 'a = {inputIs: 'a};

let df_locallyAbstractFunc 
    (type a) 
    (type b) 
    (input: a) => {
  inputIs: input
};

/* With setting ReturnValOnSameLine */
let df_locallyAbstractFuncNotSugared 
    (type a) 
    (type b) 
    (input: a) => {
  inputIs: input
};

/**
 * The following is automatically expanded at the parser level into:
 *
 *   let df_locallyAbstractFuncAnnotated:
 *     'a .
 *     'a => 'a => inputEchoRecord 'a
 *    =
 *     fun (type a) => (
 *       fun (input: a) (input: a) => {inputIs: input}:
 *         a => a => inputEchoRecord a
 *     );
 *
 */
let df_locallyAbstractFuncAnnotated:
  type a. a => a => inputEchoRecord a =
  fun (input: a) (input: a) => {inputIs: input};

/**
 * The following is automatically expanded at the parser level into:
 *
 *   let df_locallyAbstractFuncAnnotated:
 *     'a .
 *     'a => 'a => inputEchoRecord 'a
 *    =
 *     fun (type a) => (
 *       df_locallyAbstractFuncAnnotated:
 *         a => a => inputEchoRecord a
 *     );
 *
 */
let df_locallyAbstractFuncAnnotatedRef:
  type a. a => a => inputEchoRecord a = df_locallyAbstractFuncAnnotated;

/**
 * Doesn't do what you want:
 *
 *  let df_locallyAbstractFuncAnnotatedExtra: type a. a => a => inputEchoRecord a =
 *    fun (type a)
 *        (input:a)
 *        (input:a) => {
 *      inputIs: input
 *    };
 */
/**
 * The following is automatically expanded at the parser level into:
 *
 *   let df_locallyAbstractFuncAnnotatedTwo:
 *     'a 'b .
 *     'a => 'b => (inputEchoRecord 'a, inputEchoRecord 'b)
 *    =
 *     fun (type a) (type b) => (
 *       fun (input: a) (input2: b) => ({inputIs: input}, {inputIs:input2}):
 *         a => b => (inputEchoRecord a, inputEchoRecord b)
 *     );
 *
 */
let df_locallyAbstractFuncAnnotated:
  type a b.
    a => 
    b => 
    (inputEchoRecord a, inputEchoRecord b) =
  fun (input: a) (input2: b) => (
    {inputIs: input}, 
    {inputIs: input2}
  );

/**
 * This case shows why inferring what was originally sugar type a b . blahblah
 * is not so trivial. We have to take the last Pexp_constraint type, varify the
 * constructors, then check if the result is equal to the first
 * Ppat_constraint. In this case, they're not equal!
 */
let df_locallyAbstractFuncAnnotated: 'figureMeOut =
  fun (type a) (type b) => (
    fun (input: a) (input2: b) => (
      {inputIs: input}, 
      {inputIs: input2}
    ):
      a => 
      b => 
      (inputEchoRecord a, inputEchoRecord b)
  );

let createTuple_thisFuncShouldWrapCorrectlyNow: 
  'a .
  'a => 'a => 'a => ('a, 'a, 'a)
 =
  fun someVar someVar2 someVar3 => (
    someVar, 
    someVar2, 
    someVar3
  );

let theTupleTypeAnnotationShouldWrap: (
  string, 
  string, 
  string, 
  string
) = (
  "now these tuple values should wrap", 
  "now these tuple values should wrap", 
  "now these tuple values should wrap", 
  "now these tuple values should wrap"
);

let rec mutuallyRecursiveOne x => mutuallyRecursiveTwo (
  x + x
)
and mutuallyRecursiveTwo y => print_int y;

/* The only downside to this is that now you can't redeclare a binding. */
/* let newMutualRecursionSyntax x => newMutuallyRecursiveTwo (x + x); */
/* let newMutuallyRecursiveTwo y => print_int y; */
/*  */
type x = private int;

type myType 'a 'b 'c = private ('a, 'b, 'c);

type privateVariant = private
  | BigSize of int | SmallSize of int;

type doubleEqualsDoublePrivateVariant =
  privateVariant =
    private | BigSize of int | SmallSize of int;

type myRecordWithReallyLongName = {
  xx: int, 
  yy: int
};

type doubleEqualsRecord =
  myRecordWithReallyLongName = {xx: int, yy: int};

type doubleEqualsDoublePrivateRecord =
  myRecordWithReallyLongName =
    private {xx: int, yy: int};

type someConstructor =
  | SomeConstructorHi of int int;

type someRecord = {
  firstFieldInRecord: int, 
  secondField: int
};

/*
  With settings.functionBindingStyle = AttachFirstTermToLabelIffTwoTotalTerms,
  the binding name becomes part of the label when there are only two total
  terms in the binding/argument pattern list (the name, followed by one
  pattern).
*/
let funcOnSomeConstructorHi 
    (SomeConstructorHi x y) => x + y;

let funcOnSomeConstructorHi 
    (SomeConstructorHi x y) 
    secondArg => x + y;

/* With two args */
let funcOnSomeRecord 
    {firstFieldInRecord, secondField} => firstFieldInRecord + secondField;

let funcOnSomeRecord 
    {firstFieldInRecord, secondField} 
    secondArg => firstFieldInRecord + secondField;

/*
  With settings.functionBindingStyle = DontAttachFirstTermToLabel,
  the binding name becomes part of the label when there are only two total
  terms in the binding/argument pattern list (the name, followed by one
  pattern).
*/
let funcOnSomeConstructorHi 
    (SomeConstructorHi x y) => x + y;

let funcOnSomeRecord 
    {firstFieldInRecord, secondField} => firstFieldInRecord + secondField;

/* With two args */
let funcOnSomeConstructorHi 
    (SomeConstructorHi x y) 
    secondArg => x + y;

let funcOnSomeRecord 
    {firstFieldInRecord, secondField} 
    secondArg => firstFieldInRecord + secondField;

type simpleTupleVariant =
  | SimpleActuallyATuple of (int, int);

let returnTheSimpleTupleVariant i =>
  SimpleActuallyATuple (i, i);

let shouldWrapLike whenLongArg =>
  SimpleActuallyATuple (whenLongArg, whenLongArg);

type recordWithLong = {
  someField: int, 
  anotherField: string
};

/*
 * Commenting first of two mutualy recursive types.
 */
type recursiveType =
  /* First variant of first mutually recursive */ 
  | Blah 
  /* Second variant of first mutually recursive */ 
  | Another of (option anotherRecursiveType)
/*
 * Commenting second of two mutually recursive types.
 */
and anotherRecursiveType =
  /* Second variant of second mutually recursive */ 
  | Baz 
  /* Second variant of second mutually recursive */ 
  | Recursive of (option recursiveType);

/**
 * Commented GADT definition.
 */
type term _ =
  /* First variant leaf of GADT */ 
  | Int of
      /*first var arg */ 
      int 
      :/* First GADT res */
       term
         int 
  /* Second variant leaf of GADT */ 
  | Float of
      /*second var arg */ 
      int 
      :/* Second GADT res */
       term
         int 
  /* Third variant leaf of GADT */ 
  | Bool of
      /*third var arg */ 
      int 
      :/* Third GADT res */
       term
         int;

/* Commented colors */
type commentedTypeDef =
  /*
   * Commenting first variant member.
   */ 
  | First of (
      /* First field of tuple in first variant member */
      int, 
      /* Second field of tuple in first variant member */
      int
    ) 
  /*
   * Commenting second variant member.
   */ 
  | Second of int 
  /*
   * Commenting third variant member.
   */ 
  | Third of (
      list
        /* Commenting deep in type def */ 
        (list int)
    );

type colors =
  | Red of int | Black of int | Green of int;

let blah arg =>
  switch arg {
    /* Comment before Bar */
    /* Comment between bar/pattern */
    | Red _ => 1
    /* Comment Before non-first bar */
    /* Comment betwen bar/pattern */
    /* These will be formatted into the wrong place
     * and there's nothing you can do about it because
     * the bar essentially doesn't exist once parsed -
     * its location is lost - "case"s don't have locs
     */
    | Black _ => 0
    | Green _ => 0
  };

let blah =
  fun | Red _ => 1 
      | Black _ => 0 
      | Green _ => 1;

let blahCurriedX x =>
  fun /* Comment before first bar */ 
      /* Comment between first bar and OR pattern */ 
      | Red x
      | Black x
      | Green x => 1 
      /* Comment before second bar */ 
      | Black x => 0 
      | Green x => 0;

type reallyLongVariantNames =
  | ReallyLongVariantName of recordWithLong 
  | AnotherReallyLongVariantName of int int int 
  | AnotherReallyLongVariantName2 of int int int;

let howDoLongMultiBarPatternsWrap x =>
  switch x {
    | AnotherReallyLongVariantName _ _ _ => 0
    | AnotherReallyLongVariantName2 _ _ _ => 0
    | ReallyLongVariantName {
        someField, 
        anotherField
      } => 0
  };

let letsCombineTwoLongPatternsIntoOneCase x =>
  switch x {
    | AnotherReallyLongVariantName _ _ _
    | AnotherReallyLongVariantName2 _ _ _ => 0
    | ReallyLongVariantName {
        someField, 
        anotherField
      } => 0
  };

let letsPutAWhereClauseOnTheFirstTwo x =>
  switch x {
    | AnotherReallyLongVariantName _ _ _
    | AnotherReallyLongVariantName2 _ _ _
        when true => 0
    | ReallyLongVariantName {
        someField, 
        anotherField
      } => 0
  };

let letsPutAWhereClauseOnTheLast x =>
  switch x {
    | AnotherReallyLongVariantName _ _ _
    | AnotherReallyLongVariantName2 _ _ _ => 0
    | ReallyLongVariantName {
        someField, 
        anotherField
      }
        when true => 0
  };

type wrappingGadt _ =
  | ThisIsLongSoTypeWillWrap of
      int :wrappingGadt int 
  | Add :wrappingGadt (int => int => int) 
  | App of
      (wrappingGadt ('b => 'a)) 
      (wrappingGadt 'b) 
      :wrappingGadt 'a;

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

let anotherRecord = {
  ...testRecord, 
  name: "joe++", 
  age: testRecord.age + 10
};

type polymorphicCommentedType
  /* Commenting the first type variable */'a 
  /* Commenting the second type variable */'b =
  list ('a, 'b);

/**
 * Commenting the entire record definition.
 */
type withThreeFieldsCommented = {
  /* Commenting the first field */ 
  nameCommented: string, 
  /* Commenting the second field */ 
  ageCommented: int, 
  /* Commenting the third field */ 
  occupationCommented: string
};

/**
 * Commenting the entire record.
 */
let testRecordCommented = {
  /* Commenting the first field */ 
  nameCommented: "joe", 
  /* Commenting the second field */ 
  ageCommented: 20, 
  /* Commenting the last field */ 
  occupationCommented: "engineer"
};

/*
 * Test comments near the arguments.
 */
let callMeWithComments 
    /* Comment before first arg "a" */ 
    (a: int) 
    /* Comment before second arg "b" */ 
    (b: int) 
    :/* Comment before return type annotation "int" */int =>
  /* Comment above return value a + b + c */
  a +
    b + c;

let result =
  /* Comment before function to invoke */
  callMeWithComments
    /* Comment before first argument expression */ 
    (1 + 2 + 3 + 3) 
    /* Comment before second argument expression */ 
    (1 + 2 + 3 + 3);

module type ASig = {let a: int;};

module type BSig = {let b: int;};

let module AMod = {
  let a = 10;
};

let module BMod = {
  let b = 10;
};

let module CurriedSugar (A: ASig) (B: BSig) =>
  /* Commenting before First curried functor arg */
  /* If these comments aren't formatted correctly
     * see how functor args' locations aren't set
     * correclty due to the fold_left.
     */
  /* Commenting before Second curried functor arg */
  {
    let result = A.a + B.b;
  };

/* Comment at bottom of module expression */
let module CurriedSugarFunctorResult =
  /* Commenting before functor name*/
  CurriedSugar
    /* Commenting before functor arg 1 in app */ 
    AMod 
    /* Commenting before functor arg 2 in app */ 
    BMod;

let module CurriedSugarFunctorResultInline =
  /* Commenting before functor name*/
  CurriedSugar
    /* Commenting before functor arg 1 in app */ 
    {
      let a = 10;
    } 
    /* Commenting before functor arg 2 in app */ 
    {
      let b = 10;
    };

/*
 * Commenting locations
 */
let commentingBeforeEqual =
  /*beforeEqual*/
  {
    name: "hello", 
    age: 20, 
    occupation: "programmer"
  };

let commentingAfterEqual =
  /*afterEqual*/
  {
    name: "hello", 
    age: 20, 
    occupation: "programmer"
  };

let commentingBeforeEqualBeforeType:
  /*beforeEqualBeforeType*/
  withThreeFields = {
  name: "hello", 
  age: 20, 
  occupation: "programmer"
};

let commentingBeforeEqualAfterType: withThreeFields =
  /*beforeEqualAfterType*/
  {
    name: "hello", 
    age: 20, 
    occupation: "programmer"
  };

let commentingAfterEqualAfterType: withThreeFields =
  /*afterEqual*/
  {
    name: "hello", 
    age: 20, 
    occupation: "programmer"
  };

let
  /*beforePattern*/
  commentingBeforePattern: withThreeFields = {
  name: "hello", 
  age: 20, 
  occupation: "programmer"
};

let
  /*beforePattern*/
  /*beforePattern2 */
  commentingBeforePattern2: withThreeFields = {
  name: "hello", 
  age: 20, 
  occupation: "programmer"
};

let
  /**beforePattern*/
  /*beforePattern2 */
  commentingBeforePatternSpecial: withThreeFields = {
  name: "hello", 
  age: 20, 
  occupation: "programmer"
};

let produceRecord /*commentBeforeArg*/ x => {
  name: "hello", 
  age: 20, 
  occupation: "programmer"
};

let produceRecord x =>
  /*commentAfterArg*/
  {
    name: "hello", 
    age: 20, 
    occupation: "programmer"
  };

let myPolyFuncCommentBeforeColon: 
  'a .
  /*beforeColon */
  'a => 'a
 =
  fun o => o;

let myPolyFuncCommentAfterColon: 
  'a .
  /*afterColon */
  'a => 'a
 =
  fun o => o;

let myPolyFuncCommentBeforeArrow: 
  'a .
  'a => /*beforeArrow */  'a
 =
  fun o => o;

let myPolyFuncCommentAfterArrow: 
  'a .
  'a => /*afterArrow */  'a
 =
  fun o => o;

let myPolyFuncCommentBeforeEqual: 'a .'a => 'a =
  /*beforeEqual */
  fun o => o;

let myPolyFuncCommentAfterEqual: 'a .'a => 'a =
  /*afterEqual */
  fun o => o;

let myNonPolyFuncCommentBeforeColon:
  /*BeforeColon */
  'a => 'a =
  fun o => o;

let myNonPolyFuncCommentAfterColon:
  /*AfterColon */
  'a => 'a =
  fun o => o;

let myNonPolyFuncCommentBeforeArrow:
  'a => /*BeforeArrow */  'a =
  fun o => o;

let myNonPolyFuncCommentAfterArrow:
  'a => /*AfterArrow */  'a =
  fun o => o;

let myNonPolyFuncCommentBeforeEqual: 'a => 'a =
  /*BeforeEqual */
  fun o => o;

let myNonPolyFuncCommentAfterEqual: 'a => 'a =
  /*AfterEqual */
  fun o => o;

let lATCurrySugarCommentBeforeType 
    /*BeforeType */ 
    (type a) 
    (input: a) => input;

let lATCurrySugarCommentAfterType 
    /*AfterType */ 
    (type a) 
    (input: a) => input;

let lATCurrySugarCommentBeforeArg 
    (type a) 
    /*BeforeArg */ 
    (input: a) => input;

let lATCurrySugarCommentAfterArg 
    (type a) 
    (input: a) =>
  /*AfterArg */
  input;

let lATCurrySugarCommentAfterArrow 
    (type a) 
    (input: a) =>
  /*AfterArrow */
  input;

let lATNotSugaredCommentBeforeEqual 
    /*BeforeEqual*/ 
    (type a) 
    (input: a) => input;

let lATNotSugaredCommentAfterEqual 
    /*AfterEqual*/ 
    (type a) 
    (input: a) => input;

let lATNotSugaredCommentBeforeType 
    (type a) 
    /*BeforeType*/ 
    (input: a) => input;

let lATNotSugaredCommentAfterType 
    (type a) 
    /*AfterType*/ 
    (input: a) => input;

let lATNotSugaredCommentBeforeArg 
    (type a) 
    /*BeforeArg*/ 
    (input: a) => input;

let lATNotSugaredCommentAfterArg 
    (type a) 
    (input: a) =>
  /*AfterArg*/
  input;

let lATNotSugaredCommentAfterArrow 
    (type a) 
    (input: a) =>
  /*AfterArrow*/
  input;

let lAtFuncAnnotatedCommentBeforeColon:
  type a.
    /*BeforeColon*/
    a => a =
  fun (type a) (input: a) => input;

let lAtFuncAnnotatedCommentAfterColon:
  type a.
    /*AfterColon*/
    a => a =
  fun (type a) (input: a) => input;

let lAtFuncAnnotatedCommentBeforeTypeVar:
  type a.
    /*BeforeTypeVar*/
    a => a =
  fun (type a) (input: a) => input;

let lAtFuncAnnotatedCommentAfterTypeVar:
  type a.
    /*AfterTypeVar*/
    a => a =
  fun (type a) (input: a) => input;

let lAtFuncAnnotatedBeforeEqual: type a. a => a =
  /*BeforeEqual*/
  fun (type a) (input: a) => input;

let lAtFuncAnnotatedAfterEqual: type a. a => a =
  /*AfterEqual*/
  fun (type a) (input: a) => input;

/* Ternary wrapping comments */
let ternaryResult =
  /* Before Test */
  something ?
    /* Before ifTrue */  
    callThisFunction withThisArg : 
    /* Before ifFalse */  
    thatResult;

let ternaryResult =
  /* Before Test */
  something ?
    /* Before ifTrue */  
    callThisFunction withThisArg : 
    /* Before ifFalse */  
    trailingTest ?
      /* before nested ifTrue */  
      true : 
      /* before nested ifFalse */  
      false;

let returningATernary x y => x > y ? "hi" : "by";
[@@@autoFormat
  let wrap = 80; 
  let shift = 2;
];

Modules.run ();

Polymorphism.run ();

Variants.run ();

BasicStructures.run ();

TestUtils.printSection "General Syntax";

/* Won't work! */
/* let matchingFunc a = match a with */
/*   `Thingy x => (print_string "matched thingy x"); x */
/*   | `Other x => (print_string "matched other x"); x;; */
/*  */
let matchingFunc a =>
  switch a {
    | `Thingy x => {
        print_string "matched thingy x";
        let zz = 10;
        zz
      }
    | `Other x => {
        print_string "matched other x";
        x
      }
  };

type firstTwoShouldBeGroupedInParens =
  (int => int) => int => int;

type allParensCanBeRemoved =
  int => int => int => int;

type firstTwoShouldBeGroupedAndFirstThree =
  ((int => int) => int) => int;

/* Same thing now but with type constructors instead of each int */
type firstTwoShouldBeGroupedInParens =
  (list int => list int) => list int => list int;

type allParensCanBeRemoved =
  list int => list int => list int => list int;

type firstTwoShouldBeGroupedAndFirstThree =
  ((list int => list int) => list int) => 
  list int;

type myRecordType = {
  firstTwoShouldBeGroupedInParens:
    (int => int) => int => int, 
  allParensCanBeRemoved: int => int => int => int, 
  firstTwoShouldBeGroupedAndFirstThree:
    ((int => int) => int) => int
};

type firstNamedArgShouldBeGroupedInParens =
  first::(int => int) => second::int => int;

type allParensCanBeRemoved =
  first::int => second::int => third::int => int;

type firstTwoShouldBeGroupedAndFirstThree =
  first::((int => int) => int) => int;

/* Same thing now, but with type constructors instead of int */
type firstNamedArgShouldBeGroupedInParens =
  first::(list int => list int) => 
  second::list int => 
  list int;

type allParensCanBeRemoved =
  first::list int => 
  second::list int => 
  third::list int => 
  list int;

type firstTwoShouldBeGroupedAndFirstThree =
  first::((list int => list int) => list int) => 
  list int;

type firstNamedArgShouldBeGroupedInParens =
  first::(int => int)? => 
  second::int list? => 
  int;

/* The arrow necessitates parens around the next two args. The ? isn't what
 * makes the parens necessary. */
type firstNamedArgShouldBeGroupedInParensAndSecondNamedArg =
  first::(int => int)? => 
  second::(int => int)? => 
  int;

type allParensCanBeRemoved =
  first::int? => 
  second::int? => 
  third::int? => 
  int;

type firstTwoShouldBeGroupedAndFirstThree =
  first::((int => int) => int) => int;

type noParens =
  one::int => int => int => two::int => int;

type noParensNeeded =
  one::int => int => int => two::int => int;

type firstNamedArgNeedsParens =
  one::(int => int => int) => two::int => int;

/* Now, let's try type aliasing */
/* Unless wrapped in parens, types between arrows may not be aliased, may not
 * themselves be arrows. */
type parensRequiredAroundFirstArg =
  (list int as 'a) => int as 'a;

type parensRequiredAroundReturnType =
  (list int as 'a) => (int as 'a);

type parensRequiredAroundReturnType =
  (list int as 'a) => (int as 'a) as 'b;

type noParensNeededWhenInTuple =
  (list int as 'a, list int as 'b) as 'entireThing;

type myTypeDef 'a = list 'a;

type instatiatedTypeDef = myTypeDef int => int;

/* Test a type attribute for good measure */
/* We should clean up all of the attribute tagging eventually, but for now,
 * let's make it super ugly to get out of the way of all the formatting/parsing
 * implementations (fewer conflicts during parsing, fewer edge cases during
 * printing).
 */
type something = (
  int, 
  int [@lookAtThisAttribute]
);

type longWrappingTypeDefinitionExample =
  M_ReactKit__Gesture.Types.instance
    (
      TapGestureRecognizer.tapGestureFields
        unit unit
    ) 
    (
      TapGestureRecognizer.tapGestureMethods
        unit unit
    );

type semiLongWrappingTypeDefinitionExample =
  M_ReactKit__Gesture.Types.instance
    TapGestureRecognizerFinal.tapGestureFields 
    TapGestureRecognizerFinal.tapGestureMethods;

type semiLongWrappingTypeWithConstraint =
  M_ReactKit__Gesture.Types.instance
    'a 
    TapGestureRecognizerFinal.tapGestureFields 
    TapGestureRecognizerFinal.tapGestureMethods
constraint 'a = (unit, unit);

/* This must be in trunk but not in this branch of OCaml */
/* type withNestedRecords = MyConstructor of {myField: int} */
type colors =
  | Red of int | Black of int | Green of int;

/* Another approach is to require declared variants to wrap any record */
/* type myRecord = MyRecord of {name: int}; */
/* let myValue = MyRecord {name: int}; */
/* This would force importing of the module */
/* This would also lend itself naturally to pattern matching - and avoid having
to use `.` operator at all since you normally destructure. */
type nameBlahType = {nameBlah: int};

let myRecord = {nameBlah: 20};

let myRecordName = myRecord.nameBlah;

let {nameBlah}: nameBlahType = {nameBlah: 20};

print_int nameBlah;

let {nameBlah: aliasedToThisVar}: nameBlahType = {
  nameBlah: 20
};

print_int aliasedToThisVar;

let desiredFormattingForWrappedLambda:
  int => int => int => nameBlahType =
  /*

 fun is
 pre-   /firstarg\
 fix   /-coupled--\
  |-\ /-to-prefix--\       */
  fun curriedArg anotherArg lastArg => {
    nameBlah: 10
  };

type longerInt = int;

let desiredFormattingForWrappedLambdaWrappedArrow:
  longerInt => 
  longerInt => 
  longerInt => 
  nameBlahType =
  /*

 fun is
 pre-   /firstarg\
 fix   /-coupled--\
  |-\ /-to-prefix--\       */
  fun curriedArg anotherArg lastArg => {
    nameBlah: 10
  };

let desiredFormattingForWrappedLambdaReturnOnNewLine 
    /*

 fun is
 pre-   /firstarg\
 fix   /-coupled--\
  |-\ /-to-prefix--\       */ 
    curriedArg 
    anotherArg 
    lastArg => {
  nameBlah: 10
};

/*
let is
pre-
fix    /-function binding name---\
|-\   / is coupled to prefix      \   */
let desiredFormattingForWrappedSugar 
    curriedArg 
    anotherArg 
    lastArg => {
  nameBlah: 10
};

/*
let is
pre-
fix    /-function binding name---\
|-\   / is coupled to prefix      \   */
let desiredFormattingForWrappedSugarReturnOnNewLine 
    curriedArg 
    anotherArg 
    lastArg => {
  nameBlah: 10
};

/*
  let  : type t1 t2. t1 * t2 list -> t1 = ...
  let rec f : 't1 't2. 't1 * 't2 list -> 't1 =
    fun (type t1) (type t2) -> (... : t1 * t2 list -> t1)
*/
type point = {x: int, y: int};

type point3D = {x: int, y: int, z: int};

let point2D = {x: 20, y: 30};

let point3D: point3D = {x: 10, y: 11, z: 80};

/* Optional Comma */
let printPoint (p: point) => {
  print_int p.x;
  print_int p.y
};

let addPoints (p1: point, p2: point) => {
  x: p1.x + p2.x, 
  y: p1.y + p2.y
};

let res1 = printPoint point2D;

let res2 = printPoint {
  x: point3D.x, 
  y: point3D.y
};

/*
   When () were used to indicate sequences, the parser used seq_expr not only
   for grouping sequences, but also to form standard precedences.
                         /------- sequence_expr ------\
   let res3 = printPoint (addPoints (point2D, point3D));

   Interestingly, it knew that tuples aren't sequences.

   To move towards semi delimited, semi-terminated, braces-grouped sequences:
   while allowing any non-sequence expression to be grouped on parens, we make
   an explicit rule that allows one single non-semi ended expression to be
   grouped in parens.

   Actually: We will allow an arbitrary number of semi-delimited expressions to
   be wrapped in parens, but the braces grouped semi delimited (sequence)
   expressions must *also* be terminated with a semicolon.

   This allows the parser to distinguish between

       let x = {a};    /* Record {a:a} */
       let x = {a;};   /* Single item sequence returning identifier {a} */
*/
let res3 = printPoint (
  addPoints (
    point2D, 
    {x: point3D.x, y: point3D.y}
  )
);

type person = {age: int, name: string};

type hiredPerson = {
  age: string, 
  name: string, 
  dateHired: int
};

let o: person = {name: "bob", age: 10};

/* Parens needed? Nope! */
let o: person = {name: "bob", age: 10};

let printPerson (p: person) => {
  let q: person = p;
  p.name ^ p.name
};

/* let dontParseMeBro x y:int = x = y;*/
/* With this unification, anywhere eyou see `= fun` you can just ommit it */
let blah a => a;

/* Done */
let blah a => a;

/* Done (almost) */
let blah a b => a;

/* Done */
let blah a b => a;

/* Done (almost) */
/* More than one consecutive pattern must have a single case */
type blah = {blahBlah: int};

let blah a {blahBlah} => a;

let blah a {blahBlah} => a;

let module TryToExportTwice = {
  let myVal = "hello";
};

/*
  Unifying top level module syntax with local module syntax is probably a bad
  idea at the moment because it makes it more difficult to continue to support
  `let .. in` bindings. We can distinguish local modules for `let..in` that
  just happen to be defined at the top level (but not exported).

    let MyModule = {let myVal = 20;} in
    MyModule.x

  Wait, where would this ever be valid, even if we continued to support
  `let..in`?
*/
let
  onlyDoingThisTopLevelLetToBypassTopLevelSequence = {
  let x = {
    print_int 1;
    print_int 20
  };
  /* Missing trailing SEMI */
  let x = {
    print_int 1;
    print_int 20;
    /* Ensure missing middle SEMI reported well */
    print_int 20
  };
  let x = {
    print_int 1;
    print_int 20;
    10
  };
  /* Missing final SEMI */
  x + x
};

type hasA = {a: int};

let a = 10;

let returnsASequenceExpressionWithASingleIdentifier 
    () => a;

let thisReturnsA () => a;

let thisReturnsAAsWell () => a;

let recordVal: int = (thisReturnsARecord ()).a;

Printf.printf
  "\nproof that thisReturnsARecord: %n\n" 
  recordVal;

Printf.printf
  "\nproof that thisReturnsA: %n\n" 
  (thisReturnsA ());

/* Pattern matching */
let blah arg =>
  switch arg {
    /* Comment before Bar */
    /* Comment between bar/pattern */
    | Red _ => 1
    /* Comment Before non-first bar */
    /* Comment betwen bar/pattern */
    | Black _ => 0
    | Green _ => 0
  };

/* Any function that pattern matches a multicase match is interpretted as a
 * single arg that is then matched on. Instead of the above `blah` example:*/
let blah =
  fun | Red _ => 1 
      | Black _ => 0 
      | Green _ => 1;

/* `fun a => a` is read as "a function that maps a to a". Then the */
/* above example is read: "a function that 'either maps' Red to.. or maps .." */
/* Thc00f564e first bar is read as "either maps" */
/* Curried form is not supported:
   let blah x | Red _ => 1 | Black _ => 0;
   Theres no sugar rule for dropping => fun, only = fun
*/
let blahCurriedX x =>
  fun /* See, nothing says we can drop the => fun */ 
      | Red x
      | Black x
      | Green x => 1 
      /* With some effort, we can ammend the sugar rule that would */ 
      | Black x => 0 
      /* Allow us to drop any => fun.. Just need to make pattern matching */ 
      | Green x => 0;

/* Support that */
/* This should be parsed/printed exactly as the previous */
let blahCurriedX x =>
  fun | Red x
      | Black x
      | Green x => 1 
      | Black x => 0 
      | Green x => 0;

/* Any time there are multiple match cases we require a leading BAR */
let v = Red 10;

let Black x | Red x | Green x = v;

/* So this NON-function still parses */
/* This doesn't parse, however (and it doesn't in OCaml either):
  let | Black x | Red x | Green x = v;
*/
print_int x;

/* Scoping: Let sequences. Familiar syntax for lexical ML style scope and
sequences. */
let res = {
  let a = "a starts out as";
  {
    print_string a;
    let a = 20;
    print_int a
  };
  print_string a
};

let res = {
  let a = "first its a string";
  let a = 20;
  print_int a;
  print_int a;
  print_int a
};

let res = {
  let a = "a is always a string";
  print_string a;
  let b = 30;
  print_int b
};

/* let result = LyList.map (fun | [] => true | _ => false) []; */
/* OTHERWISE: You cannot tell if a is the first match case falling through or
 * a curried first arg */
/* let blah = fun a | patt => 0 | anotherPatt => 1; */
/* let blah a patt => 0 | anotherPatt => 1; */
/*simple pattern  EQUALGREATER      expr */
let blah a {blahBlah} => a;

/*            match_case             */
/*     pattern EQUALGREATER  expr */
let blah =
  fun | Red _ => 1 
      | Black _ => 0 
      | Green _ => 0;

/* Won't work! */
/* let arrowFunc = fun a b => print_string "returning aplusb from arrow"; a + b;;  */
let arrowFunc a b => {
  print_string "returning aplusb from arrow";
  a + b
};

let add a b => {
  let extra = {
    print_string "adding";
    0
  };
  let anotherExtra = 0;
  extra + a + b + anotherExtra
};

print_string (string_of_int (add 4 34));

let dummy _ => 10;

dummy res1;

dummy res2;

dummy res3;

/* Some edge cases */
let myFun firstArg (Red x | Black x | Green x) => firstArg + x;

let matchesWithWhen a =>
  switch a {
    | Red x when 1 > 0 => 10
    | Red _ => 10
    | Black x => 10
    | Green x => 10
  };

let matchesWithWhen =
  fun | Red x when 1 > 0 => 10 
      | Red _ => 10 
      | Black x => 10 
      | Green x => 10;

let matchesOne (`Red x) => 10;

/*
Typical OCaml would make you *wrap the functions in parens*! This is because it
can't tell if a semicolon is a sequence operator. Even if we had records use
commas to separate fields,
*/
type adders = {
  addTwoNumbers: int => int => int, 
  addThreeNumbers: int => int => int => int, 
  addThreeNumbersTupled: (int, int, int) => int
};

let myRecordWithFunctions = {
  addTwoNumbers: fun a b => a + b, 
  addThreeNumbers: fun a b c => a + b + c, 
  addThreeNumbersTupled:
    fun (a, b, c) => a + b + c
};

let result =
  myRecordWithFunctions.addThreeNumbers 10 20 30;

let result = myRecordWithFunctions.addThreeNumbersTupled (
  10, 
  20, 
  30
);

let lookTuplesRequireParens = (1, 2);

/* let thisDoesntParse = 1, 2;  */
let tupleInsideAParenSequence = {
  print_string "look, a tuple inside a sequence";
  let x = 10;
  (x, x)
};

let tupleInsideALetSequence = {
  print_string "look, a tuple inside a sequence";
  let x = 10;
  (x, x)
};

/* We *require* that function return types be wrapped in
  parenthesis. In this example, there's no ambiguity */
let makeIncrementer (delta: int) :(int => int) =>
  fun a => a + delta;

/* We could even force that consistency with let bindings - it's allowed
   currently but not forced.
*/
let myAnnotatedValBinding: int = 10;

/* Class functions (constructors) and methods are unified in the same way */
class classWithNoArg = {
  method x = 0; 
  method y = 0;
};

/* This parses but doesn't type check
  class myClass init => object
    method x => init
    method y => init
  end;
*/
let myFunc (a: int) (b: int) :(int, int) => (
  a, 
  b
);

let myFunc (a: int) (b: int) :list int => [1];

let myFunc (a: int) (b: int) :point => {
  x: a, 
  y: b
};

let myFunc (a: int, b: int) :point => {
  x: a, 
  y: b
};

type myThing = (int, int);

type stillARecord = {name: string, age: int};

/* Rebase latest OCaml to get the following: And fixup
  `generalized_constructor_arguments` according to master. */
/* type ('a, 'b) myOtherThing = Leaf of {first:'a, second: 'b} | Null; */
type branch 'a 'b = {first: 'a, second: 'b};

type myOtherThing 'a 'b =
  | Leaf of (branch 'a 'b) | Null;

type yourThing = myOtherThing int int;

/* Conveniently - this parses exactly how you would intend! No *need* to wrap
in an extra [], but it doesn't hurt */
/* FIXME type lookAtThesePolyVariants = list [`Red] ; */
/* FIXME type bracketsGroupMultipleParamsAndPrecedence = list (list (list [`Red])); */
/* FIXME type youCanWrapExtraIfYouWant = (list [`Red]); */
/* FIXME type hereAreMultiplePolyVariants = list [`Red | `Black]; */
/* FIXME type hereAreMultiplePolyVariantsWithOptionalWrapping = list ([`Red | `Black]); */
/*
  /* Proposal: ES6 style lambdas: */

  /* Currying */
  let lookES6Style = (`Red x) (`Black y) => { };
  let lookES6Style (`Red x) (`Black y) => { };

  /* Matching the single argument */
  let lookES6Style = oneArg => match oneArg with
    | `Red x => x
    | `Black x => x;

  /* The "trick" to currying that we already have is basically the same - we just
   * have to reword it a bit:
   * From:
   * "Any time you see [let x = fun ...] just replace it with [let x ...]"
   * To:
   * "Any time you see [let x = ... => ] just replace it with [let x ... => ]"
   */
  let lookES6Style oneArg => match oneArg with
    | `Red x => x
    | `Black x => x;

*/
/** Current OCaml Named Arguments. Any aliasing is more than just aliasing!
OCaml allows full on pattern matching of named args. */
/*
A: let named              ~a    ~b                = aa + bb in
B: let namedAlias         ~a:aa ~b:bb             = aa + bb in
C: let namedAnnot         ~(a:int) ~(b:int)       = a + b in
D: let namedAliasAnnot    ~a:(aa:int) ~b:(bb:int) = aa + bb in
E: let optional           ?a    ?b                              = 10 in
F: let optionalAlias      ?a:aa ?b:bb                           = 10 in
G: let optionalAnnot      ?(a:int option) ?(b:int option)       = 10 in
H: let optionalAliasAnnot ?a:(aa:int option) ?b:(bb:int option) = 10 in
/*
Look! When a default is provided, annotation causes inferred type of argument
to not be "option" since it's automatically destructured (because we know it
will always be available one way or another.)
*/
I: let defOptional           ?(a=10)    ?(b=10)                 = 10 in
J: let defOptionalAlias      ?a:(aa=10) ?b:(bb=10)              = 10 in
K: let defOptionalAnnot      ?(a:int=10) ?(b:int=10)            = 10 in
                            \       \
                             \label_let_pattern opt_default: no longer needed in SugarML

L: let defOptionalAliasAnnot ?a:(aa:int=10) ?b:(bb:int=10)      = 10 in
                              \        \
                               \let_pattern: still a useful syntactic building block in SugarML
*/
/**
 * In Reason, the syntax for named args uses double semicolon, since
 * the syntax for lists uses ES6 style [], freeing up the ::.
 */
let a = 10;

let b = 20;

/*A*/
let named a::a b::b => a + b;

type named = a::int => b::int => int;

/*B*/
let namedAlias a::aa b::bb => aa + bb;

let namedAlias a::aa b::bb => aa + bb;

type namedAlias = a::int => b::int => int;

/*C*/
let namedAnnot a::(a: int) b::(b: int) => 20;

/*D*/
let namedAliasAnnot a::(aa: int) b::(bb: int) => 20;

/*E*/
let myOptional a::a=? b::b=? () => 10;

type named = a::int? => b::int? => unit => int;

/*F*/
let optionalAlias a::aa=? b::bb=? () => 10;

/*G*/
let optionalAnnot a::(a: int)=? b::(b: int)=? () => 10;

/*H*/
let optionalAliasAnnot 
    a::(aa: int)=? 
    b::(bb: int)=? 
    () => 10;

/*I: */
let defOptional a::a=10 b::b=10 () => 10;

type named = a::int? => b::int? => unit => int;

/*J*/
let defOptionalAlias a::aa=10 b::bb=10 () => 10;

/*K*/
let defOptionalAnnot 
    a::(a: int)=10 
    b::(b: int)=10 
    () => 10;

/*L*/
let defOptionalAliasAnnot 
    a::(aa: int)=10 
    b::(bb: int)=10 
    () => 10;

/*M: Invoking them - Punned */
let resNotAnnotated = named a::a b::b;

/*N:*/
let resAnnotated: int = named a::a b::b;

/*O: Invoking them */
let resNotAnnotated = named a::a b::b;

/*P: Invoking them */
let resAnnotated: int = named a::a b::b;

/*Q: Here's why "punning" doesn't work!  */
/* Is b:: punned with a final non-named arg, or is b:: supplied b as one named arg? */
let b = 20;

let resAnnotated = named a::a b::b;

/*R: Proof that there are no ambiguities with return values being annotated */
let resAnnotated: ty = named a::a b;

/*S: Explicitly passed optionals are a nice way to say "use the default value"*/
let explictlyPassed =
  myOptional a::?None b::?None;

/*T: Annotating the return value of the entire function call */
let explictlyPassedAnnotated: int =
  myOptional a::?None b::?None;

/*U: Explicitly passing optional with identifier expression */
let a = None;

let explictlyPassed = myOptional a::?a b::?None;

let explictlyPassedAnnotated: int =
  myOptional a::?a b::?None;

/*
 * Showing many combinations of type annotations and named arguments.
 */
type typeWithNestedNamedArgs =
  outerOne::(
    innerOne::int => innerTwo::int => int
  ) => 
  outerTwo::int => 
  int;

type typeWithNestedOptionalNamedArgs =
  outerOne::
    (innerOne::int => innerTwo::int => int)? => 
  outerTwo::int? => 
  int;

type typeWithNestedOptionalNamedArgs =
  outerOne::list string? => outerTwo::int? => int;

let x =
  callSomeFunction
    withArg::10 andOtherArg::wrappedArg;

let res = {
  (constraintedSequenceItem: string);
  (dontKnowWheYoudWantToActuallyDoThis: string)
};

let res = {
  (
    butTheyWillBePrintedWithAppropriateSpacing: string
  );
  (soAsToInstillBestDevelopmentPractices: string)
};

let x = [
  (eachItemInListCanBeAnnotated: int), 
  (typeConstraints: float), 
  (
    tupleConstraints: int, 
    andNotFunctionInvocations: int
  )
];

let x = [
  (butWeWillPrint: int), 
  (themAsSpaceSeparated: float), 
  (toInfluenceYour: int, developmentHabbits: int)
];

let newRecord = {
  ...(annotatedSpreadRecord: someRec), 
  x: y
};

let newRecord = {
  ...(annotatedSpreadRecord: someRec), 
  blah: 0, 
  foo: 1
};

let newRecord = {
  ...(
       youCanEvenCallMethodsHereAndAnnotate them: someRec
     ), 
  blah: 0, 
  foo: 1
};

let newRecord = {
  ...(
       youCanEvenCallMethodsHereAndAnnotate
         them named::10: someRec
     ), 
  blah: 0, 
  foo: 1
};

let something: thing blah = aTypeAnnotation;

let something: thing blah = thisIsANamedArg;

let something: thing blah = aTypeAnnotation;

let something: blah = thisIsANamedArg thing;

let something: blah = typeAnnotation thing;

let newRecord = {
  ...(
       heresAFunctionWithNamedArgs argOne::i: annotatedResult
     ), 
  soAsToInstill: 0, 
  developmentHabbits: 1
};

[@@@thisIsAThing];

let x = 10;

/* Ensure that the parenthesis are preserved here because they are
 * important:
 */
let something =
  fun | None => (
          fun | [] => "emptyList" 
              | [_, ..._] => "nonEmptyList"
        ) 
      | Some _ => (
          fun | [] => "emptyList" 
              | [_, ..._] => "nonEmptyList"
        );
/* - A good way to test if formatting of infix operators groups precedences
   correctly, is to write an expression twice. Once in a form where parenthesis
   explicitly group according to the parse tree and write it another time
   without any parenthesis. After formatting, the two should be equal
   textually.
   - Reformatting n > 0 times should be idempotent.
   - Our formatting algorithm *could* decide to leave equivalently precedented
   infix applications ungrouped in parenthesis (which is what the above test
   verifies), but the additional parenthesis is nice.
 */
/* < > = all have same precedence level/direction(left) */
let parseTree = ((x > y > z) < a < b) = c = d;

let minParens = ((x > y > z) < a < b) = c = d;

let formatted = ((x > y > z) < a < b) = c = d;

/* < > = all have same precedence level and direction (left) */
let parseTree = a1 < a2 < (b1 > b2 > (y = x = z));

let minParens = a1 < a2 < (b1 > b2 > (y = x = z));

let formatted = a1 < a2 < (b1 > b2 > (y = x = z));

/* !=...(left) same level =(left) is higher than :=(right) */
let parseTree =
  a1 := a2 := b1 = b2 = (y != x != z);

let minParens =
  a1 := a2 := b1 = b2 = (y != x != z);

let formatted =
  a1 := a2 := b1 = b2 = (y != x != z);

/* !=...(left) same level =(left) is higher than :=(right) */
let parseTree =
  a1 := a2 := b1 = ((b2 = y) != x != z);

let minParens =
  a1 := a2 := b1 = ((b2 = y) != x != z);

let formatted =
  a1 := a2 := b1 = ((b2 = y) != x != z);

/* &...(left) is higher than &(right). &(right) is equal to &&(right) */
let parseTree =
  a1 && a2 && (b1 & b2 & y &|| x &|| z);

let minParens =
  a1 && a2 && (b1 & b2 & y &|| x &|| z);

let formatted =
  a1 && a2 && (b1 & b2 & y &|| x &|| z);

/* **...(right) is higher than *...(left) */
let parseTree = b1 *| b2 *| y *\*| x *\*| z;

let minParens = b1 *| b2 *| y *\*| x *\*| z;

let formatted = b1 *| b2 *| y *\*| x *\*| z;

/* **...(right) is higher than *...(left) */
let parseTree =
  b1 *| b2 *| y *\*| (x *\*| z *| a);

let minParens =
  b1 *| b2 *| y *\*| (x *\*| z *| a);

let formatted =
  b1 *| b2 *| y *\*| (x *\*| z *| a);

/* |...(left) is higher than ||(right) */
/* All parens should be removed when formatting n > 0 times */
let parseTree = b1 || b2 || y |\* x |\* z;

let minParens = b1 || b2 || y |\* x |\* z;

let formatted = b1 || b2 || y |\* x |\* z;

/* Associativity effects how parenthesis should be dropped */
/* This one *shouldn't* expand into two consecutive infix + */
first + (second + third);

/* This one *should* */
first + second + third;

/* But that's just because + is left associative. Since & is right associative,
* it's the opposite. */
/* This one *should* expand into two consecutive infix * */
first & second & third;

/* This one *shouldn't* */
(first & second) & third;

/* || is basically the same as &/&& */
first || second || third;

/* This one *shouldn't* */
(first || second) || third;

/* No parens should be added/removed from the following when formatting */
let seeWhichCharacterHasHigherPrecedence = (
                                        first |>
                                        second |> 
                                        third
                                        ) ^> fourth;

let seeWhichCharacterHasHigherPrecedence =
  first |> second |> third;

let seeWhichCharacterHasHigherPrecedence =
  first + second + third;

let comparison = (=);

/* Why would the following two cases have different grouping? */
let res =
  blah ||
    DataConstructor 10 || 
    DataConstructor 10 && 10;

let res =
  blah &&
    DataConstructor 10 && DataConstructor 10 + 10;

/* This demonstrates how broken infix pretty printing is:
 */
let curriedComparison = (=) 10;

let resultOfAdd = 10 + 20 + 40;

let resultOfAddAndMult = 10 * 1 + 20 * 1 + 40 * 1;

let greaterThanAndSubtract = 1 - 2 > 4 + 3;

let greaterThanAndFunctionCalls = pred 1 > pred 2;

let lessThanAndFunctionCalls = pred 1 < pred 2;

/* This doesn't type check because it looks like pred - 1 */
let minusAndInteger = pred - 1;

let passingMinusOneToFunction = pred (-1);

let leadingMinusIsCorrectlyNeg = (-1) + 20;

let leadingMinusIsCorrectlyNeg = 3 > (-1);

/* Custom infix without labeled args */
let (|>) first second => first + second;

/* Should reformat to actually be placed infix */
let res = first |> second;

/* Curried shouldn't place infix */
let res = (|>) first;

/* Custom infix with labeled args */
let (|>) first::first second::second => first + second;

/* Should NOT reformat named args to actually be placed infix */
let res = (|>) first::first second::second;

/* Curried shouldn't place infix */
let res = (|>) first::first;

/* Custom infix accepting *three* without labeled args */
let (|>) firsfirst second third =>
  first + second + third;

/* Should reformat to actually be placed infix if passed two args */
let res = first |> second;

let res = (first |> second) third;

/* Should NOT reformat to be placed infix if passed all three */
let res = (|>) first second third;

/* Same: Curried shouldn't place infix */
let res = (|>) first;

/* In fact, if even just one of the arguments are named, it shouldn't
 * be formatted or parsed as infix! */
(|>) first second::second;

(|>) first::first second;

(|>) first second third::third;

(first |> second) third::third;

/* Infix has lower precedence than function application */
first |> second third::third;

let leftAssocGrouping = first |> second |> third;

let rightAssocGrouping = first ^> second ^> third;

/* It's definitely the caret. */
let seeWhichCharacterHasHigherPrecedence =
  first |> second ^> third;

let seeWhichCharacterHasHigherPrecedence = first
                                        ^> second |> third;

let seeWhichCharacterHasHigherPrecedence = first
                                        ^> (
                                        second |> third
                                        ) |> fourth;

let res =
  blah &&
    DataConstructor 10 && DataConstructor 10 + 10;

/* Should be parsed as */
let res =
  blah &&
    DataConstructor 10 && DataConstructor 10 + 10;

let (++) label::label label2::label2 => label + label2;

let (++) label::label label2::label2 => label + label2;

let (++) = (++);

let (++): int => int = (++);

(++) label::20 label2::30 + 40;

/* Should be parsed as: */
(++) label::20 label2::30 + 40;

/* Great idea! */
let (=) a b => a < 0;

let (=) a b => a < 0;

let (=) = (=);

let (=): int => int = (=);

let equal = Pervasives.(=);

let starInfix_makeSureSpacesSurround = ( * );

let starInfix_makeSureSpacesSurround = ( *\*\* );

/* The following two should be equivalently parsed/printed.  */
let includesACommentCloseInIdentifier = ( *\*\/ );

let includesACommentCloseInIdentifier = ( *\*\/ );

let
  shouldSimplifyAnythingExceptApplicationAndConstruction =
  call "hi" ^
    (
      switch x {
        | _ => "hi"
      }
    ) ^ 
    "yo";

/* Add tests with IF/then mixed with infix/constructor application on left and right sides */
/**
 * Every star or forward slash after the character of an infix operator must be
 * escaped.
 */
let ( /\* ) a b => a + b;

let x = 12 /-\* 23 /-\* 12;

let y = a /\* b;

let ( !=\* ) q r => q + r;

let res = q ( !=\* ) r;

let ( !=\/\* ) q r => q + r;

let res = q ( !=\/\* ) r;

let ( ~\* ) a => a + 1;

let res = ~\*10;

/* The semicolon should be attached to someType */
let myFunc 
    aaaa 
    bbbb 
    cccc 
    dddd 
    aaaa 
    bbbb 
    cccc 
    dddd 
    aaaa => [
  blah
    aaaa bbbb cccc dddd aaaa bbbb cccc dddd aaaa, 
  ...someType
];
let module M = Something.Create {
  type resource1 = MyModule.MySubmodule.t;
  type resource2 = MyModule.MySubmodule.t;
};
/*
let str = "@[.... some formatting ....@\n\010@.";
*/
let str = "@[.... some formatting ....@\n\n@.";
/**
 * Generally, dangling attributes [@..] apply to everything to the left of it,
 * up until a comma, equals asignment, arrow, bar, or infix symbol (+/-) or
 * prefix.
 *
 * This has a nice side effect when printing the terms:
 * If a node has attributes attached to it,
 */
/**
 * Core language features:
 * ----------------------
 */
type x = int [@@itemAttributeOnTypeDef];

type attributedInt = int [@onTopLevelTypeDef];

type attributedIntsInTuple = (
  int [@onInt], 
  float [@onFloat]
) 
[@@onTopLevelTypeDef];

type myDataType 'x 'y = | MyDataType of 'x 'y;

type myType =
  (
    myDataType
      ((option int) [@onOptionInt]) 
      ((option float) [@onOption])
  ) 
  [@onEntireType];

let thisInst: myType =
  MyDataType (Some 10) (Some 10.0) 
  [@attOnEntireDatatype];

let thisInst: myType =
  MyDataType
    (Some 10 [@onFirstParam]) (Some 10.0) 
  [@attOnEntireDatatype];

let x = "hello" [@onHello];

let x = "hello" [@onHello];

let x = "hello" ^ ("goodbye" [@onGoodbye]);

let x = ("hello" [@onHello]) ^ "goodbye";

let x = ("hello" [@onHello]) ^ "goodbye";

let x = "hello" ^ ("goodbye" [@onGoodbye]);

let x = ("hello" ^ "goodbye") [@onEverything];

let x = 10 + (20 [@on20]);

let x = 10 + (20 [@on20]);

let x = (10 [@on10]) + 20;

let x = (10 [@on10]) + 20;

let x = (10 + 20) [@attrEverything];

let x = 10 - (20 [@on20]);

let x = 10 - (20 [@on20]);

let x = (10 [@on10]) - 20;

let x = (10 [@on10]) - 20;

let x = (10 - 20) [@attrEntireEverything];

let x = true && (false [@onFalse]);

let x = true && (false [@onFalse]);

let x = (true [@onTrue]) && false;

let x = (true [@onTrue]) && false;

let x = (true && false) [@attrEverything];

/* now make sure to try with variants (tagged and `) */
/**
 * How attribute parsings respond to other syntactic constructs.
 */
let add a => a [@onRet];

let add a => a [@onRet];

let add = (fun a => a) [@onEntireFunction];

let res =
  if true {
    false
  } else {
    false [@onFalse]
  };

let res =
  (
    if true {
      false
    } else {
      false
    }
  ) 
  [@onEntireIf];

let add a b => ((a [@onA]) + b) [@onEverything];

let add a b =>
  ((a [@onA]) + (b [@onB])) [@onEverything];

let add a b => a + (b [@onB]);

let both = (fun a => a) [@onEntireFunction];

let both a b => ((a [@onA]) && b) [@onEverything];

let both a b => (a [@onA]) && (b [@onB] [@onB]);

let both a b => (a && b) [@onEverything];

let thisVal = 10;

let x = 20 + 
  - 
  (add thisVal thisVal [@onFunctionCall])
;

let x =
  (20 + - (add thisVal thisVal)) [@onEverything];

let x = - (add thisVal thisVal [@onFunctionCall]);

let x = - (add thisVal thisVal) [@onEverything];

let bothTrue x y => {contents: x && y};

let something =
  !(bothTrue true true)
    [@onEverythingToRightOfEquals];

let res =
  add 2 4 [@appliesToEntireFunctionApplication];

add 2 4 [@appliesToEntireFunctionApplication];

let myObj = {method p () => {method z () => 10}};

let result =
  (myObj#p () [@attOnFirstSend])#z
    () [@onSecondSend];

type recordFunctions = {
  p: unit => recordFunctions [@onUnit], 
  q: (unit => unit) [@onArrow]
} 
[@@onRecordFunctions]
and unusedType = unit [@@onUnusedType];

let rec myRecord = {
  p: fun () => myRecord, 
  q: fun () => ()
}
and unused = ();

let result =
  (myRecord.p () [@attOnFirstSend]).q
    () [@onSecondSend];

type variantType =
  | Foo of int [@onInt] 
  | Bar of (int [@onInt]) 
  | Baz 
[@@onVariantType];

type gadtType 'x =
  | Foo of int :(gadtType int) [@onFirstRow] 
  | Bar of
      (int [@onInt]) 
      :(gadtType unit) [@onSecondRow] 
  | Baz
      :(gadtType (unit [@onUnit])) [@onThirdRow] 
[@@onVariantType];

[@@@floatingTopLevelStructureItem hello];

print_string "hello";

let firstBinding = "first"
and secondBinding = "second";

/**
 * Let bindings.
 * ----------------------
 */
let showLets () =>
  {
    let tmp = 20;
    {
      let tmpTmp = tmp + tmp;
      tmpTmp + tmpTmp
    } 
    [@onFinalLet]
  } 
  [@onOuterLet];

/**
 * Classes:
 * ------------
 */
/**
 * In curried sugar, the class_expr attribute will apply to the return.
 */
class boxA 'a (init: 'a) =>
  {
    method pr = init + init + init;
  } 
  [@onReturnClassExpr] 
[@@moduleItemAttribute];

/**
 * In non-curried sugar, the class_expr still sticks to "the simple thing".
 */
class boxB 'a (init: 'a) =>
  {
    method pr = init + init + init;
  } 
  [@stillOnTheReturnBecauseItsSimple];

/* To be able to put an attribute on just the return in that case, use
 * parens. */
class boxC 'a =
  (
    fun (init: 'a) =>
      {
        method pr = init + init + init;
      } 
      [@onReturnClassExpr]
  ) 
  [@onEntireFunction] 
[@@onBoxC x; y;];

class tupleClass 'a 'b (init: ('a, 'b)) => {
  let one = 10 [@exprAttr ten]; 
  let two = 20 [@exprAttr twenty]
  and three = 30 [@exprAttr twenty]; 
  method pr = one + two + three;
} 
[@@moduleItemAttribute onTheTupleClassItem];

class type addablePointClassType = {
  method x: int; 
  method y: int; 
  method add:
    addablePointClassType => 
    addablePointClassType => 
    int
} 
[@@structureItem]
and anotherClassType = {
  method foo: int; 
  method bar: int
} 
[@@structureItem];

let module NestedModule = {
  [@@@floatingNestedStructureItem hello];
};

module type HasAttrs = {
  type t = int [@@onTypeDef]; 
  [@@@floatingNestedSigItem hello]; 
  class type foo = {
    method foo: int; 
    method bar: int
  } 
  [@@sigItem]; 
  class fooBar : int => new foo [@@sigItem];
};
type reasonXyz =
  | X | Y of int int int | Z of int int | Q | R;

let reasonDoubleBar =
  fun | X
      | Y _ _ _
      | Z _ _
      | Q => true 
      | _ => false;

let reasonDoubleBarNested =
  fun | X
      | Y _ _ _
      | Z _ _
      | Q => true 
      | _ => false;

/* Liberal use of the Any pattern being compatible with multiple
  arguments  */
let reasonDoubleBarAnyPatterns =
  fun | X
      | Y _
      | Z _
      | Q => true 
      | _ => false;

let reasonDoubleBarNestedAnyPatterns =
  fun | X
      | Y _
      | Z _
      | Q => true 
      | _ => false;
/*
 * Syntax and fallback syntax.

 * vim: set ft=reason:
 */
switch (
  while true {
    ()
  }
) {
  | _ => ()
};

try (
  while true {
    ()
  }
) {
  | _ => ()
};

switch (
  for i in 0 to 10 {
    ()
  }
) {
  | _ => ()
};

try (
  for i in 0 to 10 {
    ()
  }
) {
  | _ => ()
};

switch (
  if true {
    print_string "switching on true"
  } else {
    print_string "switching on false"
  }
) {
  | _ => ()
};

try (
  for i in 0 to 10 {
    ()
  }
) {
  | _ => ()
};

let result =
  (
    while false {
      ()
    }
  ) ==
    () ?
    false : true;

switch (
  try (
    try () {
      | _ => ()
    }
  ) {
    | _ => ()
  }
) {
  | () => ()
};

let shouldStillLoop = {contents: false};

while shouldStillLoop.contents {
  print_string "You're in a while loop";
  print_newline ()
};

while {
  shouldStillLoop.contents <- false;
  shouldStillLoop.contents
} {
  print_string "Will never loop"
};

while ((shouldStillLoop := false) == ()) {
  print_string "Forever in the loop"
};
/**
 * Testing mutations.
 */
let holdsAUnit = ref ();

let holdsABool = ref false;

let holdsAnInt = ref 0;

let holdsAHoldsABool = ref (ref true);

let () = holdsAUnit := holdsABool := false;

/* Should be parsed as: */
/* And so they should both be printed the same */
let () = holdsAUnit := holdsABool := false;

/*
 * The following:
 *
 *   something <- x := e
 *
 * Should be parsed as:
 *
 *   something <- (x := e)
 */
holdsAUnit.contents <- holdsAnInt := 0;

holdsABool.contents <- holdsAnInt.contents == 100;

let numberToSwitchOn = 100;

switch numberToSwitchOn {
  | (-3)
  | (-2)
  | (-1) => ()
  | 0 => holdsAUnit.contents <- ()
  | 1 => holdsAUnit.contents <- holdsAnInt := 0
  | 2 =>
      true ?
        holdsAUnit.contents <- () : 
        holdsABool.contents ? () : ()
  | 3 =>
      true ?
        holdsAUnit := () : 
        holdsABool.contents ? () : ()
  | 4 => true ? holdsAnInt := 40 : ()
  | 5 => holdsAnInt := 40
  | _ => ()
};

let mutativeFunction =
  fun | Some x => holdsAUnit.contents <- () 
      | None => holdsAUnit := ();
class virtual stack 'a init => {
  /*
   * The "as this" is implicit and will be formatted away.
   */
  val virtual dummy: unit; 
  val mutable v: list 'a = (init: list 'a); 
  method virtual implementMe: int => int; 
  method pop =
    switch v {
      | [hd, ...tl] => {
          v <- tl;
          Some hd
        }
      | [] => None
    }; 
  method push hd => v <- [hd, ...v]; 
  initializer =>
    print_string "initializing object";
};

let tmp = {
  /**
   * comment here.
   */ 
  val x = 10
};

/**
 * Comment on stackWithAttributes.
 */
class virtual stackWithAttributes 'a init =>
  /* Before class */
  {
    /* The "as this" should not be formatted away because attributes. */
    as this [@thisShouldntBeFormattedAway]; 
    /* Before floatting attribute */
    [@@@floatingAttribute]; 
    /* Virtual member */
    val virtual dummy: unit; 
    val mutable v: list 'a = (init: list 'a); 
    method virtual implementMe: int => int; 
    method pop =
      switch v {
        | [hd, ...tl] => {
            v <- tl;
            Some hd
          }
        | [] => None
      }; 
    method push hd => v <- [hd, ...v]; 
    initializer =>
      print_string "initializing object";
  } 
[@@x];

class extendedStack 'a init => {
  inherit (class stack 'a) init; 
  val dummy = (); 
  method implementMe i => i;
};

class extendedStackAcknowledgeOverride 'a init => {
  inherit (class stack 'a) init; 
  val dummy = (); 
  method implementMe i => i + 1;
};

let inst = (new extendedStack) [1, 2];

/**
 * Recursive classes.
 */
/*
 * First recursive class.
 */
class firstRecursiveClass init => {
  val v = init;
}
/*
 * Second recursive class.
 */
and secondRecursiveClass init => {
  val v = init;
};

/**
 * For now, mostly for historic reasons, the syntax for type
 * definitions/annotations on anonymous objects are different than
 * "class_instance_type". That needn't be the case. The only challenge is that
 * whatever we do, there is a slight challenge in avoiding conflicts with
 * records. Clearly {x:int, y:int} will conflict. However, open object types in
 * the form of {x:int, y:int, ..} do not conflict. The only thing that must be
 * resolved is closed object types and records. you could have a special token
 * that means "closed". {x: int, y:int .}. If only closed object types would be
 * optimized in the same way that records are, records could just be replaced
 * with closed object types.
 */
/**
 * Anonymous objects.
 */
type typeDefForClosedObj = <x : int, y : int>;

type typeDefForOpenObj 'a =
  <x : int, y : int, ..> as 'a;

let anonClosedObject: <x : int, y : int> = {
  method x = 0; 
  method y = 0
};

let acceptsOpenAnonObjAsArg 
    (o: <x : int, y : int, ..>) => o#x + o#y;

let acceptsClosedAnonObjAsArg 
    (o: <x : int, y : int>) => o#x + o#y;

let res = acceptsOpenAnonObjAsArg {
  method x = 0; 
  method y = 10
};

let res = acceptsOpenAnonObjAsArg {
  method x = 0; 
  method y = 10; 
  method z = 10
};

let res = acceptsClosedAnonObjAsArg {
  method x = 0; 
  method y = 10
};

/* TODO: Unify class constructor return values with function return values */
class myClassWithAnnotatedReturnType 
      init 
      :{
         method x: int; 
         method y: int
       } => {
  method x: int = init; 
  method y = init;
};

/**
 * May include a trailing semi after type row.
 */
class myClassWithAnnotatedReturnType2 
      init 
      :{
         method x: int; 
         method y: int
       } => {
  method x: int = init; 
  method y = init;
};

/**
 * May use equals sign, and may include colon if so.
 */
class myClassWithAnnotatedReturnType3 
      init 
      :{
         method x: int; 
         method y: int
       } => {
  method x: int = init; 
  method y: int = init;
};

/**
 * The one difference between class_constructor_types and expression
 * constraints, is that we have to include the prefix word "new" before the
 * final component of any arrow. This isn't required when annotating just the
 * return value with ": foo ".
 * This is only to temporarily work around a parsing conflict.  (Can't tell if
 * in the final arrow component it should begin parsing a non_arrowed_core_type
 * or a class_instance_type). A better solution, would be to include
 * class_instance_type as *part* of core_type, but then fail when it is
 * observed in the non-last arrow position, or if a non_arrowed_core_type
 * appears in the last arrow position.
 *
 * class_instance_type wouldn't always fail if parsed as any "core type"
 * everywhere else in the grammar.
 *
 * Once nuance to that would be making a parse rule for "type application", and
 * deferring whether or not that becomes a Pcty_constr or a Ptyp_constr. (The
 * same for type identifiers and extensions.)
 */
class
  myClassWithAnnotatedReturnType3_annotated_constructor:
  int => 
  new {
    method x: int; 
    method y: int
  } =
  fun init => {
    method x: int = init; 
    method y: int = init;
  };

class tupleClass 'a 'b (init: ('a, 'b)) => {
  method pr = init;
};

let module HasTupleClasses: {
  /**
   * exportedClass.
   */ 
  class exportedClass :
    int => 
    new {
      method x: int; 
      method y: int
    }; 
  /**
   * anotherExportedClass.
   */ 
  class anotherExportedClass 'a 'b :
    ('a, 'b) => 
    new {
      method pr: ('a, 'b)
    };
} = {
  /**
   * exportedClass.
   */
  class exportedClass =
    class myClassWithAnnotatedReturnType3;
  /**
   * anotherExportedClass.
   */
  class anotherExportedClass 'a 'b =
    class tupleClass 'a 'b;
};

class intTuples = class tupleClass int int;

class intTuplesHardcoded = (
  class tupleClass int int
) (
  8, 
  8
);

/**
 * Note that the inner tupleClass doesn't have the "class" prefix because
 * they're not kinds of classes - they're types of *values*.
 * The parens here shouldn't be required.
 */
class intTuplesTuples =
  class tupleClass
    (tupleClass int int) (tupleClass int int);

let x: tupleClass int int = {
  method pr = (10, 10)
};

let x: #tupleClass int int = x;

let incrementMyClassInstance:
  int => 
  #tupleClass int int => 
  #tupleClass int int =
  fun i inst => {
    let (x, y) = inst#pr;
    {method pr = (x + i, y + i)}
  };

class myClassWithNoTypeParams = {};

/**
 * The #myClassWithNoTypeParams should be treated as "simple"
 */
type optionalMyClassSubtype 'a =
  option #myClassWithNoTypeParams as 'a;

/**
 * Remember, "class type" is really "class_instance_type" (which is the type of
 * what is returned from the constructor).
 *
 * And when defining a class:
 *
 *   addablePoint is the "class instance type" type generated in scope which is
 *   the closed object type of the return value of the constructor.
 *
 *   #addablePoint is the extensible form of addablePoint (anything that
 *   adheres to the "interface.")
 */
class type addablePointClassType = {
  method x: int; 
  method y: int; 
  method add:
    addablePointClassType => 
    addablePointClassType => 
    int
};

/**
 * Class constructor types can be annotated.
 */
class addablePoint:
  int => new addablePointClassType =
  fun init => {
    as self; 
    method add 
           (one: addablePointClassType) 
           (two: addablePointClassType) =>
      one#x + two#x + one#y + two#x; 
    method x: int = init; 
    method y = init;
  };

class addablePoint2:
  int => new addablePointClassType =
  fun init => {
    as self; 
    method add 
           (one: addablePointClassType) 
           (two: addablePointClassType) =>
      one#x + two#x + one#y + two#x; 
    method x: int = init; 
    method y = init;
  };
/**
 * Testing Sequences.
 */
let result = {
  let twenty = 20;
  let result = twenty;
  result
};

/* Final semicolon is not required */
let result = {
  let twenty = result;
  twenty
};

let anInt = result + 20;

let twenty = 20;

/**
 * Each of these are a sequence with a single item - they will be
 * printed in reduced form because sequences are a *parse* time construct.
 * To ensure these are parsed correctly, adding to an integer.
 */
let result = 0 + twenty;

let result = 0 + twenty;

let result = 0 + twenty;

let unitValue = ();

/* While loops/for loops merely accept a "simple expression" (which means
 * it is either a simple token or balanced with parens/braces). However,
 * the formatter ensures that the bodies are printed in "sequence" form even if
 * it's not required.
 */
while false {
  unitValue
};

while false {
  print_string "test"
};

while false {
  print_string "test"
};

type myRecord = {number: int};

let x = {number: 20};

let number = 20;

/*
 * The (mild) consequence of not requiring a final semi in a sequence,
 * is that we can no longer "pun" a single field record (which would)
 * be very rare anyways.
 */
let cannotPunASingleFieldRecord = {
  number: number
};

let fourty = 20 + cannotPunASingleFieldRecord.number;

let thisIsASequenceNotPunedRecord = number;

let fourty = 20 + thisIsASequenceNotPunedRecord;

type recordType = {a: int, b: int, c: int};

let a = 0;

let b = 0;

let c = 0;

/* All of these will be printed as punned because they have more than one field. */
let firstFieldPunned = {a, b, c};

let sndFieldPunned = {a, b, c};

let thirdFieldPunned = {a, b, c};

let singlePunAcceptedIfExtended = {
  ...firstFieldPunned, 
  a
};
/*
 * Testing pattern matching using ml syntax to exercise nesting of cases.
 */
type xyz =
  | X | Y of int int int | Z of int int | Q | R;

let doubleBar =
  fun | X
      | Y _ _ _
      | Z _ _
      | Q => true 
      | _ => false;

let doubleBarNested =
  fun | X
      | Y _ _ _
      | Z _ _
      | Q => true 
      | _ => false;

/* Liberal use of the Any pattern being compatible with multiple arguments  */
let doubleBarAnyPatterns =
  fun | X
      | Y _
      | Z _
      | Q => true 
      | _ => false;

let doubleBarNestedAnyPatterns =
  fun | X
      | Y _
      | Z _
      | Q => true 
      | _ => false;

type bcd = | B | C | D | E;

type a = | A of bcd;

let result =
  switch B {
    | B
    | C
    | D
    | E => ()
  };

let nested_match =
  fun | A (B | C | D | E) => 3;

let module EM = {exception E of int int;};

exception Ealias = EM.E;
