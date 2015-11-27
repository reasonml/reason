[@@@autoFormat let wrap=80; let shift=2;];
Modules.run ();
Polymorphism.run ();
Variants.run();
BasicStructures.run();

TestUtils.printSection "General Syntax";
/* Won't work! */
/* let matchingFunc a = match a with */
/*   `Thingy x => (print_string "matched thingy x"); x */
/*   | `Other x => (print_string "matched other x"); x;; */
/*  */
let matchingFunc a => switch a {
  | `Thingy x => {
    print_string "matched thingy x";
    let zz = 10;
    zz;
  }
  | `Other x => (
    print_string "matched other x";
    x;
  )
};

type firstTwoShouldBeGroupedInParens =
    (int => int) => int => int;
type allParensCanBeRemoved =
    int => (int => (int => int));
type firstTwoShouldBeGroupedAndFirstThree =
    ((int => int) => int) => int;
/* Same thing now but with type constructors instead of each int */
type firstTwoShouldBeGroupedInParens =
    (list int => list int) => list int => list int;
type allParensCanBeRemoved =
    list int => (list int => (list int => list int));
type firstTwoShouldBeGroupedAndFirstThree =
    ((list int => list int) => list int) => list int;

type myRecordType = {
  firstTwoShouldBeGroupedInParens:
    (int => int) => int => int,
  allParensCanBeRemoved:
    int => (int => (int => int)),
  firstTwoShouldBeGroupedAndFirstThree:
    ((int => int) => int) => int,
};

type firstNamedArgShouldBeGroupedInParens =
    first::(int => int) => second::int => int;
type allParensCanBeRemoved =
    first::int => (second::int => (third::int => int));
type firstTwoShouldBeGroupedAndFirstThree =
    first::((int => int) => int) => int;

/* Same thing now, but with type constructors instead of int */
type firstNamedArgShouldBeGroupedInParens =
    first::(list int => list int) => second::list int => list int;
type allParensCanBeRemoved =
    first::list int => (second::list int => (third::list int => list int));
type firstTwoShouldBeGroupedAndFirstThree =
    first::((list int => list int) => list int) => list int;


type firstNamedArgShouldBeGroupedInParens =
    first::(int => int)? => second::int list? => int;
/* The arrow necessitates parens around the next two args. The ? isn't what
 * makes the parens necessary. */
type firstNamedArgShouldBeGroupedInParensAndSecondNamedArg =
    first::(int => int)? => second::(int => int)? => int;
type allParensCanBeRemoved =
    first::int? => (second::int? => (third::int? => int));
type firstTwoShouldBeGroupedAndFirstThree =
    first::((int => int) => int) => int;

type noParens = one::int=>int=>int => two::int => int;
type noParensNeeded = one::int=>(int=>(int => (two::int => int)));
type firstNamedArgNeedsParens = one::(int=>int=>int) => two::int => int;

/* Now, let's try type aliasing */
/* Unless wrapped in parens, types between arrows may not be aliased, may not
 * themselves be arrows. */

type parensRequiredAroundFirstArg = (list int as 'a) => int as 'a;

type parensRequiredAroundReturnType = (list int as 'a) => (int as 'a);

type parensRequiredAroundReturnType = (list int as 'a) => (int as 'a) as 'b;

type noParensNeededWhenInTuple = (list int as 'a, list int as 'b) as 'entireThing;

type myTypeDef 'a = list 'a;

type instatiatedTypeDef = myTypeDef int => int;

/* Test a type attribute for good measure */
/* We should clean up all of the attribute tagging eventually, but for now,
 * let's make it super ugly to get out of the way of all the formatting/parsing
 * implementations (fewer conflicts during parsing, fewer edge cases during
 * printing).
 */
type something = (int, (int [@lookAtThisAttribute] ));

type longWrappingTypeDefinitionExample =
  M_ReactKit__Gesture.Types.instance
    (
      TapGestureRecognizer.tapGestureFields
        unit
        unit

    )
    (
      TapGestureRecognizer.tapGestureMethods
        unit
        unit

    )
  ;
type semiLongWrappingTypeDefinitionExample =
  M_ReactKit__Gesture.Types.instance
    TapGestureRecognizerFinal.tapGestureFields
    TapGestureRecognizerFinal.tapGestureMethods
  ;

type semiLongWrappingTypeWithConstraint =
  M_ReactKit__Gesture.Types.instance 'a
    TapGestureRecognizerFinal.tapGestureFields
    TapGestureRecognizerFinal.tapGestureMethods
    constraint 'a = (unit, unit)
  ;



/* This must be in trunk but not in this branch of OCaml */
/* type withNestedRecords = MyConstructor of {myField: int} */

type colors =
  | Red of int
  | Black of int
  | Green of int;

/* Another approach is to require declared variants to wrap any record */
/* type myRecord = MyRecord of {name: int}; */
/* let myValue = MyRecord {name: int}; */
/* This would force importing of the module */
/* This would also lend itself naturally to pattern matching - and avoid having
to use `.` operator at all since you normally destructure. */
type nameBlahType = {nameBlah: int};
let myRecord = {nameBlah: 20};
let myRecordName = myRecord.nameBlah;

let {nameBlah}:nameBlahType = {nameBlah: 20};
print_int nameBlah;
let {nameBlah: aliasedToThisVar}:nameBlahType = {nameBlah: 20};
print_int aliasedToThisVar;


let desiredFormattingForWrappedLambda:
  int => int => int => nameBlahType =
/*

 fun is
 pre-   /firstarg\
 fix   /-coupled--\
  |-\ /-to-prefix--\       */
  fun curriedArg
      anotherArg
      lastArg => {
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
  fun curriedArg
      anotherArg
      lastArg => {
    nameBlah: 10
  };

let desiredFormattingForWrappedLambdaReturnOnNewLine =
/*

 fun is
 pre-   /firstarg\
 fix   /-coupled--\
  |-\ /-to-prefix--\       */
  fun curriedArg
      anotherArg
      lastArg =>
  {
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
    lastArg =>
{
  nameBlah: 10
};


/*
  let  : type t1 t2. t1 * t2 list -> t1 = ...
  let rec f : 't1 't2. 't1 * 't2 list -> 't1 =
    fun (type t1) (type t2) -> (... : t1 * t2 list -> t1)
*/

type point = {x: int, y:int};
type point3D = {x: int, y:int, z:int};
let point2D = {
  x: 20,
  y: 30
};

let point3D:point3D = {
  x: 10,
  y: 11,
  z: 80,    /* Optional Comma */
};

let printPoint (p:point) => {
  print_int p.x;
  print_int p.y;
};

let addPoints (p1:point, p2:point) => {
  x: p1.x + p2.x,
  y: p1.y + p2.y
};

let res1 = printPoint point2D;
let res2 = printPoint {x:point3D.x, y:point3D.y};

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

let res3 = printPoint (addPoints (point2D, {x:point3D.x, y:point3D.y}));

type person = {age: int, name: string};
type hiredPerson = {age: string, name: string, dateHired: int};

let o: (person) = {name: "bob", age: 10};

/* Parens needed? Nope! */
let o: person = {name: "bob", age: 10};

let printPerson (p: person) => (
  let q: person = p;
  p.name ^ p.name;
);

/* let dontParseMeBro x y:int = x = y;*/

/* With this unification, anywhere eyou see `= fun` you can just ommit it */
let blah = fun a => a;         /* Done */
let blah a => a;               /* Done (almost) */

let blah = fun a b => a;       /* Done */
let blah a b => a;             /* Done (almost) */

/* More than one consecutive pattern must have a single case */
type blah = {blahBlah: int};
let blah = fun (a) {blahBlah} => a;
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

let onlyDoingThisTopLevelLetToBypassTopLevelSequence = {
    let x = {
      print_int 1;
      print_int 20;  /* Missing trailing SEMI */
    };

    let x = {
      print_int 1;
      print_int 20;   /* Ensure missing middle SEMI reported well */
      print_int 20;
    };

    let x = {
      print_int 1;
      print_int 20;
      10;
    };  /* Missing final SEMI */
    x + x;
};

type hasA = {a:int};
let a = 10;
let returnsASequenceExpressionWithASingleIdentifier () => {a};
let thisReturnsA () => {a;};
let thisReturnsAAsWell () => a;

let recordVal:int = (thisReturnsARecord ()).a;
Printf.printf "\nproof that thisReturnsARecord: %n\n" recordVal;
Printf.printf "\nproof that thisReturnsA: %n\n" (thisReturnsA());

/* Pattern matching */
let blah = fun arg => switch arg {
  /* Comment before Bar */
  | /* Comment between bar/pattern */
    Red _ => 1
  /* Comment Before non-first bar */
  | /* Comment betwen bar/pattern */
    Black _ => 0
  | Green _ => 0
};

/* Any function that pattern matches a multicase match is interpretted as a
 * single arg that is then matched on. Instead of the above `blah` example:*/

let blah = fun
  | Red _ => 1
  | Black _ => 0
  | Green _ => 1;

/* `fun a => a` is read as "a function that maps a to a". Then the */
/* above example is read: "a function that 'either maps' Red to.. or maps .." */
/* Thc00f564e first bar is read as "either maps" */


/* Curried form is not supported:
   let blah x | Red _ => 1 | Black _ => 0;
   Theres no sugar rule for dropping => fun, only = fun
*/

let blahCurriedX x => fun  /* See, nothing says we can drop the => fun */
  |(|Red x |Black x |Green x) => 1     /* With some effort, we can ammend the sugar rule that would */
  | Black x => 0                       /* Allow us to drop any => fun.. Just need to make pattern matching */
  | Green x => 0;                      /* Support that */

let blahCurriedX x => fun
  | Red x |Black x |Green x => 1
  | Black x => 0
  | Green x => 0;

/* Any time there are multiple match cases we require a leading BAR */

let v = Red 10;
let (|Black x |Red x |Green x) = v;    /* So this NON-function still parses */

/* This doesn't parse, however (and it doesn't in OCaml either):
  let | Black x | Red x | Green x = v;
*/

print_int x;

/* TODO: Why isn't curried form possible? */
/* TODO: Had to require a leading BAR to support OR patterns with consistent function syntax

     let blah | Red _ => something   /*So this isn't mistaken for an OR pattern */
     let | Black x | Red x = v;;  /* And so this NON-function still parses */
     let someDefault | (Black x | Red x) = v;;

     But what if we *required* a BAR at the first case of any match?
     That should resolve this issue which allows currying.
*/


/* Scoping: Let sequences. Familiar syntax for lexical ML style scope and
sequences. */

let res = {
  let a = "a starts out as";
  {
    print_string (a);
    let a = 20;
    print_int a;
  };
  print_string a;
};

let res = {
  let a = "first its a string";
  let a = 20;
  print_int a;
  print_int a;
  print_int a;
};

let res = {
  let a = "a is always a string";
  print_string a;
  let b = 30;
  print_int b;
};


/* let result = LyList.map (fun | [] => true | _ => false) []; */



/* OTHERWISE: You cannot tell if a is the first match case falling through or
 * a curried first arg */
/* let blah = fun a | patt => 0 | anotherPatt => 1; */
/* let blah a patt => 0 | anotherPatt => 1; */

                /*simple pattern  EQUALGREATER      expr */
let blah      a       {blahBlah}         =>             a;

               /*            match_case             */
               /* simple_pattern EQUALGREATER  expr */
let blah = fun   |Red _           =>      1   |Black _ => 0 |Green _ => 0;
/* This last form using `fun` is possible because we got rid of implicit
sequences! Notice how we required semicolons in the Red case. */

/* So either we have:

let ident optional_equal_fun simplePatterns fat_arrow expr
let ident optional_equal_fun     (onePattern fat_arrow expr)*
*/



/* Won't work! */
/* let arrowFunc = fun a b => print_string "returning aplusb from arrow"; a + b;;  */
let arrowFunc = fun a b => (print_string "returning aplusb from arrow"; a + b;);
let add a b => {
  let extra = (print_string "adding"; 0;);
  let anotherExtra = 0;
  extra + a + b + anotherExtra;
};

(print_string (string_of_int (add 4 34)));

let dummy _ => 10;
dummy res1;
dummy res2;
dummy res3;


/* Some edge cases */
let myFun firstArg (|Red x | Black x |Green x) => firstArg + x;
let matchesWithWhen a => switch a {
  | Red x when 1 > 0 => 10
  | Red _ => 10
  | Black x => 10
  | Green x => 10
};

let matchesWithWhen = fun
  | Red x when 1 > 0 => 10
  | Red _ => 10
  | Black x => 10
  | Green x => 10;


/* This is also a nice extension of the simple curried pattern form with one pattern */

let matchesOne (|`Red x) => 10;


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
  addThreeNumbersTupled: fun (a, b, c) => a + b + c
};

let result = myRecordWithFunctions.addThreeNumbers 10 20 30;
let result = myRecordWithFunctions.addThreeNumbersTupled (10, 20, 30);

/*
Reverse module invocation syntax: Makes OO people feel more comfortable with
the module system.

You can optionally do:

  List.map myList myFunc;

Or:

  myList.List.map myFunc

The benefits of separating the data from the operations that act on it can be
explained as beneficial. It seems like the right separation of concerens to
encourage. It allows *local* extension. You can "add features" to your
"objects" only locally without having to extend or polute their
implementations. Type classes can then be seen as automatically selecting the
module in the middle.

       /?\
  myList.map myFunc

This style only really makes sense when `myList` is of an opaque type.
Otherwise, things get really strange when it is a record type.

*/


let lookTuplesRequireParens = (1, 2);
/* let thisDoesntParse = 1, 2;  */
let tupleInsideAParenSequence = (
  print_string "look, a tuple inside a sequence";
  let x = 10;
  (x, x);
);

let tupleInsideALetSequence = {
  print_string "look, a tuple inside a sequence";
  let x = 10;
  (x, x);
};

/*
 * Going to *not* fix this yet. It's simple enough to say (type annotations
 * require wrapping in parens). That requirement might also make it much easier
 * to avoid conflicts with colon based named arguments.
 * However, we *can* just make a special case for when you are inside a tuple
 * expression.
 * But how?
 * Here's a named arguments invocation
 * (callFunction a:blah)
 * (callFunction a:blah) <- if no parens around tuple field annotations, this
 *                         is a valid tuple containing the field (callFunction
 *                         a), which is expected to return an blah, but it is
 *                         also a named argument a passing a variable named
 *                         `blah`.
 * We could require that named arguments are invoked with wrapped brackets []
 * but that's bad because you loose the nice named args invocation in cases
 * where you don't actually need to wrap in parens.
 * let x = someFunction a:field b:field;
 * let y = another a:field b:field;
 * So it's best to just say "all type annotations must be wrapped in parens".
 * That *does* leave you with the strange fact that the single tuple doesn't
 * have to be wrapped in a type annotation.
 *
 * (a:int)
 */

/* Here is why we must have separate syntax for curried function matching and type signatures */
/*                 function return type */
/*                      \        /      */
/* let makeIncrementer delta:int=>int => fun a => a + delta; */

/* Imagine if we were to turn the `=>` into a `=>`, we'd have no clue where the
type ends and the function body begins. */

/* So instead, we could *require* that function return types be wrapped in
parenthesis which wouldn't be so horrible considering *all the other arguments*
must already be wrapped! In this example, there's no ambiguity */
let makeIncrementer (delta:int) :(int=>int) => fun a => a + delta;

/* We could even force that consistency with let bindings - it's allowed
currently but not forced */
let (myAnnotatedValBinding:int) = 10;

/* Function Return Value Typing */
/* --------------------------------*/

/* Having to wrap the final return type makes a lot of sense, especially since
the paren is required before the `:`. It's the exact same for the arguments.
The thing before the colon is the name. But the return value has no name, so
it's a ghost */
/* let add (a:int) (b:int) (:int) => a + b; */
/* Is secretly the same as */
/* let add (a:int) (b:int) (`returnValue`:int) => a + b; */
/* This also makes functions much more readable when *only* the return type is
 * annotated. */
/* let add a b (:int) = a + b; */
/* Compare that to OCaml's:
     let add a b :int = a + b;
*/

/* It might also make sense to optimize for the named argument situation since
I would want to encourage them to be used as frequently as in objective-c.
So maybe non-named arguments require wrapping parens?
*/

/* Or maybe it makes more sense to just provide a way to annotate a function
declaration above. How would we do forall types anyways? (I guess type
parameters solve that problem syntactically) */


/* Class functions (constructors) and methods are unified in the same way */

class classWithNoArg => object
  method x => 0
  method y => 0
end;

/* This parses but doesn't type check
  class myClass init => object
    method x => init
    method y => init
  end;
*/

/* TODO: Unify class constructor return values with function return values */
class myClassWithAnnotatedReturnType init (:object method x:int method y:int end) => object
  method x => init
  method y => init
end;

let myFunc (a:int) (b:int) :(int, int) => (a, b);
let myFunc (a:int) (b:int) :list int => [1];
let myFunc (a:int) (b:int) :point => {
  x: a,
  y: b
};
let myFunc (a:int, b:int) :point => {
  x: a,
  y: b
};

/*

  Q: Won't this interfere with type constructors
   type t = C of t1 * t2;
   let x = C (1, 1);
  A: It souldn't so long as this is interpretted as a type constructor passing
  a single tuple. It isn't currently possible to have a type constructor that
  accepts multiple arguments in OCaml. Revised syntax allows for (C x y) which
  is distinct from C (x, y). I don't think requiring that tuple types are
  written as (t1, t2) makes achieving any of this more difficult.
  Actually, what SugarML does helps a bit.
  There were previously two ways to define a variant tuple type.

  type t = Black of int*int*int
  type t = Black of (int*int*int)

  In the first version int*int*int was actually special cased in the compiler.

  SugarML changes the special case of x*y*z.. to be x y z, and changes
 [core_type] such that parens wrapping comma delimited core_types are the only
 way to express tuples. So, in SugarML, the first example is expressable with:

   type t = Black of int int int

  and the second example must be written as:

    type t = Black of (int, int, int);

  The first kind of unboxed tuples really are different than regular tuples.
  They're not first class (and never were in the stock OCaml, even though their
  syntax would have you believe they were). For now, we fix the syntax issue and
  later add it back correctly.

  Remaining challenge:

  How to distinguish between

*/
type myThing = (int, int);
type stillARecord = {name: string, age: int};

/* TODO: Type variable list doesn't need parens *or* commas! */
/* Rebase latest OCaml to get the following: And fixup
  `generalized_constructor_arguments` according to master. */
/* type ('a, 'b) myOtherThing = Leaf of {first:'a, second: 'b} | Null; */
type branch 'a 'b = {first: 'a, second: 'b};
type myOtherThing 'a 'b = Leaf of (branch 'a 'b) | Null;

/* TODO: Curried Type variable list doesn't need parens *or* commas! */
/* Like the current brackets syntax, but no commas needed, whcih means parens
could then be used to group precedence again */
/* type myOtherThing 'a 'b = Leaf of {first:'a, second: 'b} | Null; */
/* type moreSpecific = myOtherThing int string; */
/* type moreSpecific = myOtherThing (int, int) string; */

type yourThing = myOtherThing int int;

/* Conveniently - this parses exactly how you would intend! No *need* to wrap
in an extra [], but it doesn't hurt */
type lookAtThesePolyVariants = list [`Red] ;

type bracketsGroupMultipleParamsAndPrecedence = list (list (list [`Red]));

type youCanWrapExtraIfYouWant = (list [`Red]);

type hereAreMultiplePolyVariants = list [`Red | `Black];
type hereAreMultiplePolyVariantsWithOptionalWrapping = list ([`Red | `Black]);



/*
  /* ES6 style lambdas: */

  /* Currying */
  let lookES6Style = (`Red x) (`Black y) => { };
  let lookES6Style (`Red x) (`Black y) => { };

  /* Matching the single argument */
  let lookES6Style = oneArg => match oneArg with
    | `Red x => x
    | `Black x => x;

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


/* Why strict colon approach for named args can't work: See reverted diff */
/* let punned a: b: => a + b; */
/* let a = 10; */
/* let b = 10; */
/* let result = punned a: b */ /* Is be another argument or value for a: */

/* The only solution would be to create a separate syntax for punned arguments
and parameters (::) */

/*
  let myFunc a:: b:blah => a + blah;

  let _ = myFunction a:10 b:20;
  let a = 10;
  let _ = myFunction a:: b:30;
*/


let a = 10;
let b = 20;
  
/*A*/
let named                 a::a      b::b             => a + b;
type named =              a::int => b::int => int;
/*B*/
let namedAlias            a::aa     b::bb  => aa + bb;
let namedAlias            a::aa     b::bb  => aa + bb;
type namedAlias =         a::int => b::int => int;
/*C*/
let namedAnnot            a::(a:int) b::(b:int)   => 20;
/*D*/
let namedAliasAnnot       a::(aa:int) b::(bb:int) => 20;
/*E*/
let myOptional            a::a=?    b::b=?      ()  => 10;
type named =              a::int? => b::int? => unit => int;
/*F*/
let optionalAlias         a::aa=?   b::bb=?   ()  => 10;
/*G*/
let optionalAnnot         a::(a:int)=? b::(b:int)=? ()  => 10;
/*H*/
let optionalAliasAnnot    a::(aa:int)=? b::(bb:int)=? ()  => 10;
/*I: */
let defOptional           a::a=10    b::b=10  ()  => 10;
type named =              a::int?  => b::int? => unit => int;
/*J*/
let defOptionalAlias      a::aa=10      b::bb=10  ()  => 10;
/*K*/
let defOptionalAnnot      a::(a:int)=10 b::(b:int)=10 ()  => 10;
/*L*/
let defOptionalAliasAnnot a::(aa:int)=10 b::(bb:int)=10 ()  => 10;

/*M: Invoking them - Punned */
let resNotAnnotated = named a::a b::b;
/*N:*/
let resAnnotated    = (named a::a b::b :int);
/*O: Invoking them */
let resNotAnnotated = named a::a b::b;
/*P: Invoking them */
let resAnnotated    = (named a::a b::b :int);

/*Q: Here's why "punning" doesn't work!  */
/* Is b:: punned with a final non-named arg, or is b:: supplied b as one named arg? */
let b = 20;
let resAnnotated    = (named a::a b:: b);

/*R: Proof that there are no ambiguities with return values being annotated */
let resAnnotated    = (named a::a b :ty);


/*S: Explicitly passed optionals are a nice way to say "use the default value"*/
let explictlyPassed =          myOptional a::?None b::?None;
/*T: Annotating the return value of the entire function call */
let explictlyPassedAnnotated = (myOptional a::?None b::?None :int);
/*U: Explicitly passing optional with identifier expression */
let a = None;
let explictlyPassed =           myOptional a::?a b::?None;
let explictlyPassedAnnotated = (myOptional a::?a b::?None :int);


/*
 * Showing many combinations of type annotations and named arguments.
 */

type typeWithNestedNamedArgs =
    outerOne::(innerOne::int => innerTwo::int => int) => outerTwo::int => int;

type typeWithNestedOptionalNamedArgs =
    outerOne::(innerOne::int => innerTwo::int => int)? => outerTwo::int? => int;

type typeWithNestedOptionalNamedArgs =
    outerOne::(list string)? => outerTwo::int? => int;

let x =
  callSomeFunction
    withArg::10
    andOtherArg::wrappedArg;


let res = {
  (constraintedSequenceItem:string);
  (dontKnowWheYoudWantToActuallyDoThis:string);
};

let res = {
  (butTheyWillBePrintedWithAppropriateSpacing : string);
  (soAsToInstillBestDevelopmentPractices : string);
};

let x = [
  eachItemInListCanBeAnnotated:int,
  typeConstraints:float,
  (tupleConstraints:int, andNotFunctionInvocations:int)
];

let x = [
  butWeWillPrint : int,
  themAsSpaceSeparated : float,
  (toInfluenceYour : int, developmentHabbits : int)
];

let newRecord = {
  ...annotatedSpreadRecord:someRec,
  x: y
};

let newRecord = {
  ...annotatedSpreadRecord : someRec,
  blah: 0,
  foo: 1
};

let newRecord = {
  ...youCanEvenCallMethodsHereAndAnnotate them : someRec,
  blah: 0,
  foo: 1
};

let newRecord = {
  ...youCanEvenCallMethodsHereAndAnnotate them named::10 :someRec,
  blah: 0,
  foo: 1
};


let something = (aTypeAnnotation : thing blah);
let something = (thisIsANamedArg: thing blah);


let something = (aTypeAnnotation: thing blah);

  

let something = (thisIsANamedArg thing:blah);
let something = (typeAnnotation thing : blah);

let newRecord = {
  ...(heresAFunctionWithNamedArgs argOne::i :annotatedResult),
  soAsToInstill: 0,
  developmentHabbits: 1
};


/*

 Colon based named arguments:
 Proposal 1:
 =============
 =============

     x = myFunc arg;
     let myFunc arg => ret;
               /---\
              /     \
             /       \
 Colon based named arguments simply extend the grammar to allow prepending
 *any* valid argument pattern with [label:] Along with extending the grammar
 for function invocation with the ability to "prepend" any argument pattern at
 application time.

     x = myFunc label:arg;
     let myFunc label:arg => ret

 Optional arguments extends the grammar to allow prepending *any* valid
 argument pattern with [label:?], and when doing so, also allows *potentially*
 suffixing each argument with [=simple_expr].

 The final extension, is the ability to "forward" the optional-ness at
 application time. (Not discussed below).

 In addition to a couple of "sugars" for punning (See A, E, I - in addition to
 the "Flaws" discussed).

A: let named              a:     b:                            :ret   => a + b;


A: let named              a:     b:                            :ret   => a + b;
B: let namedAlias         a:aa   b:bb                          :ret   => aa + bb;
/* Notice, no punning for C */
C: let namedAnnot         a:(a:int)  b:(b:int)                 :ret   => a + b;
D: let namedAliasAnnot    a:(aa:int) b:(bb:int)                :ret   => aa + bb;
E: let optional           a:?    b:?                           :ret   => 10;
F: let optionalAlias      a:?(aa)  b:?(bb)                     :ret   => 10;
      <parens optional>   a:?aa    b:?bb                       :ret   => 10;
/* Notice, no punning for G */
G: let optionalAnnot      a:?(a:int option) b:?(b:int option)  :ret   => 10;
H: let optionalAliasAnnot a:?(aa:int option) b:?(bb:int option):ret   => 10;
/*
Look! When a default is provided, annotation causes inferred type of argument
to not be "option" since it's automatically destructured (because we know it
will always be available one way or another.)
*/
I: let defOptional          a:?=10     b:?=10                  :ret       => 10;
J: let defOptionalAlias     a:?(aa)=10 b:?(bb)=10              :ret       => 10;
       <parens not needed>  a:?aa=10   b:?bb=10                :ret       => 10;

/* Notice, no punning for K */
K: let defOptionalAnnot      a:?(a:int)=10   b:?(b:int)=10     :ret       => 10;
L: let defOptionalAliasAnnot a:?(aa:int)=10 b:?(bb:int)=10     :ret       => 10;

  /* Invoking them */
  let a = 10;
  let b = 20;
  named a: b:;
  named a:a b:b;
  optional a: b:;
  optional a:a b:b;

  forwardOptional a:?a b:?b;


 Obvious flaws with the above proposal:
 ---------------------------------------
 1. Annotated return types makes there be a conflict in the grammar. Is the
 following a function that takes an [a:] aliased to [aa], or does it take an
 unlabeled [a] and return an [aa]?

     let named a:aa => a + b;

 One way to resolve this issue is to have a separate syntax for resturn types.
 You could either require double colon :: for return types, or require that
 return types be grouped in parens just like (almost) every other type
 annotation.

    let myFunc a:aliasA => a;
    let myFunc a (:retType) => a;

 Which is consistent with the fact that the return value is "unnamed" and
 simplifies the story around "annotating" types (because we can just say - you
 always just wrap them in parenthesis - there's only one edge case - let
 bindings).

 2. Even if you could distinguish return types, there's still a conflict if you
 allow punning. (Imagine (:ret) implies the return type). Does the following
 accept a labeled [a:] that has been aliased to [b], or does it take a punned
 [a:], and a final unlabeled argument?

     let named a:b (:ret) => a + b;

 This conflict could be eliminated via the removal of punning, or by creating a
 special syntax for punning. Whatever the compromise, the most valueable use
 cases to favor are:

  - Fully punned (no aliasing, no type annotations)

     let myFunc a: b: => a + b;

  - Punned with type annotations.

     let myFunc a:(a:int) b:(b:int) (:ret) => a + b;

 3. The final conflict is with disambiguating a type annotation of function invocation vs. a labeled argument application:

 let res = (callFunction a:blah)

  There's just so many remaining questions that it makes sense to stick with
 the current labeled arguments.

 Proposal 2:
 =============
 =============
 Proposal 1, but change the grammar so that :: is used to annotate types. This
 is perfect except the fact that record type annotations are now inconsistent!

 /*
  * You can still have records use colon for the types of each field. It doesn't
  * violate any consistency, in the same way that tuples look like (t1, t2).
  */
 type myRecord = {x: int, y:int};
 let myPoint::record = {x:20, y:9};

  A: let named              a:     b:                            ::ret   => a + b;
  B: let namedAlias         a:aa   b:bb                          ::ret   => aa + bb;
  /* Notice, no punning for C */
  C: let namedAnnot         a:(a::int)  b:(b::int)               ::ret   => a + b;
  D: let namedAliasAnnot    a:(aa::int) b:(bb::int)              ::ret   => aa + bb;
  E: let optional           a:?    b:?                           ::ret   => 10;
  F: let optionalAlias      a:a?  b:bb?                          ::ret   => 10;
        <parens optional>   a:aa?    b:bb?                       ::ret   => 10;
  /* Notice, no punning for G */
  G: let optionalAnnot      a:(a::int option)? b:(b::int option)?  ::ret   => 10;
  H: let optionalAliasAnnot a:(aa::int option)? b:(bb::int option)?::ret   => 10;
  I: let defOptional          a:?=10     b:?=10                  ::ret       => 10;
  J: let defOptionalAlias     a:?(aa)=10 b:?(bb)=10              ::ret       => 10;
         <parens not needed>  a:?aa=10   b:?bb=10                ::ret       => 10;

  /* Notice, no punning for K */
  K: let defOptionalAnnot      a:?(a::int)=10   b:?(b::int)=10   ::ret       => 10;
  L: let defOptionalAliasAnnot a:?(aa::int)=10 b:?(bb::int)=10     ::ret       => 10;

    /* Invoking them */
    let a = 10;
    let b = 20;
    named a: b:;
    named a:a b:b;
    optional a: b:;
    optional a:a b:b;

    forwardOptional a:?a b:?b;

*/


[@@@thisIsAThing];
let x = 10;
/*
In OCaml, any time you want to specify something about an argument, you place
parens after the ~/? and group the name. Any time you want to specify something
about the alias/pattern, you specify parens after the ~a:, so that the parens
group the alias.  That doesn't work out so well when we don't have a ~/?
because the grouping makes it look like a typical annotated anonymous argument.
If we *require* that anything beyond simple full-inference, no aliased labels,
*must* be annotated, then this could clear up the syntax.

Explanation:
1. You might have an adding function
   let add aa bb => aa + bb;
2. You might want to let the caller supply each argument by name regardless of
order. We let you place *labels* to the front of each.
   let add a:aa b:bb => aa + bb;
3. Now when the caller invokes, they can supply the names as well:
   let res = add a:10 b:10;
4. To apply pattern matching, type annotations, place the variable name as you
always would in parenthesis. Don't move or change the label in front.

   let add a:(aa: int) b:(bb: int) => aa + bb;
5. Optional params are the same! Just replace the `:` with a `?`. The type
inference will now infer the type to be an `option` of whatever type it would
have otherwise inferred, and callers are no longer required to supply that
labeled argument - if they don't the function perceives that argument to be
`None`. If the caller does supply that labeled arg, they don't need to wrap it
in a `Some`, however the function body will perceive the value to be `Some`.
6. Defaults change 5. The inferred type of optional named args that have
defaults supplied is not longer an option type and the function body never
perceives the option type.
7. There's one final piece of sugar.
- If your parameter names are the exact same as your label names, and you
  haven't written any annotations or default values, you can ommit the
  parameter names, relying only on the label names.

  blahFunctn a:a b:b => a + b
  blahOption a?a b?b => a + b

  is equivalent to

  blahFunctn a: b: => a + b
  blahOption a? b? => a + b
*/
/*
A let named                 a:                 b:                 = aa + bb;
B let namedAlias            a:aa               b:bb               = aa + bb;
' let namedAlias            a:(aa)             b:(bb)             = aa + bb;
C let namedAnnot            -
D let namedAliasAnnot       a:(aa:int)         b:(bb:int)         = aa + bb;
E let optional              a?                 b?                = 10;
F let optionalAlias         a?aa               b?bb              = 10;
G let optionalAnnot         a?(aa:int option)  b?(bb:int option) = 10;
H let optionalAliasAnnot    a?(aa:int option)  b?(bb:int option) = 10;
I let defOptional           a?(aa=10)          b?(bb=10)         = 10;
J let defOptionalAlias      a?(aa=10)          b?(bb=10)         = 10;
K let defOptionalAnnot      a?(aa:int=10)      b?(bb:int=10)     = 10;
L let defOptionalAliasAnnot a?(aa:int=10)      b?(bb:int=10)     = 10;
  /* All of them can be invoked as: */
  let res = funcToInvoke a:10 b:20;
  /* To explicitly pass Some or None to an optional param: */
  let res = optional a?(Some 10) b?None;
*/


/*

/*        Named arguments w/:       */
/* ================================ */
let addUnnamed               a b => a + b;
let addUnnamedTyped          (a:int) (b:int) => a + b;
let addNamed                 a: b: => a + b;
let addNamedDefault          (a: ? 10) (b: ? 10) => a + b;
let addNamedTyped            (a:int:) (b:int:) => a + b;
let addNamedTypedDefault     (a:int: ? 10) (b:int: ? 10) => a + b;

/*        Named arguments w/@       */
/* ================================ */
let addUnnamed               a b => a + b;
let addUnnamedTyped          (a:int) (b:int) => a + b;
let addNamed                 @a @b => a + b;
let addNamedDefault          @a=10 @b=10 => a + b;
let addNamedTyped            (@a:int) (@b:int) => a + b;
let addNamedTypedDefault     (@a:int=10) (@b:int=10) => a + b;


/* Named arguments w | for types   */
/* ================================ */
let addUnnamed               a b => a + b;
let addUnnamedTyped          (a|int) (b|int) (|int) => a + b;
let addNamed                 a: b: => a + b;
let addNamedDefault          a:10 b:10 => a + b;
let addNamedTyped            (a|int:) (b|int:) => a + b;
let addNamedTypedDefault     (a|int:10) (b|int:10) => a + b;

/* Named arguments w :: for types   */
/* ================================ */
let addUnnamed               a b => a + b;
let addUnnamedTyped          (a::int) (b::int) (::int) => a + b;
let addNamed                 a: b: => a + b;
let addNamedDefault          a:10 b:10 => a + b;
let addNamedTyped            (a::int:) (b::int:) => a + b;
let addNamedTypedDefault     (a::int:10) (b::int:10) => a + b;
type person = {age: string, name: string};
*/

/* let thisThing | Red x =>  */

/* Named arguments syntax:
 * Use @symbol to indicate @named @param
 * No question mark is needed to indicate optionality - the following equal
 * sign is sufficient to indicate such.
 * There shouldn't be a parsing ambiguity because functions require arrows!
 *
 * Type annotations still require parens.
 *
*/



/*
 Pattern matching sugar:
  /* Pattern matching: Appears familiar - like the ternary. */
  let x = someExpression ?
    Diamonds d => "d" :
    Hearts h => "h" :
    Clubs c => "c" :
    Spades s => "s" :
    "suit not supported";       /* Equivalent to _ => */

  let x = someExpression ?
    Diamonds d => "d" :
    Hearts h => "h" :
    Clubs c => "c" :
    Spades s => "s" :
    _ => "suit not supported";  /* See? */

  /* Some sugar for boolean pattern matching */
  let x = someBool ?
            "boolean was true" :
            "boolean was false";

  let x = someBool ?
/*true =>*/ "boolean was true" :
/*false=>*/ "boolean was false";

  let x = someBool ?
    true => "boolean was true" :
    false => "boolean was false";


  /* One more piece of sugar for option type matching */
  let optionString = Some "hello";
  let x = optionString ?
                s => String.capitalize(s) :
                "You had no string";

  /* Would be sugar for */
  let x = optionString ?
   /* Some */   s => String.capitalize(s) :
   /* None => */"You had no string";

  let x = optionString ?
      Some s => String.capitalize(s) :
      None   => "You had no string";

  But this syntax would be at the expense of making a certian not-very-useful
  form impossible to write:
  let x = optionString ?
                s => x :
                _ => "You had no string";

  Which has a useless default case.

  I would suggest using double QUESTION for the option sugar, and by default
  don't allow reasoning about the destructuring.

  let x = someBool ?? "default"


/* Some other ideas: What if | created a matching lambda ? */

let result = List.map items (Diamonds d => d);
let result = List.map items (Diamonds d => d | Spades s => s);
let mapper =
  | Diamonds d => d
  | Spades s => s;

let result = List.map items mapper;

let result = expression match
  | Diamonds d => d
  | Spades s => s;



/* No need for _ => - it can be implied */

let s = x ?
  | Clubs (c) -> c
  | 100

/* Combine with star capturer - How about some sugar for extraction */
let s = x ? Some t -> t | _ -> default;

let s = x ? Some * | default;

/*      Star % can just imply -> %    */

/* Use star to extract out deeep parts */
let s = x ? Clubs (_, %) | 200;

/* Or use it in the typical default place - to return the object being matched itself! */
/* The presense of a star only implies "-> %" so that means it works with "as" */
let nodeToRecurse = node ? Leaf as % | %;




*/
