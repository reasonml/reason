Format general implementation syntax
  $ refmt ./input.re
  /* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
  
  [@autoFormat
    let wrap = 80;
    let shift = 2
  ];
  Modules.run();
  Polymorphism.run();
  Variants.run();
  BasicStructures.run();
  
  TestUtils.printSection("General Syntax");
  /* Won't work! */
  /* let matchingFunc a = match a with */
  /*   `Thingy x => (print_string "matched thingy x"); x */
  /*   | `Other x => (print_string "matched other x"); x;; */
  /*  */
  let matchingFunc = a =>
    switch (a) {
    | `Thingy(x) =>
      print_string("matched thingy x");
      let zz = 10;
      zz;
    | `Other(x) =>
      print_string("matched other x");
      x;
    };
  
  type firstTwoShouldBeGroupedInParens =
    (int => int, int) => int;
  type allParensCanBeRemoved =
    (int, int, int) => int;
  type firstTwoShouldBeGroupedAndFirstThree =
    ((int => int) => int) => int;
  /* Same thing now but with type constructors instead of each int */
  type firstTwoShouldBeGroupedInParens =
    (list(int) => list(int), list(int)) =>
    list(int);
  type allParensCanBeRemoved =
    (list(int), list(int), list(int)) =>
    list(int);
  type firstTwoShouldBeGroupedAndFirstThree =
    ((list(int) => list(int)) => list(int)) =>
    list(int);
  
  type myRecordType = {
    firstTwoShouldBeGroupedInParens:
      (int => int, int) => int,
    allParensCanBeRemoved: (int, int, int) => int,
    firstTwoShouldBeGroupedAndFirstThree:
      ((int => int) => int) => int,
  };
  
  type firstNamedArgShouldBeGroupedInParens =
    (~first: int => int, ~second: int) => int;
  type allParensCanBeRemoved =
    (~first: int, ~second: int, ~third: int) => int;
  type firstTwoShouldBeGroupedAndFirstThree =
    (~first: (int => int) => int) => int;
  
  /* Same thing now, but with type constructors instead of int */
  type firstNamedArgShouldBeGroupedInParens =
    (
      ~first: list(int) => list(int),
      ~second: list(int)
    ) =>
    list(int);
  type allParensCanBeRemoved =
    (
      ~first: list(int),
      ~second: list(int),
      ~third: list(int)
    ) =>
    list(int);
  type firstTwoShouldBeGroupedAndFirstThree =
    (
      ~first:
        (list(int) => list(int)) => list(int)
    ) =>
    list(int);
  
  type firstNamedArgShouldBeGroupedInParens =
    (
      ~first: int => int=?,
      ~second: list(int)=?
    ) =>
    int;
  /* The arrow necessitates parens around the next two args. The ? isn't what
   * makes the parens necessary. */
  type firstNamedArgShouldBeGroupedInParensAndSecondNamedArg =
    (
      ~first: int => int=?,
      ~second: int => int=?
    ) =>
    int;
  type allParensCanBeRemoved =
    (
      ~first: int=?,
      ~second: int=?,
      ~third: int=?
    ) =>
    int;
  type firstTwoShouldBeGroupedAndFirstThree =
    (~first: (int => int) => int) => int;
  
  type noParens =
    (~one: int, int, int, ~two: int) => int;
  type noParensNeeded =
    (~one: int, int, int, ~two: int) => int;
  type firstNamedArgNeedsParens =
    (~one: (int, int) => int, ~two: int) => int;
  
  /* Now, let's try type aliasing */
  /* Unless wrapped in parens, types between arrows may not be aliased, may not
   * themselves be arrows. */
  
  type parensRequiredAroundFirstArg =
    (list(int) as 'a) => int as 'a;
  
  type parensRequiredAroundReturnType =
    (list(int) as 'a) => (int as 'a);
  
  type parensRequiredAroundReturnType =
    (list(int) as 'a) => (int as 'a) as 'b;
  
  type noParensNeededWhenInTuple =
    (list(int) as 'a, list(int) as 'b) as 'entireThing;
  
  type myTypeDef('a) = list('a);
  
  type instatiatedTypeDef = myTypeDef(int) => int;
  
  /* Test a type attribute for good measure */
  /* We should clean up all of the attribute tagging eventually, but for now,
   * let's make it super ugly to get out of the way of all the formatting/parsing
   * implementations (fewer conflicts during parsing, fewer edge cases during
   * printing).
   */
  type something = (
    int,
    [@lookAtThisAttribute] int,
  );
  
  type longWrappingTypeDefinitionExample =
    M_RK__G.Types.instance(
      TGRecognizer.tGFields(unit, unit),
      TGRecognizer.tGMethods(unit, unit),
    );
  
  type semiLongWrappingTypeDefinitionExample =
    M_RK__Gesture.Types.instance(
      TGRecognizerFinal.tGFields,
      TGRecognizerFinal.tGMethods,
    );
  
  type semiLongWrappingTypeWithConstraint =
    M_RK__Gesture.Types.instance(
      'a,
      TGRecognizerFinal.tGFields,
      TGRecognizerFinal.tGMethods,
    )
  constraint 'a = (unit, unit);
  
  type onelineConstrain = 'a constraint 'a = int;
  
  /* This must be in trunk but not in this branch of OCaml */
  /* type withNestedRecords = MyConstructor {myField: int} */
  
  type colors =
    | Red(int)
    | Black(int)
    | Green(int);
  
  /* Another approach is to require declared variants to wrap any record */
  /* type myRecord = MyRecord {name: int}; */
  /* let myValue = MyRecord {name: int}; */
  /* This would force importing of the module */
  /* This would also lend itself naturally to pattern matching - and avoid having
     to use `.` operator at all since you normally destructure. */
  type nameBlahType = {nameBlah: int};
  let myRecord = { nameBlah: 20 };
  let myRecordName = myRecord.nameBlah;
  
  let { nameBlah }: nameBlahType = {
    nameBlah: 20,
  };
  print_int(nameBlah);
  let { nameBlah: aliasedToThisVar }: nameBlahType = {
    nameBlah: 20,
  };
  print_int(aliasedToThisVar);
  
  let desiredFormattingForWrappedLambda
      : (int, int, int) => nameBlahType =
      /*
  
       fun is
       pre-   /firstarg\
       fix   /-coupled--\
        |-\ /-to-prefix--\       */
      (curriedArg, anotherArg, lastArg) => {
    nameBlah: 10,
  };
  
  type longerInt = int;
  let desiredFormattingForWrappedLambdaWrappedArrow
      : (longerInt, longerInt, longerInt) =>
        nameBlahType =
      /*
  
       fun is
       pre-   /firstarg\
       fix   /-coupled--\
        |-\ /-to-prefix--\       */
      (curriedArg, anotherArg, lastArg) => {
    nameBlah: 10,
  };
  
  let desiredFormattingForWrappedLambdaReturnOnNewLine =
      /*
  
       fun is
       pre-   /firstarg\
       fix   /-coupled--\
        |-\ /-to-prefix--\       */
      (curriedArg, anotherArg, lastArg) => {
    nameBlah: 10,
  };
  
  /*
   let is
   pre-
   fix    /-function binding name---\
   |-\   / is coupled to prefix      \   */
  let desiredFormattingForWrappedSugar =
      (curriedArg, anotherArg, lastArg) => {
    nameBlah: 10,
  };
  
  /*
   let is
   pre-
   fix    /-function binding name---\
   |-\   / is coupled to prefix      \   */
  let desiredFormattingForWrappedSugarReturnOnNewLine =
      (curriedArg, anotherArg, lastArg) => {
    nameBlah: 10,
  };
  
  /*
     let  : type t1 t2. t1 * t2 list -> t1 = ...
     let rec f : 't1 't2. 't1 * 't2 list -> 't1 =
       fun (type t1) (type t2) -> (... : t1 * t2 list -> t1)
   */
  
  type point = {
    x: int,
    y: int,
  };
  type point3D = {
    x: int,
    y: int,
    z: int,
  };
  let point2D = {
    x: 20,
    y: 30,
  };
  
  let point3D: point3D = {
    x: 10,
    y: 11,
    z: 80 /* Optional Comma */
  };
  
  let printPoint = (p: point) => {
    print_int(p.x);
    print_int(p.y);
  };
  
  let addPoints = (p1: point, p2: point) => {
    x: p1.x + p2.x,
    y: p1.y + p2.y,
  };
  
  let res1 = printPoint(point2D);
  let res2 =
    printPoint({
      x: point3D.x,
      y: point3D.y,
    });
  
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
  let res3 =
    printPoint(
      addPoints(
        point2D,
        {
          x: point3D.x,
          y: point3D.y,
        },
      ),
    );
  
  type person = {
    age: int,
    name: string,
  };
  type hiredPerson = {
    age: string,
    name: string,
    dateHired: int,
  };
  
  let o: person = {
    name: "bob",
    age: 10,
  };
  
  /* Parens needed? Nope! */
  let o: person = {
    name: "bob",
    age: 10,
  };
  
  let printPerson = (p: person) => {
    let q: person = p;
    p.name ++ p.name;
  };
  
  /* let dontParseMeBro x y:int = x = y;*/
  
  /* With this unification, anywhere eyou see `= fun` you can just ommit it */
  let blah = a => a; /* Done */
  let blah = a => a; /* Done (almost) */
  
  let blah = (a, b) => a; /* Done */
  let blah = (a, b) => a; /* Done (almost) */
  
  /* More than one consecutive pattern must have a single case */
  type blah = {blahBlah: int};
  let blah = (a, { blahBlah }) => a;
  let blah = (a, { blahBlah }) => a;
  
  module TryToExportTwice = {
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
      print_int(1);
      print_int(20); /* Missing trailing SEMI */
    };
  
    let x = {
      print_int(1);
      print_int(20); /* Ensure missing middle SEMI reported well */
      print_int(20);
    };
  
    let x = {
      print_int(1);
      print_int(20);
      10;
      /* Comment in final position */
    }; /* Missing final SEMI */
    x + x;
  };
  
  type hasA = {a: int};
  let a = 10;
  let returnsASequenceExpressionWithASingleIdentifier =
      () => {
    a;
  };
  let thisReturnsA = () => {
    a;
  };
  let thisReturnsAAsWell = () => a;
  
  let recordVal: int = thisReturnsARecord().a;
  Printf.printf(
    "\nproof that thisReturnsARecord: %n\n",
    recordVal,
  );
  Printf.printf(
    "\nproof that thisReturnsA: %n\n",
    thisReturnsA(),
  );
  
  /* Pattern matching */
  let blah = arg =>
    switch (arg) {
    /* Comment before Bar */
    | /* Comment between bar/pattern */ Red(_) => 1
    /* Comment Before non-first bar */
    | /* Comment betwen bar/pattern */ Black(_) => 0
    | Green(_) => 0
    };
  
  /* Any function that pattern matches a multicase match is interpretted as a
   * single arg that is then matched on. Instead of the above `blah` example:*/
  
  let blah =
    fun
    | Red(_) => 1
    | Black(_) => 0
    | Green(_) => 1;
  
  /* `fun a => a` is read as "a function that maps a to a". Then the */
  /* above example is read: "a function that 'either maps' Red to.. or maps .." */
  /* Thc00f564e first bar is read as "either maps" */
  
  /* Curried form is not supported:
        let blah x | Red _ => 1 | Black _ => 0;
        Theres no sugar rule for dropping => fun, only = fun
     */
  
  /* let blahCurriedX x => fun  /* See, nothing says we can drop the => fun */ */
  /*   |(Red x | Black x | Green x) => 1     /* With some effort, we can ammend the sugar rule that would */ */
  /*   | Black x => 0                       /* Allow us to drop any => fun.. Just need to make pattern matching */ */
  /*   | Green x => 0;                      /* Support that */ */
  /*  */
  
  let blahCurriedX = x =>
    fun
    | Red(x)
    | Black(x)
    | Green(x) => 1 /* With some effort, we can ammend the sugar rule that would */
    | Black(x) => 0 /* Allow us to drop any => fun.. Just need to make pattern matching */
    | Green(x) => 0; /* Support that */
  
  let sameThingInLocal = {
    let blahCurriedX = x =>
      fun
      | Red(x)
      | Black(x)
      | Green(x) => 1 /* With some effort, we can ammend the sugar rule that would */
      | Black(x) => 0 /* Allow us to drop any => fun.. Just need to make pattern matching */
      | Green(x) => 0; /* Support that */
    blahCurriedX;
  };
  
  /* This should be parsed/printed exactly as the previous */
  let blahCurriedX = x =>
    fun
    | Red(x)
    | Black(x)
    | Green(x) => 1
    | Black(x) => 0
    | Green(x) => 0;
  
  /* Any time there are multiple match cases we require a leading BAR */
  
  let v = Red(10);
  let Black(x) | Red(x) | Green(x) = v; /* So this NON-function still parses */
  
  /* This doesn't parse, however (and it doesn't in OCaml either):
       let | Black(x) | Red(x) | Green(x) = v;
     */
  
  print_int(x);
  
  /* Scoping: Let sequences. Familiar syntax for lexical ML style scope and
     sequences. */
  
  let res = {
    let a = "a starts out as";
    {
      print_string(a);
      let a = 20;
      print_int(a);
    };
    print_string(a);
  };
  
  let res = {
    let a = "first its a string";
    let a = 20;
    print_int(a);
    print_int(a);
    print_int(a);
  };
  
  let res = {
    let a = "a is always a string";
    print_string(a);
    let b = 30;
    print_int(b);
  };
  
  /* let result = LyList.map((fun | [] => true | _ => false), []); */
  
  /* OTHERWISE: You cannot tell if a is the first match case falling through or
   * a curried first arg */
  /* let blah = fun a | patt => 0 | anotherPatt => 1; */
  /* let blah a patt => 0 | anotherPatt => 1; */
  
  /*simple pattern  EQUALGREATER      expr */
  let blah = (a, { blahBlah }) => a;
  
  /*            match_case             */
  /*     pattern EQUALGREATER  expr */
  let blah =
    fun
    | Red(_) => 1
    | Black(_) => 0
    | Green(_) => 0;
  
  /* Won't work! */
  /* let arrowFunc = fun a b => print_string "returning aplusb from arrow"; a + b;;  */
  let arrowFunc = (a, b) => {
    print_string("returning aplusb from arrow");
    a + b;
  };
  let add = (a, b) => {
    let extra = {
      print_string("adding");
      0;
    };
    let anotherExtra = 0;
    extra + a + b + anotherExtra;
  };
  
  print_string(string_of_int(add(4, 34)));
  
  let dummy = _ => 10;
  dummy(res1);
  dummy(res2);
  dummy(res3);
  
  /* Some edge cases */
  let myFun =
      (firstArg, Red(x) | Black(x) | Green(x)) =>
    firstArg + x;
  let matchesWithWhen = a =>
    switch (a) {
    | Red(x) when 1 > 0 => 10
    | Red(_) => 10
    | Black(x) => 10
    | Green(x) => 10
    };
  
  let matchesWithWhen =
    fun
    | Red(x) when 1 > 0 => 10
    | Red(_) => 10
    | Black(x) => 10
    | Green(x) => 10;
  
  let matchesOne = (`Red(x)) => 10;
  
  /*
   Typical OCaml would make you *wrap the functions in parens*! This is because it
   can't tell if a semicolon is a sequence operator. Even if we had records use
   commas to separate fields,
   */
  type adders = {
    addTwoNumbers: (int, int) => int,
    addThreeNumbers: (int, int, int) => int,
    addThreeNumbersTupled:
      ((int, int, int)) => int,
  };
  let myRecordWithFunctions = {
    addTwoNumbers: (a, b) => a + b,
    addThreeNumbers: (a, b, c) => a + b + c,
    addThreeNumbersTupled: ((a, b, c)) =>
      a + b + c,
  };
  
  let result =
    myRecordWithFunctions.addThreeNumbers(
      10,
      20,
      30,
    );
  let result =
    myRecordWithFunctions.addThreeNumbersTupled((
      10,
      20,
      30,
    ));
  
  let lookTuplesRequireParens = (1, 2);
  /* let thisDoesntParse = 1, 2;  */
  let tupleInsideAParenSequence = {
    print_string(
      "look, a tuple inside a sequence",
    );
    let x = 10;
    (x, x);
  };
  
  let tupleInsideALetSequence = {
    print_string(
      "look, a tuple inside a sequence",
    );
    let x = 10;
    (x, x);
  };
  
  /* We *require* that function return types be wrapped in
     parenthesis. In this example, there's no ambiguity */
  let makeIncrementer = (delta: int): (int => int) =>
    a => a + delta;
  
  /* We could even force that consistency with let bindings - it's allowed
        currently but not forced.
     */
  let myAnnotatedValBinding: int = 10;
  
  /* Class functions (constructors) and methods are unified in the same way */
  
  class classWithNoArg = {
    pub x = 0;
    pub y = 0;
  };
  
  /* This parses but doesn't type check
       class myClass init => object
         pub x => init
         pub y => init
       end;
     */
  
  let myFunc = (a: int, b: int): (int, int) => (
    a,
    b,
  );
  let myFunc = (a: int, b: int): list(int) => [
    1,
  ];
  let myFunc = (a: int, b: int): point => {
    x: a,
    y: b,
  };
  let myFunc = (a: int, b: int): point => {
    x: a,
    y: b,
  };
  
  type myThing = (int, int);
  type stillARecord = {
    name: string,
    age: int,
  };
  
  /* Rebase latest OCaml to get the following: And fixup
     `generalized_constructor_arguments` according to master. */
  /* type ('a, 'b) myOtherThing = Leaf {first:'a, second: 'b} | Null; */
  type branch('a, 'b) = {
    first: 'a,
    second: 'b,
  };
  type myOtherThing('a, 'b) =
    | Leaf(branch('a, 'b))
    | Null;
  
  type yourThing = myOtherThing(int, int);
  
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
  let named = (~a, ~b) => a + b;
  type named = (~a: int, ~b: int) => int;
  /*B*/
  let namedAlias = (~a as aa, ~b as bb) =>
    aa + bb;
  let namedAlias = (~a as aa, ~b as bb) =>
    aa + bb;
  type namedAlias = (~a: int, ~b: int) => int;
  /*C*/
  let namedAnnot = (~a: int, ~b: int) => 20;
  /*D*/
  let namedAliasAnnot =
      (~a as aa: int, ~b as bb: int) => 20;
  /*E*/
  let myOptional = (~a=?, ~b=?, ()) => 10;
  type named = (~a: int=?, ~b: int=?, unit) => int;
  /*F*/
  let optionalAlias = (~a as aa=?, ~b as bb=?, ()) => 10;
  /*G*/
  let optionalAnnot = (~a: int=?, ~b: int=?, ()) => 10;
  /*H*/
  let optionalAliasAnnot =
      (~a as aa: int=?, ~b as bb: int=?, ()) => 10;
  /*I: */
  let defOptional = (~a=10, ~b=10, ()) => 10;
  type named = (~a: int=?, ~b: int=?, unit) => int;
  /*J*/
  let defOptionalAlias =
      (~a as aa=10, ~b as bb=10, ()) => 10;
  /*K*/
  let defOptionalAnnot =
      (~a: int=10, ~b: int=10, ()) => 10;
  /*L*/
  let defOptionalAliasAnnot =
      (~a as aa: int=10, ~b as bb: int=10, ()) => 10;
  
  /*M: Invoking them - Punned */
  let resNotAnnotated = named(~a, ~b);
  /*N:*/
  let resAnnotated: int = named(~a, ~b);
  /*O: Invoking them */
  let resNotAnnotated = named(~a, ~b);
  /*P: Invoking them */
  let resAnnotated: int = named(~a, ~b);
  
  /*Q: Here's why "punning" doesn't work!  */
  /* Is b:: punned with a final non-named arg, or is b:: supplied b as one named arg? */
  let b = 20;
  let resAnnotated = named(~a, ~b);
  
  /*R: Proof that there are no ambiguities with return values being annotated */
  let resAnnotated: ty = named(~a, b);
  
  /*S: Explicitly passed optionals are a nice way to say "use the default value"*/
  let explictlyPassed =
    myOptional(~a=?None, ~b=?None);
  /*T: Annotating the return value of the entire function call */
  let explictlyPassedAnnotated: int =
    myOptional(~a=?None, ~b=?None);
  /*U: Explicitly passing optional with identifier expression */
  let a = None;
  let explictlyPassed = myOptional(~a?, ~b=?None);
  let explictlyPassedAnnotated: int =
    myOptional(~a?, ~b=?None);
  
  let nestedLet = {
    let _ = 1;
    ();
  };
  
  let nestedLet = {
    let _ = 1;
    ();
  };
  
  let nestedLet = {
    let _ = 1;
    ();
  };
  
  let nestedLet = {
    let _ = 1;
    2;
  };
  
  /*
   * Showing many combinations of type annotations and named arguments.
   */
  
  type typeWithNestedNamedArgs =
    (
      ~outerOne:
        (~innerOne: int, ~innerTwo: int) => int,
      ~outerTwo: int
    ) =>
    int;
  
  type typeWithNestedOptionalNamedArgs =
    (
      ~outerOne:
        (~innerOne: int, ~innerTwo: int) => int=?,
      ~outerTwo: int=?
    ) =>
    int;
  
  type typeWithNestedOptionalNamedArgs =
    (
      ~outerOne: list(string)=?,
      ~outerTwo: int=?
    ) =>
    int;
  
  let f =
      (~tuple="long string to trigger line break") =>
    ();
  
  let x =
    callSomeFunction(
      ~withArg=10,
      ~andOtherArg=wrappedArg,
    );
  
  let res = {
    (constraintedSequenceItem: string);
    (dontKnowWheYoudWantToActuallyDoThis: string);
  };
  
  let res = {
    (
      butTheyWillBePrintedWithAppropriateSpacing: string
    );
    (
      soAsToInstillBestDevelopmentPractices: string
    );
  };
  
  let x = [
    (eachItemInListCanBeAnnotated: int),
    (typeConstraints: float),
    (
      tupleConstraints: int,
      andNotFunctionInvocations: int,
    ),
  ];
  
  let x = [
    (butWeWillPrint: int),
    (themAsSpaceSeparated: float),
    (
      toInfluenceYour: int,
      developmentHabbits: int,
    ),
  ];
  
  let newRecord = {
    ...(annotatedSpreadRecord: someRec),
    x: y,
  };
  
  let newRecord = {
    ...(annotatedSpreadRecord: someRec),
    blah: 0,
    foo: 1,
  };
  
  let newRecord = {
    ...(
      youCanEvenCallMethodsHereAndAnnotate(them): someRec
    ),
    blah: 0,
    foo: 1,
  };
  
  let newRecord = {
    ...(
      youCanEvenCallMethodsHereAndAnnotate(
        them,
        ~named=10,
      ): someRec
    ),
    blah: 0,
    foo: 1,
  };
  
  let something: thing(blah) = aTypeAnnotation;
  let something: thing(blah) = thisIsANamedArg;
  
  let something: thing(blah) = aTypeAnnotation;
  
  let something: blah = thisIsANamedArg(thing);
  let something: blah = typeAnnotation(thing);
  
  let newRecord = {
    ...(
      heresAFunctionWithNamedArgs(~argOne=i): annotatedResult
    ),
    soAsToInstill: 0,
    developmentHabbits: 1,
  };
  
  [@thisIsAThing];
  let x = 10;
  
  /* Ensure that the parenthesis are preserved here because they are
   * important:
   */
  let something =
    fun
    | None => (
        fun
        | [] => "emptyList"
        | [_, ..._] => "nonEmptyList"
      )
    | Some(_) => (
        fun
        | [] => "emptyList"
        | [_, ..._] => "nonEmptyList"
      );
  
  /*  A | B = X; */
  let A | B = X;
  
  /*  A | (B | C) = X; */
  let A | (B | C) = X;
  
  /* (A | B) | (C | D) = X; */
  let A | B | (C | D) = X;
  
  /*  A | B | (C | D) = X; */
  let A | B | (C | D) = X;
  
  /* (A | B) | C = X; */
  let A | B | C = X;
  
  /*  A | B | C = X; */
  let A | B | C = X;
  
  /** External function declaration
   *
   */
  external f: int => int = "foo";
  
  let x = { contents: 0 };
  
  let unitVal = x.contents = 210;
  
  let match = "match";
  
  let method = "method";
  
  let foo =
      (x, ~x as bar, ~z, ~foo as bar, ~foo as z) => {
    bar + 2;
  };
  
  let zzz = myFunc(1, 2, [||]);
  
  /* 1492 */
  let registerEventHandlers =
      (
        ~window: Window.t,
        ~mouseDown:
           option(
             (
               ~button: Events.buttonStateT,
               ~state: Events.stateT,
               ~x: int,
               ~y: int
             ) =>
             unit,
           )=?,
        (),
      ) => 1;
  
  /* #1320: record destrucuring + renaming */
  let x = ({ state as prevState }) => 1;
  let x = ({ ReasonReact.state as prevState }) => 1;
  
  /* 1567: optional parens around expr constraint in constructor expression */
  Some(x: int);
  Some(x: int);
  Some(x, y: int, b);
  Some(x, y: int, b);
  
  foo(~x=-. bar);
  
  Some(-1, -1, -1);
  Some(-1, -1, -1);
  Some(-1g, -1G, -1z);
  Some(-1g, -1G, -1z);
  Some(-0.1, -0.1, -0.1);
  Some(-0.1, -0.1, -0.1);
  Some(-0.1G, -0.1x, -0.1H);
  Some(-0.1G, -0.1x, -0.1H);
  Some([@foo] -1, [@foo] -1, [@foo] -1);
  Some([@foo] -1z, [@foo] -1z, [@foo] -1z);
  Some([@foo] -0.1, [@foo] -0.1, [@foo] -0.1);
  Some([@foo] -0.1m, [@foo] -0.1n, [@foo] -0.1p);
  
  foo(~x=-1, ~y=-2);
  foo(~x=-1, ~y=-2);
  foo(~x=-. 1, ~y=-. 2);
  foo(~x=-. 1, ~y=-. 2);
  foo(~x=-1g, ~y=-1G, ~z=-1z);
  foo(~x=-1g, ~y=-1G, ~z=-1z);
  foo(~x=-0.1G, ~y=-0.1x, ~z=-0.1H);
  foo(~x=-0.1G, ~y=-0.1x, ~z=-0.1H);
  foo(~x=[@foo] -1, ~y=[@foo] -1, ~z=[@foo] -1);
  foo(
    ~x=[@foo] -1z,
    ~y=[@foo] -1z,
    ~z=[@foo] -1z,
  );
  foo(
    ~x=[@foo] -0.1,
    ~y=[@foo] -0.1,
    ~z=[@foo] -0.1,
  );
  foo(
    ~x=[@foo] -0.1m,
    ~y=[@foo] -0.1n,
    ~z=[@foo] -0.1p,
  );
  
  /* Smooth formatting of functions with callbacks as arguments */
  funWithCb("text", () => doStuff());
  
  funWithCb([@attr] "text", [@myAttr] () =>
    doStuff()
  );
  
  funWithCb(~text="text", ~f=() => doStuff());
  
  funWithCb(~text=[@attr] "text", ~f=[@myAttr] () =>
    doStuff()
  );
  
  funWithCb(~text="text", ~f=?() => doStuff());
  
  funWithCb(~text=[@attr] "text", ~f=?[@myAttr] () =>
    doStuff()
  );
  
  test("my test", () => {
    let x = a + b;
    let y = z + c;
    x + y;
  });
  
  test([@attr] "my test", [@attr] () => {
    let x = a + b;
    let y = z + c;
    x + y;
  });
  
  test(~desc="my test", ~f=() => {
    let x = a + b;
    let y = z + c;
    x + y;
  });
  
  test(~desc=[@attr] "my test", ~f=[@myAttr] () => {
    let x = a + b;
    let y = z + c;
    x + y;
  });
  
  test(~desc=?"my test", ~f=?() => {
    let x = a + b;
    let y = z + c;
    x + y;
  });
  
  test(~desc=?[@attr] "my test", ~f=?[@attr] () => {
    let x = a + b;
    let y = z + c;
    x + y;
  });
  
  describe("App", () => {
    test("math", () => {
      Expect.expect(1 + 2) |> toBe(3)
    })
  });
  
  describe([@attr] "App", [@attr] () => {
    test([@attr] "math", [@attr] () => {
      Expect.expect(1 + 2) |> toBe(3)
    })
  });
  
  describe(~text="App", ~f=() =>
    test(~text="math", ~f=() =>
      Expect.expect(1 + 2) |> toBe(3)
    )
  );
  
  describe(~text=[@attr] "App", ~f=[@attr] () =>
    test(~text=[@attr] "math", ~f=[@attr] () =>
      Expect.expect(1 + 2) |> toBe(3)
    )
  );
  
  describe(~text=?"App", ~f=?() =>
    test(~text=?"math", ~f=?() =>
      Expect.expect(1 + 2) |> toBe(3)
    )
  );
  
  describe(~text=?[@attr] "App", ~f=?[@attr] () =>
    test(~text=?[@attr] "math", ~f=?[@attr] () =>
      Expect.expect(1 + 2) |> toBe(3)
    )
  );
  
  Thing.map(foo, bar, baz, (abc, z) =>
    MyModuleBlah.toList(argument)
  );
  
  Thing.map(
    [@attr] foo, bar, baz, [@attr] (abc, z) =>
    MyModuleBlah.toList(argument)
  );
  
  Thing.map(
    ~a=[@attr] foo, ~b=bar, ~c=?baz, ~f=(abc, z) =>
    MyModuleBlah.toList(argument)
  );
  
  Thing.map(
    ~a=foo,
    ~b=bar,
    ~c=?[@attr] baz,
    ~f=[@attr] (abc, z) =>
    MyModuleBlah.toList(argument)
  );
  
  Thing.map(~a=foo, ~b=bar, ~c=?baz, ~f=?(abc, z) =>
    MyModuleBlah.toList(argument)
  );
  
  Thing.map(
    ~a=foo,
    ~b=bar,
    ~c=?[@attr] baz,
    ~f=?[@attr] (abc, z) =>
    MyModuleBlah.toList(argument)
  );
  
  Thing.map(
    foo,
    bar,
    baz,
    foo2,
    bakjlksjdf,
    okokokok,
    (abc, z) => {
      let x = 1;
      MyModuleBlah.toList(x, argument);
    },
  );
  
  Thing.map(
    foo,
    bar,
    baz,
    foo2,
    [@attr] bakjlksjdf,
    okokokok,
    [@attr] (abc, z) => {
      let x = 1;
      MyModuleBlah.toList(x, argument);
    },
  );
  
  Thing.map(
    ~a=foo,
    ~b=bar,
    ~c=baz,
    ~d=foo2,
    ~e=bakjlksjdf,
    ~f=okokokok,
    ~cb=(abc, z) => {
      let x = 1;
      MyModuleBlah.toList(x, argument);
    },
  );
  
  Thing.map(
    ~a=foo,
    ~b=bar,
    ~c=[@attr] baz,
    ~d=foo2,
    ~e=bakjlksjdf,
    ~f=okokokok,
    ~cb=[@attr] (abc, z) => {
      let x = 1;
      MyModuleBlah.toList(x, argument);
    },
  );
  
  Thing.map(
    ~a=?foo,
    ~b=?bar,
    ~c=?baz,
    ~d=?foo2,
    ~e=?bakjlksjdf,
    ~f=?okokokok,
    ~cb=?(abc, z) => {
      let x = 1;
      MyModuleBlah.toList(x, argument);
    },
  );
  
  Thing.map(
    ~a=?foo,
    ~b=?bar,
    ~c=?baz,
    ~d=?foo2,
    ~e=?[@attr] bakjlksjdf,
    ~f=?okokokok,
    ~cb=?[@attr] (abc, z) => {
      let x = 1;
      MyModuleBlah.toList(x, argument);
    },
  );
  
  Thing.map(
    foo,
    bar,
    baz,
    (abc, z) => MyModuleBlah.toList(argument),
    (abc, z) => MyModuleBlah.toList(argument),
  );
  
  Thing.map(
    foo,
    bar,
    baz,
    [@attr] (abc, z) =>
      MyModuleBlah.toList(argument),
    [@attr] (abc, z) =>
      MyModuleBlah.toList(argument),
  );
  
  Js.Option.andThen((. w) => w#getThing());
  
  Thing.map(
    ~a=?foo,
    ~b=?bar,
    ~c=?baz,
    ~d=?foo2,
    ~e=?[@attr] bakjlksjdf,
    ~f=?okokokok,
    ~cb=?[@attr]
         [@attr2]
         [@aslkdfjlsdjf]
         [@sldkfjlksdjflksjdlkjf]
         [@sldkflskjdf]
         (abc, z) => {
      let x = 1;
      MyModuleBlah.toList(x, argument);
    },
  );
  
  let result =
    F.call(x => {
      let x = 123;
      let y = 2345;
      doStuff();
    });
  
  let result = F.call(x => doStuff(x));
  
  let () =
    x |> Bigarray.Genarray.get(_, [|1, 2, 3, 4|]);
  
  let () = x |> Bigarray.Array1.get(_, 1);
  
  let () = x |> Bigarray.Array2.get(_, 1, 2);
  
  let () = x |> Bigarray.Array3.get(_, 1, 2, 3);
  
  let x = -. 1;
  
  let x = (-1);
  
  let x = 1;
  
  let x = +. 1;
  
  let x = (~a: int=(-1)) => a;
  
  let x = (~a=(-1)) => a;
  
  let x: float = -. 1;
  
  let x: int = (-1);
  
  let x: int = 1;
  
  let x: float = +. 1;
  
  foo(~a=?-1);
  
  /*
     https://github.com/facebook/reason/issues/1992
     Pexp_override
   */
  let z = {<state: 0, x: y>};
  let z = {<>};
  
  /* https://github.com/facebook/reason/issues/2056 */
  type foo = (~a: bool=?) => int;
  
  /* https://github.com/facebook/reason/issues/2070 */
  f(~commit=!build);
  
  /* https://github.com/facebook/reason/issues/2032 */
  let predicate =
    predicate === Functions.alwaysTrue1
      ? defaultPredicate
      : fun
        | None => false
        | Some(exn) => predicate(exn);
  
  let predicate =
    predicate === Functions.alwaysTrue1
      ? fun
        | None => false
        | Some(exn) => predicate(exn)
      : fun
        | None => false
        | Some(exn) => predicate(exn);
  
  /* https://github.com/facebook/reason/issues/2125 */
  foo(~a);
  
  foo(~a: int);
  
  foo(~a: int);
  
  foo(~(a :> int));
  
  foo(~(a :> int));
  
  foo(~a=?Foo.a);
  
  foo(~a=Foo.a);
  
  /* https://github.com/facebook/reason/issues/2155#issuecomment-422077648 */
  true
    ? (
      Update({
        ...a,
        b: 1,
      }),
      None,
    )
    : x;
  true
    ? {
      ...a,
      b: 1,
    }
    : a;
  true
    ? (
      a,
      {
        ...a,
        b: 1,
      },
    )
    : a;
  
  true ? ([x, ...xs]) => f(x, xs) : a;
  
  /* https://github.com/facebook/reason/issues/2200 */
  foo(~x=(-1) + 2);
  
  foo(~x=(-1) + 2: int);
  
  foo(~not);
  
  let foo = (~not) => ();
  
  let foo = (~not: string) => ();
  
  foo(~not: string);
  
  /* https://github.com/facebook/reason/issues/2141 */
  let testCallNamedArgs =
      (foo: (~a: int, ~b: int) => int, a, b) =>
    foo(~a, ~b);
  
  let testCallNamedArgs =
      (foo: (~a: int, ~b: int=?) => int, a, b) =>
    foo(~a, ~b);
  
  let Foo.{ name } = bar;
  let Foo.Bar.{ name } = bar;
  
  let Foo.{
    destruct1,
    destruct2,
    destruct3,
    destruct4,
    destruct5,
  } = fooBar;
  
  let Foo.[name] = bar;
  let Foo.Bar.[name] = bar;
  
  let Foo.Bar.[] = bar;
  let Foo.Bar.[||] = bar;
  let Foo.() = foo;
  
  /* let Foo.(bar, baz) = foo; */
  
  let Foo.(exception bar) = baz;
  
  try({
    let this = try_exp;
    let has = hugged;
    parens;
  }) {
  | _ => ()
  };
  
  /* Pcl_open (4.06+) */
  class x = {
    open EM;
    as self;
  };
  
  class y = {
    open EM;
    open OM;
    as self;
  };
  
  // Arbitrary precision literals
  
  let x = 1G;
  let x = 1.123g;
  
  let x =
    switch () {
    | _ => .
    };
  
  let%foo Foo.{
    destruct1,
    destruct2,
    destruct3,
    destruct4,
    destruct5,
  } = fooBar;
  
  open%foo Bar;
  open! %foo Bar;
  
  let () = {
    open%foo Bar;
    switch (1 + 1) {
    | 2 => ()
    | _ => ()
    };
  };
  
  let () = {
    open%foo Bar;
    3;
  };
  
  let () = {
    open%foo Bar;
    let x = 1;
    3;
  };
  
  module type x = {
    let a: 'a. 'a => unit;
  };
