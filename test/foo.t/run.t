  $ refmt --enable-new-printer ./input.re
  let x1 = (()) => 1
  let x2 = (a) => 1
  let x3 = ((a: int)) => (b) => 1
  let x4 = ((a, b)) => 1
  let x5 = (a) => (b) => (1: int)
  let x6 = (~x) => (~y) => 1
  let x7 = (~x) => (~y) => 1
  let x8 = (~x=?5) => (~y=?) => (~z=?) => (()) => 1
  
  type a = int
  type b = float
  type c = string
  type t1 = a => b
  type t2 = a => b => c
  type t3 = (a, b) => c
  type t4 = ~x: int => ~y: string => c
  type t5 = ~x: a=? => b
  type tf = int => int => string
  type tNested2 = int => int => string
  type tNested3 = int => int => int => string
  type tNested4 = int => int => string
  type tNested5 = (int, int) => string
  type tNested7 = array(list(int) => string)
  
  type t6 = int
  type t7('a)  = list('a)
  type t8('a, 'b)  = (list('a), 'b)
  type t9 = t8(string, int)
  
  class type restricted_point_type = { pub get_x: int; pub bump: unit }
  class type t10('a)  = { pub thing: 'a }
  class type t11('a, 'b)  = { pub thing: ('a, list('b)) }
  
  module MyFirstModule = { let x = 0; type i = int and n = string }
  
  module type HasTT = { type tt }
  
  module SubModule = ({ type tt = int }: HasTT)
  
  module type HasEmbeddedHasTT = { module SubModuleThatHasTT: module SubModule }
  
  module type HasPolyType = { type t('a)  }
  module type HasDoublePoly = { type m('b, 'c)  }
  
  module type HasDestructivelySubstitutedPolyType = HasPolyType with type ('a) t := list('a)
  
  module type HasDestructivelySubstitutedSubPolyModule = { /* Cannot perform destructive substitution on submodules! */
  /* module X: HasPolyType with type t := list (int, int); */
  module X: HasDestructivelySubstitutedPolyType }
  
  module type HasSubPolyModule = { /* Cannot perform destructive substitution on submodules! */
  /* module X: HasPolyType with type t := list (int, int); */
  module X: HasPolyType }
  
  module EmbedsSubPolyModule = ({ module X = { type t('a)  = list('a) } }: HasSubPolyModule)
  
  module InliningSig = ({ let x = 10; let y = 20 }: { let x: int; let y: int })
  
  module MyFunctor = (M: HasTT) => { type reexportedTT = M.tt; let someValue = 1000 }
  
  module MyFunctorResult = MyFunctor({ type tt = string })
  module type ASig = { let a: int }
  module type BSig = { let b: int }
  module CurriedSugar = (A: ASig) => (B: BSig) => { let result = (+)(A.a, B.b) }
  
  type withThreeFields = { name: string, age: int, occupation: string }
  
  let testRecord = { name: "joe", age: 20, occupation: "engineer" }
  
  let makeRecordBase = (()) => { name: "Joe", age: 30, occupation: "Engineer" }
  
  
  type t = | A | B(int) | C(int, int) | D((int, int))
  
  type foo = { x: int }
  let result = Some({ x: 1 })
  
  
  type tt1 = | A(int) | B(bool, string)
  
  type tt2 = | A(int) | B((bool, string))
  
  type tt3 = [ | `A(int) | `B((bool, string)) | `C ]
  
  type tt4 = [ | `A(int) | `B((bool, string)) | `C ]
  
  let (=) = 0
  
  let (==) = 0
  
  let (<>) = 0
  
  let (!=) = 0
  
  type foobar(_)  = | Foo('a): foobar(unit)
  
  type expr(_)  = | Int(int): expr(int) | String(string): expr(string) | Pair('a, 'b): expr(('a, 'b))
  
  let eval = (type a) => ((e: expr(a))) => (switch (e) { | Int((n)) => n | String((s)) => s | Pair((x, y)) => (x, y) }: a)
  
  type point = | Point({ x: int, y: int })
  type person = | Person({ name: string, age: int }) | Anonymous
  
  type covariant(+'a)  = list('a)
  type contravariant(-'a)  = 'a => unit
  type mixed(+'a, -'b)  = 'a => 'b => unit
  
  type open_variant = [ > | `A | `B(int) ]
  type open_with_values = [ > | `Red | `Blue(string) | `Green((int, int)) ]
  
  exception GenericError('a)
  
  exception ParseError('a, 'b): exn
  
  type exn += CustomError('a): exn
  
  let with_extension = [%test "payload"]
  let with_complex = [%derive.show { x: 1 }]
  [%%toplevel_ext "payload"]
  
  [%%foo let x = 1]
  
  /* Test attributes rendering */
  [@deprecated "Use new_module instead"] module OldModule = { let x = 1 }
  
  let inline_func = (x) => (+)(x, 1)
  
  [@warning "-27"] module type IgnoreUnused = { let unused: int }
  
  [%%lwt let x = 1]
  
  let x = [%platform switch (()) { | Server => 1 | Client => 2 }]
  
  [@custom_attr ]
  
  [@custom_attr ] include SomeModule
  
  let baseRecord = { x: 1, y: 2 }
  let extendedRecord = { baseRecord, z: 3 }
  
  let rec foo = (a) => (b) => (+)(a, b) and bar = (a) => (b) => foo((-)(a, b))
  
  external foo: type_ = "%caml_something_or_other"
  
  let html = <div className={"foo"}> <span /> </div>
  
  let a = 1
  
  // This is a comment about b
  let b = 2
  
  /* Multi-line comment
     about c */
  let c = 3
  
  let d = 4 // inline comment
  
  /* Test */
  let rec /* asdfasdf */
  /* wwww */
  foo = (a) => (b) => (+)(a, b) and bar = (a) => (b) => foo((-)(a, b))
  
  /* Test with many comments */
  let rec foo = (/* a */
  a) => /* b */
  (b) => /* body */
  (+)(a, b) and bar = (a) => (b) => foo((-)(a, b))
  
  /* module comment */
  module CommentedModule = { /* inside module */
  let x = 1 }
  
  /* functor comment */
  module CommentedFunctor = (M: HasTT) => { /* inside functor */
  type t = M.tt }
  
  /* curried functor */
  module CurriedFunctor = (A: ASig) => (B: BSig) => { /* body */
  let result = (+)(A.a, B.b) }
  
  /* ===== Module types with comments ===== */
  /* module type comment */
  module type CommentedSig = { /* inside sig */
  type t; /* before val */
  let x: int }
  
  /* functor sig */
  module type FunctorSig = (M: HasTT) => { /* inside */
  type reexported = M.tt }
  
  /* with constraint */
  module type WithConstraint = HasPolyType with type t = t('a)  = list('a)
  
  /* jsx simple */
  let jsxSimple = <div className={"container"} />
  
  let jsxWithChildren = <Parent prop={1}>
  /* child comment */
    <Child key={"a"} /* this prop is weird */ />
    <Child key={"a" /* this string is weird */} />
    <Child key={33 /* this 33 is weird */} />
    <Child /* key is deprecated */
      key={33}
      />
    /* child comment */
  </Parent> /* wouhou */
  
  /* ===== Types with comments ===== */
  /* type comment */
  type commentedType = int
  
  /* variant comment */
  type commentedVariant
  /* A comment */ = | A /* B comment */
  | B(/* int comment */
  int) /* C comment */
  | C(/* first */
  int, /* second */
  string)
  
  /* record comment */
  type commentedRecord
  /* name comment */ = { name: string, /* age comment */
  age: int }
  
  /* poly comment */
  type commentedPoly('a, 'b)  = (/* first */
  'a /* second */, 'b)
  
  /* gadt comment */
  type commentedGadt(_)  = /* Int comment */
  | Int(/* arg */
  int): commentedGadt(int) /* String comment */
  | String(/* arg */
  string): commentedGadt(string)
  
  exception ParseError(/* a before */
  'a /* a after */, /* b comment */
  'b): exn /* after constraint */
  
  /* **** comment */
  /*** comment */
  let /* comment */
  /*** comment */
  /**** comment */
  /***** comment */
  /*** */
  /**** */
  /***/
  /****/
  /* /** comment */ */
  /* /*** comment */ */
  /* comment **/
  /* comment ***/
  /* comment ****/
  /* comment *****/
  
  testingNotQuiteEndOfLineComments = ::(("Item 1" /* Comment For First Item */, ::(("Item 2" /* Comment For Second Item */, ::(("Item 3" /* Comment For Third Item */, ::(("Item 4" /* Comment For Fourth Item - but no semi */
  /* Comment after last item in list. */, [])))))))) /* Comment after list bracket */
  
  let testingEndOfLineComments = ::(("Item 1" /* Comment For First Item */, ::(("Item 2" /* Comment For Second Item */, ::(("Item 3" /* Comment For Third Item */, ::(("Item 4" /* Comment For Fourth Item - but before semi */
  /* Comment after last item in list. */, [])))))))) /* Comment after list bracket */
  
  /* This time no space between bracket and comment */
  let testingEndOfLineComments = [] /* Comment after list bracket */
  
  
  type t = (int, int) /* End of line on t */
  
  type t22 /* End of t22 line on type t22 = */ = (int, int)
  
  
  type variant
  /* Comment above X */ = | X(int) /* End of line on X */ /* Comment above Y */
  | Y(int) /* End of line on Y */
  /* Comment on entire type def for variant */
  
  type x /* not attached *above* x */ = { fieldOne: int } /* Attached end of line after x */ and y /* not attached *above* y */ = { fieldTwo: int } /* Attached end of line after y */
  
  let result = switch (X((3))) { | X((x)) /* Where does this comment go? */ => let tmp = x; (+)(x, tmp) | Y((x)) => /* How about this one */
  let tmp = x; (+)(x, tmp) }
  
  let result = switch (None) { | Some(({ fieldOne: 20 })) /* Where does this comment go? */ => let tmp = 0; (+)(2, tmp) | Some(({ fieldOne: n })) => /* How about this one */
  let tmp = n; (+)(n, tmp) | None => 20 }
  
  type pointWithManyKindsOfComments
  /* Line before x */ = { x: string /* x field */, /* Line before y */
  y: string /* y field */
  /* Final row of record */ }
  
  type typeParamPointWithComments('a)  = { /* Line before x */
  x: 'a /* x field */, /* Line before y */
  y: 'a /* y field */
  /* Final row of record */ }
  
  
  let name_equal = (x) => (y) => x = y
  
  let equal = (i1) => (i2) => (&&)((=)(i1.contents, i2.contents), true) /* most unlikely first */
  
  let equal = (i1) => (i2) => compare(compare(0, 0), compare(1, 1)) /* END OF LINE HERE */
  
  module Temp = { let v = true /* true */ /* after dec */; let /* before call */
  logIt /* before call */ = (str) => print_string(/* wrong string */
  str) /* after call */ }
  
  let store_attributes = (arg) => let attributes_file = "test"; let proc_name = (^)(attributes_file, ".proc"); let should_write = (||)(Temp.v, not_(Temp.v)); if (should_write) { Temp.logIt(proc_name) }
  
  module MyFirstModule = { let x = 0; /* asdfasdf */
  type i = int /* asfdasdf */ and /* baz */
  n /* bar */ = string /* fooo */ }
  
  /* let binding */
  let /* L1 */
  x /* L2 */ /* L3 */ = "booo" /* L4 */
  
  /* function */
  let /* F1 */
  f /* F2 */ /* F3 */ = (/* F4 */
  a) => /* F5 */
  /* F6 */
  /* F7 */
  a /* F8 */
  
  /* record type */
  type /* RT1 */
  rt /* RT2 */ /* RT3 */ /* RT4 */ = { x /* RT5 */ /* RT6 */: int /* RT7 */ } /* RT8 */
  
  /* variant type */
  type /* VT1 */
  vt /* VT2 */ /* VT3 */ /* VT4 */ = | A /* VT5 */ /* VT6 */ | B(/* VT7 */
  /* VT8 */
  int /* VT9 */) /* VT10 */
  
  /* tuple type */
  type /* TT1 */
  tt /* TT2 */ /* TT3 */ = (/* TT4 */
  int /* TT5 */ /* TT6 */, string /* TT7 */) /* TT8 */
  
  /* switch expression */
  let /* SW1 */
  sw /* SW2 */ /* SW3 */ = switch (/* SW4 */
  /* SW5 */
  x /* SW6 */ /* SW7 */) { | /* SW8 */
  /* SW9 */
  A /* SW10 */ /* SW11 */ /* SW12 */ => 1 | /* SW13 */
  /* SW14 */
  B /* SW15 */ /* SW16 */ /* SW17 */((n)) /* SW18 */ /* SW19 */ /* SW20 */ => n } /* SW21 */
  
  /* if expression */
  let /* IF1 */
  ife /* IF2 */ /* IF3 */ = if (/* IF4 */
  /* IF5 */
  true /* IF6 */ /* IF7 */ /* IF8 */ /* IF9 */ /* IF10 */ /* IF11 */ /* IF12 */ /* IF13 */) { 1 } else { 2 } /* IF14 */
  
  /* try expression */
  let /* TR1 */
  tr /* TR2 */ /* TR3 */ = try (/* TR4 */
  /* TR5 */
  raise(/* TR6 */
  /* TR7 */
  Not_found /* TR8 */ /* TR9 */) /* TR10 */ /* TR11 */) { | /* TR12 */
  Not_found /* TR13 */ /* TR14 */ /* TR15 */ => 0 } /* TR16 */
  
  /* array */
  let /* AR1 */
  arr /* AR2 */ /* AR3 */ = [| /* AR4 */
  1 /* AR5 */ /* AR6 */, 2 /* AR7 */ /* AR8 */, 3 /* AR9 */ |] /* AR10 */
  
  /* list */
  let /* LS1 */
  lst /* LS2 */ /* LS3 */ = ::((/* LS4 */
  1 /* LS5 */ /* LS6 */, ::((2 /* LS7 */ /* LS8 */, ::((3 /* LS9 */, [])))))) /* LS10 */
  
  /* tuple expression */
  let /* TE1 */
  tup /* TE2 */ /* TE3 */ = (/* TE4 */
  1 /* TE5 */ /* TE6 */, "a" /* TE7 */) /* TE8 */
  
  /* record expression */
  let /* RE1 */
  rec_ /* RE2 */ /* RE3 */ = { /* RE4 */
  x /* RE5 */ /* RE6 */: 1 /* RE7 */ } /* RE8 */
  
  /* function application */
  let /* AP1 */
  app /* AP2 */ /* AP3 */ = print_string(/* AP4 */
  /* AP5 */
  "hi" /* AP6 */) /* AP7 */
  
  /* labeled args */
  let /* LA1 */
  lab /* LA2 */ /* LA3 */ = (/* LA4 */
  ~x) => /* LA5 */
  /* LA6 */
  (~y=?) => /* LA7 */
  /* LA8 */
  /* LA9 */
  /* LA10 */
  x /* LA11 */
  
  /* module */
  module /* M1 */
  M /* M2 */ /* M3 */ /* M4 */ /* M5 */ /* M6 */ /* M7 */ /* M8 */ /* M9 */ = { let x = 1 } /* M10 */
  
  /* module type */
  module type /* MT1 */
  MT /* MT2 */ /* MT3 */ /* MT4 */ /* MT5 */ /* MT6 */ /* MT7 */ = { type t } /* MT8 */
  
  /* functor */
  module /* FN1 */
  Fn /* FN2 */ /* FN3 */ /* FN4 */ /* FN5 */ /* FN6 */ /* FN7 */ /* FN8 */ /* FN9 */ /* FN10 */ /* FN11 */ /* FN12 */ /* FN13 */ /* FN14 */ /* FN15 */ = (X: MT) => { type u = X.t } /* FN16 */
  
  /* external */
  external /* EX1 */
  ext /* EX2 */ /* EX3 */ /* EX4 */ /* EX5 */ /* EX6 */ /* EX7 */: int => int = "ext" /* EX8 */
  
  /* exception */
  exception /* EC1 */
  Exc(/* EC2 */
  /* EC3 */
  int /* EC4 */, /* EC5 */
  string /* EC6 */) /* EC7 */
  
  /* poly variant */
  type /* PV1 */
  pv /* PV2 */ /* PV3 */ = [ | /* PV4 */
  /* PV5 */
  `A /* PV6 */ /* PV7 */ | `B /* PV8 */ /* PV9 */ /* PV10 */ /* PV11 */(int) ] /* PV12 */
  
  /* type constraint */
  let /* TC1 */
  tc /* TC2 */ /* TC3 */ /* TC4 */ /* TC5 */ = 1 /* TC6 */
  
  /* pattern matching in let */
  let /* PM1 */
  (/* PM2 */
  a /* PM3 */ /* PM4 */, b /* PM5 */) /* PM6 */ /* PM7 */ = (/* PM8 */
  1 /* PM9 */ /* PM10 */, 2 /* PM11 */) /* PM12 */
  
  /* JSX */
  let /* JX1 */
  jsx /* JX2 */ /* JX3 */ = <div /* JX5 */
    className={"x"} /* JX8 */
    /> /* JX9 */
  
  /* ========================================= */
  /* Additional examples from reasonComments-re.t */
  /* ========================================= */
  /* End of line comments in lists - after item */
  let testingEndOfLineComments1 = ::(("Item 1" /* Comment For First Item */, ::(("Item 2" /* Comment For Second Item */, ::(("Item 3" /* Comment For Third Item */, ::(("Item 4" /* Comment For Fourth Item - but before trailing comma */
  /* Comment after last item in list. */, [])))))))) /* Comment after rbracket */
  
  /* End of line comments in lists - after comma */
  let testingEndOfLineComments2 = ::(("Item 1" /* Comment For First Item */, ::(("Item 2" /* Comment For Second Item */, ::(("Item 3" /* Comment For Third Item */, ::(("Item 4" /* Comment For Fourth Item - but before trailing comma */
  /* Comment after last item in list. */, [])))))))) /* Comment after rbracket */
  
  /* Empty list with comment */
  let testingEndOfLineComments3 = [] /* Comment after entire let binding */
  
  /* Function with comments in args */
  let myFunction = /* First arg */
  (withFirstArg) => /* Second Arg */
  (andSecondArg) => (+)(withFirstArg, andSecondArg) /* After Semi */
  
  /* Record type with comments */
  type point2 = { x: string /* x field */, y: string /* y field */ }
  
  /* Switch with comments */
  let res1 = /* Before switch */
  switch (X((2, 3))) { | /* Above X line */
  X(_) /* End of arrow and X line */ => "result of X" | /* Above Y line */
  Y(_) /* End of arrow and Y line */ => "result of Y" } /* After final semi in switch */
  
  /* Switch with comments after arrow */
  let res2 = switch (X((2, 3))) { | X((0, 0)) /* After X arrow */ => "result of X" /* End of X body line */ | X((1, 0)) /* Before X's arrow */ => "result of X" /* End of X body line */ | X(_) /* After X _ arrow */ => "result of X" /* End of X body line */ | /* Above Y line */
  Y(_) => /* Comment above Y body */
  "result of Y" }
  
  /* Variant with comments above constructors */
  type variant2
  /* Comment above X */ = | X(int, int) /* End of line on X */ /* Comment above Y */
  | Y(int, int)
  
  /* Tuple type with comments */
  type optionalTuple = | OptTup(option((int /* First int */, int /* Second int */)))
  
  type intPair = (int /* First int */, int /* Second int */)
  
  type intPair2 = (/* First int */
  int, /* Second int */
  int)
  
  /* For loop with comment */
  let a2 = ()
  for (i in 0 to 10) { /* bla  */
  a2 }
  
  /* If with comment */
  if (true) { /* hello */
  () }
  
  /* Pattern with end of line comments */
  type color = | Red(int) /* After red end of line */ | Black(int) /* After black end of line */ | Green(int) /* After green end of line */
  /* On next line after color type def */
  /* Or pattern with comments */
  let blahCurriedX = (x) => () => | Red((10)) | Black((20)) | Green((10)) /* After or pattern green */ => 1 | Red((x)) /* After red */ => 0 | Black((x)) /* After black */ => 0 | Green((x)) => 0 /* After second green */
  /* On next line after blahCurriedX def */
  /* Comments inside empty function bodies */
  let fun_def_comment_inline = (()) => {   }
  
  let fun_def_comment_newline = (()) => {   }
  
  let fun_def_comment_long = (()) => {   }
  
  /* For/while/if with comments */
  let trueThing = true
  
  for (i in 0 to 1) { /* comment */
  print_newline(()) }
  
  while (trueThing) { /* comment */
  print_newline(()) }
  
  if (trueThing) { /* comment */
  print_newline(()) }
  
  /* Multiple prints with comments */
  /* Comment before if test */
  if (trueThing) { /* Comment before print */
  print_newline(()); /* Comment before print */
  print_newline(())
  /* Comment after final print */ }
  
  /* If-else with comments */
  /* Comment before if test */
  if (trueThing) { /* Comment before print */
  print_newline(()
  /* Comment after final print */) } else { /* Comment before print */
  print_newline(()
  /* Comment after final print */) }
  
  /* Function call with comments in args */
  let f2 = (a) => (b) => (c) => (d) => (+)((+)((+)(a, b), c), d)
  
  while (trueThing) { f2(/* a */
  1, /* b */
  2, /* c */
  3, /* d */
  4
  /* does work */) }
  
  /* Constructor with comments between args */
  type tester('a, 'b)  = | TwoArgsConstructor('a, 'b) | OneTupleArgConstructor(('a, 'b))
  let callFunctionTwoArgs = (a) => (b) => ()
  let callFunctionOneTuple = (tuple) => ()
  
  let y1 = TwoArgsConstructor((1 /*eol1*/, 2 /* eol2 */))
  
  let y2 = callFunctionTwoArgs(1 /*eol1*/, 2 /* eol2 */)
  
  let y3 = OneTupleArgConstructor(((1 /*eol1*/, 2 /* eol2 */)))
  
  /* Record with comments */
  type polyRecord('a, 'b)  = { fieldOne: 'a, fieldTwo: 'b }
  
  let r1 = { fieldOne: 1 /*eol1*/, fieldTwo: 2 /* eol2 */ }
  
  let r2 = { fieldOne: 1 /*eol1*/, fieldTwo: 2 /* eol2 with trailing comma */ }
  
  /* Doc comments on externals */
  external url: t => string = ""
  
  external url2: t => string = ""
  
  external url3: t => string = ""
  
  /* normal comment */
  external url4: t => string = ""
  
  type q = { a: int, b: string }
  
  let letter = { a: 42, b: "answer" }
