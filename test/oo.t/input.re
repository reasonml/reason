/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

class virtual stack('a)(init) {
  /*
   * The "as this" is implicit and will be formatted away.
   */
  as this;
  val virtual dummy : unit;
  val mutable v : list('a) = init;

  pub virtual implementMe: (int) => int;
  pub pop =
    switch (v) {
      | [hd, ...tl] => {
        v = tl;
        Some(hd);
      }
      | [] => None
    };

  pub push(hd) {v = [hd, ...v]};
  initializer {
    print_string("initializing object");
  };
  pub explicitOverrideTest(a) { a + 1 };
  pri explicitOverrideTest2(a) { a + 1 };
};

let tmp = {
  /**
   * comment here.
   */
  as this;
  val x = 10;
};

/**
 * Comment on stackWithAttributes.
 */
[@thisShouldntBeFormattedAway]
class virtual stackWithAttributes('a)(init)
/* Before class */
{

  /* The "as this" should not be formatted away because attributes. */

  as [@thisShouldntBeFormattedAway] this;
  /* Before floatting attribute */
  [@floatingAttribute];
  /* Virtual member */
  [@itemAttr1]
  val virtual dummy : unit;
  [@itemAttr2]
  val mutable v : list('a) = init;

  pub virtual implementMe: (int) => int;
  pub pop =
    switch (v) {
      | [hd, ...tl] => {
        v = tl;
        Some(hd);
      }
      | [] => None
    };

  pub push(hd) {v = [hd, ...v]};
  initializer {
    print_string("initializing object");
  };
}
;

class extendedStack('a)(init) {
  inherit (class stack('a))(init);
  val dummy = ();
  pub implementMe(i) = i;
};

class extendedStackAcknowledgeOverride('a)(init) {
  inherit (class stack('a))(init);
  val dummy = ();
  pub implementMe(i) { i + 1 };
  pub! explicitOverrideTest(a) { a + 2 };
  pri! explicitOverrideTest2(a) { a + 2 };
};

let inst = new extendedStack([1, 2]);

/**
 * Recursive classes.
 */
/*
 * First recursive class.
 */
class firstRecursiveClass(init) {
  val v = init;
}
/*
 * Second recursive class.
 */
and secondRecursiveClass(init) {
  val v = init;
};



/**
 * For now, mostly for historic reasons, the syntax for type
 * definitions/annotations on anonymous objects are different than
 * "class_instance_type". That needn't be the case. The only challenge is that
 * whatever we do, there is a slight challenge in avoiding conflicts with
 * records. Clearly {x:int, y:int} will conflict. However, open object types in
 * the form of {.. x:int, y:int} do not conflict. The only thing that must be
 * resolved is closed object types and records. you could have a special token
 * that means "closed". {. x: int, y:int}. If only closed object types would be
 * optimized in the same way that records are, records could just be replaced
 * with closed object types.
 */
/**
 * Anonymous objects.
 */

type closedObj = {.};

let (<..>)(a,b) = a + b;
let five = 2 <..> 3;

type nestedObj = {. bar : {. a: int}};

let (>>)(a,b) = a > b;

let bigger = 3 >> 2;

type typeDefForClosedObj = {. x: int, y:int};
type typeDefForOpenObj('a) = {.. x:int, y:int} as 'a;
let anonClosedObject: {. x:int, y:int} = {
  pub x { 0 };
  pub y { 0 };
};

let onlyHasX = {pub x = 0};
let xs: list({. x:int}) = [onlyHasX, anonClosedObject :> {. x: int}];

let constrainedAndCoerced =
  ([anonClosedObject, anonClosedObject] : list({. x:int, y:int}) :> list({. x:int}));

/* If one day, unparenthesized type constraints are allowed on the RHS of a
 * record value, we're going to have to be careful here because >} is parsed as
 * a separate kind of token (for now). Any issues would likely be caught in the
 * idempotent test case.
 */
let xs: ref({. x:int}) = {contents: (anonClosedObject :> {. x: int})};

let coercedReturn = {
  let tmp = anonClosedObject;
  (tmp :> {. x: int})
};

let acceptsOpenAnonObjAsArg (o: {.. x: int, y:int}) = o#x + o#y;
let acceptsClosedAnonObjAsArg (o: {. x: int, y:int}) = o#x + o#y;
let res = acceptsOpenAnonObjAsArg {
  pub x = 0;
  pub y = 10;
};

let res = acceptsOpenAnonObjAsArg {
  pub x = 0;
  pub y = 10;
  pub z = 10;
};

let res = acceptsClosedAnonObjAsArg {
  pub x = 0;
  pub y = 10;
};

/* TODO: Unify class constructor return values with function return values */
class myClassWithAnnotatedReturnType(init) : {pub x : int; pub y : int} {
  pub x = ( init : int );
  pub y = init;
};
/**
 * May include a trailing semi after type row.
 */
class myClassWithAnnotatedReturnType2(init):{pub x : int; pub y : int;} {
  pub x = ( init : int );
  pub y = init;
};

/**
 * May use equals sign, and may include colon if so.
 */
class myClassWithAnnotatedReturnType3(init):{pub x : int; pub y : int;} {
  pub x = ( init : int );
  pub y:int = init;
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
class myClassWithAnnotatedReturnType3_annotated_constructor :
      (int) => {pub x : int; pub y : int;} =
  (init) => {
    pub x = ( init : int );
    pub y:int = init;
  };

class tupleClass('a,'b) (init: ('a, 'b)) {
  pub pr = init;
};

module HasTupleClasses : {
  /**
   * exportedClass.
   */
  class exportedClass: (int) => {pub x : int; pub y : int};
  /**
   * anotherExportedClass.
   */
  class anotherExportedClass('a,'b) : (('a, 'b)) => {pub pr: ('a, 'b)};
} = {
  /**
   * exportedClass.
   */
  class exportedClass = myClassWithAnnotatedReturnType3;

  /**
   * anotherExportedClass.
   */
  class anotherExportedClass('a,'b) = (class tupleClass('a,'b));
};

class intTuples = (class tupleClass(int,int));

class intTuplesHardcoded = (class tupleClass(int,int))((8, 8));

/**
 * Note that the inner tupleClass doesn't have the "class" prefix because
 * they're not kinds of classes - they're types of *values*.
 * The parens here shouldn't be required.
 */
class intTuplesTuples = (
  class tupleClass(
    (tupleClass(int,int)),
    (tupleClass(int,int))
  )
);

let x: tupleClass(int,int) = {pub pr = (10, 10);};

let x: #tupleClass(int,int) = x;

let incrementMyClassInstance: (int, #tupleClass(int,int)) => #tupleClass(int,int) =
  (i,inst) => {
    let (x, y) = inst#pr;
    {pub pr = (x + i, y + i);};
  };

class myClassWithNoTypeParams = {};
/**
 * The #myClassWithNoTypeParams should be treated as "simple"
 */
type optionalMyClassSubtype('a) = option(#myClassWithNoTypeParams) as 'a;



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
  pub x: int;
  pub y: int;
  pub add: (addablePointClassType, addablePointClassType) => int;
};

/**
 * Class constructor types can be annotated.
 */
class addablePoint: (int) => addablePointClassType = fun(init) => {
  as self;
  pub add (one: addablePointClassType, two:addablePointClassType) =
    one#x + two#x + one#y + two#x;
  pub x = (init : int);
  pub y = init;
};

class addablePoint2 = (fun(init) => {
  as self;
  pub add (one: addablePointClassType, two:addablePointClassType) =
    one#x + two#x + one#y + two#x;
  pub x = (init : int);
  pub y = init;
} : (int) => addablePointClassType);

module type T = {
  class virtual cl('a) : {}
  and cl2 : {};
};

let privacy = {
    pri x(c) = 5 + c;
};

module Js = {
  type t('a);
};

/* supports trailing comma */
type stream('a) = {. "observer": ('a => unit) => unit,};
