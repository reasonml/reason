/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
class virtual stack 'a init => {
  /*
   * The "as this" is implicit and will be formatted away.
   */
  val virtual dummy: unit;
  val mutable v: list 'a = init;
  method virtual implementMe: int => int;
  method pop =
    switch v {
    | [hd, ...tl] => {
        v = tl;
        Some hd
      }
    | [] => None
    };
  method push hd => v = [hd, ...v];
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
    val mutable v: list 'a = init;
    method virtual implementMe: int => int;
    method pop =
      switch v {
      | [hd, ...tl] => {
          v = tl;
          Some hd
        }
      | [] => None
      };
    method push hd => v = [hd, ...v];
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
type closedObj = <>;

let (<..>) a b => a + b;

let five = 2 <..> 3;

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

module type T = {
  class virtual cl 'a :
    new {}
  and cl2 :
    new {};
};
