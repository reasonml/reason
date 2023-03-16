type point = {
  x: int,
  y: int,
};

let id = x => x;

type myVariant =
  | TwoCombos(inner, inner)
  | Short
  | AlsoHasARecord(int, int, point)
and inner =
  | Unused
  | HeresTwoConstructorArguments(int, int);

let computeTuple = (a, b, c, d, e, f, g, h) => (
  a + b,
  c + d,
  e + f,
  g + h,
);

let res =
  switch (TwoCombos(Unused, Unused)) {
  | TwoCombos(
      HeresTwoConstructorArguments(x, y),
      HeresTwoConstructorArguments(a, b),
    ) => (
      x,
      y,
      a,
      b,
    )
  | TwoCombos(_, _) => (0, 0, 0, 0)
  | Short
  | AlsoHasARecord(300, _, _) => (
      100000,
      100000,
      100000,
      100000,
    )
  | AlsoHasARecord(firstItem, two, {x, y}) =>
    computeTuple(
      firstItem,
      firstItem,
      firstItem,
      firstItem,
      firstItem,
      two,
      two,
      two,
    )
  };

/**
 * Match bodies may include sequence expressions, but without the `{}`
 * braces required.
 */
let res =
  switch (TwoCombos(Unused, Unused)) {
  | TwoCombos(
      HeresTwoConstructorArguments(x, y),
      HeresTwoConstructorArguments(a, b),
    ) =>
    let ret = (x, y, a, b);
    ret;
  | TwoCombos(_, _) =>
    /**
     * See, no braces required - saves indentation as well!
     */
    let ret = (0, 0, 0, 0);
    ret;
  | Short
  | AlsoHasARecord(300, _, _) =>
    /**
     * And no final semicolon is required.
     */
    let ret = (100000, 100000, 100000, 100000);
    ret;
  | AlsoHasARecord(firstItem, two, {x, y}) =>
    computeTuple(
      firstItem,
      firstItem,
      firstItem,
      firstItem,
      firstItem,
      two,
      two,
      two,
    )
  };

/**
 * Ensure that nested Pexp_functions are correctly wrapped in parens.
 *
 */
let res =
  switch (TwoCombos(Unused, Unused)) {
  | TwoCombos(
      HeresTwoConstructorArguments(x, y),
      HeresTwoConstructorArguments(a, b),
    ) => (
      fun
      | Some(x) => x + 1
      | None => 0
    )
  | TwoCombos(_, _) =>
    let x = (
      fun
      | Some(x) => x + 1
      | None => 0
    );
    x;
  | Short
  | AlsoHasARecord(300, _, _) =>
    id(
      fun
        | Some(x) => x + 1
        | None => 0
      ,
    )
  | AlsoHasARecord(firstItem, two, {x, y}) =>
    id(
      fun
        | Some(x) => x + 1
        | None => 0
      ,
    )
  };


switch (Some(())) {
| Some(()) => 1
| _ => 2
};


switch (Some(())) {
| Some(()) => 1
| _ => 2
};


switch (Some(())) {
| Some(()) => 1
| _ => 2
};


switch (Some(())) {
| Some(()) => 1
| _ => 2
};

type foo =  | Foo(unit);


switch (Foo(())) {
| Foo(()) => 1
};


switch (Foo(())) {
| Foo(()) => 1
};


switch (Foo(())) {
| Foo(()) => 1
};


switch (Foo(())) {
| Foo(()) => 1
};


switch (()) {
| () => 1
};


switch (()) {
| () => 1
};


switch (()) {
| () => 1
};


switch (()) {
| () => 1
};


switch (Some(1)) {
| Some(1) => 1
| None => 2
| _ => 3
};

let myInt = 100;

todo attributelet rangeInt = 0;

let myChar = 'x';

let rangeChar =
  switch (myChar) {
  | todo: Ppat_interval => "a to b"
  | todo: Ppat_interval => "b to z"
  | c => "something else"
  };


switch (None) {
| Some([]) => ()
| Some(::(_, [])) => ()
| Some(::(x, [])) => ()
| Some(::(x, xs)) => ()
| Some(::(x, ::(y, ::(z, [])))) => ()
| _ => ()
};


switch (None) {
| Some([]) => ()
| Some(::(_, [])) => ()
| Some(::(x, [])) => ()
| Some(::(x, xs)) => ()
| Some(::(x, ::(y, ::(z, [])))) => ()
| _ => ()
};


switch (None) {
| Some(todo: Ppat_array) => "empty"
| Some(todo: Ppat_array) => "one any"
| Some(todo: Ppat_array) => "one"
| Some(todo: Ppat_array) => "two"
| _ => "many"
};


switch (None) {
| Some(todo: Ppat_array) => "empty"
| Some(todo: Ppat_array) => "one any"
| Some(todo: Ppat_array) => "one"
| Some(todo: Ppat_array) => "two"
| _ => "many"
};


switch (None) {
| Some({x}) => ()
| Some({x, y}) => ()
| _ => ()
};


switch (None) {
| Some({x}) => ()
| Some({x, y}) => ()
| _ => ()
};


switch (None) {
| Some(todo: Ppat_array) => ()
| _ => ()
};


switch (None) {
| Some(
    (someSuperLongString, thisShouldBreakTheLine),
  ) => ()
| _ => ()
};


switch (None) {
| Some(
    ::(
      someSuperLongString,
      ::(thisShouldBreakTheLine, []),
    ),
  ) => ()
| Some(
    ::(
      someSuperLongString,
      es6ListSugarLikeSyntaxWhichIsSuperLong,
    ),
  ) => ()
| Some(
    ::(
      someSuperLongString,
      es6ListSugarLikeSyntaxWhichIsSuperLong,
    ),
  ) => ()
| _ => ()
};

type aOrB =
  | A(int)
  | B(int);

let todo: Ppat_constraint = 0;

let todo: Ppat_constraint = A(0);

type test_foo =
  | VariantType1
  | VariantType2;

let branch_with_variant_and_annotation = (
  fun
  | todo: Ppat_constraint => true
  | VariantType2 => false
);

type intRange = {
  from: string1,
  to_: string1,
};

type optIntRange = todo: ptype_abstract;

let optIntRangeOfIntRange = (
  fun
  | todo: Ppat_constraint => todo Pexp_constraint
  | {from, to_} => Some(todo Pexp_record)
);
