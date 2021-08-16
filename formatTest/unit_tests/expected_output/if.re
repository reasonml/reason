[@reason.version 3.7];
/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

let logTSuccess = self =>
  if (self > other) {
    print_string("Did T");
    print_newline();
  } else {
    ();
  };
let something =
  if (self.ext.logSuccess) {
    print_string("Did T");
    print_newline();
  };

let logTSuccess = self =>
  if (self.ext.logSuccess) {
    print_string("Did T");
    print_newline();
  } else {
    ();
  };

if (if (x) {true} else {false}) {
  true;
} else {
  false;
};

/* Parens are required around if if it's an argument - this is the same as before. */
if (callSomeFunction(
      if (true) {true} else {false},
    )) {
  true;
} else {
  false;
};

/* Notice that to do something strange, your code must *look* strange. */
/* That's generally a good thing */
if (callSomeFunction) {
  if (true) {true};
} else {
  false;
};

if (callSomeFunction(
      {
        thisIsAnArgument;
        notTheControlFlow;
      },
    )) {
  thisIsTheControlFlow;
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
if (printIfFirstArgGreater) {
  simpleThen;
} else {
  thisDoesnt(even, have2, be, simple);
};

if (if (x) {true} else {false}) {
  ();
} else {
  ();
};

/**                            TERNARY
 *============================================================================
 */

let ternaryResult =
  something
    ? callThisFunction(withThisArg) : thatResult;

let annotatedTernary =
  true && (something ? true : false: bool);

let annotatedBranch =
  true
  && (something ? (true: bool) : false: bool);

/* The following should be... */
let whatShouldThisBeParsedAs =
  something
    ? callThisFunction(withThisArg)
    : trailingTest ? true : false;

/* ... it should be parsed as */
let whatShouldThisBeParsedAs =
  something
    ? callThisFunction(withThisArg)
    : trailingTest ? true : false;

/* Should *not* be parsed as */
let whatShouldThisBeParsedAs =
  (
    something
      ? callThisFunction(withThisArg)
      : trailingTest
  )
    ? true : false;

/* the following shoud be... */
let ternaryFormatting =
  something
    ? notLongEnoughToCauseTwoLineBreaks : other;

/* ideally formatted as
   let ternaryFormatting =
     something
     ? notLongEnoughToCauseTwoLineBreaks
     : other
   */
let ternaryFormatting =
  something
    ? notLongEnoughToCauseTwoLineBreaks : other;

/* but is currently formatted as the following (which is less than desirable)
   let ternaryFormatting =
     something
     ? notLongEnoughToCauseTwoLineBreaks : other
    */
let ternaryFormatting =
  something
    ? notLongEnoughToCauseTwoLineBreaks : other;

let ternaryResult =
  aaaaaa
    ? bbbbbbb
    : ccccc ? ddddddd : eeeee ? fffffff : ggggg;

/* Should be parsed as: */
let ternaryResult =
  aaaaaa
    ? bbbbbbb
    : ccccc ? ddddddd : eeeee ? fffffff : ggggg;

let ternaryResult =
  /* The first Parens *must* be preserved! */
  (x ? y : z)
    ? bbbbbbb
    : ccccccc
        ? ddddddd : eeeeeee ? fffffff : ggggg;

let ternaryResult =
  aaaaaaa
    ? bbbbbbb
    /* The second Parens *must* be preserved! */
    : (x ? y : z)
        ? ddddddd : eeeeeee ? fffffff : ggggg;

let ternaryResult =
  aaaaaaa
    ? bbbbbbb
    : x
        ? y
        : z
            ? ddddddd
            /* The final parent don't need to be preserved */
            : eeeeeee ? fffffff : x ? y : z;

let addOne = x => x + 1;

let result =
  addOne(0) + 0 > 1
    ? print_string("this wont print")
    : print_string("this will");
/*
 * Should be parsed as:
 */
let result =
  addOne(0) + 0 > 1
    ? print_string("this wont print")
    : print_string("this will");

/*
 * Try shouldn't be aliased as ternary!
 */
let res =
  try(something) {
  | true => "hi"
  | false => "bye"
  };

/*
 * Many levels of if elseif should be formatted very nicely.
 */
let result =
  if (something) {
    Console.log("First Branch");
  } else if (anotherThing) {
    Console.log("Second Branch");
  } else if (yetAnotherThing) {
    Console.log("Third Branch");
  } else {
    Console.log("Final Case");
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
  pixRation > 1
    ? "@" ++ string_of_int(pixRation) ++ "x.png"
    : ".png";
