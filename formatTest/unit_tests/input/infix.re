/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

/* - A good way to test if formatting of infix operators groups precedences
   correctly, is to write an expression twice. Once in a form where parenthesis
   explicitly group according to the parse tree and write it another time
   without any parenthesis. After formatting, the two should be equal
   textually.
   - Reformatting n > 0 times should be idempotent.
   - Our formatting algorithm *could* decide to leave equivalently precedented
   infix applications ungrouped in parenthesis (which is what the above test
   verifies), but the additional parenthesis is nice.  */
/* < > = all have same precedence level/direction(left) */
let parseTree = ((x > y > z) < a < b) == c == d;

let minParens = ((x > y > z) < a < b) == c == d;

let formatted = ((x > y > z) < a < b) == c == d;

/* Case with === */

let parseTree = ((x > y > z) < a < b) === c === d;

let minParens = ((x > y > z) < a < b) === c === d;

let formatted = ((x > y > z) < a < b) === c === d;

/* < > = all have same precedence level and direction (left) */
let parseTree = a1 < a2 < (b1 > b2 > (y == x == z));

let minParens = a1 < a2 < (b1 > b2 > (y == x == z));

let formatted = a1 < a2 < (b1 > b2 > (y == x == z));

/* Case with === */

let parseTree = a1 < a2 < (b1 > b2 > (y === x === z));

let minParens = a1 < a2 < (b1 > b2 > (y === x === z));

let formatted = a1 < a2 < (b1 > b2 > (y === x === z));

/* !=...(left) same level =(left) is higher than :=(right) */
let parseTree = a1 := a2 := b1 == b2 == (y != x != z);

let minParens = a1 := a2 := b1 == b2 == (y != x != z);

let formatted = a1 := a2 := b1 == b2 == (y != x != z);

/* Case with === */

let parseTree = a1 := a2 := b1 === b2 === (y !== x !== z);

let minParens = a1 := a2 := b1 === b2 === (y !== x !== z);

let formatted = a1 := a2 := b1 === b2 === (y !== x !== z);

/* !=...(left) same level =(left) is higher than :=(right) */
let parseTree = a1 := a2 := b1 == ((b2 == y) != x != z);

let minParens = a1 := a2 := b1 == ((b2 == y) != x != z);

let formatted = a1 := a2 := b1 == ((b2 == y) != x != z);

/* Case with === */

let parseTree = a1 := a2 := b1 === ((b2 === y) !== x !== z);

let minParens = a1 := a2 := b1 === ((b2 === y) !== x !== z);

let formatted = a1 := a2 := b1 === ((b2 === y) !== x !== z);

/* &...(left) is higher than &(right). &(right) is equal to &&(right) */
let parseTree = a1 && (a2 && (b1 & b2 & y &|| x &|| z));

let minParens = a1 && a2 && (b1 & b2 & y &|| x &|| z);

let formatted = a1 && a2 && (b1 & b2 & y &|| x &|| z);

/**
 * Now, let's try an example that resembles the above, yet would require
 * parenthesis everywhere.
 */
/* &...(left) is higher than &(right). &(right) is equal to &&(right) */
let parseTree = ((((a1 && a2) && b1) & b2) & y) &|| (x &|| z);

let minParens = ((((a1 && a2) && b1) & b2) & y) &|| (x &|| z);

let formatted = ((((a1 && a2) && b1) & b2) & y) &|| (x &|| z);

/* **...(right) is higher than *...(left) */
let parseTree = ((b1 *| b2) *| (y *\*| (x *\*| z)));

let minParens = b1 *| b2 *| y *\*| x *\*| z;

let formatted = b1 *| b2 *| y *\*| x *\*| z;


/* **...(right) is higher than *...(left) */
let parseTree = ((b1 *| b2) *| (y *\*| ((x *\*| z) *| a)));

let minParens = b1 *| b2 *| y *\*| (x *\*| z *| a);

let formatted = b1 *| b2 *| y *\*| (x *\*| z *| a);


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
let seeWhichCharacterHasHigherPrecedence = (first |> second |> third) ^> fourth;

let seeWhichCharacterHasHigherPrecedence = first |> second |> third;

let seeWhichCharacterHasHigherPrecedence = first + second + third;

let comparison = (==);

/* Why would the following two cases have different grouping? */
let res = blah || DataConstructor 10 || DataConstructor 10 && 10;

let res = blah && DataConstructor 10 && DataConstructor 10 + 10;

/* This demonstrates how broken infix pretty printing is:
 */
let curriedComparison = (==) 10;

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
let (|>) firsfirst second third => first + second + third;

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
let seeWhichCharacterHasHigherPrecedence = first |> second ^> third;

let seeWhichCharacterHasHigherPrecedence = first ^> second |> third;

let seeWhichCharacterHasHigherPrecedence = first ^> (second |> third) |> fourth;

let res = blah && DataConstructor 10 && DataConstructor 10 + 10;

/* Should be parsed as */
let res = blah && DataConstructor 10 && DataConstructor 10 + 10;

let (++) label::label label2::label2 => label + label2;

let (++) label::label label2::label2 => label + label2;

let (++) = (++);

let (++): int => int = (++);

(++) label::20 label2::30 + 40;

/* Should be parsed as: */
(++) label::20 label2::30 + 40;

/* Great idea! */
let (==) a b => a < 0;

let (==) a b => a < 0;

let (==) = (==);

let (==): int => int = (==);

let equal = Pervasives.(==);

let starInfix_makeSureSpacesSurround = ( * );

let starInfix_makeSureSpacesSurround = ( *\*\* );

/* The following two should be equivalently parsed/printed.  */
let includesACommentCloseInIdentifier = ( *\*\/ );

let includesACommentCloseInIdentifier = ( *\*\/ );

let shouldSimplifyAnythingExceptApplicationAndConstruction = call "hi" ^ (switch x {
                                                                    | _ => "hi"
                                                                    }) ^ "yo";

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

let res = f - - x;

let res = f - (- x);

let res = - (- x);

let res = f (- x);

/**
 * Test using almost simple prefix as regular function.
 */
let (!!) a b => a + b;

let res = (!!) 20 40;

/* The semicolon should be attached to someType */
let myFunc aaaa bbbb cccc dddd aaaa bbbb cccc dddd aaaa =>
  [blah aaaa bbbb cccc dddd aaaa bbbb cccc dddd aaaa, ...someType];

/**
 * Testing various fixity.
 */

/**
 * For each of these test cases for imperative updates, we'll test both record
 * update, object member update and array update.
 */
let containingObject = {
  val mutable y = 0;
  val arr = [|true, false, false|];
  val bigArr = "goodThingThisIsntTypeChecked";
  val str = "string";
  pub testCases () => {
    /**
     * The lowest precedence token is =, followed by :=, and then ?, then :.
     *
     * The following text
     *
     *     x.contents = tenaryTest ? ifTrue : ifFalse
     *
     * Generates the following parse tree:
     *
     *                =
     *              /   \
     *             /     \
     *         record   ternary
     *
     * Because when encountering the ? the parser will shift on the ? instead of
     * reducing  expr = expr
     */

    /**
     * Without a + 1
     */
    x.contents = something ? hello : goodbye;
    y = something ? hello : goodbye;
    arr.(0) = something ? hello : goodbye;
    bigArr.{0} = something ? hello : goodbye;
    str.[0] = something ? hello : goodbye;

    (x.contents = something) ? hello : goodbye;
    (y = something) ? hello : goodbye;
    (arr.(0) = something) ? hello : goodbye;
    (bigArr.{0} = something) ? hello : goodbye;
    (str.[0] = something) ? hello : goodbye;

    x.contents = (something ? hello : goodbye);
    y = (something ? hello : goodbye);
    arr.(0) = (something ? hello : goodbye);
    bigArr.{0} = (something ? hello : goodbye);
    str.[0] = (something ? hello : goodbye);


    /**
     * With a + 1
     */
    x.contents = something + 1 ? hello : goodbye;
    x := something + 1 ? hello : goodbye;
    y = something + 1 ? hello : goodbye;
    arr.(0) = something + 1 ? hello : goodbye;
    bigArr.{0} = something + 1 ? hello : goodbye;
    str.[0] = something + 1 ? hello : goodbye;

    (x.contents = something + 1) ? hello : goodbye;
    (x := something + 1) ? hello : goodbye;
    (y = something + 1) ? hello : goodbye;
    (arr.(0) = something + 1) ? hello : goodbye;
    (bigArr.{0} = something + 1) ? hello : goodbye;
    (str.[0] = something + 1) ? hello : goodbye;

    x.contents = (something + 1 ? hello : goodbye);
    x := (something + 1 ? hello : goodbye);
    y = (something + 1 ? hello : goodbye);
    arr.(0) = (something + 1 ? hello : goodbye);
    bigArr.{0} = (something + 1 ? hello : goodbye);
    str.[0] = (something + 1 ? hello : goodbye);


    /**
     * #NotActuallyAConflict
     * Note that there's a difference with how = and := behave.
     * We only *simulate* = being an infix identifier for the sake of printing,
     * but for parsing it's a little more nuanced. There *isn't* technically a
     * shift/reduce conflict in the following that must be resolved via
     * precedence ranking:
     *
     *     a + b.c = d
     *
     * No conflict between reducing a + b.c, and shifting =, like there would
     * be if it was := instead of =. That's because the rule for = isn't the
     * infix rule with an arbitrary expression on its left - it's something
     * much more specific.
     *
     * (simple_expr) DOT LIDENT EQUAL expression.
     *
     * So with the way yacc/menhir works, when it sees an equal sign, it knows
     * that there is no valid parse where a + b.c is reduced to an expression
     * with an = immediately appearing after, so it shifts the equals.
     *
     * If you replace = with :=, you'd see different behavior.
     *
     *     a + b.c := d
     *
     *  Since := has lower precedence than +, it would be parsed as:
     *
     *     (a + b.c) := d
     *
     * However, our printing logic will print = assignment with parenthesis:
     *
     *     a + (b.c = d)
     *
     * Even though they're not needed, because it doesn't know details about
     * which rules are valid, we just told it to print = as if it were a valid
     * infix identifier.
     *
     * Another case:
     *
     *    something >>= fun x => x + 1;
     *
     * Will be printed as:
     *
     *    something >>= (fun x => x + 1);
     *
     * Because the arrow has lower precedence than >>=, but it wasn't needed because
     *
     *    (something >>= fun x) => x + 1;
     *
     * Is not a valid parse. Parens around the `=>` weren't needed to prevent
     * reducing instead of shifting. To optimize this part, we need a much
     * deeper encoding of the parse rules to print parens only when needed.
     *
     */

    /* The following */
    x + something.contents = y;
    x + something = y;
    x + something.contents := y;
    x + something := y;

    /* Should be parsed as: */
    x + (something.contents = y); /* Because of the #NotActuallyAConflict above */
    x + (something = y); /* Same */
    (x + something.contents) := y;
    (x + something) := y;

    /* To make the := parse differently, we must use parens */
    x + (something.contents := y);
    x + (something := y);


    /**
     * Try with ||
     */
    x.contents || something + 1 ? hello : goodbye;
    y || something + 1 ? hello : goodbye;
    arr.(0) || something + 1 ? hello : goodbye;
    bigArr.{0} || something + 1 ? hello : goodbye;
    str.[0] || something + 1 ? hello : goodbye;

    (x.contents || something + 1) ? hello : goodbye;
    (y || something + 1) ? hello : goodbye;
    (arr.(0) || something + 1) ? hello : goodbye;
    (bigArr.{0} || something + 1) ? hello : goodbye;
    (str.[0] || something + 1) ? hello : goodbye;

    x.contents || (something + 1 ? hello : goodbye);
    y || (something + 1 ? hello : goodbye);
    arr.(0) || (something + 1 ? hello : goodbye);
    bigArr.{0} || (something + 1 ? hello : goodbye);
    str.[0] || (something + 1 ? hello : goodbye);


    /**
     * Try with &&
     */
    x.contents && something + 1 ? hello : goodbye;
    y && something + 1 ? hello : goodbye;
    arr.(0) && something + 1 ? hello : goodbye;
    bigArr.{0} && something + 1 ? hello : goodbye;
    str.[0] && something + 1 ? hello : goodbye;

    (x.contents && something + 1) ? hello : goodbye;
    (y && something + 1) ? hello : goodbye;
    (arr.(0) && something + 1) ? hello : goodbye;
    (bigArr.{0} && something + 1) ? hello : goodbye;
    (str.[0] && something + 1) ? hello : goodbye;

    x.contents && (something + 1 ? hello : goodbye);
    y && (something + 1 ? hello : goodbye);
    arr.(0) && (something + 1 ? hello : goodbye);
    bigArr.{0} && (something + 1 ? hello : goodbye);
    str.[0] && (something + 1 ? hello : goodbye);



    /**
     * See how regular infix operators work correctly.
     */
    x.contents = (2 + 4);
    y = (2 + 4);
    arr.(0) = (2 + 4);
    bigArr.{0} = (2 + 4);
    str.[0] = (2 + 4);

    (x.contents = 2) + 4;
    (y = 2) + 4;
    (arr.(0) = 2) + 4;
    (bigArr.{0} = 2) + 4;
    (str.[0] = 2) + 4;


    /**
     * Ensures that record update, object field update, and := are all right
     * associative.
     */
    x.contents = y.contents = 10;
    y = x.contents = 10;
    arr.(0) = x.contents = 10;
    bigArr.{0} = x.contents = 10;
    str.[0] = x.contents = 10;
    /* Should be the same as */
    x.contents = (x.contents = 10);
    y = (x.contents = 10);
    arr.(0) = (x.contents = 10);
    bigArr.{0} = (x.contents = 10);
    str.[0] = (x.contents = 10);


    /**
     * Ensures that record update, object field update, and := are all right
     * associative.
     */
    x := x := 10;
    /* Should be the same as */
    x := (x := 10);

    /* By default, without parens*/
    x ? y : z ? a : b;

    /* It is parsed as the following: */
    x ? y : (z ? a : b);

    /* Not this: */
    (x ? y : z) ? a : b;

    /**
     *          ^
     * When rendering the content to the left of the ? we know that we want the
     * parser to reduce the thing to the left of the ? when the ? is seen.  So we
     * look at the expression to the left of ? and discover what precedence level
     * it is (token of its rightmost terminal). We then compare it with ? to see
     * who would win a shift reduce conflict. We want the term to the left of the ?
     * to be reduced. So if it's rightmost terminal isn't higher precedence than ?,
     * we wrap it in parens.
     */


    /***
     * The following
     */
    x.contents = something ? x.contents = somethingElse : goodbye;
    y = something ? y = somethingElse : goodbye;
    arr.(0) = something ? arr.(0) = somethingElse : goodbye;
    bigArr.{0} = something ? bigArr.{0} = somethingElse : goodbye;
    str.[0] = something ? str.[0] = somethingElse : goodbye;
    /*
     * Should be parsed as
     */
    x.contents = (something ? x.contents = somethingElse : goodbye);
    y = (something ? y = somethingElse : goodbye);
    arr.(0) = (something ? arr.(0) = somethingElse : goodbye);
    bigArr.{0} = (something ? bigArr.{0} = somethingElse : goodbye);
    str.[0] = (something ? str.[0] = somethingElse : goodbye);

    /** And this */
    y := something ? y := somethingElse : goodbye;
    arr.(0) := something ? arr.(0) := somethingElse : goodbye;
    bigArr.{0} := something ? bigArr.{0} := somethingElse : goodbye;
    str.[0] := something ? str.[0] := somethingElse : goodbye;

    /* Should be parsed as */
    y := (something ? (y := somethingElse) : goodbye);
    arr.(0) := (something ? (arr.(0) := somethingElse) : goodbye);
    bigArr.{0} := (something ? (bigArr.{0} := somethingElse) : goodbye);
    str.[0] := (something ? (str.[0] := somethingElse) : goodbye);


    /* The following */
    x := something ? x.contents = somethingElse ? goodbye : goodbye : goodbye;
    x := something ? arr.(0) = somethingElse ? goodbye : goodbye : goodbye;
    x := something ? bigArr.{0} = somethingElse ? goodbye : goodbye : goodbye;
    x := something ? str.[0] = somethingElse ? goodbye : goodbye : goodbye;
    /* Is parsed as */
    x := (something ? x.contents = (somethingElse ? goodbye : goodbye) : goodbye);
    x := (something ? arr.(0) = (somethingElse ? goodbye : goodbye) : goodbye);
    x := (something ? bigArr.{0} = (somethingElse ? goodbye : goodbye) : goodbye);
    x := (something ? str.[0] = (somethingElse ? goodbye : goodbye) : goodbye);
    /* is not the same as */
    x := something ? (x.contents = somethingElse) ? goodbye : goodbye : goodbye;
    x := something ? (arr.(0) = somethingElse) ? goodbye : goodbye : goodbye;
    x := something ? (bigArr.{0} = somethingElse) ? goodbye : goodbye : goodbye;
    x := something ? (str.[0] = somethingElse) ? goodbye : goodbye : goodbye;

    /**
     * And
     */

    /** These should be parsed the same */
    something ? somethingElse : x.contents = somethingElse ? x : z;
    something ? somethingElse : (x.contents = (somethingElse ? x : z));
    /* Not: */
    something ? somethingElse : (x.contents = somethingElse) ? x : z;
    (something ? somethingElse : (x.contents = somethingElse)) ? x : z;

    /* These should be parsed the same */
    something ? somethingElse : x := somethingElse ? x : z;
    something ? somethingElse : (x := (somethingElse ? x : z));
    /* Not: */
    something ? somethingElse : (x := somethingElse) ? x : z;
    (something ? somethingElse : (x := somethingElse)) ? x : z;

    /** These should be parsed the same */
    something ? somethingElse : y = somethingElse ? x : z;
    something ? somethingElse : (y = (somethingElse ? x : z));
    /* Not: */
    something ? somethingElse : (y = somethingElse) ? x : z;
    (something ? somethingElse : (y = somethingElse)) ? x : z;

    /** These should be parsed the same */
    something ? somethingElse : arr.(0) = somethingElse ? x : arr.(0);
    something ? somethingElse : (arr.(0) = (somethingElse ? x : arr.(0)));
    /* Not: */
    something ? somethingElse : (arr.(0) = somethingElse) ? x : z;
    (something ? somethingElse : (arr.(0) = somethingElse)) ? x : z;

    /** These should be parsed the same */
    something ? somethingElse : bigArr.{0} = somethingElse ? x : bigArr.{0};
    something ? somethingElse : (bigArr.{0} = (somethingElse ? x : bigArr.{0}));
    /* Not: */
    something ? somethingElse : (bigArr.{0} = somethingElse) ? x : z;
    (something ? somethingElse : (bigArr.{0} = somethingElse)) ? x : z;

    /** These should be parsed the same */
    something ? somethingElse : arr.[0] = somethingElse ? x : arr.[0];
    something ? somethingElse : (arr.[0] = (somethingElse ? x : arr.[0]));
    /* Not: */
    something ? somethingElse : (str.[0] = somethingElse) ? x : z;
    (something ? somethingElse : (str.[0] = somethingElse)) ? x : z;

    /**
     * It creates a totally different meaning when parens group the :
     */
    x.contents = something ? (x.contents = somethingElse : x) : z;
    y = something ? (y = somethingElse : x) : z;
    arr.(0) = something ? (arr.(0) = somethingElse : x) : z;
    bigArr.{0} = something ? (bigArr.{0} = somethingElse : x) : z;
    str.[0] = something ? (str.[0] = somethingElse : x) : z;

    /**
     * Various precedence groupings.
     */
    true ? true ? false : false : false;
    /* Is the same as */
    true ? (true ? false : false) : false;
    /*
     * Just some examples of how prefix will be printed.
     */
    - x + something.contents = y;
    - x + something = y;
    - x + something.contents := y;
    - x + something := y;
    x + - something.contents = y;
    x + - something = y;
    x + - something.contents := y;
    x + - something := y;
    x.contents || something + 1 ? - hello : goodbye;
    bigArr.{0} || - something + 1 ? hello : goodbye;
    let result = - x + something.contents = y;

    /* Prefix minus is actually sugar for regular function identifier ~-*/
    let result = 2 + (~-) (add 4 0);
    /* Same as */
    let result = 2 + ~- add 4 0;
    /* Same as */
    let result = 2 + - add 4 0;

    /* That same example but with ppx attributes on the add application */
    let result = 2 + (~-) (add 4 0 [@ppx]);
    /* Same as */
    let result = 2 + ~- add 4 0 [@ppx];
    /* Same as */
    let result = 2 + - add 4 0 [@ppx];


    /* Multiple nested prefixes */
    let result = 2 + - - - add 4 0;

    /* And with attributes */
    let result = 2 + - - - add 4 0 [@onAddApplication];


    /**
     * TODO: Move all of these test cases to attributes.re.
     */
    /* Attribute on the prefix application */
    let res = (- something blah blah) [@attr];
    /* Attribute on the regular function application, not prefix */
    let res = - something blah blah [@attr];
    let attrOnPrefix = (- 1) [@ppxOnPrefixApp];
    let attrOnPrefix = 5 + - 1 [@ppxOnPrefixApp];
    let result = arr.[0] [@ppxAttributeOnSugarGetter];


    /**
     * Unary plus/minus has lower precedence than prefix operators:
     * And unary plus has same precedence as unary minus.
     */
    let res = - !record;
    /* Should be parsed as: */
    let res = - (! record);
    /* Although that precedence ranking doesn't likely have any effect in that
     * case. */
    /**
     * And this
     */
    let res = - + callThisFunc ();
    /* should be parsed as: */
    let res = - + callThisFunc ();

    /**
     * And this
     */
    let res = ! (- callThisFunc ());
    /* Should be parsed (and should remain printed as: */
    let res = ! (- callThisFunc ());


    let res = !x [@onApplication];
    let res = !(x [@onX]);

    let res = !(x [@onX]);
    (something.contents = "newvalue") [@shouldBeRenderedOnEntireSetField];
    something.contents = "newvalue" [@shouldBeRenderedOnString];
  };
};
