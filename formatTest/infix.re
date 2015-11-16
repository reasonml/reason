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
let parseTree = a1 := a2 := b1 = b2 = (y != x != z);

let minParens = a1 := a2 := b1 = b2 = (y != x != z);

let formatted = a1 := a2 := b1 = b2 = (y != x != z);

/* !=...(left) same level =(left) is higher than :=(right) */
let parseTree = a1 := a2 := b1 = ((b2 = y) != x != z);

let minParens = a1 := a2 := b1 = ((b2 = y) != x != z);

let formatted = a1 := a2 := b1 = ((b2 = y) != x != z);

/* &...(left) is higher than &(right). &(right) is equal to &&(right) */
let parseTree = a1 && a2 && (b1 & b2 & y &|| x &|| z);

let minParens = a1 && a2 && (b1 & b2 & y &|| x &|| z);

let formatted = a1 && a2 && (b1 & b2 & y &|| x &|| z);

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

let comparison = (=);

/* Why would the following two cases have different grouping? */
let res = blah || DataConstructor 10 || DataConstructor 10 && 10;

let res = blah && DataConstructor 10 && DataConstructor 10 + 10;

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

/* The semicolon should be attached to someType */
let myFunc aaaa bbbb cccc dddd aaaa bbbb cccc dddd aaaa =>
  [blah aaaa bbbb cccc dddd aaaa bbbb cccc dddd aaaa, ...someType];
