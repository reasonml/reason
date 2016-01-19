
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
type attributedIntsInTuple = (int [@onInt], float [@onFloat]) [@@onTopLevelTypeDef];

type myDataType 'x 'y = | MyDataType of 'x 'y;

type myType =
  myDataType
    ((option int) [@onOptionInt])
    (option float [@onOption]) [@onEntireType];


let thisInst : myType =
  MyDataType (Some 10) (Some 10.0) [@attOnEntireDatatype];

let thisInst : myType =
  MyDataType ((Some 10) [@onFirstParam]) (Some 10.0) [@attOnEntireDatatype];

let x = ("hello" [@onHello]);
let x = "hello" [@onHello];

let x = "hello" ^ ("goodbye" [@onGoodbye]);
let x = ("hello" [@onHello]) ^ "goodbye";
let x = "hello" [@onHello] ^ "goodbye";
let x = "hello" ^ "goodbye" [@onGoodbye];
let x = ("hello" ^ "goodbye") [@onEverything];

let x = 10 + (20 [@on20]);
let x = 10 + 20 [@on20];
let x = 10 [@on10] + 20;
let x = (10 [@on10]) + 20;
let x = (10 + 20) [@attrEverything];

let x = 10 - (20 [@on20]);
let x = 10 - 20 [@on20];
let x = 10 [@on10] - 20;
let x = (10 [@on10]) - 20;
let x = (10 - 20) [@attrEntireEverything];

let x = true && (false [@onFalse]);
let x = true && false [@onFalse];
let x = true [@onTrue] && false;
let x = (true [@onTrue]) && false;
let x = (true && false) [@attrEverything];


/* now make sure to try with variants (tagged and `) */

/**
 * How attribute parsings respond to other syntactic constructs.
 */
let add a => a [@onRet];
let add = fun a => a [@onRet];
let add = (fun a => a) [@onEntireFunction];

let res = if true false else false [@onFalse];
let res = (if true false else false) [@onEntireIf];


let add a b => (a [@onA] + b) [@onEverything];
let add a b => (a [@onA] + (b [@onB])) [@onEverything];
let add = fun a b => a + b [@onEverything];

let both = (fun a => a) [@onEntireFunction];
let both a b => (a [@onA] && b) [@onEverything];
let both a b => a [@onA] && (b [@onB]) [@onB];
let both = fun a b => (a && b) [@onEverything];

let thisVal = 10;
let x = 20 + - add thisVal thisVal [@onFunctionCall];
let x = (20 + - add thisVal thisVal) [@onEverything];
let x = - add thisVal thisVal [@onFunctionCall];
let x = (- add thisVal thisVal) [@onEverything];


let bothTrue x y => {contents: x && y};
let something = !(bothTrue true true) [@onEverythingToRightOfEquals];

let res = add 2 4 [@appliesToEntireFunctionApplication];
add 2 4 [@appliesToEntireFunctionApplication];


let myObj = {
  method p () => {
    method z () => 10
  };
};

let result = (myObj#p () [@attOnFirstSend])#z () [@onSecondSend];

type recordFunctions = {
  p: unit => recordFunctions [@onUnit],
  q: (unit => unit) [@onArrow]
} [@@onRecordFunctions]
and unusedType = unit [@@onUnusedType];
let rec myRecord = {
  p: fun () => myRecord,
  q: fun () => ()
} [@@onMyRecord]
and unused = () [@@onUnused];
let result = (myRecord.p() [@attOnFirstSend]).q() [@onSecondSend];

type variantType =
   | Foo of int [@onInt]
   | Bar of (int [@onInt])
   | Baz [@@onVariantType];

type gadtType 'x =
   | Foo of int : gadtType int [@onFirstRow]
   | Bar of (int [@onInt]) : gadtType unit [@onSecondRow]
   | Baz: gadtType (unit [@onUnit]) [@onThirdRow] [@@onVariantType];

[@@@floatingTopLevelStructureItem hello];
print_string "hello" [@@itemAttributeOnEval];

let firstBinding = "first" [@@itemAttrOnFirst]
and secondBinding = "second" [@@itemAttrOnSecond];

/**
 * Let bindings.
 * ----------------------
 */
let showLets () => {
  let tmp = 20;
  {
    let tmpTmp = tmp + tmp;
    tmpTmp + tmpTmp;
  } [@onFinalLet]
} [@onOuterLet];


/**
 * Classes:
 * ------------
 */
/**
 * In curried sugar, the class_expr attribute will apply to the return.
 */
class boxA 'a (init: 'a) => {
  method pr => init + init + init;
} [@onReturnClassExpr] [@@moduleItemAttribute];

/**
 * In non-curried sugar, the class_expr still sticks to "the simple thing".
 */
class boxB 'a =
  fun (init: 'a) => {
    method pr => init + init + init;
  } [@stillOnTheReturnBecauseItsSimple];

/* To be able to put an attribute on just the return in that case, use
 * parens. */
class boxC 'a = (
  fun (init: 'a) => (
    {
      method pr => init + init + init;
    } [@onReturnClassExpr]
  )
) [@onEntireFunction]
  [@@onBoxC x ; y]
  ;

class tupleClass 'a 'b (init: ('a, 'b)) => {
  let one = 10   [@exprAttr ten;];
  let two = 20   [@exprAttr twenty;]
  and three = 30 [@exprAttr twenty;];
  method pr => one + two + three [@@pr prMember;];
} [@@moduleItemAttribute onTheTupleClassItem;];

class type addablePointClassType = {
  method x: int;
  method y: int;
  method add: addablePointClassType => addablePointClassType => int;
}
[@@structureItem]
and anotherClassType = {
  method foo: int;
  method bar: int;
}
[@@structureItem];


let module NestedModule = {
  [@@@floatingNestedStructureItem hello];
};
module type HasAttrs = {
  type t = int [@@onTypeDef];
  [@@@floatingNestedSigItem hello];
  class type foo = {method foo: int; method bar: int;}
  [@@sigItem];
  class fooBar: int => new foo
  [@@sigItem];
}
[@@structureItem];
