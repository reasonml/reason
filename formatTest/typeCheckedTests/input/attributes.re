/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
/**
 * Generally, dangling attributes [@..] apply to everything to the left of it,
 * up until a comma, equals asignment, arrow, bar, or infix symbol (+/-) or
 * prefix.
 *
 * This has a nice side effect when printing the terms:
 * If a node has attributes attached to it,
 */

[@@@ocaml.text "Floating comment text should be removed"];

/**
 * Core language features:
 * ----------------------
 */

[@@@ocaml.doc "Floating doc text should be removed"];

type itemText = int [@@itemAttributeOnTypeDef] [@@ocaml.text "removed text on type def"];
type nodeText = int [@ocaml.text "removed text on item"];
type nodeAndItemText =
  int [@ocaml.text "removed text on item"]
  [@@itemAttributeOnTypeDef]
  [@@ocaml.text "removed text on type def"];

type itemDoc = int [@@itemAttributeOnTypeDef] [@@ocaml.doc "removed doc on type def"];
type nodeDoc = int [@ocaml.text "removed text on item"] [@@itemAttributeOnTypeDef];
type nodeAndItemDoc =
  int [@ocaml.text "removed text on item"]
  [@@itemAttributeOnTypeDef] [@@ocaml.doc "removed doc on type def"];

type x = int [@@itemAttributeOnTypeDef];
type attributedInt = int [@onTopLevelTypeDef];
type attributedIntsInTuple = (int [@onInt], float [@onFloat]) [@@onTopLevelTypeDef];

type myDataType 'x 'y = | MyDataType 'x 'y;

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
let add = fun a b => a + b [@onB];

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
let something = !(bothTrue true true [@onlyOnArgumentToBang]);

let res = add 2 4 [@appliesToEntireFunctionApplication];
add 2 4 [@appliesToEntireFunctionApplication];


let myObj = {
  pub p () => {
    pub z () => 10
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
   | Foo int [@onInt]
   | Bar (int [@onInt])
   | Baz [@@onVariantType];

type gadtType 'x =
   | Foo int : gadtType int [@onFirstRow]
   | Bar (int [@onInt]) : gadtType unit [@onSecondRow]
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
  [@@@ocaml.text "Floating comment text should be removed"];
  [@@@ocaml.doc "Floating comment text should be removed"];
  pub pr => init + init + init;
} [@onReturnClassExpr] [@@moduleItemAttribute];

/**
 * In non-curried sugar, the class_expr still sticks to "the simple thing".
 */
class boxB 'a =
  fun (init: 'a) => {
    pub pr => init + init + init;
  } [@stillOnTheReturnBecauseItsSimple];

/* To be able to put an attribute on just the return in that case, use
 * parens. */
class boxC 'a = (
  fun (init: 'a) => (
    {
      pub pr => init + init + init;
    } [@onReturnClassExpr]
  )
) [@onEntireFunction]
  [@@onBoxC x ; y]
  ;

class tupleClass 'a 'b (init: ('a, 'b)) => {
  let one = 10   [@exprAttr ten;];
  let two = 20   [@exprAttr twenty;]
  and three = 30 [@exprAttr twenty;];
  pub pr => one + two + three [@@pr prMember;];
} [@@moduleItemAttribute onTheTupleClassItem;];

class type addablePointClassType = {
  [@@@ocaml.text "Floating comment text should be removed"];
  [@@@ocaml.doc "Floating comment text should be removed"];
  pub x: int;
  pub y: int;
  pub add: addablePointClassType => addablePointClassType => int;
}
[@@structureItem]
and anotherClassType = {
  pub foo: int;
  pub bar: int;
}
[@@structureItem];

class type _y = { pub height : int [@@bs.set] };

class type _z = { pub height : int }[@@bs];

module NestedModule = {
  [@@@floatingNestedStructureItem hello];
};
module type HasAttrs = {
  type t = int [@@onTypeDef];
  [@@@floatingNestedSigItem hello];
  class type foo = {pub foo: int; pub bar: int;}
  [@@sigItem];
  class fooBar: int => new foo
  [@@sigItem];
  [@@@ocaml.text "Floating comment text should be removed"];
  [@@@ocaml.doc "Floating comment text should be removed"];
}
[@@structureItem];

type s = S string;

let S (str [@onStr]) = S ("hello" [@onHello]);
let (S str) [@onConstruction] = (S "hello") [@onConstruction];

type xy = | X string
          | Y string;

let myFun = fun (X hello [@onConstruction] | Y hello [@onConstruction]) => hello;
let myFun = fun (X (hello [@onHello]) | Y (hello [@onHello])) => hello;

/* Another bug: Cannot have an attribute on or pattern
let myFun = fun ((X hello | Y hello) [@onOrPattern]) => hello;
*/

/* Bucklescript FFI item attributes */

external imul : int => int => int = "Math.imul" [@@bs.val];

let module Js = {
  type t 'a;
};

type classAttributesOnKeys = {
  .
  key1 [@bs.set] : string,

  /* The follow two are the same */
  key2 [@bs.get {null}] : Js.t int [@onType2],
  key3 [@bs.get {null}] : ((Js.t int) [@onType2]),

  key4 : Js.t (int [@justOnInt])
};

type attr = ..;

type attr +=
  | Str [@tag1] [@tag2]
  | Float [@tag3]
[@@block];

type reconciler 'props = ..;

type reconciler 'props +=
 | Foo int : reconciler int [@onFirstRow]
 | Bar (int [@onInt]) : reconciler unit [@onSecondRow]
 | Baz: reconciler (unit [@onUnit]) [@onThirdRow] [@@onVariantType];

type element;

type reactElement;

type reactClass;

/* "react-dom" shouldn't spread the attribute over multiple lines */
external render : reactElement => element => unit = "render" [@@bs.val] [@@bs.module "react-dom"];

external f : int => int = "f" [@@bs.module "f"];

external createCompositeElementInternalHack : reactClass =>
                                              Js.t {.. reasonProps : 'props} =>
                                              array reactElement =>
                                              reactElement = "createElement" [@@bs.val] [@@bs.module"react"] [@@bs.splice];

external add_nat: int => int => int = "add_nat_bytecode" "add_nat_native";
