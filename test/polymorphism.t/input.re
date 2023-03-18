/* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */

let run = fun () => {
  TestUtils.printSection("Polymorphism");
};

type myType('a) = list('a);
type myTwoParamType('a,'b) = ('a, 'b);

type myTupleType = (int, int);
type myPolymorphicTupleType('a) = ('a, 'a);

type extensible('a) = 'a
constraint 'a = [ | `Base(int)];

type intListTranformer = (list(int)) => list(int);

type x = list (int, string);


module HoldsAType = {
  type hasPrime('a,'b,'c) = Hashtbl.t (list('a),list('b));
};

type myType2 = (myTwoParamType(myType((int) => int), int)) => int;


                                     /* Confusing because => looks like part
                                     of the return type signature. */
let myFunc (a:(int)=>int, b:(int)=>int) :myType(int) =
    [a(20) + b(30)];

let myFunc (a:(int)=>int, b:(int)=>int) : ((myType(int)) => myType(int)) =
    fun(lst) => lst;


let certainlyRequiresWrapping:
  (option((Mod.handler(p,re), Mod.Types.handler)),
   option((Mod.touch(props,(props, state),resource), (list(Mod.t), list(Mod.t)))),
   list(Mod.update(props,(props, state),resource))) =>
   list(Mod.update(props,(props, state),resource)) = ();


/* Because of the confusion in the last two examples, I believe we should
   switch back to the `=` based syntax.

     let add a b = a + b;

     Pexp_function printing:

     Decide on either:

       let add Some (Hearts n) = n + n
         | add Some (Diamonds n) = 0
         | add Some (Spades n) = 0
         | add None = 0
         | _ = 0

     Or:
       let add = x => match x with
         | Some (Hearts n) => n + n
         | Some (Diamonds n) => 0
         | Some (Spades n) => 0
         | None => 0
         | _ => 0

       let add =
         | Some (Hearts n) => n + n
         | Some (Diamonds n) => 0
         | Some (Spades n) => 0
         | None => 0
         | _ => 0

     let myFunc = (a:int) (b:int) => a + b; */

/* Fringe features */

/*
  /* This parses, but doesn't type check */
  module TryExtendingType = {type t = Hello of string;};
  type TryExtendingType.t += LookANewExtension of string;
 */
"end";
