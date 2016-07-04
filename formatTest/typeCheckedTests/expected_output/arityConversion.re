
Some (1, 2, 3);

type bcd =
  | TupleConstructor (int, int)
  | MultiArgumentsConstructor int int;

let a = TupleConstructor (1, 2);

let b =
  MultiArgumentsConstructor 1 2 [@implicit_arity];

let module Test = {
  type a = | And (int, int) | Or (int, int);
};

let _ = Test.And (1, 2);

let _ = Test.Or (1, 2);

let _ = Some 1;
