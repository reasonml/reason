Some (1, 2, 3);

type bcd =
  | TupleConstructor of (int, int)
  | MultiArgumentsConstructor of int int;

let a = TupleConstructor (1, 2);

let b =
  MultiArgumentsConstructor 1 2 [@implicit_arity];

let module Test = {
  type a = | And of (int, int) | Or of (int, int);
};

Test.And (1, 2);

Test.Or (1, 2);

Some 1;
