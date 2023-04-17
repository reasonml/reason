type t = A { a : int } | B;

let f = fun
  | B => 0
  | A { a } => a;

type nonrec u('a) = Box('a);

type expr('a) =
  | Val {value: 'a} :expr('a)
  | Add {left: expr(int), right: expr(int)} :expr(int)
  | Is0 {test: expr(int)} :expr(bool)
  | If {pred: expr(bool), true_branch: expr('a), false_branch: expr('a)} :expr('a);

let rec eval: type a. (expr(a)) => a =
  fun(e) =>
    switch (e) {
    | Is0 {test} => eval(test) == 0
    | Val {value} => value
    | Add {left, right} => eval(left) + eval(right)
    | If {pred, true_branch, false_branch} =>
        if (eval(pred)) {
          eval(true_branch)
        } else {
          eval(false_branch)
        }
    };

type hlist =
  | [] : hlist;

let foo (type a, type b) = 5;
