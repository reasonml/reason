[@reason.version 3.7];
f(.);

[@attr]
f(.);

f(.);

f(. a, b, c);

[@attr]
f(. a, b, c);

f(. a);

f(. (1, 2));

f([@bs] (1, 2));

f(. [@bs] (1, 2));

f(. (1, 2), (3, 4));

f(. [@bs] (1, 2), [@bs] (3, 4));

(f(. [@bs] (1, 2)))(. [@bs] (3, 4));

f(. "string");

f(. "string", "2string");

(f(. "string"))(. "2string");

(f(. [@bs] "string"))(. [@bs] "2string");

f(. 1);

f(. [@bs] 1);

f(. {
  a: "supersupersupersupersupersuperlong",
  b: "supersupersupersupersupersuperlong",
});

let f = (. a, b) => a + b;

let f = [@attr] ((. a, b) => a + b);

let f = (. a, b) => a + b;

let f = ("hello", (. b, c) => b + c);

let f = ("hello", [@attr] ((. b, c) => b + c));

let f = ("hello", (. b, c) => b + c);

let obj: tesla = {
  pub drive = (. speed, safe) => (speed, safe);
  pub drive2 =
    [@attr] ((. speed, safe) => (speed, safe));
  pub park = (.) => ();
  pub park2 = [@attr] ((.) => ())
};

type f = (. int, int) => int;

type f = [@attr] ((. int, int) => int);

type f = (. int, int) => int;

type z = (. unit) => unit;

type z = [@attr] ((. unit) => unit);

type z = (. unit) => unit;

type tesla = {. drive: (. int, int) => int};

class type _rect =
  [@bs]
  {
    [@bs.set]
    pub height: int;
    [@bs.set]
    pub width: int;
    pub draw: unit => unit;
  };

class type _rect =
  [@bs]
  {
    [@bs.set]
    pub height: int;
    [@bs.set]
    pub width: int;
    pub draw: unit => unit;
  };

funWithCb("text", (.) => doStuff());

funWithCb("text", (. test) => doStuff());

funWithCb("text", [@attr] (. arg) => doStuff());

test(~desc="my test", (.) => {
  let x = a + b;
  let y = z + c;
  x + y;
});

test(~desc="my test", [@attr] (. a, b, c) => {
  let x = a + b;
  let y = z + c;
  x + y;
});

Thing.map(
  ~a=?foo,
  ~b=?bar,
  ~c=?baz,
  ~d=?foo2,
  ~e=?bakjlksjdf,
  ~f=?okokokok,
  ~cb=[@attr] (. abc, z) => {
    let x = 1;
    MyModuleBlah.toList(x, argument);
  },
);

type f = int => (. int) => unit;

type f = int => (. int) => unit;

add(. 2);

(add(. 2))(. 3);

add(. 2, [@bs] 3);

((add(. 2, 3, 4))(. 5, 6, 7))(. 8, 9, 10);

type timerId;

[@bs.val]
external setTimeout:
  ((. unit) => unit, int) => timerId =
  "setTimeout";

let id =
  setTimeout((.) => Js.log("hello"), 1000);

let id =
  setTimeout(1000, (.) => Js.log("hello"));

foo([@bs] {val a = 1});

foo(. [@bs] {val a = 1});

foo(. [@bs] {val a = 1});

foo([@attr1] [@bs] [@attr2] {val a = 1});

add([@attr] [@bs] [@attr] 1);

add(. [@attr] [@bs] [@attr] 1);

add(. [@attr] [@bs] [@attr] 1);

let a = foo(. foo(. 3));

let a = foo(. foo(. 3));

(add(1, 2))(. 3, 4);

(add(1))(. 2, 3, 4);

(add(1, 2, 3))(. 4);

let run =
    (
      ~dry as [@attr] dry: bool=false,
      ~mMap: string=?,
      logger,
    ) => {};

f(. _ => 1);

f(. a, _ => 1);

f(. a, (. _) => 1);

let u =
  (. ~f: t, a, b) => {
    f(. ~x=a, ~y=b)->Js.log;
    f(. ~y=b, ~x=a)->Js.log;
  };
