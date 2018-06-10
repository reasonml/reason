/* tests adapted from https://github.com/BuckleScript/bucklescript/blob/master/jscomp/test/pipe_syntax.ml */
include (
  {
    let t0 = (x, f) => x |. f |. f |. f;
    let t1 = (x, f) => x |. f;
    let t2 = (x, f, g) => x |. f |. g(x, x) |. f(x);
    let t3 = (x, f) => x |. f(~h=1, ~x=2);
  }: {
    let t0: ('a, 'a => 'a) => 'a;
    let t1: ('a, 'a => 'b) => 'b;
    let t2: ('a, ('a, 'a) => 'b, ('a => 'b, 'a, 'a) => 'a) => 'b;
    let t3: ('a, ('a, ~h: int, ~x: int) => 'b) => 'b;
  }
);

/* let f = (a, b, c) => a |. (b, c); */

/* let f1 = (a, b, c, d) => a(b) |. (c, d); */

/* let f2 = (a, b, c, d) => { */
  /* let (u, v) = a(b) |. (c, d); */
  /* u + v; */
/* }; */

/* let f3 = (a, b, c, d, e) => */
  /* a(b) |. (c(d), d(1, 2), e) |. (((u, v, h)) => u + v + h); */

/* let f4 = (a, b, c) => a |. (b(c), b(c)); */

/* let f5 = (a, b, c, d) => { */
  /* let (v0, v1, v2) = a |. (b(c, c), b(c, c), b(d, d)); */
  /* v0 + v1 + v2; */
/* }; */

let f6 = a => a |. Some;

include (
  {
    let t0 = (x, f) => x -> f -> f -> f;
    let t1 = (x, f) => x -> f;
    let t2 = (x, f, g) => x -> f -> g(x, x) -> f(x);
    let t3 = (x, f) => x -> f(~h=1, ~x=2);
  }: {
    let t0: ('a, 'a => 'a) => 'a;
    let t1: ('a, 'a => 'b) => 'b;
    let t2: ('a, ('a, 'a) => 'b, ('a => 'b, 'a, 'a) => 'a) => 'b;
    let t3: ('a, ('a, ~h: int, ~x: int) => 'b) => 'b;
  }
);

/* let f = (a, b, c) => a -> (b, c); */

/* let f1 = (a, b, c, d) => a(b) -> (c, d); */

/* let f2 = (a, b, c, d) => { */
/* let (u, v) = a(b) -> (c, d); */
/* u + v; */
/* }; */

/* let f3 = (a, b, c, d, e) => */
/* a(b) -> (c(d), d(1, 2), e) -> (((u, v, h)) => u + v + h); */

/* let f4 = (a, b, c) => a -> (b(c), b(c)); */

/* let f5 = (a, b, c, d) => { */
/* let (v0, v1, v2) = a -> (b(c, c), b(c, c), b(d, d)); */
/* v0 + v1 + v2; */
/* }; */

let f6 = a => a -> Some;

/* let f7 = a => a |. (Some, Some, Some); */

