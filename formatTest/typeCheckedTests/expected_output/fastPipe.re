let (|.) = (x, y) => x + y;

let a = 1;
let b = 2;
let c = 3;

/* parses as 10 < (a->b->c) */
let t1: bool = 10 < a->b->c;

type coordinate = {
  x: int,
  y: int,
};
let coord = {x: 1, y: 1};

/* parses as (coord.x)->a->b->c */
let t2: int = coord.x->a->b->c;

let (|.) = (x, y) => x || y;

let a = true;
let b = false;
let c = true;

/* parses as !(a->b->c) */
let t3: bool = ! a->b->c;
