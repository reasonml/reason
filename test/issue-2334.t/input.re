let f = (x, p) => p(x);
let g = (a, b) => a + b;

let __x = 42;

Js.log(7->f(g(_, __x)));

7->f(g(_, foo))

