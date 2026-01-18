  $ refmt ./input.ml | tee formatted.re
  let f = (x): int => x;
  let f = (x): Foo.bar => x;
  let f = (x, y): int => x + y;
  let f = (x): (int => int) => x;

  $ refmt ./formatted.re | tee formatted_back.re
  let f = (x): int => x;
  let f = (x): Foo.bar => x;
  let f = (x, y): int => x + y;
  let f = (x): (int => int) => x;

  $ diff formatted.re formatted_back.re
