module type FOO = {
  let foo: string;
};

module type FOO_WITH_X = {
  let foo: string;
  let x: int;
};

/* A package constraint on a lambda body must stay on the body: hoisted into
   a return annotation, `(module FOO)` re-parses as the start of an arrow
   type (issue #2925). */
let some_foo =
  some_foo_with_x
  |> Option.map((module Foo_with_x: FOO_WITH_X) =>
       (module Foo_with_x: FOO)
     );

/* Same bug when the lambda is not the last argument. */
let r = apply((module X: FOO_WITH_X) => ((module X): (module FOO)), 0);

/* Ordinary return types keep the return-annotation form. */
let s = apply((x: int): int => x + 1, 0);

/* Let bindings keep the return-annotation form: it re-parses correctly
   there. */
let f = (module X: FOO_WITH_X): (module FOO) => (module X);
