Format lambdas whose body has a first-class-module package constraint (issue #2925)
  $ refmt ./input.re > ./formatted.re
  $ cat ./formatted.re
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
         ((module Foo_with_x): (module FOO))
       );
  
  /* Same bug when the lambda is not the last argument. */
  let r =
    apply(
      (module X: FOO_WITH_X) =>
        ((module X): (module FOO)),
      0,
    );
  
  /* Ordinary return types keep the return-annotation form. */
  let s = apply((x: int): int => x + 1, 0);
  
  /* Let bindings keep the return-annotation form: it re-parses correctly
     there. */
  let f = (module X: FOO_WITH_X): (module FOO) =>
    (module X);

The formatted output must re-parse with the same meaning: the callbacks stay
functions instead of collapsing into a constraint with an arrow type
  $ refmt --parse re --print ml ./formatted.re
  module type FOO  = sig val foo : string end
  module type FOO_WITH_X  = sig val foo : string val x : int end
  let some_foo =
    some_foo_with_x |>
      (Option.map
         (fun ((module Foo_with_x)  : (module FOO_WITH_X)) -> ((module
            Foo_with_x) : (module FOO))))
  let r =
    apply
      (fun ((module X)  : (module FOO_WITH_X)) -> ((module X) : (module FOO)))
      0
  let s = apply (fun (x : int) -> (x + 1 : int)) 0
  let f ((module X)  : (module FOO_WITH_X)) = ((module X) : (module FOO))

Formatting must be idempotent
  $ refmt ./formatted.re | diff ./formatted.re -
