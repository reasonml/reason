ES6 arrows with return-type annotations in argument position (reasonml/reason#2925)

Type-check the source as written

  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl input.re

Format it

  $ refmt ./input.re > ./formatted.re
  $ cat ./formatted.re
  /* Regression test for reasonml/reason#2925.
   *
   * An ES6 arrow with a return-type annotation in argument position, e.g.
   * `f((x: int): int => x)`, is ambiguous when the body is also valid *type*
   * syntax: the same tokens can be read as the constrained expression
   * `f((x: int) : (int => x))`. The parser must prefer the arrow-function
   * reading (as it already does in let-binding position); the expression
   * reading is still expressible by parenthesizing the type:
   * `f((x: int): (int => x))` never forks in the first place, and
   * `(e: ((a, b) => t))` disambiguates constraints against arrow types.
   *
   * This also matters for the printer: `(module M: SIG) => (module M: SUB)`
   * is printed with the body's constraint hoisted into return position,
   * `(module M: SIG): (module SUB) => (module M)`, which must re-parse as the
   * same function. */
  module type FOO = {
    let foo: string;
  };
  
  module type FOO_WITH_X = {
    let foo: string;
    let x: int;
  };
  
  /* Narrowing a first-class module to a smaller signature (the report's
     example): the printer hoists the body constraint into return position. */
  let some_foo_with_x: option(module FOO_WITH_X) =
    Some(
      (module
       {
         let foo = "foo";
         let x = 1;
       }),
    );
  
  let some_foo =
    some_foo_with_x
    |> Option.map(
         (module Foo_with_x: FOO_WITH_X): (module FOO) =>
         (module Foo_with_x)
       );
  
  /* Hand-written return annotation, module-free, body is type-shaped (a plain
     ident): used to mis-parse as `(n: int) : (int => n)`. */
  let idents = l =>
    List.map((n: int): int => n, l);
  
  /* Hand-written return annotation whose return type is a package type, body
     projects a module out of the parameter. */
  module type ID = {
    let id: int;
  };
  module type VIEW = {
    module Id: ID;
    let name: string;
  };
  
  let apply = (h, x: (module VIEW)) => h(x);
  
  let project = (x: (module VIEW)) =>
    apply(
      (module T: VIEW): (module ID) =>
        (module T.Id),
      x,
    );

Formatting must preserve well-typedness (the formatted output re-parses as
the same program)

  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re

Formatting is idempotent

  $ refmt ./formatted.re > ./formatted_back.re
  $ diff formatted.re formatted_back.re
