/*
   0. 'hashtags' for polymorphic variants.
   4.1 Object type syntax. (Done partially, need subtyping)
   4.1 Object subtype sugar for 'constraint' etc.
   4.2 Where do type constraints play into the mandatory | BAR function matches?
       Function return type anotations are already pretty messed up:

        Allowed:
        utop # let blah x :(int) = 10;;
        val blah : 'a => int = <fun>
        Not Allowed:
        utop # let blah :(int) = function Red _ => 10 | Black => 20;;
        Error: This expression should not be a function, the expected type is
   5. Object pattern matching
   8. Curried pattern match on last argument:
       Note: This might be confusing and maybe should not be done.
       let blah a
        | Red _ => 1
        | Black => 0;
    9. Do not require parens around let matches
        let (|Red x |Black x) = y;
        /* And assuming curried form ever works */
        let myFun firstArg (|Red x |Black x) => x + firstArg;
        /* Some precedence grouping would be required */
        let myFun |`FirstThing y (|Red x |Black x) => y + x;
    11. Possible to implement OPA style (matchable) records?
         let add
           | {x, y, z} => x + y + z
           | {a, b} => x + y;
        let addTwoPoints {x, y} => x + y

    12. Require leading bar *everywhere*.

    Unify objects and poly-variants, which could achieve OPA style syntax.

    /* Maybe this could even be sugar for matching on poly variants? */
    /* Actually this is probably a bad idea */
    let myPoint =
      |x 10
      |y 20;
    let myPoint = fun msg => match msg with |`X => 10 |`Y => 20;

    let myMutualExclusivelyExtendedPoint = fun msg =>
      |`Z => 40
      |_ => myPoint msg;

    /* Now the inferred value of `myMutualExc..` should be `[`X `Y `Z]` */

    Syntacticaly, we could write this like:

    let myPoint = {x: 10, y: 20};
    let myMutualExclusivelyExtendedPoint = {...myPoint, z: 40};

    Not sure how we could accomplish the a/b|x/y/z pattern matching.

   16. Only have three levels of precedence:
   - Curried application precedence
   - Sum
   - Multiply
   - Everything else requires parenthesis or is considered a parsing error.

   17. Required leading bar allows elimination of `with` in `match`, and
   possibly even elimiation of `match`.

   let result = match x
     | Red => 10
     | Black => 20;

   let result = x
     | Red => 10
     | Black => 20;

   /* Would require some disambiguiating - bar would have lowest precedence */
   let myFunc x => resultOfCallingSomeFuncWithX x | Red => 20;

   18. Make `let rec` the default for let bindings.
      - Should not have mutually recursive values by default (only functions).
      - Should probably not make (mutually recursive) the default because:
      - mutual recursion still doesn't require the semicolon, but should
        require "and let". (OR, the abscense of semicolon can imply mutual
        recursion! - doubt that is conflict-free though)

      a. It's so common do want to do
          let result = ..;
          let result = ...;

        Having recurive definitions by default won't make that more difficult
        but having *mutally* recursive definitions by default would (`and`).

      b. If semis implied mutual recursion, then things that *cannot* be
         mutually recursive would appear to be mutually recursive (local module
         definitions).

     So we have:

        let implicitlyRecursive x = 200 + implicitlyRecursive (x-1);
        let mutRecurseOne x = mutRecurseTwo x and
        let mutRecurseTwo y = mutRecurseOne y;



      - Type definitions on the other hand probably could be made mutually
        recursive by default.
        - Often, types aren't rebound (as results were above).

      This may be difficult because one let rec makes an "and" also recursive.
      If let rec was implicit (and default):
      let someFunc x = ... and
      let myVal = 2::myVal;

      The value is now recursive - but hey, *you* made it recursive. The
      concern is that a "rec" wasn't needed to open up a recursive value after
      the "and" - which is different than.

      let myVal = [0]; /* This one is ignored! */
      let rec recursiveVal p = 2::p and
      let myVal = 2::myVal;

      AA: The real issue is that a recursive binding in OCaml *requires* that
      following *mutually recursive* bindings are also internally recursive.
      This is unfortunate -but possibly has some reason?

      Hold the phone - actually, it doesn't seem possible to have *mutually*
      recursive functions without each being individually recursive.
      let one x = two x
      and two x = one x;;

      So what's the problem with `AA` above?
      The issue is that if you have "rec" be the default for functions and not
      for values, then for two mutually recursive things using `and`, the binding
      you place *first* influences the recuriveness of the other one.

      /* Suppose functions are default let rec */
      let someRecursiveFunc x => ... and   /* This is recursive */
      let someValue = ...;                 /* This is recursive */

      let someValue = ... and              /* Now this is not recursive */
      let someRecursiveFunc x => ... and   /* And this is not recursive */

      /* We could do some interesting parsing strategy where */

      That's probably not resolveable but to implement let rec as default, we
      need to know if we parsed a function or not (which could be burried very deep
      inside Pexp_constraints). So we can carve out two expression paths in the
      compiler.  One that is capable of parsing functions, and another that is not.
      Then We must do something similar at pretty printing stage (search the
      expression deeply to see if it is a functional expression)

      In fact, it would be *much* better if the parser/pretty printer
      acknowledged that mutually recursive functions inherit the individual
      recursiveness of the first binding. That may open the doors to be able
      to support individual recursion by default.

      Similar to this:
      -----------------------------------------------------------------

        let rec myRecValue = ... and
        let rec myRecFunction x => ...;

        let rec myRecFunction x => ... and
        let rec myRecValue = ... and

        /* Does Not Parse! */
        /* Exposes subtle unintensional recursive values (cycles!) */
        let rec myRecFunction x => ... and
        let myRecValue = ... and


      But with rec by default for functions, and non-rec by default for
      non-functions.
      -----------------------------------------------------------------

        /* Following can occur but is not very useful for most programming
        scenerios. The only difference between this and two let/ins, is that
        the second binding value is not evaluated with the first binding in
        the environment. */
        let myRecValue = ... and   /* Vals implicitly non-recursive */
        let nrc myFunction x =>...;/* Funcs must admit non-recursiveness*/

        /* The following is an incredibly rare situation! */
        /* Seldom do you want to make mutually recursive values *and*
        functions. */
        let myRecFunction x => ... and  /* Funcs implicitly recursive */
        let rec myRecValue = ... and    /* Values must admit recursion! */

        /* Does Not Parse! Likely an unintentional recursive value */
        let myRecFunction x => ... and   /* Implicitly recursive */
        let myRecValue = ... and         /* NON-recursive by default*/


      Having to specify "let" after each "and" would require that we also
      specify "type" after each mutually recursive type.

      Alternative proposal: Everything is exactly as it is in OCaml.
      But a lone function is recursive, a lone value is nonrecursive.
      Several "simultaneous" values must be specified with a "let" or
      "let rec" that indicates how the bindings will be (both mutually
      and self) recursive.

      let myRecFun x => myRecFun x;

      let myNonRecVal = [];

      let myNonRecFun x => blah and
      let myNonRecVal = [];

      let rec myRecFun x => myRecFun x and
      let myNonRecVal = [];

      Amendment: For "simultaneous" bindings, you could require *either*
      nonrec or rec as a leading keyword that always overrides any convention
      at the individual binding level. The following most clearly explains
      what is happening: mutual recursiveness and individual recursiveness
      are inseparable.

      nonrec let myNonRecFun x => blah andnonrec
      nonrec let myNonRecVal = [];

      rec let myRecFun x => myRecFun x andrec
      rec let myNonRecVal = [];

      A more conservative (actually practical) solution is to only include the
      description of recursiveness at the opening binding. This way, it's clear
      that any default recursiveness is overriden.

      norec
        let myNonRecFun x => blah and
        let myNonRecVal = [];

      rec
        let myRecFun x => myRecFun x and
        let myNonRecVal = [];
      -----------------------------------------------------------------

      TODO: Confirm that the model of implicitly inheriting the first
      binding's internal recursiveness is actually accurate.

      TODO: Confirm how this behaves when the first binding is a recursive
      function and the second binding is a record with functions inside of
      it. Do those functions also inherit the recursiveness of the value that
      wraps them (the record)? Does that even matter?

        Now change the [let id] to [let rec id] and see that the
        recursiveness of the first binding propagates to the next because of
        the "mutual recursion" as introduced by [and].

        The recursivenes of a single binding (in this case the second) no
        only applies to functions, but also values (which allows creation of
        cycles), and functions inside of those values that reference those
        values.


    20. Allows annotating return types of lambdas. To make compatible with
        OCaml code, parse (CORRECTION: OCaml's syntax doesn't allow annotating
        the return value of lambdas so striving for parity isn't really that important.)

          let myFun = fun (x:int):int => x + 1;

        into

          let myFun = fun (x:int) => (x + 1: int);

        and support both forms in the parser - but always printing the former
        in the pretty printer.

        WARNING: This will make it even more difficult to implement
        colon-based named args.

           // is it a named arg or an annotated return type?
           let myFun = fun nonNamedArg :returnType => nonNamedArg + nonNamedArg;

           // This isn't just a problem with lambdas - even the sugar
           // syntax for curried args has the same problem.
           let myFun nonNamedArg :returnType => ...

           // The only known solution to date is to use :: for types,
           // or to require parens around arguments (gross).

        Might want to defer this one because of #21 below.

    21. Apply the same to functors

        let module MyModule (A:Sig) (B:Sig): DSig => {};

        would be parsed into:

        let module MyModule (A:Sig) (B:Sig) => ({}:DSig);

     22. Applicative functors should use curried syntax (See Lapply)

        let l : Compose(List)(Maybe)(Char).t = [Some 'a'];

        let l : (Compose List  Maybe  Char).t = [Some 'a'];

     23. Fix parsing of Ppat_constraint/Pexp_constraint (See
         pprintsugarast.ml).

     24. Or patterns in parens must have a leading bar which is unfortunate,
         but the pretty printer doesn't print that leading bar.


*/
/* Solve nested pattern matching pain:

  Allow grouping of *only* the cases which doesn't cause additional
  grouping:

  In OCaml, you can't just format code like this:

    let x = match p with
      | P1 -> match q with
        | Q1 -> 10
      | P2 -> 20  /*ERROR! */

  You have to format like this: (So much extra indentation is required if you
  want nicely ballancing parens!

    let x = (
      match p with
        | P1 -> (
          match q with
            | Q1 -> 10
        )
        | P2 -> 20
    )

  If you just group things to the right of arrows, it looks ugly (against the
  bars) (and still required indentation of the inner)

    let x = match p with
      | P1 -> (
        match q with
          | Q1 -> 10
      )| P2 -> 20

  Proposal One:  Allow/require grouping of *all* case statements using parens
  or {}. This does require one extra line per match statement and no extra
  indenting - which is better than the current requirement of two extra lines
  per match statement.

  Though, with the current OCaml, there are zero extra lines required when no
  nested matches - but OCaml matches always bite you when you go to add
  another level of nesting.

    let x = match p with {
      | P1 -> match q with {
        | Q1 -> 10
      }
      | P2 -> 20
    }

  Again, in question mark form:
    let x = p ?? {
      P1 ? q ?? {
        Q1 : 10
      }
      : P2 ? 20
    }

*/
/* REVERT
   2. After #1, allow this to parse

      let Black x | Red x |Green x = v;

   4. Consider going back to [let x a b = c;] instead of [let x a b => c;]

 */
/* Debunked ideas:


   14. Consider removing the requirement for a final semicolon for module
   structures. (Actually, this might not be such a great idea since it's
   inconsistent with how other SEMI delimited sequences work.).

    19. Consider capital letters for type variables. (Actually, no, this would
    conflict with module/functor syntax unification).

    type myType T X = Hashtbl.t T X;
*/
