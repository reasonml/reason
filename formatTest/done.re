
/* TODO:
   0. @mentions for named arguments.

      var printTweet
         | #BigTweet => 'Big'
         | #SmallTweet => 'small'
      var printName @first='none' @last='none' => first ^ last;
      // But see note about concessions (maybe we can just use colons for
      // anything named) that's not a let binding.

   1. [DONE] Fix semis.
   2. [DONE] Unify function matching syntax.
   3. [DONE] Make all curried forms have arrows.
   4. [DONE] Object syntax.
   6. Named args cleanup.
     let myFunction @name='John Default' @age=23 => ...;
     - Implicit optionals determined by having `=`.

   7: [DONE] All of these are related and should be done in one shot:
   7. [DONE] Ensure that tuples are always wrapped in () [this frees up syntactic real
   estate] for the remaining items.
   7. [DONE] Make tuple types look like tuples.
   7.1.[DONE]  Fix tuple type precedence like FSharp has:
      let myTuple = (10: int, 12: int);
   8. [DONE] Once 7 is done: Records and objects should just use commas (7 is likely
   required to free up real estate)
    [DONE] 13. Further unify functor syntax to avoid requiring parens around functor
    application. (Parens will always be needed around arguments in functor
    definitions since arguments to functors must be annotated).

     let module MyFunctor (A:MySig) => {let x = A.x};
     let module MyModule = MyFunctor A;

     let module MyFunctor (A:MySig) (B:MySig) => {let x = A.x + B.x;};
     let module MyModule = MyFunctor A B;  /* Or you could curry them */

     /* Non-curried form: Notice position of arrow */
     let module MyFunctor = functor (A:MySig) => {let x = A.x;};


     let module Typeahead = React.Create {
        type props = {initialCount: int};
        type state = {count: 0};
        let getInitialState props => {
          count: 10
        };
        let render {props, state} => {
          <div>
            <span><BigBox></span>
          </div>;
        };
     };

   13.1 [DONE] Unify return type anntations for functors and functions:
   - In OCaml and SugarML, you cannot annotate the return value of a fun ->
     binding.
     So in SugarML, you cannot annotate the return value of a functor =>
     binding (truthfully, it was just too hard to avoid parser conflicts).
    - In OCaml and SugarML, you *can* annotate non-fun bindings

      let x y z :returnVal => blah

      So in SugarML, you can annotate non-functor bindings return types.

      let module MyFunctor (A:B) (C:D) :E => Blah

   15. [DONE] Functor types should not require the "functor" word - it should be
   curried with an => to deliniate functor arguments just like regular function
   annotations.

*/

/*
   Features to revert:
   1. [DONE] *SUPER* sugary let x | Red => "red";
      *Even though* it ruins the narrative for "= fun is always optional", it
       wasn't true in the first place! For example:

          let curried a b = fun | Red => "red".

   3. [DONE] No explicit_arity/unified syntax for polymorphic variants. There are deep
      assumptions about polymorphic variants *not* having multiple arguments,
     and only actually supporting a single argument. At least with this new syntax
     we can force that fact to the surface, and *require* tuple syntax.

   5. [DONE] Remove "fn" keyword and use "fun" exclusively. It causes formatted
      code to align more consistently with "lets" etc.
 */


