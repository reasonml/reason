### TODO

Make tasks out of all of these:
---------------

Here is a list of the most important features needed to be completed.

 * [EASY] Make `REPL` `<enter>` behavior match the Chrome console.
 * [MED] There is currently a bug where functors do not obey function
   application syntax when in type annotation position - see
   `formatTest/modules.re`
 * [MED/HARD]: Automatically generate `ocamldebug` initialization scripts that
   have all of the source locations/build paths pre-loaded. So that we can use
   `ocamldebug` (which is like `gdb`) to step through the time travelling
   debugger.
 * Automatically generate byte code debugger script. For any `CommonML`
   project, after building, running the time-travelling debugger should be "one
   click". (Currently, `JavaScript` target debugging w/ sourcemaps is
   one-click and very easy, but we should also be able to debug with `gdb` or
   the byte code debugger just as easily).
 * Upgrade `JavaScript` compiler to take better advantage of in-browser
   debugging with source maps.
 * Integrate debugger into Vim, Sublime, and Nuclide (Integrated Vim
   time-travelling debugger WIP).
 * Migrating to the newest version of the AST to support many upcoming
   features.
 * Editor support for Nuclide and Sublime.
 * Make pretty printer comprehensive (supporting obscure language features)
 * Make pretty printer correctly distribute comments in all cases.
 * Greatly improve `CommonML` so that it supports parallel, recoverable builds
   (possibly replace with `Buck`).
 * `Buck` plugin to support both native binary and `JavaScript` targets.
 * Fix autocomplete by adding support for syntax extensions `-pp` to `merlin`.
 * Use the `Reason` pretty printer to print `REPL` output and compiler error
   messages. (Compiler message formatting WIP).
 * [IMPORTANT] Fix formatting of unary minus (parenthesis aren't preserved
   around places where they need to be) `add 2 (-x)`, although they are
   preserved around `add 2 (-3)`. Another solution is to use the newly freed up
   `~` symbol as unary minus which may never be confused for an infix
   subtraction operator.
 * Editor tooling to support "upgrading" the AST at break-neck speeds. Meaning,
   if the stock `Reason` syntax/grammar changes, editors should be able to
   handle it and automatically show code in the new syntax (this means creating
   a mode that literally stores the AST on disk instead of the text - which is
   actually very very close to what already happens).
 * [Difficult] Implement async syntax transform.
 * Test ppx attributes and confirm there is an upgrade path for people using
   common camlp4 syntax extensions (such as with/deriving).


TODO: First class module auto packing/unpacking.
  When eliminating `let module`, we will have to distinguish Constructor from
  Module. One proposal is to have constructors prefixed with @. If we, instead
  have modules prefixed with @, and even module literals prefixed with @, then
  we can unify modules and expressions almost automatically.



TODO: URGENT: Fix the fact that the following two have completely different meanings:
(This only occurs in function scope btw).

    let (module DisplayObjectInstance) = m;                   /* module Pattern unpacking */
    let module DisplayObjectInstance = struct end;            /* module Binding */

Automatic packing of first class modules might solve this, but there might be a
conflict. Since ths seems to be the only conflict, we might be able to special
case/hack this one.

    @DisplayObjectInstance = someValue;
    @DisplayObjectInstance = @{type t = int;};


Note, even if we did not unify let/let module, there is still an ambiguity when
trying to auto-pack/unpack.

    let myRegularFunction = fun a => {
      let x = 10;
      print_string("hello");
    };

Does this return a 1st class module or not?

There are tradeoffs with every approach to combining any number of:

- Unifying `let/let module`.
- Auto pack/unpacking of modules.




TODO:  URGENT: There is *one* edge case in the syntax where whitespace between
tokens matters. Since we have named arguments punning `~b: int` is very
different from `~b:int`, yet we print one as the other.


    let resAnnotated    = (named ~a:a ~b: int);
    let resAnnotated    = (named ~a:a ~b :int);

- We might use a totally different token for type annotations `::`, or a
  totally different token for named args `::` that eliminates the problem. We
  should either fix the pretty printer for the current choice of tokens,
  or move to having two separate tokens.

- TODO: Better named args syntax: I have settled on the following.
  - Types need to either be represented by : or ::.
  - We want to encourage using named arguments, probably even more than
    encouraging type annotations tbph.
  - Type definitions should look like their types.
  - When trying to unify : for type annotations and named args, punning
    introduces some challenges.

    Is this

        (named a:b :typeOrValue);
        (named a: b:typeOrValue);


  - [Almost rules out : for both]:
    We'd be okay with giving up punning in order to have `:` represent both
    named args and type annotations. In fact it does solve everything except
    the following problem where a single white space changes meaning.

      let resAnnotated    = (named a:a b: typeOrValue);
      let resAnnotated    = (named a:a b :typeOrValue);

    That could be tolerated, but in order to even accomplish that distinction
    between `b: x` and `b :x` the lexer needs to treat `b:` as a single token.
    If that's the case, then what should we do about these?

        let y = (x:int);
        let z:int = 4;
        (P x:p);;

    You cannot just make a *special* parsing rule for `LET LABEL` because of
    the third example. Because of this, we need two different characters for
    type vs. named arguments. Some candidates for either type annotations or
    named arguments. It doesn't matter which is used as log as they are
    different.

    Latin-1 compatible.
    » ¦ « :: : | @

    Here's an example of what that would look like with :: for types, : for
    named args. It turns out that when you chose separate tokens, you do not
    gain back the ability to pun because of the following cases:


    - Is this annotating the return val with an unnamed arg b? Or is b a named
      arg?

        let named                 a:       b:int           => a + b;

    - Cannot pun! Is this a punned [a] with a following unnamed arg passed [b]?
      or is it a named [a] argument that is not punned?

        let resAnnotated    = (named a: b );


Alternative using :: for type annotations:

    let a::int = 10;
    let b::int = 20;
    /*A*/
    let named                 a:a      b:b             => a + b;
    type named =              a:int => b:int => int;
    /*B*/
    let namedAlias            a:aa     b:bb            => aa + bb;
    /*C*/
    let namedAnnot            a:(a::int) b:(b::int)   => 20;
    /*D*/
    let namedAliasAnnot       a:(aa::int) b:(bb::int) => 20;
    /*E*/
    let myOptional            a:a=?    b:a=?    ()  => 10;
    type named =              a:int? => b:int? unit => int;
    /*F*/
    let optionalAlias         a:aa=?   b:bb=?   ()  => 10;
    /*G*/
    let optionalAnnot         a:(a::int)=? b:(b::int)=?     ()  => 10;
    /*H*/
    let optionalAliasAnnot    a:(aa::int)=? b:(bb::int)=?   ()  => 10;
    /*I: */
    let defOptional           a:a=10    b:b=10            ()  => 10;
    type named =              a:int=? => b:int=? unit => int;
    /*J*/
    let defOptionalAlias      a:aa=10    b:bb=10          ()  => 10;
    /*K*/
    let defOptionalAnnot      a:(a::int)=10 b:(b::int)=10   ()  => 10;
    /*L*/
    let defOptionalAliasAnnot a:(aa::int)=10 b:(bb::int)=10 ()  => 10;

    /* Invoking them - Punned */
    let resNotAnnotated = named a:a b:b;
    let resAnnotated    = (named a:a b:b ::int);
    /* Invoking them */
    let resNotAnnotated = named a:a b:b;
    let resAnnotated    = (named a:a b:b ::int);

    /* Here's why punning doesn't work! */
    let resAnnotated    = (named a:a b: int);
    let resAnnotated    = (named a:a b ::int);

    let explictlyPassed =          myOptional a:?None b:?None;
    let explictlyPassedAnnotated = (myOptional a:?None b:?None ::int);
    let a = None;
    let explictlyPassed =           myOptional a:?a b:?None;
    let explictlyPassedAnnotated = (myOptional a:?a b:?None ::int);


Alternative Using Equals For Name Args, Colon For Types:

    let a:int = 10;
    let b:int = 20;
    /*A*/
    let named                 a=a      b=b             => a + b;
    type named =              a=int => b=int => int;
    /*B*/
    let namedAlias            a=aa     b=bb            => aa + bb;
    /*C*/
    let namedAnnot            a=(a:int) b=(b:int)   => 20;
    /*D*/
    let namedAliasAnnot       a=(aa:int) b=(bb:int) => 20;
    /*E*/
    let myOptional            a=a?    b=b?    ()  => 10;
    type named =              a=int? => b=int? unit => int;
    /*F*/
    let optionalAlias         a=aa?   b=bb?   ()  => 10;
    /*G*/
    let optionalAnnot         a=(a:int)? b=(b:int)?     ()  => 10;
    /*H*/
    let optionalAliasAnnot    a=(aa:int)=? b=(bb:int)=?   ()  => 10;
    /*I: */
    let defOptional           a=a=10    b=b=10            ()  => 10;
    type named =              a=int? => b=int? unit => int;
    /*J*/
    let defOptionalAlias      a=aa=10    b=bb=10          ()  => 10;
    /*K*/
    let defOptionalAnnot      a=(a:int)=10 b=(b:int)=10   ()  => 10;
    /*L*/
    let defOptionalAliasAnnot a=(aa:int)=10 b=(bb:int)=10 ()  => 10;

    /* Invoking them - Punned */
    let resNotAnnotated = named a=a b=b;
    let resAnnotated    = (named a=a b=b :int);
    /* Invoking them */
    let resNotAnnotated = named a=a b=b;
    let resAnnotated    = (named a=a b=b :int);

    /* Here's why punning doesn't work! */
    let resAnnotated    = (named a=a b= int);
    let resAnnotated    = (named a=a b :int);

    let explictlyPassed =          myOptional a=?None b=?None;
    let explictlyPassedAnnotated = (myOptional a=?None b=?None :int);
    let a = None;
    let explictlyPassed =           myOptional a=?a b=?None;
    let explictlyPassedAnnotated = (myOptional a=?a b=?None :int);




   More Ideas:
   To achieve punning without requiring wrapping tokens @name: use different
   syntaxes for punning:

       - @a could mean a:a
       - a:~ could mean a:a
       - _:a could mean a:a
       - a:_ could mean a:a
       - ::a (If two syntaxes for named args vs. types with something like :
         representing types).

         let x: someThing = fun ::a b::bAlias c::cAlias:int => a + bAlias + cAlias;

         This is perfect because the strange ::: that would *normally* occur
         when type annotating a punned name, goes away. but it's difficult to
         parse, and difficult to learn.

         let x: someThing = fun ::a ::b ::c:int => a + b + c;

         let x: someThing = fun ::a ::b ::c:int ::d:int e::e => a + b + c;

         Something like the previous suggestions makes more sense, though
         requires more typing.

   To use colons without conflict, experiment with named args being wrapped as
   "things" that are passed.

       let result = callThis (a: 1) (b: 20);
       - Of course this requires that a different notation be used for type
         annotations. If that were the case, we wouldn't need to entertain
         alternative named args syntax in the first place!

What we currently have is:

    let a = 10;
    let b = 20;
    /*A*/
    let named              @a    @b                => a + b;
    /*B*/
    let namedAlias         @a:aa @b:bb             => aa + bb;
    /*C*/
    let namedAnnot         @(a:option int) @(b:option int)           => 20;
    /*D*/
    let namedAliasAnnot    @a:(aa:option int) @b:(bb:option int)     => 20;
    /*E*/
    let optional           ?a    ?b                              ()  => 10;
    /*F*/
    let optionalAlias      ?a:aa ?b:bb                           ()  => 10;
    /*G*/
    let optionalAnnot      ?(a:option int) ?(b:option int)       ()  => 10;
    /*H*/
    let optionalAliasAnnot ?a:(aa:option int) ?b:(bb:option int) ()  => 10;
    /*I: This one is really annoying? Where's the visual label?*/
    let defOptional           ?(a=10)    ?(b=10)                 ()  => 10;
    /*J*/
    let defOptionalAlias      ?a:(aa=10) ?b:(bb=10)              ()  => 10;
    /*K*/
    let defOptionalAnnot      ?(a:int=10) ?(b:int=10)            ()  => 10;
    /*L*/
    let defOptionalAliasAnnot ?a:(aa:int=10) ?b:(bb:int=10)      ()  => 10;

What we currently have is:

    let a = 10;
    let b = 20;
    /*A*/
    let named              @a    @b                => a + b;
    /*B*/
    let namedAlias         @a:aa @b:bb             => aa + bb;
    /*C*/
    let namedAnnot         @(a:option int) @(b:option int)           => 20;
    /*D*/
    let namedAliasAnnot    @a:(aa:option int) @b:(bb:option int)     => 20;
    /*E*/
    let optional           ?a    ?b                              ()  => 10;
    /*F*/
    let optionalAlias      ?a:aa ?b:bb                           ()  => 10;
    /*G*/
    let optionalAnnot      ?(a:option int) ?(b:option int)       ()  => 10;
    /*H*/
    let optionalAliasAnnot ?a:(aa:option int) ?b:(bb:option int) ()  => 10;
    /*I: This one is really annoying? Where's the visual label?*/
    let defOptional           ?(a=10)    ?(b=10)                 ()  => 10;
    /*J*/
    let defOptionalAlias      ?a:(aa=10) ?b:(bb=10)              ()  => 10;
    /*K*/
    let defOptionalAnnot      ?(a:int=10) ?(b:int=10)            ()  => 10;
    /*L*/
    let defOptionalAliasAnnot ?a:(aa:int=10) ?b:(bb:int=10)      ()  => 10;


Also note that the function arrows make optional args defaults much nicer:

    // Before arrows
    let myFun @x ?(y=20) = result

    // After arrows (no longer *need* to wrap y=20) - although
    // we needlessly currently require it in our current parser.
    let myFun @x ?y=20 => result

    // Proposal:
    let myFun @x @y?=20 => result
    let myFun @x @y? => result
    let myFun (@x:int) @y  => result
    let myFun @x (@y: int) :int => result
    let myFun @x (@y?20: int) :int => result
    let myFun @x @y? => result
    let myFun @x (@y?:list int) => result
    // Proposal: type definitions should match (with @), and optionalness
    // faked in the "type"
    let myFun: @x:int @y?:int

    // Lofty Wish:
    let myFun x: y:?20 => result
    let myFun x: y:? => result
    let myFun (x: :int) y:  => result
    let myFun (x:x:int) @y  => result
    let myFun x: y:|int |int => result
    let myFun @x (@y?20: int) :int => result
    let myFun @x @y? => result
    let myFun @x (@y?:list int) => result
    let myFun: @x:int @y?:int


TODO: Need to properly format unary minus as being simple. Formatting is
correct when applied to constant numbers:

Reformatting the following preserves the required parens:

    max 100.0 (-. 100.0)

But the following does not have required parens being preserved:

    max 100.0 (-. something)

FYI: any time there is a non-constant to the right of a unary minus, it's
parsed into a differently named "~-.".


TODO: Make sure that !! the operator is distinguished (via parens/spacing) from
 !(!x), two BANG operators. It's likely the stock pretty printer doesn't handle
 this. `!(!foo.bar)` is currently incorreclty formatted as `!!foo.bar` where
 `!!` is itself a single prefix operator. We should create a formatting method
 similar to consecutiveInfixApplicationItems (but for prefix).


TODO: Once upstream support for m17n lands, `reason` should be built on top of
`m17n` so that `reason` will not only work with unicode identifiers, but also
allow customization of syntax using non-english keywords (as opposed to `let`,
`fun`).

TODO: Unnecessary parens for nested ifs, when a branch of if is wrapped in sequence expression:
    if hasSetup.contents then {
      (
        if not (ReactMetal.Reconciler.isClean graph.contents AnyPri) then {
          ReactMetal.TestUtils.logGraph graph.contents;
        }
      );
      dispatchDisplayLinkEvent frameCount.contents;
    };

TODO: Integrating with other syntaxes that don't distinguish between tuples and
multiple record args:
- When pretty printing, if the AST did *not* have explicit_arity on a
  pattern/expression, printing as Reason syntax, should render a tuple with
  `no_explicit_arity` attribute. This `no_explicit_arity` attribute that is
  visible, will be checked for when parsing as Reason syntax, and if seen, the
  `explicit_arity` attribute will *not* be added to the AST node.  So Reason's
  syntax has the visible *opposite* of the `explicit_arity` flag, but always
  parses *into* the `explicit_arity` form. The Reason syntax and standard AST
  simply choose opposite encodings of an AST node's explicit arity flag.


TODO: Parsing error with reference infix operators on RHS of switch arrows

    | _ => (args := arg::!args)

TODO: Create alternative pervasives for common infix operators that is more
intuitive.

    !=    (Should be redefined as [not (=)] (based on reference equality))
    !     (Should be redefined as [not] instead of dereference)

TODO: Unify on some standard convention for things like switch/if/then.
I propose a convention that allows any simple expression, though I'm not
confident this wouldn't just be confusing. (See notes below about switch -
the best solution might be to allow non-simple expressions between switch and {,
as long as there is always a leading bar before each match case.


     if simpleExpr {
     } else {
     }

     switch simpleExpr {
     }


An infix token for matching punts on the issue for matches, and resembles
ternary.

   let x = callSome func withArgs ?? {
      | X => asdf
   }

TODO: Unify syntax for records with that of module structures and sequence
expressions by having commas universally represent "simultaneous bindings" just
as they do in records (where one is not in scope of the other) and semis
represent scoped bindings.

    let MyMod = {
       let x = blah,   (* instead of using "and" *)
       let y = foo;    (* Consistent with JavaScript, in fact *)
    }

TODO: Use standard comments that everyone is used to.

TODO: Allow {} to represent () even if it's printed as () - this just makes
dummy implementations of functions easier - they compile with type errors
instead of parsing errors - which is good because non-recovering parsers stop
everything.

Make function syntax more intuitive simpler:
===================================
The fact that multiple arguments are space separated confuses people, but it
doesn't have to. The fact that there exists syntactic sugar for binding
multiple arguments is the most confusing part. There's a very easy way to
eliminate some sugar and make a syntax that is almost fully compatible with
ES6.

Furthermore, it likely makes it easier to get rid of the `fun` keyword.
Proposal:

By eliminating the fun keyword and requiring arrows between each argument, we
are *fully* compatible with ES6.

    let addCurried = a => b => a + b;
    addCurried(10)(20);
    let addNotCurried = (a, b) => a + b;
    addNotCurried(10, 20);

Then we simply can say the following:

    "Oh, by the way, when invoking any function with a 'simple' and 'single
    comma - separated item', you can ommit the parens". "This still applies
    when currying." This explanation can even be ommitted in the documentation
    until the very end in the "sugar" section.

                 (  )(  )
    let res = add 10  20;

On average, you mind find that the average number of arguments is around 2-3,
so this might even be less additional typing on average because one arg
functions are shorter by three characters, and two "arg" functions are longer
by only one.

    let print = s => log s;
    let print = fun s => log s;

    let addCurried = a => b => a + b;
    let addCurried = fun a b => a + b;

Remaining Issue With Proposal:
- Where would you put the return type annotation?

    let print = s :unit => log s;
    let print = fun s :unit => log s;           /*Actually this isn't currently allowed! */

    let print = (s:string) :unit => log s;
    let print = fun (s:string) :unit => log s;  /*Actually this isn't currently allowed! */

    let addCurried = (a:int) => (b:int) : int => a + b;
    let addCurried = fun (a:int) (b:int) :int  => a + b;

Unifying Records And Module Syntax:
===================================
TODO: See what 1ML does and consider adopting its syntax.
Unify the module syntax, record syntax even further by not requiring `let
module`/`module type`, instead using a single `let`/`type`.


    type x = int;
    let x = ref 20;
    x.contents <- 30;
    let myRecord = {
      something: 20
    };
    let myVal = 10;
    let myFunc x => {
      let p = 20;
      p;
    };
    let anotherFunc x => {
      p:int;
    };
    (* It's a good thing we use arrows to represents functions/functors! This
       removes one ambiguity when deprecating "let module" keyword.  If
       sugar/curried let bindings didn't use the arrow, the following
       MyDestructuredNonFunctor would be ambiguous. *)

    let MyDestructuredNonFunctor x = MyDestructuredNonFunctor 20;
    let MakeMapFunctor Y => {};
    type Map = {
      type key;
      let add: key => map => map;
    };
    let HashMap = MakeMapFunctor(MyMap: Map);

ERROR: The above strategy will not *quite* work because the following is
ambiguously parsed.

    let IsThisAModuleOrVariant = IsThisAModuleOrVariant;

Fixing The Final Ambiguity When Unifying Modules/Records:
--------------------------------------------------------
The way to resolve the final ambiguity is to have variants begin with some
interesting token, since modules have claimed the "capital" case. Let's assume
`#`.

    let #MyDestructuredNonFunctor x = #MyDestructuredNonFunctor 20;
    let MakeMapFunctor Y => {};
    type Map = {
      type key;
      let add: key => map => map;
    };
    let HashMap = MakeMapFunctor(MyMap: Map);
    let #IsThisAModuleOrVariant = #IsThisAModuleOrVariant;

Eliminating let altogether.
--------------------------------------------------------
Once you can accomplish unifying of `let`/`let module`, you can finally
eliminate `let` all together!

    type p = int;
    x = ref 20;
    x.contents <- 30;
    myRecord = {
      something: 20
    };
    myVal = 10;
    myFunc x => {
      p = 20;
      p;
    };
    anotherFunc x => {
      p:int;
    };
    #MyDestructuredNonFunctor x = #MyDestructuredNonFunctor 20;
    MakeMapFunctor Y => {};
    type Map = {
      type key;
      let add: key => map => map;
    };
    HashMap = MakeMapFunctor(MyMap: Map);

Counterargument to eliminating let altogether.
--------------------------------------------------------

Sugar binding functions become harder to read:

  Compare:

    let myFun
        x
        y
        anotherThing => x + y + anotherThing;

  To:

    myFun
      x
      y
      anotherThing => x + y + anotherThing;

  It isn't even clear it's a let binding vs. function call until the arrow!
  So eliminating `let` would likely need to also eliminate function curry sugar.

    myFun =
      x =>
      y =>
      anotherThing => x + y + anotherThing;

Parsing deep pattern destructuring might be in conflict with expressions:

Parsing type annotations might be harder to read:


Allowing mutually recursive, and/or independent bindings
--------------------------------------------------------

If `let` is gone, then we need a way to distinguish multiple bindings.
`and`/`;` may still suffice here! But how do we distinguish `rec`/`nonrec`?

    y = 10;
    y = "not a number" and
    x = y + 0;

    rec infiniteList = 2::infiniteList;


Or perhaps the `rec` should be indicated in the individual `=` bindings?

    infiniteList
       =+ 2::infiniteList and
    myRecursiveFunc x =+
      myRecursiveFunc (x + 1);

A series of recursive `=~` bindings separated by `and` would be equivalent to
`let rec .. and ;`. We would need to ensure that every binding separated by
`and` had the same kind of `=`/`=~` because ML's `let rec` implies that *all*
of the following `and` bindngs can see eachother, and each binding can see
themselves. We really do want/need the ability to independently control whether
or not two bindings can see eachother independent from whether or not a binding
can see themselves.

Currently: `rec` changes the meaning of `and`.

     let ; ;                                Each subsequent binding sees the previous.

     let and and ;                          Each subsequent binding in this "and" series are mutually
                                            independent and cannot see eachother and cannot see themselves.

     let rec .. and and;                    Each subsequent binding in this "and" series are
                                            mutually *dependent* and can see eachother, *and* can see themselves.

What would be awesome, (but is too difficult):

     let x=; y=;                            Each subsequent binding sees the previous.

     let x= and y= and z=;                  Each subsequent binding in this "and" series are mutually
                                            independent and cannot see eachother and cannot see themselves.

     let x=~ and y=~ and z=~;               Each subsequent binding in this "and" series are
                                            mutually *independent* and cannot
                                            see eachother, *but* can see
                                            themselves.

     let x= and~ y= and~ z=;                Each subsequent binding in this "and" series are mutually
                                            *dependent* and can see eachother but cannot see themselves.

     let x=~ and~ y=~ and~ z=~;             Each subsequent binding in this
                                            "and" series are mutually
                                            *dependent* and can see eachother,
                                            *but* cannot see themselves.


The later is more flexible in that you can mix and match =~ and = within a
single string of "and"s, however, that could even be restricted syntactically.


So how would this work in a world without a `let`/`let module`?

    let rec x = .. and
            y = ..;

Would then be equivalent to


   x =~ .. and
   y =~


Downside of unifying module/value let.
--------------------------------------------------------
If it weren't for the unification of `let module`/`let`, we might be able to
more easily accomplish automatically packing/unpacking all modules into/out-of
first class modules purely via syntactic awareness.

    let someFunction = fun (ModuleReference:Sig) => ModuleReference.x + ModuleReference.y;
    let myThing = someFunction ModuleReference;

Would become

    let someFunction = fun (module ModuleReference : Sig) => ModuleReference.x + ModuleReference.y;
    let myThing = someFunction (module ModuleReference);

With the current syntax, it's pretty easy to tell when to automatically
pack/unpack modules as long as variants receive their own leading character
that is not uppercase.

X = {p =

TODO: Find a way to allow a single expression without a semicolon inside of {}
so that refactoring function return vals becomes easier.

TODO: Eliminate the ability to even use polymorphic compare symbol (=)
reserving it for syntactic elements (possible named arguments?) Or elimination
of `let` might require this.


TODO: Since we have curried patterns requiring arrows, we can eliminate the
superfluous parens around default arguments.

    let myFun ?blah=20 => blah + blah;

TODO: You might not think that punned named arguments are *worth* the tradeoff
that every *non* punned named argument now requires two characters `@` and `:`.
If you eliminate punning, you could use something like a single equals sign.

    let myThing = someFunc firstArg = blah secondArg=foo;

However, when *defining* named argument functions, the punning is really handy.
Otherwise you have to write:

    let myFunWithNamed firstArg=firstArg secondArg=secondArg : int => secondArg + firstArg;

Instead of:

    let myFunWithNamed @firstArg @secondArg : int => secondArg + firstArg;

So simply cleaning up the optional case might be the best approach. Always
requiring a leading `@` even in the optional case, makes it easier to remember.

    let myFunc
      @named
      @anotherNamed?
      (@anotherNamedWithType:int)
      @anotherNamedOptional?
      (@anotherNamedOptionalType?:int)
      @anotherNamedWithDefault?=20
      (@anotherNamedWithDefaultAndType?=20:int)


TODO: Consider different syntax for switch, that uses semicolons instead of
leading bars. (See notes about why this is not likely the best solution)

    let x = switch something {
      OptionOne => blah;
      OptionTwo x y => x + y;
    };

    The advantages here, is that:
    - Less indentation required (2 spaces saved)
    - It is then easy to take a single branch expression and turn it into a
      sequence. See how `blah;` became `{blah;};`.  Otherwise, you'd forget to
      add the semicolon! Actually, it's arguable that now you have to add
      *another* semicolon.

    let x = switch something {
      OptionOne => {
        blah;
      };
      OptionTwo x y => x + y;
    };

    Another option would be to use commas. The benfits are:
    - Less indentation required (2 spaces saved)
    - Clean explanation for when semicolons are used (they're *always* paired
      to lets/types).
    - They resemble records and can use the same indentation rules in editors.

    let x = switch something {
      OptionOne => blah,
      OptionTwo x y => x + y
    };

    The downside of either commas or semicolons are that they ruin any chance
    of having syntactic sugar for record field functions.

    let myFunctions = {
      blah x y => x + y,
      value: 20
    };

TODO: Consider allowing non-simple expressions before switch matches.
- This would match Swift, and also match if/else.

    let x = switch callSome funcWith arg {
       | None => 0
       | Some _ => 2
    };


TODO: Consider option for formatting chained ifelses:


    let sortFour q r s t => {
      let (l1, l2) = if q > r then (r, q) else (q, r);
      let (r1, r2) = if s > t then (t, s) else (s, t);
      if l2 <= r1 then (l1, l2, r1, r2)
      else if r2 <= l1 then (r1, r2, l1, l2)
      else if l1 <= r1 && l2 <= r2 then (l1, r1, l2, r2)
      else if l1 <= r1 && r2 <= l2 then (l1, r1, r2, l2)
      else if r1 <= l1 && r2 <= l2 then (r1, l1, r2, l2)
      else if r1 <= l1 && l2 <= r2 then (r1, l1, l2, r2)
      else raise (Invalid_argument "Error in implementation of sortFour");
    };

    Instead of the current formatting (though it does make more sense).

    let sortFour q r s t => {
      let (l1, l2) = if q > r then (r, q) else (q, r);
      let (r1, r2) = if s > t then (t, s) else (s, t);
      if l2 <= r1 then (l1, l2, r1, r2)
      else
        if r2 <= l1 then (r1, r2, l1, l2)
        else
          if l1 <= r1 && l2 <= r2 then (l1, r1, l2, r2)
          else
            if l1 <= r1 && r2 <= l2 then (l1, r1, r2, l2)
            else
              if r1 <= l1 && r2 <= l2 then (r1, l1, r2, l2)
              else
                if r1 <= l1 && l2 <= r2 then (r1, l1, l2, r2)
                else raise (Invalid_argument "Error in implementation of sortFour");
    };




TODO: When importing OCaml code, comments might contain `/*` or `*/` which trip
up the Reason parser. The fix is to have the comment printer escape them, and
the comment parser unescape them on the way in. Similarly, any time `(*` occurs
in Reason comments they should be escaped.

TODO: It's possible to create a better convention for symbol characters
requiring escaping *only* when it would otherwise cause conflicts with
comments. This should be a valid operator `|*`, `|/` and `|-**`.
