type component = {displayName: string};

module Bar = {
  let createElement c::c=? ::children () => {displayName: "test"};
};

module Nesting = {
  let createElement ::children () => {displayName: "test"};
};

module Much = {
  let createElement ::children () => {displayName: "test"};
};

module Foo = {
    let createElement a::a=? b::b=? ::children () => {displayName: "test"};
};

module One = {
    let createElement test::test=? foo::foo=? ::children () => {displayName: "test"};
    let createElementobvioustypo test::test ::children () => {displayName: "test"};
};

module Two = {
    let createElement foo::foo=? ::children () => {displayName: "test"};
};

module Sibling = {
    let createElement foo::foo=? children::(children: list component) () => {displayName: "test"};
};

module Test = {
    let createElement yo::yo=? ::children () => {displayName: "test"};
};

module So {
    let createElement ::children () => {displayName: "test"};
};

module Foo2 = {
    let createElement ::children () => {displayName: "test"};
};

module Text = {
    let createElement ::children () => {displayName: "test"};
};

module Exp = {
    let createElement ::children () => {displayName: "test"};
};

module Pun = {
    let createElement intended::intended=? ::children () => {displayName: "test"};
};

module Namespace {
    module Foo {
        let createElement intended::intended=? anotherOptional::x=100 ::children () => {displayName: "test"};
    };
};

module Optional1 = {
    let createElement ::required ::children () => {
        switch required {
            | Some a => {displayName: a}
            | None => {displayName: "nope"}
        };
     };
};

module Optional2 = {
    let createElement ::optional=? ::children () => {
        switch optional {
            | Some a => {displayName: a}
            | None => {displayName: "nope"}
        };
     };
};

module DefaultArg = {
    let createElement ::default=(Some "foo") ::children () => {
         switch default {
            | Some a => {displayName: a}
            | None => {displayName: "nope"}
        };
     };
};


module LotsOfArguments = {
    let createElement argument1::argument1=? argument2::argument2=? argument3::argument3=? argument4::argument4=? argument5::argument5=? argument6::argument6=? ::children () => {displayName: "test"};
};

let div argument1::argument1=? ::children () => {
    displayName: "test"
};

module List1 = {
    let createElement ::children () => {displayName: "test"};
};

module List2 = {
    let createElement ::children () => {displayName: "test"};
};

module List3 = {
    let createElement ::children () => {displayName: "test"};
};

module NotReallyJSX = {
    let createElement ::foo ::bar children => {displayName: "test"};
};

let notReallyJSX ::foo ::bar children => {
    displayName: "test"
};

let fakeRender (el:component) => {
    el.displayName
};

/* end of setup */

let (/><) a b => a + b;
let (><) a b => a + b;
let (/>) = fun a b => a + b;
let ( ><\/ ) = fun a b => a + b;

let tag1 = 5 />< 6;
let tag2 = 5 >< 7;
let tag3 = 5 /> 7;
let tag4 = 5 ><\/ 7;

let b = 2;
let selfClosing = <Foo />;
let selfClosing2 = <Foo a=1 b=true />;
let selfClosing3 =
  <Foo
    a="really long values that should"
    b="cause the entire thing to wrap"
  />;
let a = <Foo> <Bar c=(fun a => a + 2) /> </Foo>;
let a3 = <So> <Much> <Nesting> </Nesting> </Much> </So>;
let a4 = <Sibling> <One test=true foo=b /> <Two foo=b> </Two> </Sibling>;
let a5 = <Foo>"testing a string here"</Foo>;
let a6 =
  <Foo2>
    <Text>
        "testing a string here"</Text>
        <Test yo=1 /> <Text>"another string"</Text> <Bar> </Bar> <Exp>( 2 + 4 )</Exp>
  </Foo2>;
let intended = true;
let punning = <Pun intended />;
let namespace = <Namespace.Foo />;
let c = <Foo> </Foo>;
let d = <Foo> </Foo>;

let spaceBefore = <So> <Much> <Nesting> </Nesting> </Much> </So>;
let spaceBefore2 = <So> <Much/> </So>;
let siblingNotSpaced = <So>   <Much> </Much> <Much> </Much> </So>;
let jsxInList = [ <Foo> </Foo>];
let jsxInList2 = [ <Foo />];
let jsxInListA = [ <Foo> </Foo> ];
let jsxInListB = [ <Foo /> ];
let jsxInListC = [ <Foo> </Foo>];
let jsxInListD = [ <Foo />];
let jsxInList3 = [ <Foo> </Foo>, <Foo> </Foo>, <Foo> </Foo>];
let jsxInList4 = [ <Foo />, <Foo />, <Foo />];
let jsxInList5 = [ <Foo> </Foo>, <Foo> </Foo> ];
let jsxInList6 = [ <Foo />, <Foo /> ];
let jsxInList7 = [ <Foo> </Foo>, <Foo> </Foo>];
let jsxInList8 = [ <Foo />, <Foo />];
let testFunc b => b;
let jsxInFnCall = testFunc (<Foo />);
let lotsOfArguments = <LotsOfArguments argument1=1 argument2=2 argument3=3 argument4=4 argument5=5 argument6="test"> <Namespace.Foo /> </LotsOfArguments>;
let lowerCase = <div argument1=1 />;

let b = 0;
let d = 0;
/*
 * Should pun the first example:
 */
let a = <Foo a=a>5</Foo>;
let a = <Foo a=b>5</Foo>;
let a = <Foo a=b b=d>5</Foo>;
let a = <Foo a>0.55</Foo>;
let a = Foo.createElement children::[] () [@JSX];
let ident = <Foo>{a}</Foo>;
let fragment1 = <> <Foo /> <Foo /> </>;
let fragment2 = <> <Foo /> <Foo /> </>;
let fragment3 = <> <Foo /> <Foo /> </>;
let fragment4 = <> <Foo /> <Foo /> </>;
let fragment5 = <> <Foo> </Foo> <Foo> </Foo> </>;
let fragment6 = <> <Foo> </Foo> <Foo> </Foo> </>;
let fragment7 = <> <Foo> </Foo> <Foo> </Foo> </>;
let fragment8 = <> <Foo> </Foo> <Foo> </Foo> </>;
let fragment9 = <> 2 2 2 2 </>;
let fragment10 = <>2.2 3.2 4.6 1.2 </>;
let fragment11 = <>"str"</>;
let fragment12 = <>(6 + 2) (6 + 2) (6 + 2)</>;
let fragment13 = <>fragment11 fragment11</>;
let listOfItems1 = <List1>1 2 3 4 5</List1>;
let listOfItems2 = <List2>1.0 2.8 3.8 4.0 5.1</List2>;
let listOfItems3 = <List3>fragment11 fragment11</List3>;



/*
 * Several sequential simple jsx expressions must be separated with a space.
 */
let thisIsRight a b => ();
let tagOne = fun ::children () => ();
let tagTwo = fun ::children () => ();
/* thisIsWrong <tagOne /><tagTwo />; */
thisIsRight <tagOne /> <tagTwo />;

/* thisIsWrong <tagOne> </tagOne><tagTwo> </tagTwo>; */
thisIsRight <tagOne> </tagOne> <tagTwo> </tagTwo>;


let a = fun ::children () => ();
let b = fun ::children () => ();

let thisIsOkay =
  <List1>
    <a> </a> <b> </b>
    <a/> <b/>
  </List1>;

let thisIsAlsoOkay =
  <List1>
    <a> </a> <b> </b>
  </List1>;


/* Doesn't make any sense, but suppose you defined an
   infix operator to compare jsx */
<a /> < <b />;
<a /> > <b />;

<a> </a> < <b> </b>;
<a> </a> > <b> </b>;

let listOfListOfJsx = [<> </>];
let listOfListOfJsx = [<> <Foo> </Foo> </>];
let listOfListOfJsx = [<> <Foo /> </>, <> <Bar /> </> ];
let listOfListOfJsx = [<> <Foo /> </>, <> <Bar /> </>, ...listOfListOfJsx];


let sameButWithSpaces = [ <> </>];
let sameButWithSpaces = [ <> <Foo /> </>];
let sameButWithSpaces = [ <> <Foo /> </>, <> <Bar /> </>];
let sameButWithSpaces = [ <> <Foo /> </>, <> <Bar /> </>, ...sameButWithSpaces];

/*
 * Test named tag right next to an open bracket.
 */

let listOfJsx = [];
let listOfJsx = [<Foo> </Foo>];
let listOfJsx = [<Foo />, <Bar /> ];
let listOfJsx = [<Foo />, <Bar /> , ...listOfJsx];


let sameButWithSpaces = [];
let sameButWithSpaces = [<Foo />];
let sameButWithSpaces = [<Foo />, <Bar />];
let sameButWithSpaces = [<Foo />, <Bar />, ...sameButWithSpaces];

/**
 * Test no conflict with polymorphic variant types.
 */
type thisType = [`Foo  | `Bar];
type t 'a = [< thisType ] as 'a;

let asd = (One.createElement test::true foo::2 children::["a", "b"] ()) [@JSX] [@foo];
let asd2 = (One.createElementobvioustypo test::false children::["a", "b"] ()) [@JSX] [@foo];

let span test::(test : bool) foo::(foo : int) ::children () => 1;
let asd = span test::true foo::2 children::["a", "b"] ()[@JSX] [@foo];
/* "video" call doesn't end with a list, so the expression isn't converted to JSX */
let video test::(test : bool) children => children;
let asd2 = video test::false 10 [@JSX] [@foo];


let div ::children => 1;
(((fun () => div) ()) children::[])[@JSX];

let myFun () => {
  <>
        <Namespace.Foo intended=(true) anotherOptional=200 />
        <Namespace.Foo intended=(true) anotherOptional=200 />
        <Namespace.Foo intended=(true) anotherOptional=200>
          <Foo />
          <Foo />
          <Foo />
          <Foo />
          <Foo />
          <Foo />
          <Foo />
        </Namespace.Foo>
  </>;

};

let myFun () => {
<>
</>;
};

let myFun () => {
  <>
        <Namespace.Foo intended=(true) anotherOptional=200 />
        <Namespace.Foo intended=(true) anotherOptional=200 />
        <Namespace.Foo intended=(true) anotherOptional=200>
          <Foo />
          <Foo />
          <Foo />
          <Foo />
          <Foo />
          <Foo />
          <Foo />
        </Namespace.Foo>
  </>;
};

/**
 * Children should wrap without forcing attributes to.
 */
<Foo a=10 b=0>
  <Bar />
  <Bar />
  <Bar />
  <Bar />
</Foo>;

/**
 * Failing test cases:
 */
/* let res = <Foo a=10 b=(<Foo a=200 />) > */
/*   <Bar /> */
/* </Foo>; */

/* let res = <Foo a=10 b=(<Foo a=200 />) />; */
let zzz = Some "oh hai";
let optionalCallSite = <Optional1 required=?zzz />;
fakeRender optionalCallSite;
let optionalArgument = <Optional2 />;
fakeRender optionalArgument;
let optionalArgument = <Optional2 optional=?zzz />;
fakeRender optionalArgument;
let defaultArg = <DefaultArg />;
fakeRender defaultArg;
let defaultArg = <DefaultArg default=zzz />;
fakeRender defaultArg;

NotReallyJSX.createElement [] foo::1 bar::2 [@JSX][@bla];
NotReallyJSX.createElement foo::1 [] bar::2 [@bla][@JSX];
notReallyJSX [] foo::1 [@JSX][@bla];
notReallyJSX foo::1 [] bar::2 [@bla][@JSX];

/* children can be at any position */
span children::[] test::true foo::2 () [@JSX];

Optional1.createElement children::[] required::(Some "hi") () [@JSX];

/* preserve some other attributes too! */
span children::[] test::true foo::2 () [@JSX][@bla];
span children::[] test::true foo::2 () [@bla][@JSX];

Optional1.createElement children::[] required::(Some "hi") () [@JSX][@bla];
Optional1.createElement children::[] required::(Some "hi") () [@bla][@JSX];
