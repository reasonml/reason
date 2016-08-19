type component = {
    displayName: string
};


let (/><) a b => a + b;
let (><) a b => a + b;
let (/>) = fun a b => a + b;
let (></) = fun a b => a + b;

let tag1 = 5 />< 6;
let tag2 = 5 >< 7;
let tag3 = 5 /> 7;
let tag4 = 5 ></ 7;

let module Bar = {
  let createElement c::c=? children => {displayName: "test"};
};

let module Nesting = {
  let createElement children => {displayName: "test"};
};

let module Much = {
  let createElement children => {displayName: "test"};
};

let module Foo = {
    let createElement a::a=? b::b=? children => {displayName: "test"};
};

let module One = {
    let createElement test::test=? foo::foo=? children => {displayName: "test"};
};

let module Two = {
    let createElement foo::foo=? children => {displayName: "test"};
};

let module Sibling = {
    let createElement foo::foo=? (children: list component) => {displayName: "test"};
};

let module Test = {
    let createElement yo::yo=? children => {displayName: "test"};
};

let module So = {
    let createElement children => {displayName: "test"};
};

let module Foo2 = {
    let createElement children => {displayName: "test"};
};

let module Text = {
    let createElement children => {displayName: "test"};
};

let module Exp = {
    let createElement children => {displayName: "test"};
};

let module Pun = {
    let createElement intended::intended=? children => {displayName: "test"};
};

let module Namespace = {
    let module Foo = {
        let createElement intended::intended=? children => {displayName: "test"};
    };
};

let module LotsOfArguments = {
    let createElement argument1::argument1=? argument2::argument2=? argument3::argument3=? argument4::argument4=? argument5::argument5=? argument6::argument6=? children => {displayName: "test"};
};

let div argument1::argument1=? children => {
    displayName: "test"
};

let b = 2;
let selfClosing = <Foo />;
let selfClosing2 = <Foo a=1 b=true />;
let a = <Foo><Bar c=(fun a => a + 2) /></Foo>;
let a3 = <So> <Much><Nesting> </Nesting></Much></So>;
let a4 = <Sibling><One test=true foo=b /> <Two foo=b> </Two></Sibling>;
let a5 = <Foo>"testing a string here"</Foo>;
let a6 = <Foo2>
    <Text>
        "testing a string here"</Text> <Test yo=1 /> <Text>"another string"</Text> <Bar></Bar> <Exp>{ 2 + 4 }</Exp></Foo2>;
let intended = true;
let punning = <Pun intended />;
let namespace = <Namespace.Foo />;
let c = <Foo> </Foo>;
let d = <Foo></Foo>;
let spaceBefore = <So> <Much> <Nesting></Nesting></Much></So>;
let spaceBefore2 = <So> <Much/></So>;
let siblingNotSpaced = <So>   <Much></Much><Much></Much> </So>;
let jsxInList = [<Foo></Foo>];
let jsxInList2 = [<Foo />];
let jsxInListA = [<Foo></Foo> ];
let jsxInListB = [<Foo /> ];
let jsxInListC = [ <Foo></Foo>];
let jsxInListD = [ <Foo />];
let jsxInList3 = [<Foo></Foo>, <Foo></Foo>, <Foo></Foo>];
let jsxInList4 = [<Foo />, <Foo />, <Foo />];
let jsxInList5 = [<Foo></Foo>, <Foo></Foo> ];
let jsxInList6 = [<Foo />, <Foo /> ];
let jsxInList7 = [ <Foo></Foo>, <Foo></Foo>];
let jsxInList8 = [ <Foo />, <Foo />];
let testFunc b => b;
let jsxInFnCall = testFunc (<Foo />);
let lotsOfArguments = <LotsOfArguments argument1=1 argument2=2 argument3=3 argument4=4 argument5=5 argument6="test"><Namespace.Foo /></LotsOfArguments>;
let lowerCase = <div argument1=1 />;
let a = <Foo a>{5}</Foo>;
let a = <Foo a>{0.55}</Foo>;
let a = Foo.createElement "" [@JSX];
let ident = <Foo>{a}</Foo>;
let fragment1 = <> <Foo /><Foo /> </>;
let fragment2 = <><Foo /><Foo /></>;
let fragment3 = <><Foo /><Foo /> </>;
let fragment4 = <> <Foo /><Foo /></>;
let fragment5 = <> <Foo></Foo><Foo></Foo> </>;
let fragment6 = <><Foo></Foo><Foo></Foo></>;
let fragment7 = <><Foo></Foo><Foo></Foo> </>;
let fragment8 = <> <Foo></Foo><Foo></Foo></>;
let fragment9 = <> {2}</>;
let fragment10 = <>{2.2}</>;
let fragment11 = <>"str"</>;
let fragment12 = <>{6 + 2}</>;
