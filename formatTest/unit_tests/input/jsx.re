type component = {
    displayName: string
};

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
    let createElement test::test=? children => {displayName: "test"};
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

let b = 2;
let selfClosing = <Foo />;
let selfClosing2 = <Foo a={1} b={true} />;
let a = <Foo> <Bar c={fun a => a + 2} /> </Foo>;
let a3 = <So> <Much> <Nesting> </Nesting> </Much> </So>;
let a4 = <Sibling> <One test={true}/> <Two foo={b}> </Two> </Sibling>;
let a5 = <Foo>"testing a string here"</Foo>;
let a6 = <Foo2> <Text>"testing a string here"</Text> <Test yo={1} /> <Text>"another string"</Text> <Bar> </Bar> <Exp>{ 2 + 4 }</Exp> </Foo2>;