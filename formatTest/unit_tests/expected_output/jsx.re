type component = {displayName: string};

let module Bar = {
  let createElement c::c=? children => {
    displayName: "test"
  };
};

let module Nesting = {
  let createElement children => {
    displayName: "test"
  };
};

let module Much = {
  let createElement children => {
    displayName: "test"
  };
};

let module Foo = {
  let createElement a::a=? b::b=? children => {
    displayName: "test"
  };
};

let module One = {
  let createElement test::test=? children => {
    displayName: "test"
  };
};

let module Two = {
  let createElement foo::foo=? children => {
    displayName: "test"
  };
};

let module Sibling = {
  let createElement
      foo::foo=?
      (children: list component) => {
    displayName: "test"
  };
};

let module Test = {
  let createElement yo::yo=? children => {
    displayName: "test"
  };
};

let module So = {
  let createElement children => {
    displayName: "test"
  };
};

let module Foo2 = {
  let createElement children => {
    displayName: "test"
  };
};

let module Text = {
  let createElement children => {
    displayName: "test"
  };
};

let module Exp = {
  let createElement children => {
    displayName: "test"
  };
};

let module Pun = {
  let createElement intended::intended=? children => {
    displayName: "test"
  };
};

let b = 2;

let selfClosing = Foo.createElement [];

let selfClosing2 =
  Foo.createElement a::1 b::true [];

let a = Foo.createElement [
  Bar.createElement c::(fun a => a + 2) []
];

let a3 = So.createElement [
  Much.createElement [Nesting.createElement []]
];

let a4 = Sibling.createElement [
  One.createElement test::true [],
  Two.createElement foo::b []
];

let a5 = Foo.createElement [
  "testing a string here"
];

let a6 = Foo2.createElement [
  Text.createElement ["testing a string here"],
  Test.createElement yo::1 [],
  Text.createElement ["another string"],
  Bar.createElement [],
  Exp.createElement [2 + 4]
];

let intended = true;

let punning =
  Pun.createElement intended::intended [];
