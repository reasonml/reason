let selfClosing = <Foo />;
let selfClosing = <Foo a={1} b={true} />;
let a = <Foo> <Bar c={fun a => a + 2} /> </Foo>;
let a = <So> <Much> <Nesting> </Nesting> </Much> </So>;
let a = <Sibling> <One test={true}/> <Two foo={b}> </Two> </Sibling>;
let a = <Foo>"testing a string here"</Foo>;
let a = <Foo>"testing a string here" <Test /> "another string" <Bar> </Bar> </Foo>;
let inline_expr = <Foo> {5 + 1} </Foo>;