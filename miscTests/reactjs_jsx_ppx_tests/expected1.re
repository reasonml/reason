ReactDOMRe.createElement("div")[||];

ReactDOMRe.createElement("div", props::ReactDOMRe.props(className::"hello")())[||];

ReactDOMRe.createElement("div", props::ReactDOMRe.props(className::"hello", width::"10")())[||];

ReactDOMRe.createElement
  ("div", props::ReactDOMRe.props(className::"hello", width::"10")())
  [|ReactDOMRe.createElement("li")[||], Foo.createElement(children::[])()|];

ReactDOMRe.createElement
  (
    "div",
    props::ReactDOMRe.props(className::"hello", comp::Foo.createElement(bar::1, children::[])())()
  )
  [|ReactDOMRe.createElement("li")[||], Foo.createElement(bar::2, children::[])()|];

Foo.createElement(children::[])();

Foo.createElement(className::"hello", children::[])();

Foo.createElement(className::"hello", width::"10", children::[])();

Foo.createElement
  (
    className::"hello",
    width::"10",
    children::[ReactDOMRe.createElement("li")[||], Bar.createElement(children::[])()]
  )
  ();

Foo.createElement
  (
    className::"hello",
    comp::Bar.createElement(bar::1, children::[])(),
    children::[ReactDOMRe.createElement("li")[||], Bar.createElement(bar::2, children::[])()]
  )
  ();
