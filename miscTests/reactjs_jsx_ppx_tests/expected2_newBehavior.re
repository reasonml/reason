[@bs.config {foo: foo}];

module ReactDOMRe = {
  let createElement (tag, :props=?, children) = 1;
  let props (:className=?, :width=?, :comp=?, :compCallback=?, ()) = 1;
};

module Foo = {
  let make (:className=?, :width=?, :comp=?, :bar=?, children) = 1;
  let createElement (:className=?, :ref=?, :key=?, :width=?, :comp=?, :bar=?, :children, ()) = 1;
  module Bar = {
    let make (:className=?, children) = 1;
    let createElement (:className=?, :ref=?, :key=?, :children, ()) = 1;
  };
};

module Bar = {
  let make (:bar=?, children) = 1;
  let createElement (:bar=?, :children, ()) = 1;
};

ReactDOMRe.createElement("div", [||]);

ReactDOMRe.createElement("div", :props ReactDOMRe.props(:className "hello", ()), [||]);

ReactDOMRe.createElement(
  "div",
  :props ReactDOMRe.props(:className "hello", :width "10", ()),
  [||]
);

ReactDOMRe.createElement(
  "div",
  :props ReactDOMRe.props(:className "hello", :width "10", ()),
  [|
    ReactDOMRe.createElement("li", [|ReactDOMRe.createElement("p", [||])|]),
    Foo.createElement(:children [Bar.createElement(:children [], ())], ())
  |]
);

ReactDOMRe.createElement(
  "div",
  :props
    ReactDOMRe.props(:className "hello", :comp Foo.createElement(:bar 1, :children [], ()), ()),
  [|ReactDOMRe.createElement("li", [||]), Foo.createElement(:bar 2, :children [], ())|]
);

ReactDOMRe.createElement(
  "div",
  :props
    ReactDOMRe.props(
      :className "hello",
      :compCallback () => Foo.createElement(:bar 1, :children [], ()),
      ()
    ),
  [|ReactDOMRe.createElement("li", [||]), (() => Foo.createElement(:bar 2, :children [], ()))(())|]
);

Foo.createElement(:children [], ());

Foo.createElement(:className "hello", :children [], ());

Foo.createElement(:className "hello", :width "10", :children [], ());

Foo.createElement(
  :className "hello",
  :width "10",
  :children [
    ReactDOMRe.createElement("li", [|ReactDOMRe.createElement("p", [||])|]),
    Foo.createElement(:children [Bar.createElement(:children [], ())], ())
  ],
  ()
);

Foo.createElement(
  :className "hello",
  :comp Bar.createElement(:bar 1, :children [], ()),
  :children [ReactDOMRe.createElement("li", [||]), Bar.createElement(:bar 2, :children [], ())],
  ()
);

Foo.createElement(:key "someKey", :className "hello", :children [], ());

Foo.createElement(:key Some("someKey"), :ref Some(ref), :className "hello", :children [], ());

Foo.createElement(:key? Some("someKey"), :ref? Some(ref), :className "hello", :children [], ());

Foo.Bar.createElement(
  :key "someKey",
  :ref Some(ref),
  :className "hello",
  :children [Bar.createElement(:children [], ())],
  ()
);
