[@bs.config {foo: foo}];

module ReactDOMRe = {
  let createElement (tag, :props=?, children) = 1;
  let props (:className=?, :width=?, :comp=?, :compCallback=?, ()) = 1;
};

module Foo = {
  let make (:className=?, :width=?, :comp=?, :bar=?, children) = 1;
  module Bar = {
    let make (:className=?, children) = 1;
  };
};

module Bar = {
  let make (:bar=?, children) = 1;
};

module ReasonReact = {
  let element (:key=?, :ref=?, component) = 1;
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
    ReasonReact.element(Foo.make([|ReasonReact.element(Bar.make([||]))|]))
  |]
);

ReactDOMRe.createElement(
  "div",
  :props
    ReactDOMRe.props(:className "hello", :comp ReasonReact.element(Foo.make(:bar 1, [||])), ()),
  [|ReactDOMRe.createElement("li", [||]), ReasonReact.element(Foo.make(:bar 2, [||]))|]
);

ReactDOMRe.createElement(
  "div",
  :props
    ReactDOMRe.props(
      :className "hello",
      :compCallback () => ReasonReact.element(Foo.make(:bar 1, [||])),
      ()
    ),
  [|ReactDOMRe.createElement("li", [||]), (() => ReasonReact.element(Foo.make(:bar 2, [||])))()|]
);

ReasonReact.element(Foo.make([||]));

ReasonReact.element(Foo.make(:className "hello", [||]));

ReasonReact.element(Foo.make(:className "hello", :width "10", [||]));

ReasonReact.element(
  Foo.make(
    :className "hello",
    :width "10",
    [|
      ReactDOMRe.createElement("li", [|ReactDOMRe.createElement("p", [||])|]),
      ReasonReact.element(Foo.make([|ReasonReact.element(Bar.make([||]))|]))
    |]
  )
);

ReasonReact.element(
  Foo.make(
    :className "hello",
    :comp ReasonReact.element(Bar.make(:bar 1, [||])),
    [|ReactDOMRe.createElement("li", [||]), ReasonReact.element(Bar.make(:bar 2, [||]))|]
  )
);

ReasonReact.element(:key "someKey", Foo.make(:className "hello", [||]));

ReasonReact.element(:key Some("someKey"), :ref Some(ref), Foo.make(:className "hello", [||]));

ReasonReact.element(:key? Some("someKey"), :ref? Some(ref), Foo.make(:className "hello", [||]));

ReasonReact.element(
  :key "someKey",
  :ref Some(ref),
  Foo.Bar.make(:className "hello", [|ReasonReact.element(Bar.make([||]))|])
);
