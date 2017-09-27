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

module ReasonReact = {
  let element (:key=?, :ref=?, component) = 1;
};

let divRef = ReactDOMRe.createElement("div", [||]);

"=== DOM component ===";

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

"=== Custom component ===";

ReasonReact.element(Foo.make([||]));

ReasonReact.element(Foo.make([|ReactDOMRe.createElement("div", [||])|]));

ReasonReact.element(Foo.make([|ReasonReact.element(Bar.make([||]))|]));

ReasonReact.element(
  Foo.make([|ReactDOMRe.createElement("div", [||]), ReasonReact.element(Bar.make([||]))|])
);

ReasonReact.element(Foo.make([|divRef, divRef|]));

ReasonReact.element(Foo.make(:className "hello", [||]));

ReasonReact.element(Foo.make(:className "hello", [|ReactDOMRe.createElement("div", [||])|]));

ReasonReact.element(Foo.make(:className "hello", [|ReasonReact.element(Bar.make([||]))|]));

ReasonReact.element(
  Foo.make(
    :className "hello",
    [|ReactDOMRe.createElement("div", [||]), ReasonReact.element(Bar.make([||]))|]
  )
);

ReasonReact.element(Foo.make(:className "hello", [|divRef, divRef|]));

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

ReasonReact.element(
  Foo.make(
    :comp ReasonReact.element(Bar.make([|divRef, divRef|])),
    [|ReactDOMRe.createElement("li", [||])|]
  )
);

ReasonReact.element(
  Foo.make(
    :comp ReasonReact.element(Bar.make([|ReactDOMRe.createElement("div", [||])|])),
    [|ReactDOMRe.createElement("li", [||])|]
  )
);

"=== Special-cased in V3, no wrapping for single child that's not JSX ===";

ReasonReact.element(Foo.make(() => 1));

ReasonReact.element(Foo.make(() => ReasonReact.element(Bar.make([||]))));

ReasonReact.element(Foo.make((1, 2)));

ReasonReact.element(Foo.make([|1|]));

ReasonReact.element(Foo.make([||]));

ReasonReact.element(Foo.make([]));

ReasonReact.element(Foo.make(divRef));

ReasonReact.element(Foo.make([|divRef, divRef|]));

ReasonReact.element(Foo.make(:className "hello", () => 1));

ReasonReact.element(Foo.make(:className "hello", (1, 2)));

ReasonReact.element(Foo.make(:className "hello", [|1, 2|]));

ReasonReact.element(Foo.make(:className "hello", divRef));

ReasonReact.element(
  Foo.make(:comp ReasonReact.element(Bar.make(divRef)), [|ReactDOMRe.createElement("li", [||])|])
);

"=== With ref/key ===";

ReasonReact.element(:key "someKey", Foo.make(:className "hello", [||]));

ReasonReact.element(:key Some("someKey"), :ref Some(ref), Foo.make(:className "hello", [||]));

ReasonReact.element(:key? Some("someKey"), :ref? Some(ref), Foo.make(:className "hello", [||]));

ReasonReact.element(
  :key "someKey",
  :ref Some(ref),
  Foo.Bar.make(:className "hello", [|ReasonReact.element(Bar.make([||]))|])
);

ReasonReact.element(Foo.make([||]));
