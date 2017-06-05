ReactDOMRe.createElement "div" [||];

ReactDOMRe.createElement "div" props::(ReactDOMRe.props className::"hello" ()) [||];

ReactDOMRe.createElement "div" props::(ReactDOMRe.props className::"hello" width::"10" ()) [||];

ReactDOMRe.createElement
  "div"
  props::(ReactDOMRe.props className::"hello" width::"10" ())
  [|ReactDOMRe.createElement "li" [||], ReasonReact.element (Foo.make [||])|];

ReactDOMRe.createElement
  "div"
  props::(
    ReactDOMRe.props className::"hello" comp::(ReasonReact.element (Foo.make bar::1 [||])) ()
  )
  [|ReactDOMRe.createElement "li" [||], ReasonReact.element (Foo.make bar::2 [||])|];

ReasonReact.element (Foo.make [||]);

ReasonReact.element (Foo.make className::"hello" [||]);

ReasonReact.element (Foo.make className::"hello" width::"10" [||]);

ReasonReact.element (
  Foo.make
    className::"hello"
    width::"10"
    [|ReactDOMRe.createElement "li" [||], ReasonReact.element (Bar.make [||])|]
);

ReasonReact.element (
  Foo.make
    className::"hello"
    comp::<Bar bar=1 />
    [|ReactDOMRe.createElement "li" [||], ReasonReact.element (Bar.make bar::2 [||])|]
);

ReasonReact.element key::"someKey" (Foo.make className::"hello" [||]);

ReasonReact.element key::"someKey" ref::(some ref) (Foo.make className::"hello" [||]);

ReasonReact.element
  key::"someKey"
  ref::(some ref)
  (Foo.Bar.make className::"hello" [|ReasonReact.element (Bar.make [||])|]);
