let make = (~onClick, _children) => {
  ...component,
  reducer: (action, _state) =>
    switch action {
    | Click =>
      ReasonReact.UpdateWithSideEffects(nextState, (self => Js.log("leaf onClick triggered")))
    },
  render: self => <div onClick=(e => self.send(Click))> (string("hello")) </div>
};

