Format issue #1711 - Printing too many parens for lambda
  $ refmt ./input.re | tee formatted.re
  let make = (~onClick, _children) => {
    ...component,
    reducer: (action, _state) =>
      switch (action) {
      | Click =>
        ReasonReact.UpdateWithSideEffects(
          nextState,
          (self => Js.log("leaf onClick triggered")),
        )
      },
    render: self =>
      <div onClick={e => self.send(Click)}>
        {string("hello")}
      </div>,
  };

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

