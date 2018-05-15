type t = {displayName: string};

let div = (~children=[], ()) => {
  displayName: "div",
};

module X = {
  let createElement = (~children=[], ()) => {
    displayName: "X",
  };
};

module F = (X: (module type of X)) => {
  let createElement = X.createElement;
};

let body =
  <div>
    (
      [@JSX]
        {
          module ReasonInternal_F_X = F(X);
          ReasonInternal_F_X.createElement;
        }(
          ~children=[],
          (),
        )
    )
  </div>;
