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

module G =
       (
         X: (module type of X),
         Y: (module type of X),
       ) => {
  let createElement = Y.createElement;
};

let body =
  <div>
    <F(X) />
    (
      [@JSX]
        {
          module G(X)(Y) = G(X, Y);
          G(X)(Y).createElement;
        }(
          ~children=[],
          (),
        )
    )
  </div>;
