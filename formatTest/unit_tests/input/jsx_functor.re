type t = {displayName:string};

let div = (~children=[], ()) => {displayName: "div"};

module X = {
  let createElement = (~children=[], ()) => {
    displayName: "X"
  };
};

module F(X: (module type of X)) = {
  let createElement = X.createElement;
};

let body =
  <div>
    <F(X) />
  </div>;
