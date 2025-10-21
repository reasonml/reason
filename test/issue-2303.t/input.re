type t = {x: int => int};

let _ = {
  x: [@log] x => {
    Js.log(x);
    x;
  },
};

