let (let+) = (x, f) => List.map(f, x);

let (and+) = List.map2((x, y) => (x, y));

let x = {
  let+ x = [2]
  and+ y = [3];

  (x, y);
};

let y = {
  let+ x = [2];
  x;
};
