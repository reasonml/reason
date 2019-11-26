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

let (let+opt) = (x, f) => switch x { | None => None | Some(x) => f(x) };
let (let+&opt) = (x, f) => switch x { | None => None | Some(x) => Some(f(x)) };

let z = {
  let+opt a = Some(2);
  let+&opt b = Some(5);
  a + b
}