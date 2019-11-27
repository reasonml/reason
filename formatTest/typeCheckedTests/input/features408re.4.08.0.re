let (let.opt) = (x, f) => switch x { | None => None | Some(x) => f(x) };
let (let.&opt) = (x, f) => switch x { | None => None | Some(x) => Some(f(x)) };

let z = {
  let.opt a = Some(2);
  let.&opt b = Some(5);
  a + b
}