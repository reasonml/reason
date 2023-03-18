
let (let.opt) = (x, f) => switch x { | None => None | Some(x) => f(x) };
let (let.&opt) = (x, f) => switch x { | None => None | Some(x) => Some(f(x)) };
let (and.opt) = (a, b) => switch (a, b) { | (Some(a), Some(b)) => Some((a, b)) | _ => None };

let x = {
  let.opt a = Some(1);
  let.opt b = Some(2)
  and.opt c = Some(3)
  and.opt d = Some(4);
  Some((a, b, c, d))
}
let y = {
  let.opt a = Some(1)
  and.opt b = None
  and.opt c = Some(4);
  Some((a, b, c))
}
assert(x == Some((1,2,3,4)));
assert(y == None);
print_endline("Success")
