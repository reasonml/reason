let x: t = x;
let (x as y): t = x;
let ((x, y) as pair): t = x;
let (Some(x) as opt): t = opt;
let ({url, mode} as target): t = x;
let ({url, mode, protocol} as target): TargetT.Safe.t =
  multiTarget->MultiTargetT.toIgnorableTargetT;
let ({url, mode}): t = x;
let ({url: u, mode: m} as target): t = x;
let Foo.{url, mode}: t = x;
let (Foo.{url, mode} as target): t = x;
let ([x, y] as listPair): t = value;
let (_ as anyValue): t = value;
let ((x as y: u): t) = value;
