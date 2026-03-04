let (x as y): t = x;
let ({url, mode, protocol} as target): TargetT.Safe.t =
  multiTarget->MultiTargetT.toIgnorableTargetT;
let ({url, mode}): t = x;
