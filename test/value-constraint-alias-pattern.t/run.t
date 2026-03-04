  $ refmt ./input.re | tee formatted.re
  let (x as y): t = x;
  let ({ url, mode, protocol } as target): TargetT.Safe.t =
    multiTarget->MultiTargetT.toIgnorableTargetT;
  let { url, mode }: t = x;

  $ refmt ./formatted.re | tee formatted_back.re
  let (x as y): t = x;
  let ({ url, mode, protocol } as target): TargetT.Safe.t =
    multiTarget->MultiTargetT.toIgnorableTargetT;
  let { url, mode }: t = x;

  $ diff formatted.re formatted_back.re
