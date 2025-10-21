let withLatestFrom =
    (~selector: ('a, 'b) => 'c, other: Observable.t('b))
    : Operator.t('a, 'c) =>
  ();

