Format issue #1976 - Multi argument function definition formatting with refmt
  $ refmt ./input.re | tee formatted.re
  let withLatestFrom =
      (
        ~selector: ('a, 'b) => 'c,
        other: Observable.t('b),
      )
      : Operator.t('a, 'c) =>
    ();

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

