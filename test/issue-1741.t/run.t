Format issue #1741 - Comment on first argument does not stay attached
  $ refmt ./input.re | tee formatted.re
  let fn =
      /* Comment about argument 1 */
      (
        longArgumentName1,
        longArgumentName2,
        longArgumentName3,
        longArgumentName4,
        longArgumentName5,
        longArgumentName6,
        longArgumentName7,
        longArgumentName8,
        longArgumentName9,
        longArgumentName10,
        longArgumentName11,
        longArgumentName12,
      ) =>
    ();

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

