Format basic

  $ refmt ./input.ml | tee formatted.re
  /* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
  
  let x =
    ignore(y => {
      let y = 4;
      y;
    });

Format the formatted file back
  $ refmt ./formatted.re | tee formatted_back.re
  /* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
  
  let x =
    ignore(y => {
      let y = 4;
      y;
    });

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
