Format issue #1673 - Functor type signature formats badly
  $ refmt ./input.re | tee formatted.re
  module type Screen =
    (Config: {type transition;}) => {type state; let run: (state, env) => Config.transition;};

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

