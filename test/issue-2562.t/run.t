Format issue #2562 - Js.t obj type in external gets aligned wrong for named args
  $ refmt ./input.re | tee formatted.re
  external make:
    (
      ~prettyLongNameGoesHereBecauseItsDescriptive: (
                                                      'a,
                                                      {
                                                        .
                                                        "handleClick":
                                                          [@bs.meth]
                                                          (
                                                            ReactEvent.Mouse.t => unit
                                                          ),
                                                        "handleClick":
                                                          [@bs.meth]
                                                          (
                                                            ReactEvent.Mouse.t => unit
                                                          ),
                                                      }
                                                    ) =>
                                                    React.element
    ) =>
    React.element =
    "Hi";

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re

