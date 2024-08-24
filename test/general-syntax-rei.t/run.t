Format general interface syntax
  $ refmt ./input.rei
  /* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
  
  /**
   * Typically the "interface file" is where you would write a ton of
   * comments/documentation.
   */
  type adders = {
    /*
     * Adds two numbers together.
     */
    addTwoNumbers: (int, int) => int,
    /*
     * Amazingly, adds *three* numbers together.
     */
    addThreeNumbers: (int, int, int) => int,
    /*
     * Tuple version of previous function.
     */
    addThreeNumbersTupled:
      ((int, int, int)) => int,
  };
  
  /**
   * Public function.
   */
  let myRecordWithFunctions: adders;
  /**
   * Public result.
   */
  let result: int;
  
  /* https://github.com/facebook/reason/issues/1614 */
  module Event: (module type of {
    include ReactEventRe;
  });
  
  module type Event = (module type of {
    include ReactEventRe;
  });
  
  /* https://github.com/facebook/reason/issues/2169 */
  let not: string => string;
  
  let other: string => not;
  
  include
     (module type of Bos.Cmd) with
      type t = Bos.Cmd.t;
  
  external%foo bar: string => string;
  external%foo bar: int => int = "hello";
  
  let%foo foo: bar;
  let%foo foo: bar;
  
  module%foo X: Y;
  
  module%foo X = Y;
  
  module%foo rec X: Y;
  
  let wrapReasonForJs:
    (
      ~component:
        componentSpec(
          'state,
          'initialState,
          'retainedProps,
          'initialRetainedPropssssssssssssssssss,
          'action,
        )
    ) =>
    reactClass;
  
  open%foo Bar;
  
  open! %foo Bar;
  
  type%foo t = int;
  
  type%x foo +=
    | Int;
  
  module type x = {
    let a: 'a. 'a => unit;
  };
  
  let a: 'a. 'a => unit;
