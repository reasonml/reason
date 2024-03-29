Format basic
  $ refmt --print re ./input.re > ./formatted.re

Print the formatted file
  $ cat ./formatted.re
  /* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
  
  /**
   * Testing mutations.
   */
  let holdsAUnit = ref();
  
  let holdsABool = ref(false);
  
  let holdsAnInt = ref(0);
  
  let holdsAHoldsABool = ref(ref(true));
  
  let () = holdsAUnit := holdsABool := false;
  
  /* Should be parsed as: */
  /* And so they should both be printed the same */
  let () = holdsAUnit := holdsABool := false;
  
  /*
   * The following:
   *
   *   something = x := e
   *
   * Should be parsed as:
   *
   *   something = (x := e)
   */
  holdsAUnit.contents = holdsAnInt := 0;
  
  holdsABool.contents = holdsAnInt.contents == 100;
  
  let numberToSwitchOn = 100;
  
  switch (numberToSwitchOn) {
  | (-3)
  | (-2)
  | (-1) => ()
  | 0 => holdsAUnit.contents = ()
  | 1 => holdsAUnit.contents = holdsAnInt := 0
  | 2 =>
    true
      ? holdsAUnit.contents = ()
      : holdsABool.contents ? () : ()
  | 3 =>
    true
      ? holdsAUnit := ()
      : holdsABool.contents ? () : ()
  | 4 => true ? holdsAnInt := 40 : ()
  | 5 => holdsAnInt := 40
  | _ => ()
  };
  
  let mutativeFunction =
    fun
    | Some(x) => holdsAUnit.contents = ()
    | None => holdsAUnit := ();

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
