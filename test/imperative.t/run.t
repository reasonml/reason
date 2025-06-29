Format basic
  $ refmt --print re ./input.re > ./formatted.re

Print the formatted file
  $ cat ./formatted.re
  /* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. */
  
  /*
   * Syntax and fallback syntax.
  
   * vim: set ft=reason:
   */
  switch (
    while (true) {
      ();
    }
  ) {
  | _ => ()
  };
  
  try(
    while (true) {
      ();
    }
  ) {
  | _ => ()
  };
  
  switch (
    for (i in 0 to 10) {
      ();
    }
  ) {
  | _ => ()
  };
  
  try(
    for (i in 0 to 10) {
      ();
    }
  ) {
  | _ => ()
  };
  
  switch (
    if (true) {
      print_string("switching on true");
    } else {
      print_string("switching on false");
    }
  ) {
  | _ => ()
  };
  
  try(
    for (i in 0 to 10) {
      ();
    }
  ) {
  | _ => ()
  };
  
  let result =
    (
      while (false) {
        ();
      }
    )
    == ()
      ? false : true;
  
  switch (
    try(
      try() {
      | _ => ()
      }
    ) {
    | _ => ()
    }
  ) {
  | () => ()
  };
  
  let shouldStillLoop = { contents: false };
  
  while (shouldStillLoop.contents) {
    print_string("You're in a while loop");
    print_newline();
  };
  
  while ({
           shouldStillLoop.contents = false;
           shouldStillLoop.contents;
         }) {
    print_string("Will never loop");
  };
  
  while ((shouldStillLoop := false) == ()) {
    print_string("Forever in the loop");
  };

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
