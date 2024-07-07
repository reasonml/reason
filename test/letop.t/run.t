Format basic
  $ refmt --print re ./input.re > ./formatted.re

Print the formatted file
  $ cat ./formatted.re
  let (let.opt) = (x, f) =>
    switch (x) {
    | None => None
    | Some(x) => f(x)
    };
  let (let.&opt) = (x, f) =>
    switch (x) {
    | None => None
    | Some(x) => Some(f(x))
    };
  
  let z = {
    let.opt a = Some(2);
    let.&opt b = Some(5);
    a + b;
  };
  
  let (let./\/) = (x, f) =>
    switch (x) {
    | None => None
    | Some(x) => f(x)
    };
  let ( let.&/\* ) = (x, f) =>
    switch (x) {
    | None => None
    | Some(x) => Some(f(x))
    };
  
  /* Test syntax that could potentially conflict with comments */
  let z = {
    let./\/ a = Some(2);
    let.&/\* b = Some(5);
    a + b;
  };
  
  let _ = {
    let.opt _ = Some("a");
  
    let.opt _ = Some("c");
  
    // hello
  
    None;
  };
  
  // test that the type annotation prints with parenthesis
  let _ = {
    let.opt (x: string) as _y = Some("a");
    None;
  };

Type-check basics
  $ ocamlc -c -pp 'refmt --print binary' -intf-suffix .rei -impl formatted.re

Format the formatted file back
  $ refmt --print re ./formatted.re > ./formatted_back.re

Ensure idempotency: first format and second format are the same
  $ diff formatted.re formatted_back.re
