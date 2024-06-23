Test uchar escape lexing

  $ refmt ./input.re
  let x = "\u{1F42B}";
  let y = "\u{0}";
  let y = "\u{00}";
  let y = "\u{000}";
  let y = "\u{000000}";
  let y = "\u{0000E9}";
  let y = "\u{10FFFF}";
  
  let x = "\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}";
  let () = Format.eprintf("x: %s@.", x);
  
  // in a comment
  /* "\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}" */

check internal translation

  $ ocamlc -dsource -pp 'refmt --print binary' -intf-suffix .rei -impl input.re -o test
  let x = "\240\159\144\171"
  let y = "\000"
  let y = "\000"
  let y = "\000"
  let y = "\000"
  let y = "\195\169"
  let y = "\244\143\191\191"
  let x =
    "\240\159\144\171\240\159\144\171\240\159\144\171\240\159\144\171\240\159\144\171\240\159\144\171"
  let () = Format.eprintf "x: %s@." x

  $ ./test
  x: 🐫🐫🐫🐫🐫🐫
