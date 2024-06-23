

let x  = "\u{1F42B}";
let y  = "\u{0}";
let y  = "\u{00}";
let y  = "\u{000}";
let y = "\u{000000}";
let y = "\u{0000E9}";
let y = "\u{10FFFF}";

let x  = "\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}";
let () = Format.eprintf ("x: %s@.", x);

// in a comment
/* "\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}\u{1F42B}" */
