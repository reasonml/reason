/* Extension sugar */
%extend
open M;

%extend
module M = {};

%extend
module type M = {};

%extend
let x = "hi";

let x = {
  %extend
  let x = ();
  ignore();
  %extend
  ignore();
  %extend
  let x = ();
  %extend
  return("hi");
};

let x = {
  %extend
  if (true) {1} else {2};
  %extend
  switch None {
  | Some(x) => assert false
  | None => ()
  };
  %extend
  try (raise(Not_found)) {
  | Not_found => ()
  | Invalid_argument(msg) => prerr_endline(msg)
  };
};

let x = [%extend if (true) {1} else {2}];

let x = [%extend
  switch None {
  | Some(x) => assert false
  | None => ()
  }
];

let x = [%extend
  try (raise(Not_found)) {
  | Not_found => ()
  | Invalid_argument(msg) => prerr_endline(msg)
  }
];
