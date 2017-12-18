/* Extension sugar */
%extend
open M;

%extend
module M = {};

%extend
module type M = {};

let%extend x = "hi";

let x = {
  let%extend x = ();
  ignore();
  %extend
  ignore();
  let%extend x = ();
  %extend
  return("hi");
};

let x = {
  if%extend (true) {1} else {2};
  switch%extend None {
  | Some(x) => assert false
  | None => ()
  };
  try%extend (raise(Not_found)) {
  | Not_found => ()
  | Invalid_argument(msg) => prerr_endline(msg)
  };
};

let x = if%extend (true) {1} else {2};

let x =
  switch%extend None {
  | Some(x) => assert false
  | None => ()
  };

let x =
  try%extend (raise(Not_found)) {
  | Not_found => ()
  | Invalid_argument(msg) => prerr_endline(msg)
  };
