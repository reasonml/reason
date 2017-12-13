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
