type t2 =
  int * int (* attributed to entire type not binding *)

type color =
  | Red of int (* After red *)
  | Black of int (* After black *)
  | Green of int (* Does not remain here *)


let blahCurriedX x =
  function
  | Red 10
  | Black 20
  | Green 10 -> 1 (* After or pattern green *)
  | Red x -> 0 (* After red *)
  | Black x -> 0 (* After black *)
  | Green x -> 0 (* After second green *)
(* On next line after blahCurriedX def *)
