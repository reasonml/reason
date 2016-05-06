(* **** comment *)
(*** comment *)
(** docstring *)
(* comment *)
(** docstring *)
(*** comment *)
(**** comment *)
(***** comment *)

(** *)
(*** *)
(**** *)

(**)
(***)
(****)



(** (** comment *) *)
(** (*** comment *) *)

(* (** comment *) *)
(* (*** comment *) *)
(* *(*** comment *) *)

(* comment **)
(* comment ***)
(* comment ****)
(* comment *****)

(**
  * Multiline
  *)

(** Multiline
  *
  *)

(**
  **
  *)

let testingNotQuiteEndOfLineComments = [
  "Item 1"(* Comment For First Item *);  
  "Item 2" (* Comment For Second Item *);
  "Item 3" (* Comment For Third Item *);
  "Item 4" (* Comment For Fourth Item - but no semi *)
  (* Comment after last item in list. *)
] (* Comment after list bracket *)

let testingEndOfLineComments = [
  "Item 1";(* Comment For First Item *)  
  "Item 2"; (* Comment For Second Item *)
  "Item 3";  (* Comment For Third Item *)
  "Item 4" (* Comment For Fourth Item - but before semi *);
  (* Comment after last item in list. *)
] (* Comment after list bracket *)

(* This time no space between bracket and comment *)
let testingEndOfLineComments = [
](* Comment after list bracket *)


type t = int * int (* End of line on t *)
type t2 =
  int * int (* End of line on int * int *)


type variant =
  (* Comment above X *)
  | X of int * int (* End of line on X *)
  (* Comment above Y *)
  | Y of int * int (* End of line on Y *)


type x = { (* attached *above* x *)
  fieldOne : int
} (* Attached end of line after x *)
and y = { (* attached *above* y *)
  fieldTwo : int
} (* Attached end of line after y *)
