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
