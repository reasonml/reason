(** This module provides utilities. *)

type 't info = {
  is_tainted : bool;
  (** Taintedness status *)
  cost : 't
  (** Cost of the output layout *)
}
(** An [info] record, returned from the pretty printer. *)

