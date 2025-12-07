module Range : sig
  type t =
    { lnum_start : int
    ; lnum_end : int
    }
  (** [t] represents an interval, including endpoints, * delimited by two
      linenumbers. *)

  val makeRangeBetween : Location.t -> Location.t -> t
  val containsLoc : t -> Warnings.loc -> bool

  val containsWhitespace :
     ?comments:Reason_comment.t list
    -> range:t
    -> unit
    -> bool
end

val hasSpaceBetween : Location.t -> Location.t -> bool
(** compute if there's space (one or more line) between [loc1] and [loc2] *)
