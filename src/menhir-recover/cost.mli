type t

val zero : t
val infinite : t
val compare : t -> t -> int
val add : t -> t -> t
val of_int : int -> t
val to_int : t -> int
val is_infinite : t -> bool
val arg_min : ('a -> t) -> 'a -> 'a -> 'a
val pp : Format.formatter -> t -> unit
