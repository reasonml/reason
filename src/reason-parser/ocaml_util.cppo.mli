val print_loc : Format.formatter -> Location.t -> unit

val print_error :
   loc:Location.t
  -> f:(Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a
  -> unit

#if OCAML_VERSION >= (5,3,0)
module Utf8_lexeme = Misc.Utf8_lexeme
#else
module Utf8_lexeme : sig
  type t = string

  val normalize : t -> (t, string) result
  val is_capitalized : t -> bool

  type validation_result =
    | Valid
    | Invalid_character of Uchar.t   (** Character not allowed *)
    | Invalid_beginning of Uchar.t   (** Character not allowed as first char *)

  val validate_identifier : ?with_dot:bool -> t -> validation_result

  val is_valid_identifier : t -> bool

  val is_lowercase : t -> bool
end
#endif
