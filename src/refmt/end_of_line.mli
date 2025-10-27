type t =
  | LF
  | CRLF

module Detect : sig
  val default : t
  val get_eol_for_file : string -> t
end

module Convert : sig
  val get_formatter : out_channel -> t -> Format.formatter
end
