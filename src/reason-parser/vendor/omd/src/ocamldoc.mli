open Ast

type t =
  | TripleSurround of string * t * string * t * string
  | Surround of string * t * string
  | BlockSurround of string * t * string
  | GeneralBlock of t
  | Text of string
  | Raw of string
  | Null
  | Concat of t * t

val of_doc: block list -> t

val to_string : t -> string
