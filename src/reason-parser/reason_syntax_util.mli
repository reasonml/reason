open Ppxlib

val ml_to_reason_swap : string -> string
val escape_string : string -> string
val reason_to_ml_swap : string -> string

module TrailingCommaMarker : sig
  val char : char
  val string : string
end

module EOLMarker : sig
  val char : char
  val string : string
end

val pick_while : ('a -> bool) -> 'a list -> 'a list * 'a list
val split_by : ?keep_empty:bool -> (char -> bool) -> string -> string list
val processLineEndingsAndStarts : string -> string
val isLineComment : string -> bool
val remove_stylistic_attrs_mapper : Ast_traverse.map
val is_letop : string -> bool
val is_andop : string -> bool
val compress_letop_identifier : string -> string
val expand_letop_identifier : string -> string
val backport_letopt_mapper : Ast_traverse.map
val escape_stars_slashes : string -> string

class escape_stars_slashes_mapper : Ast_traverse.map
class reason_to_ml_swap_operator_mapper : Ast_traverse.map
class ml_to_reason_swap_operator_mapper : Ast_traverse.map

val attribute_exists : string -> Parsetree.attributes -> bool
val attributes_conflicted : string -> string -> Parsetree.attributes -> bool

val normalized_attributes :
   string
  -> Parsetree.attributes
  -> Parsetree.attributes

val apply_mapper_to_structure :
   Ast_traverse.map
  -> Parsetree.structure
  -> Parsetree.structure

val apply_mapper_to_signature :
   Ast_traverse.map
  -> Parsetree.signature
  -> Parsetree.signature

val apply_mapper_to_type :
   Ast_traverse.map
  -> Parsetree.core_type
  -> Parsetree.core_type

val apply_mapper_to_expr :
   Ast_traverse.map
  -> Parsetree.expression
  -> Parsetree.expression

val apply_mapper_to_pattern :
   Ast_traverse.map
  -> Parsetree.pattern
  -> Parsetree.pattern

val apply_mapper_to_toplevel_phrase :
   Ast_traverse.map
  -> Parsetree.toplevel_phrase
  -> Parsetree.toplevel_phrase

val apply_mapper_to_use_file :
   Ast_traverse.map
  -> Parsetree.toplevel_phrase list
  -> Parsetree.toplevel_phrase list

val map_first : ('a -> 'a) -> 'a list -> 'a list
val map_last : ('a -> 'a) -> 'a list -> 'a list
val location_is_before : Location.t -> Location.t -> bool
val location_contains : Location.t -> Location.t -> bool
val split_compiler_error : Location.Error.t -> Location.t * string
val explode_str : string -> char list

module Clflags : module type of Ocaml_common.Clflags

val parse_lid : string -> Longident.t
