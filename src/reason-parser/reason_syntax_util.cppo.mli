(* Hello! Welcome to the Reason syntax util logic.

  This file's shared between the Reason repo and the BuckleScript repo. In
  Reason, it's in src/reason-parser. In BuckleScript, it's in
  jscomp/outcome_printer. We periodically copy this file from Reason (the source
  of truth) to BuckleScript, then uncomment the #if #else #end cppo macros you
  see in the file. That's because BuckleScript's on OCaml 4.02 while Reason's on
  4.04; so the #if macros surround the pieces of code that are different between
  the two compilers.

  When you modify this file, please make sure you're not dragging in too many
  things. You don't necessarily have to test the file on both Reason and
  BuckleScript; ping @chenglou and a few others and we'll keep them synced up by
  patching the right parts, through the power of types(tm)
*)
open Migrate_parsetree.Ast_404

val ml_to_reason_swap : string -> string

val escape_string : string -> string

(* Everything below is used by reason repo but not the BuckleScript repo *)

#ifdef BS_NO_COMPILER_PATCH

val reason_to_ml_swap : string -> string

module TrailingCommaMarker : sig val char : char val string : string end
module EOLMarker : sig val char : char val string : string end

val pick_while : ('a -> bool) -> 'a list -> 'a list * 'a list

val split_by : ?keep_empty:bool -> (char -> bool) -> string -> string list

val processLineEndingsAndStarts : string -> string

val isLineComment : string -> bool

val remove_stylistic_attrs_mapper : Ast_mapper.mapper

val escape_stars_slashes_mapper :
  Ast_mapper.mapper -> Ast_mapper.mapper

val reason_to_ml_swap_operator_mapper :
  Ast_mapper.mapper -> Ast_mapper.mapper

val ml_to_reason_swap_operator_mapper :
  Ast_mapper.mapper -> Ast_mapper.mapper

val attribute_exists : 'a -> ('a Asttypes.loc * 'b) list -> bool

val attributes_conflicted :
  'a -> 'a -> ('a Asttypes.loc * 'b) list -> bool

val normalized_attributes :
  'a ->
  ('a Asttypes.loc * 'b) list -> ('a Asttypes.loc * 'b) list

val apply_mapper_to_structure :
  Parsetree.structure -> Ast_mapper.mapper -> Parsetree.structure

val apply_mapper_to_signature :
  Parsetree.signature -> Ast_mapper.mapper -> Parsetree.signature

val apply_mapper_to_type :
  Parsetree.core_type -> Ast_mapper.mapper -> Parsetree.core_type

val apply_mapper_to_expr :
  Parsetree.expression -> Ast_mapper.mapper -> Parsetree.expression

val apply_mapper_to_pattern :
  Parsetree.pattern -> Ast_mapper.mapper -> Parsetree.pattern

val apply_mapper_to_toplevel_phrase :
  Parsetree.toplevel_phrase -> Ast_mapper.mapper -> Parsetree.toplevel_phrase

val apply_mapper_to_use_file : Parsetree.toplevel_phrase list ->
  Ast_mapper.mapper -> Parsetree.toplevel_phrase list

val map_first : ('a -> 'a) -> 'a list -> 'a list

val map_last : ('a -> 'a) -> 'a list -> 'a list

val location_is_before : Location.t -> Location.t -> bool

val location_contains : Location.t -> Location.t -> bool

val split_compiler_error : Location.error -> Location.t * string

val explode_str : string -> char list
#endif

module Clflags : sig
  include module type of Clflags

#if OCAML_VERSION >= (4, 8, 0)
  val fast : bool ref
#endif
end
