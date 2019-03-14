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

val syntax_error_extension_node :
  Ast_404.Location.t ->
  string -> string Ast_404.Location.loc * Ast_404.Parsetree.payload

val remove_stylistic_attrs_mapper : Ast_404.Ast_mapper.mapper

val escape_stars_slashes_mapper :
  Ast_404.Ast_mapper.mapper -> Ast_404.Ast_mapper.mapper

val reason_to_ml_swap_operator_mapper :
  Ast_404.Ast_mapper.mapper -> Ast_404.Ast_mapper.mapper

val ml_to_reason_swap_operator_mapper :
  Ast_404.Ast_mapper.mapper -> Ast_404.Ast_mapper.mapper

val attribute_exists : 'a -> ('a Ast_404.Asttypes.loc * 'b) list -> bool

val attributes_conflicted :
  'a -> 'a -> ('a Ast_404.Asttypes.loc * 'b) list -> bool

val normalized_attributes :
  'a ->
  ('a Ast_404.Asttypes.loc * 'b) list -> ('a Ast_404.Asttypes.loc * 'b) list

val apply_mapper_to_structure :
  Ast_404.Parsetree.structure ->
  Ast_404.Ast_mapper.mapper -> Ast_404.Parsetree.structure

val apply_mapper_to_signature :
  Ast_404.Parsetree.signature ->
  Ast_404.Ast_mapper.mapper -> Ast_404.Parsetree.signature

val apply_mapper_to_type :
  Ast_404.Parsetree.core_type ->
  Ast_404.Ast_mapper.mapper -> Ast_404.Parsetree.core_type

val apply_mapper_to_expr :
  Ast_404.Parsetree.expression ->
  Ast_404.Ast_mapper.mapper -> Ast_404.Parsetree.expression

val apply_mapper_to_pattern :
  Ast_404.Parsetree.pattern ->
  Ast_404.Ast_mapper.mapper -> Ast_404.Parsetree.pattern

val apply_mapper_to_toplevel_phrase :
  Ast_404.Parsetree.toplevel_phrase ->
  Ast_404.Ast_mapper.mapper -> Ast_404.Parsetree.toplevel_phrase

val apply_mapper_to_use_file :
  Ast_404.Parsetree.toplevel_phrase list ->
  Ast_404.Ast_mapper.mapper -> Ast_404.Parsetree.toplevel_phrase list

type error = Syntax_error of string

exception Error of Ast_404.Location.t * error

val map_first : ('a -> 'a) -> 'a list -> 'a list

val map_last : ('a -> 'a) -> 'a list -> 'a list

type menhirMessagesError = { msg : string; loc : Ast_404.Location.t; }

type menhirError =
    NoMenhirMessagesError
  | MenhirMessagesError of menhirMessagesError

val findMenhirErrorMessage : Ast_404.Location.t -> menhirError

val default_error_message : string

val add_error_message : menhirMessagesError -> unit

val location_is_before : Ast_404.Location.t -> Ast_404.Location.t -> bool

val location_contains : Ast_404.Location.t -> Ast_404.Location.t -> bool

val explode_str : string -> char list
#endif
