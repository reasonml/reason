open Ppxlib

type attributesPartition =
  { arityAttrs : attributes
  ; docAttrs : attributes
  ; stdAttrs : attributes
  ; jsxAttrs : attributes
  ; stylisticAttrs : attributes
  ; uncurried : bool
  }
(** Kinds of attributes *)

val partitionAttributes :
   ?partDoc:bool
  -> ?allowUncurry:bool
  -> attribute list
  -> attributesPartition
(** Partition attributes into kinds *)

val extract_raw_literal : attribute list -> label option * attribute list

val maybe_remove_stylistic_attrs :
   attribute list
  -> should_preserve:bool
  -> attribute list

val without_stylistic_attrs : attribute list -> attribute list
val has_open_notation_attr : attribute list -> bool
val has_jsx_attributes : attribute list -> bool
val has_preserve_braces_attrs : attribute list -> bool
val has_quoted_extension_attrs : attribute list -> bool
val extractStdAttrs : attribute list -> attributes
