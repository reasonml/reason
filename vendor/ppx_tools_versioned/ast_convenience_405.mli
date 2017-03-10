open Ast_405

(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

(** {1 Convenience functions to help build and deconstruct AST fragments.} *)

open Asttypes
open Ast_helper
open Parsetree

(** {2 Compatibility modules} *)

module Label : sig
  type t = Asttypes.arg_label

  type desc = Asttypes.arg_label =
      Nolabel
    | Labelled of string
    | Optional of string

  val explode : t -> desc

  val nolabel : t
  val labelled : string -> t
  val optional : string -> t

end

(** {2 Provides a unified abstraction over differences in Parsetree.constant and Asttypes.constant
 * types defined in ocaml 4.03 and 4.02 respectively}*)
module Constant : sig
  type t = Parsetree.constant =
     Pconst_integer of string * char option
   | Pconst_char of char
   | Pconst_string of string * string option
   | Pconst_float of string * char option

  (** Convert Asttypes.constant to Constant.t *)
  val of_constant : Parsetree.constant -> t

  (** Convert Constant.t to Asttypes.constant *)
  val to_constant : t -> Parsetree.constant

end

(** {2 Misc} *)

val lid: ?loc:loc -> string -> lid

(** {2 Expressions} *)

val evar: ?loc:loc -> ?attrs:attrs -> string -> expression
val let_in: ?loc:loc -> ?attrs:attrs -> ?recursive:bool -> value_binding list -> expression -> expression

val constr: ?loc:loc -> ?attrs:attrs -> string -> expression list -> expression
val record: ?loc:loc -> ?attrs:attrs -> ?over:expression -> (string * expression) list -> expression
val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression

val nil: ?loc:loc -> ?attrs:attrs -> unit -> expression
val cons: ?loc:loc -> ?attrs:attrs -> expression -> expression -> expression
val list: ?loc:loc -> ?attrs:attrs -> expression list -> expression

val unit: ?loc:loc -> ?attrs:attrs -> unit -> expression

val func: ?loc:loc -> ?attrs:attrs -> (pattern * expression) list -> expression
val lam: ?loc:loc -> ?attrs:attrs -> ?label:Label.t -> ?default:expression -> pattern -> expression -> expression
val app: ?loc:loc -> ?attrs:attrs -> expression -> expression list -> expression

val str: ?loc:loc -> ?attrs:attrs -> string -> expression
val int: ?loc:loc -> ?attrs:attrs -> int -> expression
val int32: ?loc:loc -> ?attrs:attrs -> int32 -> expression
val int64: ?loc:loc -> ?attrs:attrs -> int64 -> expression
val char: ?loc:loc -> ?attrs:attrs -> char -> expression
val float: ?loc:loc -> ?attrs:attrs -> float -> expression

val sequence: ?loc:loc -> ?attrs:attrs -> expression list -> expression
(** Return [()] if the list is empty. Tail rec. *)

(** {2 Patterns} *)

val pvar: ?loc:loc -> ?attrs:attrs -> string -> pattern
val pconstr: ?loc:loc -> ?attrs:attrs -> string -> pattern list -> pattern
val precord: ?loc:loc -> ?attrs:attrs -> ?closed:closed_flag -> (string * pattern) list -> pattern
val ptuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern

val pnil: ?loc:loc -> ?attrs:attrs -> unit -> pattern
val pcons: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
val plist: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern

val pstr: ?loc:loc -> ?attrs:attrs -> string -> pattern
val pint: ?loc:loc -> ?attrs:attrs -> int -> pattern
val pchar: ?loc:loc -> ?attrs:attrs -> char -> pattern
val pfloat: ?loc:loc -> ?attrs:attrs -> float -> pattern

val punit: ?loc:loc -> ?attrs:attrs -> unit -> pattern


(** {2 Types} *)

val tconstr: ?loc:loc -> ?attrs:attrs -> string -> core_type list -> core_type

(** {2 AST deconstruction} *)

val get_str: expression -> string option
val get_str_with_quotation_delimiter: expression -> (string * string option) option
val get_lid: expression -> string option

val has_attr: string -> attributes -> bool
val find_attr: string -> attributes -> payload option
val find_attr_expr: string -> attributes -> expression option
