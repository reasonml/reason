open Migrate_parsetree
include Ast_406

module From_current = Convert(OCaml_current)(OCaml_406)
module To_current = Convert(OCaml_406)(OCaml_current)

module type Toolchain = sig
  (* Parsing *)
  val core_type_with_comments: Lexing.lexbuf -> (Parsetree.core_type * Reason_comment.t list)
  val implementation_with_comments: Lexing.lexbuf -> (Parsetree.structure * Reason_comment.t list)
  val interface_with_comments: Lexing.lexbuf -> (Parsetree.signature * Reason_comment.t list)

  val core_type: Lexing.lexbuf -> Parsetree.core_type
  val implementation: Lexing.lexbuf -> Parsetree.structure
  val interface: Lexing.lexbuf -> Parsetree.signature
  val toplevel_phrase: Lexing.lexbuf -> Parsetree.toplevel_phrase
  val use_file: Lexing.lexbuf -> Parsetree.toplevel_phrase list

  (* Printing *)
  val print_interface_with_comments: Format.formatter -> (Parsetree.signature * Reason_comment.t list) -> unit
  val print_implementation_with_comments: Format.formatter -> (Parsetree.structure * Reason_comment.t list) -> unit

end

module type Toolchain_spec = sig
  val safeguard_parsing: Lexing.lexbuf ->
    (unit -> ('a * Reason_comment.t list)) -> ('a * Reason_comment.t list)

  type token
  type invalid_docstrings

  module Lexer : sig
    type t
    val init: ?insert_completion_ident:Lexing.position ->
              Lexing.lexbuf -> t
    val get_comments: t -> invalid_docstrings -> (string * Location.t) list
  end

  val core_type: Lexer.t -> Parsetree.core_type * invalid_docstrings
  val implementation: Lexer.t -> Parsetree.structure * invalid_docstrings
  val interface: Lexer.t -> Parsetree.signature * invalid_docstrings
  val toplevel_phrase: Lexer.t -> Parsetree.toplevel_phrase * invalid_docstrings
  val use_file: Lexer.t -> Parsetree.toplevel_phrase list * invalid_docstrings

  val format_interface_with_comments: (Parsetree.signature * Reason_comment.t list) -> Format.formatter -> unit
  val format_implementation_with_comments: (Parsetree.structure * Reason_comment.t list) -> Format.formatter -> unit
end

let insert_completion_ident : Lexing.position option ref = ref None
