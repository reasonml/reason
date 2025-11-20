open Ppxlib

module type Toolchain = sig
  (* Parsing *)
  val core_type_with_comments :
     Lexing.lexbuf
    -> Parsetree.core_type * Reason_comment.t list

  val implementation_with_comments :
     Lexing.lexbuf
    -> Parsetree.structure * Reason_comment.t list

  val interface_with_comments :
     Lexing.lexbuf
    -> Parsetree.signature * Reason_comment.t list

  val core_type : Lexing.lexbuf -> Parsetree.core_type
  val implementation : Lexing.lexbuf -> Parsetree.structure
  val interface : Lexing.lexbuf -> Parsetree.signature
  val toplevel_phrase : Lexing.lexbuf -> Parsetree.toplevel_phrase
  val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list

  (* Printing *)
  val print_interface_with_comments :
     Format.formatter
    -> Parsetree.signature * Reason_comment.t list
    -> unit

  val print_implementation_with_comments :
     Format.formatter
    -> Parsetree.structure * Reason_comment.t list
    -> unit
end

module type Toolchain_spec = sig
  val safeguard_parsing :
     Lexing.lexbuf
    -> (unit -> 'a * Reason_comment.t list)
    -> 'a * Reason_comment.t list

  type token
  type invalid_docstrings

  module Lexer : sig
    type t

    val init : ?insert_completion_ident:Lexing.position -> Lexing.lexbuf -> t
    val get_comments : t -> invalid_docstrings -> (string * Location.t) list
  end

  val core_type : Lexer.t -> Parsetree.core_type * invalid_docstrings
  val implementation : Lexer.t -> Parsetree.structure * invalid_docstrings
  val interface : Lexer.t -> Parsetree.signature * invalid_docstrings

  val toplevel_phrase :
     Lexer.t
    -> Parsetree.toplevel_phrase * invalid_docstrings

  val use_file : Lexer.t -> Parsetree.toplevel_phrase list * invalid_docstrings

  val format_interface_with_comments :
     Parsetree.signature * Reason_comment.t list
    -> Format.formatter
    -> unit

  val format_implementation_with_comments :
     Parsetree.structure * Reason_comment.t list
    -> Format.formatter
    -> unit
end

val insert_completion_ident : Lexing.position option ref

module From_current : sig
  val copy_structure :
     Ppxlib_ast.Compiler_version.Ast.Parsetree.structure
    -> Ppxlib.Parsetree.structure

  val copy_signature :
     Ppxlib_ast.Compiler_version.Ast.Parsetree.signature
    -> Ppxlib.Parsetree.signature

  val copy_expression :
     Ppxlib_ast.Compiler_version.Ast.Parsetree.expression
    -> Ppxlib.Parsetree.expression

  val copy_core_type :
     Ppxlib_ast.Compiler_version.Ast.Parsetree.core_type
    -> Ppxlib.Parsetree.core_type

  val copy_pattern :
     Ppxlib_ast.Compiler_version.Ast.Parsetree.pattern
    -> Ppxlib.Parsetree.pattern

  val copy_case :
     Ppxlib_ast.Compiler_version.Ast.Parsetree.case
    -> Ppxlib.Parsetree.case

  val copy_toplevel_phrase :
     Ppxlib_ast.Compiler_version.Ast.Parsetree.toplevel_phrase
    -> Ppxlib.Parsetree.toplevel_phrase

  val copy_out_value :
     Reason_omp.OCaml_current.Ast.Outcometree.out_value
    -> Reason_omp.OCaml_414.Ast.Outcometree.out_value

  val copy_out_type :
     Reason_omp.OCaml_current.Ast.Outcometree.out_type
    -> Reason_omp.OCaml_414.Ast.Outcometree.out_type

  val copy_out_class_type :
     Reason_omp.OCaml_current.Ast.Outcometree.out_class_type
    -> Reason_omp.OCaml_414.Ast.Outcometree.out_class_type

  val copy_out_module_type :
     Reason_omp.OCaml_current.Ast.Outcometree.out_module_type
    -> Reason_omp.OCaml_414.Ast.Outcometree.out_module_type

  val copy_out_sig_item :
     Reason_omp.OCaml_current.Ast.Outcometree.out_sig_item
    -> Reason_omp.OCaml_414.Ast.Outcometree.out_sig_item

  val copy_out_type_extension :
     Reason_omp.OCaml_current.Ast.Outcometree.out_type_extension
    -> Reason_omp.OCaml_414.Ast.Outcometree.out_type_extension

  val copy_out_phrase :
     Reason_omp.OCaml_current.Ast.Outcometree.out_phrase
    -> Reason_omp.OCaml_414.Ast.Outcometree.out_phrase
end

module To_current : sig
  val copy_structure :
     Ppxlib.Parsetree.structure
    -> Ppxlib_ast.Compiler_version.Ast.Parsetree.structure

  val copy_signature :
     Ppxlib.Parsetree.signature
    -> Ppxlib_ast.Compiler_version.Ast.Parsetree.signature

  val copy_core_type :
     Ppxlib.Parsetree.core_type
    -> Ppxlib_ast.Compiler_version.Ast.Parsetree.core_type

  val copy_toplevel_phrase :
     Ppxlib.Parsetree.toplevel_phrase
    -> Ppxlib_ast.Compiler_version.Ast.Parsetree.toplevel_phrase

  val copy_out_value :
     Reason_omp.OCaml_414.Ast.Outcometree.out_value
    -> Reason_omp.OCaml_current.Ast.Outcometree.out_value

  val copy_out_type :
     Reason_omp.OCaml_414.Ast.Outcometree.out_type
    -> Reason_omp.OCaml_current.Ast.Outcometree.out_type

  val copy_out_class_type :
     Reason_omp.OCaml_414.Ast.Outcometree.out_class_type
    -> Reason_omp.OCaml_current.Ast.Outcometree.out_class_type

  val copy_out_module_type :
     Reason_omp.OCaml_414.Ast.Outcometree.out_module_type
    -> Reason_omp.OCaml_current.Ast.Outcometree.out_module_type

  val copy_out_sig_item :
     Reason_omp.OCaml_414.Ast.Outcometree.out_sig_item
    -> Reason_omp.OCaml_current.Ast.Outcometree.out_sig_item

  val copy_out_type_extension :
     Reason_omp.OCaml_414.Ast.Outcometree.out_type_extension
    -> Reason_omp.OCaml_current.Ast.Outcometree.out_type_extension

  val copy_out_phrase :
     Reason_omp.OCaml_414.Ast.Outcometree.out_phrase
    -> Reason_omp.OCaml_current.Ast.Outcometree.out_phrase
end
