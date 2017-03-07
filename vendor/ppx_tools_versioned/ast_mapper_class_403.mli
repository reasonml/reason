open Ast_403

(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

(** Class-based customizable mapper *)

open Parsetree

class mapper:
  object
    method attribute: attribute -> attribute
    method attributes: attribute list -> attribute list
    method case: case -> case
    method cases: case list -> case list
    method class_declaration: class_declaration -> class_declaration
    method class_description: class_description -> class_description
    method class_expr: class_expr -> class_expr
    method class_field: class_field -> class_field
    method class_signature: class_signature -> class_signature
    method class_structure: class_structure -> class_structure
    method class_type: class_type -> class_type
    method class_type_declaration: class_type_declaration -> class_type_declaration
    method class_type_field: class_type_field -> class_type_field
    method constructor_arguments: constructor_arguments -> constructor_arguments
    method constructor_declaration: constructor_declaration -> constructor_declaration
    method expr: expression -> expression
    method extension: extension -> extension
    method extension_constructor: extension_constructor -> extension_constructor
    method include_declaration: include_declaration -> include_declaration
    method include_description: include_description -> include_description
    method label_declaration: label_declaration -> label_declaration
    method location: Location.t -> Location.t
    method module_binding: module_binding -> module_binding
    method module_declaration: module_declaration -> module_declaration
    method module_expr: module_expr -> module_expr
    method module_type: module_type -> module_type
    method module_type_declaration: module_type_declaration -> module_type_declaration
    method open_description: open_description -> open_description
    method pat: pattern -> pattern
    method payload: payload -> payload
    method signature: signature -> signature
    method signature_item: signature_item -> signature_item
    method structure: structure -> structure
    method structure_item: structure_item -> structure_item
    method typ: core_type -> core_type
    method type_declaration: type_declaration -> type_declaration
    method type_extension: type_extension -> type_extension
    method type_kind: type_kind -> type_kind
    method value_binding: value_binding -> value_binding
    method value_description: value_description -> value_description
    method with_constraint: with_constraint -> with_constraint
  end

val to_mapper: #mapper -> Ast_mapper.mapper
(** The resulting mapper is "closed", i.e. methods ignore
    their first argument. *)
