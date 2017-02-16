module Ppx_deriving :
  sig
    type quoter
  end

val pp_type_of_decl :
  options:(string * Parsetree.expression) list ->
  path:'a -> Parsetree.type_declaration -> Parsetree.core_type

val show_type_of_decl :
  options:(string * Parsetree.expression) list ->
  path:'a -> Parsetree.type_declaration -> Parsetree.core_type

val sig_of_type :
  options:(string * Parsetree.expression) list ->
  path:'a -> Parsetree.type_declaration -> Parsetree.signature_item list

val expr_of_typ :
  Ppx_deriving.quoter -> Parsetree.core_type -> Parsetree.expression

val str_of_type :
  options:(string * Parsetree.expression) list ->
  path:string list ->
  Parsetree.type_declaration -> Parsetree.value_binding list
