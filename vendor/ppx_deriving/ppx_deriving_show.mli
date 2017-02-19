open Ast_404.Parsetree

val str_of_type
  :  options:(string * expression) list
  -> path:string list
  -> type_declaration
  -> value_binding list
