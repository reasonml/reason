module From = Ast_500
module To = Ast_414

let copy_structure : Ast_500.Parsetree.structure -> Ast_414.Parsetree.structure
    =
 fun x -> x

let copy_signature : Ast_500.Parsetree.signature -> Ast_414.Parsetree.signature
    =
 fun x -> x

let copy_toplevel_phrase :
    Ast_500.Parsetree.toplevel_phrase -> Ast_414.Parsetree.toplevel_phrase =
 fun x -> x

let copy_core_type : Ast_500.Parsetree.core_type -> Ast_414.Parsetree.core_type
    =
 fun x -> x

let copy_expression :
    Ast_500.Parsetree.expression -> Ast_414.Parsetree.expression =
 fun x -> x

let copy_pattern : Ast_500.Parsetree.pattern -> Ast_414.Parsetree.pattern =
 fun x -> x

let copy_case : Ast_500.Parsetree.case -> Ast_414.Parsetree.case = fun x -> x

let copy_type_declaration :
    Ast_500.Parsetree.type_declaration -> Ast_414.Parsetree.type_declaration =
 fun x -> x

let copy_type_extension :
    Ast_500.Parsetree.type_extension -> Ast_414.Parsetree.type_extension =
 fun x -> x

let copy_extension_constructor :
    Ast_500.Parsetree.extension_constructor ->
    Ast_414.Parsetree.extension_constructor =
 fun x -> x

let copy_out_value:
  Ast_500.Outcometree.out_value -> Ast_414.Outcometree.out_value  = fun x -> x

let copy_out_type:
  Ast_500.Outcometree.out_type -> Ast_414.Outcometree.out_type = fun x -> x

let copy_out_class_type:
  Ast_500.Outcometree.out_class_type -> Ast_414.Outcometree.out_class_type = fun x -> x

let copy_out_module_type:
  Ast_500.Outcometree.out_module_type -> Ast_414.Outcometree.out_module_type = fun x -> x

let copy_out_sig_item:
  Ast_500.Outcometree.out_sig_item -> Ast_414.Outcometree.out_sig_item = fun x -> x

let copy_out_type_extension:
  Ast_500.Outcometree.out_type_extension -> Ast_414.Outcometree.out_type_extension = fun x -> x

let copy_out_phrase:
  Ast_500.Outcometree.out_phrase -> Ast_414.Outcometree.out_phrase = fun x -> x

let copy_mapper :
    Ast_500.Ast_mapper.mapper ->
    Ast_414.Ast_mapper.mapper =
  fun x -> x
