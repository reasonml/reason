(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Migrate_parsetree_406_405_migrate

(*$ open Printf
  let fields = [
    "attribute"; "attributes"; "case"; "cases"; "class_declaration";
    "class_description"; "class_expr"; "class_field"; "class_signature";
    "class_structure"; "class_type"; "class_type_declaration";
    "class_type_field"; "constructor_declaration"; "expr"; "extension";
    "extension_constructor"; "include_declaration"; "include_description";
    "label_declaration"; "location"; "module_binding"; "module_declaration";
    "module_expr"; "module_type"; "module_type_declaration";
    "open_description"; "pat"; "signature"; "signature_item"; "structure";
    "structure_item"; "typ"; "type_declaration"; "type_extension";
    "type_kind"; "value_binding"; "value_description";
    "with_constraint"; "payload"
  ]
  let foreach_field f =
    printf "\n";
    List.iter f fields
*)(*$*)

let copy_mapper = fun
  ({ From.Ast_mapper.
     (*$ foreach_field (printf "%s;\n")*)
     attribute;
     attributes;
     case;
     cases;
     class_declaration;
     class_description;
     class_expr;
     class_field;
     class_signature;
     class_structure;
     class_type;
     class_type_declaration;
     class_type_field;
     constructor_declaration;
     expr;
     extension;
     extension_constructor;
     include_declaration;
     include_description;
     label_declaration;
     location;
     module_binding;
     module_declaration;
     module_expr;
     module_type;
     module_type_declaration;
     open_description;
     pat;
     signature;
     signature_item;
     structure;
     structure_item;
     typ;
     type_declaration;
     type_extension;
     type_kind;
     value_binding;
     value_description;
     with_constraint;
     payload;
     (*$*)
   } as mapper) ->
  let module R = Migrate_parsetree_405_406_migrate in
  {
    To.Ast_mapper.
    (*$ foreach_field (fun s ->
        printf
          "%s = (fun _ x -> copy_%s (%s mapper (R.copy_%s x)));\n" s s s s)
    *)
    attribute = (fun _ x -> copy_attribute (attribute mapper (R.copy_attribute x)));
    attributes = (fun _ x -> copy_attributes (attributes mapper (R.copy_attributes x)));
    case = (fun _ x -> copy_case (case mapper (R.copy_case x)));
    cases = (fun _ x -> copy_cases (cases mapper (R.copy_cases x)));
    class_declaration = (fun _ x -> copy_class_declaration (class_declaration mapper (R.copy_class_declaration x)));
    class_description = (fun _ x -> copy_class_description (class_description mapper (R.copy_class_description x)));
    class_expr = (fun _ x -> copy_class_expr (class_expr mapper (R.copy_class_expr x)));
    class_field = (fun _ x -> copy_class_field (class_field mapper (R.copy_class_field x)));
    class_signature = (fun _ x -> copy_class_signature (class_signature mapper (R.copy_class_signature x)));
    class_structure = (fun _ x -> copy_class_structure (class_structure mapper (R.copy_class_structure x)));
    class_type = (fun _ x -> copy_class_type (class_type mapper (R.copy_class_type x)));
    class_type_declaration = (fun _ x -> copy_class_type_declaration (class_type_declaration mapper (R.copy_class_type_declaration x)));
    class_type_field = (fun _ x -> copy_class_type_field (class_type_field mapper (R.copy_class_type_field x)));
    constructor_declaration = (fun _ x -> copy_constructor_declaration (constructor_declaration mapper (R.copy_constructor_declaration x)));
    expr = (fun _ x -> copy_expr (expr mapper (R.copy_expr x)));
    extension = (fun _ x -> copy_extension (extension mapper (R.copy_extension x)));
    extension_constructor = (fun _ x -> copy_extension_constructor (extension_constructor mapper (R.copy_extension_constructor x)));
    include_declaration = (fun _ x -> copy_include_declaration (include_declaration mapper (R.copy_include_declaration x)));
    include_description = (fun _ x -> copy_include_description (include_description mapper (R.copy_include_description x)));
    label_declaration = (fun _ x -> copy_label_declaration (label_declaration mapper (R.copy_label_declaration x)));
    location = (fun _ x -> copy_location (location mapper (R.copy_location x)));
    module_binding = (fun _ x -> copy_module_binding (module_binding mapper (R.copy_module_binding x)));
    module_declaration = (fun _ x -> copy_module_declaration (module_declaration mapper (R.copy_module_declaration x)));
    module_expr = (fun _ x -> copy_module_expr (module_expr mapper (R.copy_module_expr x)));
    module_type = (fun _ x -> copy_module_type (module_type mapper (R.copy_module_type x)));
    module_type_declaration = (fun _ x -> copy_module_type_declaration (module_type_declaration mapper (R.copy_module_type_declaration x)));
    open_description = (fun _ x -> copy_open_description (open_description mapper (R.copy_open_description x)));
    pat = (fun _ x -> copy_pat (pat mapper (R.copy_pat x)));
    signature = (fun _ x -> copy_signature (signature mapper (R.copy_signature x)));
    signature_item = (fun _ x -> copy_signature_item (signature_item mapper (R.copy_signature_item x)));
    structure = (fun _ x -> copy_structure (structure mapper (R.copy_structure x)));
    structure_item = (fun _ x -> copy_structure_item (structure_item mapper (R.copy_structure_item x)));
    typ = (fun _ x -> copy_typ (typ mapper (R.copy_typ x)));
    type_declaration = (fun _ x -> copy_type_declaration (type_declaration mapper (R.copy_type_declaration x)));
    type_extension = (fun _ x -> copy_type_extension (type_extension mapper (R.copy_type_extension x)));
    type_kind = (fun _ x -> copy_type_kind (type_kind mapper (R.copy_type_kind x)));
    value_binding = (fun _ x -> copy_value_binding (value_binding mapper (R.copy_value_binding x)));
    value_description = (fun _ x -> copy_value_description (value_description mapper (R.copy_value_description x)));
    with_constraint = (fun _ x -> copy_with_constraint (with_constraint mapper (R.copy_with_constraint x)));
    payload = (fun _ x -> copy_payload (payload mapper (R.copy_payload x)));
    (*$*)
  }
