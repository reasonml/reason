open Parsetree_plus

let map_head f = function
  | [] -> []
  | hd::tl ->
     (f hd) :: tl

let map_option f = function
  | None -> None
  | Some p ->
     Some (f p)

let to_parsetree_label label = label

let of_parsetree_label label = label

let to_parsetree_rec_flag = function
  | Nonrecursive -> Asttypes.Nonrecursive
  | Recursive -> Asttypes.Recursive

let of_parsetree_rec_flag = function
  | Asttypes.Nonrecursive -> Nonrecursive
  | Asttypes.Recursive -> Recursive

let to_parsetree_direction_flag = function
  | Upto -> Asttypes.Upto
  | Downto -> Asttypes.Downto

let of_parsetree_direction_flag = function
  | Asttypes.Upto -> Upto
  | Asttypes.Downto -> Downto

let to_parsetree_override_flag = function
  | Override -> Asttypes.Override
  | Fresh -> Asttypes.Fresh

let of_parsetree_override_flag = function
  | Asttypes.Override -> Override
  | Asttypes.Fresh -> Fresh

let to_parsetree_closed_flag = function
  | Closed -> Asttypes.Closed
  | Open -> Asttypes.Open

let of_parsetree_closed_flag = function
  | Asttypes.Closed -> Closed
  | Asttypes.Open -> Open

let to_parsetree_private_flag = function
  | Private -> Asttypes.Private
  | Public -> Asttypes.Public

let of_parsetree_private_flag = function
  | Asttypes.Private -> Private
  | Asttypes.Public -> Public

let to_parsetree_mutable_flag = function
  | Immutable -> Asttypes.Immutable
  | Mutable -> Asttypes.Mutable

let of_parsetree_mutable_flag = function
  | Asttypes.Immutable -> Immutable
  | Asttypes.Mutable -> Mutable

let to_parsetree_virtual_flag = function
  | Virtual -> Asttypes.Virtual
  | Concrete -> Asttypes.Concrete

let of_parsetree_virtual_flag = function
  | Asttypes.Virtual -> Virtual
  | Asttypes.Concrete -> Concrete

let to_parsetree_variance = function
  | Covariant -> Asttypes.Covariant
  | Contravariant -> Asttypes.Contravariant
  | Invariant -> Asttypes.Invariant

let of_parsetree_variance = function
  | Asttypes.Covariant -> Covariant
  | Asttypes.Contravariant -> Contravariant
  | Asttypes.Invariant -> Invariant

let to_parsetree_arg_label = function
  | Nolabel -> ""
  | Labelled string -> string
  | Optional string -> "?" ^ string

let of_parsetree_arg_label = function
  | "" -> Nolabel
  | string when string.[0] = '?' -> Optional (String.sub string 1 (String.length string))
  | string -> Labelled string


let rec to_parsetree_attribute (loc, payload) = (loc, to_parsetree_payload payload)

let rec of_parsetree_attribute (loc, payload) = (loc, of_parsetree_payload payload)

and to_parsetree_extension (loc, payload) = (loc, to_parsetree_payload payload)

and of_parsetree_extension (loc, payload) = (loc, of_parsetree_payload payload)

and to_parsetree_attributes attributes = List.map to_parsetree_attribute attributes

and of_parsetree_attributes attributes = List.map of_parsetree_attribute attributes

and to_parsetree_payload = function
  | PStr structure ->
     let open Parsetree in
     PStr (to_parsetree_structure structure)
  | PSig signature -> not_supported ()
  | PTyp core_type ->
     let open Parsetree in
     PTyp (to_parsetree_core_type core_type)
  | PPat (pattern, expression_option) ->
     let open Parsetree in
     PPat (to_parsetree_pattern pattern, map_option to_parsetree_expression expression_option)

and of_parsetree_payload = function
  | Parsetree.PStr structure ->
     PStr (of_parsetree_structure structure)
  | Parsetree.PTyp core_type ->
     PTyp (of_parsetree_core_type core_type)
  | Parsetree.PPat (pattern, expression_option) ->
     PPat (of_parsetree_pattern pattern, map_option of_parsetree_expression expression_option)

and to_parsetree_core_type item = {
    Parsetree.ptyp_desc= to_parsetree_core_type_desc item.ptyp_desc;
    Parsetree.ptyp_loc= item.ptyp_loc;
    Parsetree.ptyp_attributes= to_parsetree_attributes item.ptyp_attributes;
  }

and of_parsetree_core_type item = {
    ptyp_desc= of_parsetree_core_type_desc item.ptyp_desc;
    ptyp_loc= item.ptyp_loc;
    ptyp_attributes= of_parsetree_attributes item.ptyp_attributes;
  }

and to_parsetree_core_type_desc = function
  | Ptyp_any ->
     let open Parsetree in
     Ptyp_any
  | Ptyp_var string ->
     let open Parsetree in
     Ptyp_var string
  | Ptyp_arrow (arg_label, core_type1, core_type2) ->
     let open Parsetree in
     Ptyp_arrow (
         to_parsetree_arg_label arg_label,
         to_parsetree_core_type core_type1,
         to_parsetree_core_type core_type2
       )

  | Ptyp_tuple core_type_list ->
     let open Parsetree in
     Ptyp_tuple (List.map to_parsetree_core_type core_type_list)

  | Ptyp_constr (longident_loc, core_type_list) ->
     let open Parsetree in
     Ptyp_constr (
         longident_loc,
         List.map to_parsetree_core_type core_type_list
       )

  | Ptyp_object (string_loc_attributes_core_type_list, closed_flag) ->
     let open Parsetree in
     Ptyp_object (
         List.map (
             fun (string_loc, attributes, core_type) ->
             (string_loc.txt,
              to_parsetree_attributes attributes,
              to_parsetree_core_type core_type)
           )
                  string_loc_attributes_core_type_list,
         to_parsetree_closed_flag closed_flag
       )

  | Ptyp_class (longident_loc, core_type_list) ->
     let open Parsetree in
     Ptyp_class (longident_loc, List.map to_parsetree_core_type core_type_list)

  | Ptyp_alias (core_type, string) ->
     let open Parsetree in
     Ptyp_alias (to_parsetree_core_type core_type, string)

  | Ptyp_variant (row_field_list, closed_flag, label_list_option) ->
     let open Parsetree in
     Ptyp_variant (
         List.map to_parsetree_row_field row_field_list,
         to_parsetree_closed_flag closed_flag,
         label_list_option
       )

  | Ptyp_poly (string_loc_list, core_type) ->
     let open Parsetree in
     Ptyp_poly (
         List.map (fun string_loc -> string_loc.txt) string_loc_list,
         to_parsetree_core_type core_type
       )

  | Ptyp_package package_type ->
     let open Parsetree in
     Ptyp_package (to_parsetree_package_type package_type)
  | Ptyp_extension extension ->
     let open Parsetree in
     Ptyp_extension (to_parsetree_extension extension)
        (* [%id] *)


and of_parsetree_core_type_desc = function
  | Parsetree.Ptyp_any ->
     Ptyp_any
  | Parsetree.Ptyp_var string ->
     Ptyp_var string
  | Parsetree.Ptyp_arrow (arg_label, core_type1, core_type2) ->
     Ptyp_arrow (
         of_parsetree_arg_label arg_label,
         of_parsetree_core_type core_type1,
         of_parsetree_core_type core_type2
       )

  | Parsetree.Ptyp_tuple core_type_list ->
     Ptyp_tuple (List.map of_parsetree_core_type core_type_list)

  | Parsetree.Ptyp_constr (longident_loc, core_type_list) ->
     Ptyp_constr (
         longident_loc,
         List.map of_parsetree_core_type core_type_list
       )

  | Parsetree.Ptyp_object (string_loc_attributes_core_type_list, closed_flag) ->
     Ptyp_object (
         List.map (
             fun (string_loc, attributes, core_type) ->
             (string_loc.txt,
              of_parsetree_attributes attributes,
              of_parsetree_core_type core_type)
           )
                  string_loc_attributes_core_type_list,
         of_parsetree_closed_flag closed_flag
       )

  | Parsetree.Ptyp_class (longident_loc, core_type_list) ->
     Ptyp_class (longident_loc, List.map of_parsetree_core_type core_type_list)

  | Parsetree.Ptyp_alias (core_type, string) ->
     Ptyp_alias (of_parsetree_core_type core_type, string)

  | Parsetree.Ptyp_variant (row_field_list, closed_flag, label_list_option) ->
     Ptyp_variant (
         List.map of_parsetree_row_field row_field_list,
         of_parsetree_closed_flag closed_flag,
         label_list_option
       )

  | Parsetree.Ptyp_poly (string_loc_list, core_type) ->
     Ptyp_poly (
         List.map (fun string_loc -> string_loc.txt) string_loc_list,
         of_parsetree_core_type core_type
       )

  | Parsetree.Ptyp_package package_type ->
     Ptyp_package (of_parsetree_package_type package_type)
  | Parsetree.Ptyp_extension extension ->
     Ptyp_extension (of_parsetree_extension extension)
        (* [%id] *)

and to_parsetree_expression item = {
    Parsetree.pexp_desc= to_parsetree_expression_desc item.pexp_desc;
    Parsetree.pexp_loc= item.pexp_loc;
    Parsetree.pexp_attributes= to_parsetree_attributes item.pexp_attributes;
  }

and of_parsetree_expression item = {
    pexp_desc= of_parsetree_expression_desc item.pexp_desc;
    pexp_loc= item.pexp_loc;
    pexp_attributes= of_parsetree_attributes item.pexp_attributes;
  }

and to_parsetree_constant = function
  | Pconst_integer (i,None) ->
     let open Asttypes in
     Const_int (Int_literal_converter.int i)
  | Pconst_integer (i,Some 'l') ->
     let open Asttypes in
     Const_int32 (Int_literal_converter.int32 i)
  | Pconst_integer (i,Some 'L') ->
     let open Asttypes in
     Const_int64 (Int_literal_converter.int64 i)
  | Pconst_integer (i,Some 'n') ->
     let open Asttypes in
     Const_nativeint (Int_literal_converter.nativeint i)
  | Pconst_integer (i,Some c) ->
     raise (Unknown_literal (i, c))
  | Pconst_char c ->
     let open Asttypes in
     Const_char c
  | Pconst_string (s,d) ->
     let open Asttypes in
     Const_string (s,d)
  | Pconst_float (f,None)->
     let open Asttypes in
     Const_float f
  | Pconst_float (f,Some c) ->
     raise (Unknown_literal (f, c))

and to_parsetree_expression_desc = function
  | Pexp_ident (longident_loc) ->
     let open Parsetree in
     Pexp_ident (longident_loc)

  | Pexp_constant constant ->
     let open Parsetree in
     Pexp_constant (to_parsetree_constant constant)

  | Pexp_let (rec_flag, value_binding_list, expression) ->
     let open Parsetree in
     Pexp_let (to_parsetree_rec_flag rec_flag,
               List.map to_parsetree_value_binding value_binding_list,
               to_parsetree_expression expression)

  | Pexp_function case_list ->
     let open Parsetree in
     Pexp_function (List.map to_parsetree_case case_list)

  | Pexp_fun (arg_label, expression_option, pattern, expression) ->
     let open Parsetree in
     Pexp_fun (to_parsetree_arg_label arg_label,
               map_option to_parsetree_expression expression_option,
               to_parsetree_pattern pattern,
               to_parsetree_expression expression)

  | Pexp_apply (expression, arg_label_expression_list) ->
     let open Parsetree in
     Pexp_apply (to_parsetree_expression expression,
                 List.map (fun (arg_label, expression) ->
                     (to_parsetree_arg_label arg_label,
                      to_parsetree_expression expression))
                          arg_label_expression_list)

  | Pexp_match (expression, case_list) ->
     let open Parsetree in
     Pexp_match (to_parsetree_expression expression, List.map to_parsetree_case case_list)

  | Pexp_try (expression, case_list) ->
     let open Parsetree in
     Pexp_try (to_parsetree_expression expression, List.map to_parsetree_case case_list)

  | Pexp_tuple expression_list ->
     let open Parsetree in
     Pexp_tuple (List.map to_parsetree_expression expression_list)

  | Pexp_construct (longident_loc, expression_option) ->
     let open Parsetree in
     Pexp_construct (longident_loc, map_option to_parsetree_expression expression_option)

  | Pexp_variant (label, expression_option) ->
     let open Parsetree in
     Pexp_variant (label, map_option to_parsetree_expression expression_option)

  | Pexp_record (longident_loc_expression_list, expression_option) ->
     let open Parsetree in
     Pexp_record (List.map (fun (longident_loc, expression) ->
                     (longident_loc,
                      to_parsetree_expression expression))
                           longident_loc_expression_list,
                  map_option to_parsetree_expression expression_option)

  | Pexp_field (expression, longident_loc) ->
     let open Parsetree in
     Pexp_field (to_parsetree_expression expression, longident_loc)

  | Pexp_setfield (expression1, longident_loc, expression2) ->
     let open Parsetree in
     Pexp_setfield (to_parsetree_expression expression1, longident_loc, to_parsetree_expression expression2)

  | Pexp_array expression_list ->
     let open Parsetree in
     Pexp_array (List.map to_parsetree_expression expression_list)

  | Pexp_ifthenelse (expression1, expression2, expression_option) ->
     let open Parsetree in
     Pexp_ifthenelse (to_parsetree_expression expression1,
                      to_parsetree_expression expression2,
                      map_option to_parsetree_expression expression_option)

  | Pexp_sequence (expression1, expression2) ->
     let open Parsetree in
     Pexp_sequence (to_parsetree_expression expression1,
                    to_parsetree_expression expression2)

  | Pexp_while (expression1, expression2) ->
     let open Parsetree in
     Pexp_while (to_parsetree_expression expression1,
                 to_parsetree_expression expression2)

  | Pexp_for (pattern, expression1, expression2, direction_flag, expression3) ->
     let open Parsetree in
     Pexp_for (to_parsetree_pattern pattern,
               to_parsetree_expression expression1,
               to_parsetree_expression expression2,
               to_parsetree_direction_flag direction_flag,
               to_parsetree_expression expression3)

  | Pexp_constraint (expression, core_type) ->
     let open Parsetree in
     Pexp_constraint (to_parsetree_expression expression, to_parsetree_core_type core_type)

  | Pexp_coerce (expression, core_type_option, core_type) ->
     let open Parsetree in
     Pexp_coerce (to_parsetree_expression expression,
                  map_option to_parsetree_core_type core_type_option,
                  to_parsetree_core_type core_type)


  | Pexp_send (expression, string_loc) ->
     let open Parsetree in
     Pexp_send (to_parsetree_expression expression, string_loc.txt)

  | Pexp_new (longident_loc) ->
     let open Parsetree in
     Pexp_new longident_loc

  | Pexp_setinstvar (string_loc, expression) ->
     let open Parsetree in
     Pexp_setinstvar (string_loc, to_parsetree_expression expression)

  | Pexp_override string_loc_expression_list ->
     let open Parsetree in
     Pexp_override (List.map (fun (string_loc, expression) ->
                        (string_loc,
                         to_parsetree_expression expression))
                             string_loc_expression_list)


  | Pexp_letmodule (string_loc, module_expr, expression) ->
     let open Parsetree in
     Pexp_letmodule (string_loc,
                     to_parsetree_module_expr module_expr,
                     to_parsetree_expression expression)

  | Pexp_letexception (extension_constructor, expression) ->
     not_supported ()

  | Pexp_assert expression ->
     let open Parsetree in
     Pexp_assert (to_parsetree_expression expression)

  | Pexp_lazy expression ->
     let open Parsetree in
     Pexp_lazy (to_parsetree_expression expression)

  | Pexp_poly (expression, core_type_option) ->
     let open Parsetree in
     Pexp_poly (to_parsetree_expression expression,
                map_option to_parsetree_core_type core_type_option)

  | Pexp_object class_structure ->
     let open Parsetree in
     Pexp_object (to_parsetree_class_structure class_structure)

  | Pexp_newtype (string_loc, expression) ->
     let open Parsetree in
     Pexp_newtype (string_loc.txt, to_parsetree_expression expression)


  | Pexp_pack module_expr ->
     let open Parsetree in
     Pexp_pack (to_parsetree_module_expr module_expr)

  | Pexp_open (override_flag, longident_loc, expression) ->
     let open Parsetree in
     Pexp_open (to_parsetree_override_flag override_flag,
                longident_loc,
                to_parsetree_expression expression)

  | Pexp_extension extension ->
     let open Parsetree in
     Pexp_extension (to_parsetree_extension extension)

  | Pexp_unreachable ->
     not_supported ()

and to_parsetree_case item = {
    Parsetree.pc_lhs= to_parsetree_pattern item.pc_lhs;
    Parsetree.pc_guard= map_option to_parsetree_expression item.pc_guard;
    Parsetree.pc_rhs= to_parsetree_expression item.pc_rhs;
  }

and to_parsetree_value_description item = {
    Parsetree.pval_name= item.pval_name;
    Parsetree.pval_type= to_parsetree_core_type item.pval_type;
    Parsetree.pval_prim= item.pval_prim;
    Parsetree.pval_attributes= to_parsetree_attributes item.pval_attributes;
    Parsetree.pval_loc= item.pval_loc;
  }

and to_parsetree_type_declaration item = {
    Parsetree.ptype_name= item.ptype_name;
    Parsetree.ptype_params=
      List.map (fun (core_type, variance) -> (
                  to_parsetree_core_type core_type,
                  to_parsetree_variance variance)
        ) item.ptype_params;
    Parsetree.ptype_cstrs=
      List.map (fun (core_type1, core_type2, location) -> (
                  to_parsetree_core_type core_type1,
                  to_parsetree_core_type core_type2,
                  location
                )
        ) item.ptype_cstrs;
    Parsetree.ptype_kind= to_parsetree_type_kind item.ptype_kind;
    Parsetree.ptype_private= to_parsetree_private_flag item.ptype_private;
    Parsetree.ptype_manifest= map_option to_parsetree_core_type item.ptype_manifest;
    Parsetree.ptype_attributes= to_parsetree_attributes item.ptype_attributes;
    Parsetree.ptype_loc= item.ptype_loc;
  }

and to_parsetree_type_kind = function
  | Ptype_abstract ->
     let open Parsetree in
     Ptype_abstract
  | Ptype_variant constructor_declaration_list ->
     let open Parsetree in
     Ptype_variant (List.map to_parsetree_constructor_declaration constructor_declaration_list)

  | Ptype_record label_declaration_list ->
     let open Parsetree in
     Ptype_record (List.map to_parsetree_label_declaration label_declaration_list)
        (* Invariant: non-empty list *)
  | Ptype_open ->
     let open Parsetree in
     Ptype_open

and to_parsetree_label_declaration item = {
     Parsetree.pld_name= item.pld_name;
     Parsetree.pld_mutable= to_parsetree_mutable_flag item.pld_mutable;
     Parsetree.pld_type= to_parsetree_core_type item.pld_type;
     Parsetree.pld_loc= item.pld_loc;
     Parsetree.pld_attributes= to_parsetree_attributes item.pld_attributes;
    }

and to_parsetree_constructor_declaration item = {
     Parsetree.pcd_name= item.pcd_name;
     Parsetree.pcd_args= to_parsetree_constructor_arguments item.pcd_args;
     Parsetree.pcd_res= map_option to_parsetree_core_type item.pcd_res;
     Parsetree.pcd_loc= item.pcd_loc;
     Parsetree.pcd_attributes= to_parsetree_attributes item.pcd_attributes;
    }

and to_parsetree_constructor_arguments = function
  | Pcstr_tuple core_type_list ->
     let open Parsetree in
     List.map to_parsetree_core_type core_type_list
  | Pcstr_record label_declaration_list ->
     not_supported ()

and to_parsetree_package_type (location_loc, longident_loc_core_type_list)= (
  location_loc,
  List.map (fun (longident_loc, core_type)-> (
              longident_loc,
              to_parsetree_core_type core_type
            )) longident_loc_core_type_list)

and to_parsetree_row_field = function
  | Rtag (label, attributes, bool, core_type_list) ->
     let open Parsetree in
     Rtag (to_parsetree_label label,
           to_parsetree_attributes attributes,
           bool,
           List.map to_parsetree_core_type core_type_list
          )
  | Rinherit core_type ->
     let open Parsetree in
     Rinherit (to_parsetree_core_type core_type)

and to_parsetree_pattern item = {
    Parsetree.ppat_desc= to_parsetree_pattern_desc item.ppat_desc;
    Parsetree.ppat_loc= item.ppat_loc;
    Parsetree.ppat_attributes= to_parsetree_attributes item.ppat_attributes;
  }

and to_parsetree_pattern_desc = function
  | Ppat_any ->
     let open Parsetree in
     Ppat_any

  | Ppat_var string_loc ->
     let open Parsetree in
     Ppat_var string_loc

  | Ppat_alias (pattern, string_loc) ->
     let open Parsetree in
     Ppat_alias (to_parsetree_pattern pattern,
                 string_loc
                )

  | Ppat_constant constant ->
     let open Parsetree in
     Ppat_constant (to_parsetree_constant constant)

  | Ppat_interval (constant1, constant2) ->
     let open Parsetree in
     Ppat_interval (to_parsetree_constant constant1, to_parsetree_constant constant2)

  | Ppat_tuple pattern_list ->
     let open Parsetree in
     Ppat_tuple (List.map to_parsetree_pattern pattern_list)

  | Ppat_construct (longident_loc, pattern_option) ->
     let open Parsetree in
     Ppat_construct (longident_loc, map_option to_parsetree_pattern pattern_option)

  | Ppat_variant (label, pattern_option) ->
     let open Parsetree in
     Ppat_variant (to_parsetree_label label, map_option to_parsetree_pattern pattern_option)

  | Ppat_record (longident_loc_pattern_list, closed_flag) ->
     let open Parsetree in
     Ppat_record (
         List.map (fun (longident_loc, pattern) ->
             (longident_loc, to_parsetree_pattern pattern)
           ) longident_loc_pattern_list,
         to_parsetree_closed_flag closed_flag
       )

  | Ppat_array pattern_list ->
     let open Parsetree in
     Ppat_array (List.map to_parsetree_pattern pattern_list)

  | Ppat_or (pattern1, pattern2) ->
     let open Parsetree in
     Ppat_or (to_parsetree_pattern pattern1,
              to_parsetree_pattern pattern2
             )

  | Ppat_constraint (pattern, core_type) ->
     let open Parsetree in
     Ppat_constraint (to_parsetree_pattern pattern,
              to_parsetree_core_type core_type)

  | Ppat_type longident_loc ->
     let open Parsetree in
     Ppat_type (longident_loc)

  | Ppat_lazy pattern ->
     let open Parsetree in
     Ppat_lazy (to_parsetree_pattern pattern)

  | Ppat_unpack string_loc ->
     let open Parsetree in
     Ppat_unpack string_loc

  | Ppat_exception pattern ->
     let open Parsetree in
     Ppat_exception (to_parsetree_pattern pattern)

  | Ppat_extension extension ->
     let open Parsetree in
     Ppat_extension (to_parsetree_extension extension)

  | Ppat_open (longident_loc, pattern) ->
     not_supported ()

and to_parsetree_type_extension item = {
    Parsetree.ptyext_path= item.ptyext_path;
    Parsetree.ptyext_params= List.map (fun (core_type, variance) ->
                                 (to_parsetree_core_type core_type,
                                  to_parsetree_variance variance))
                                   item.ptyext_params;

    Parsetree.ptyext_constructors=
      List.map to_parsetree_extension_constructor item.ptyext_constructors;

    Parsetree.ptyext_private= to_parsetree_private_flag item.ptyext_private;
    Parsetree.ptyext_attributes= to_parsetree_attributes item.ptyext_attributes;
  }

and to_parsetree_extension_constructor item =  {
    Parsetree.pext_name= item.pext_name;
    Parsetree.pext_kind= to_parsetree_extension_constructor_kind item.pext_kind;
    Parsetree.pext_loc= item.pext_loc;
    Parsetree.pext_attributes= to_parsetree_attributes item.pext_attributes;
  }

and to_parsetree_extension_constructor_kind = function
  | Pext_decl (constructor_arguments, core_type_option) ->
     let open Parsetree in
     Pext_decl (to_parsetree_constructor_arguments constructor_arguments,
                map_option to_parsetree_core_type core_type_option)

  | Pext_rebind longident_loc ->
     let open Parsetree in
     Pext_rebind longident_loc

and to_parsetree_class_type item = {
     Parsetree.pcty_desc= to_parsetree_class_type_desc item.pcty_desc;
     Parsetree.pcty_loc= item.pcty_loc;
     Parsetree.pcty_attributes= to_parsetree_attributes item.pcty_attributes;
    }

and to_parsetree_class_type_desc = function
  | Pcty_constr (longident_loc, core_type_list) ->
     let open Parsetree in
     Pcty_constr (longident_loc,
                  List.map to_parsetree_core_type core_type_list
                 )

  | Pcty_signature class_signature ->
     let open Parsetree in
     Pcty_signature (to_parsetree_class_signature class_signature)

  | Pcty_arrow (arg_label, core_type, class_type) ->
     let open Parsetree in
     Pcty_arrow (to_parsetree_arg_label arg_label,
                 to_parsetree_core_type core_type,
                 to_parsetree_class_type class_type)

  | Pcty_extension extension ->
     let open Parsetree in
     Pcty_extension (to_parsetree_extension extension)

and to_parsetree_class_signature item = {
    Parsetree.pcsig_self= to_parsetree_core_type item.pcsig_self;
    Parsetree.pcsig_fields= List.map to_parsetree_class_type_field item.pcsig_fields;
}

and to_parsetree_class_type_field item = {
    Parsetree.pctf_desc= to_parsetree_class_type_field_desc item.pctf_desc;
    Parsetree.pctf_loc= item.pctf_loc;
    Parsetree.pctf_attributes= to_parsetree_attributes item.pctf_attributes;
  }

and to_parsetree_class_type_field_desc = function
  | Pctf_inherit class_type ->
     let open Parsetree in
     Pctf_inherit (to_parsetree_class_type class_type)

  | Pctf_val (string_loc, mutable_flag, virtual_flag, core_type) ->
     let open Parsetree in
     Pctf_val (string_loc.txt,
               to_parsetree_mutable_flag mutable_flag,
               to_parsetree_virtual_flag virtual_flag,
               to_parsetree_core_type core_type
              )

  | Pctf_method (string_loc, private_flag, virtual_flag, core_type) ->
     let open Parsetree in
     Pctf_method (string_loc.txt,
                  to_parsetree_private_flag private_flag,
                  to_parsetree_virtual_flag virtual_flag,
                  to_parsetree_core_type core_type
                 )

  | Pctf_constraint (core_type1, core_type2) ->
     let open Parsetree in
     Pctf_constraint (to_parsetree_core_type core_type1,
                      to_parsetree_core_type core_type2
                     )

  | Pctf_attribute attribute ->
     let open Parsetree in
     Pctf_attribute (to_parsetree_attribute attribute)

  | Pctf_extension extension ->
     let open Parsetree in
     Pctf_extension (to_parsetree_extension extension)

and to_parsetree_class_structure item = {
    Parsetree.pcstr_self= to_parsetree_pattern item.pcstr_self;
    Parsetree.pcstr_fields= List.map to_parsetree_class_field item.pcstr_fields;
  }

and to_parsetree_class_field item = {
     Parsetree.pcf_desc= to_parsetree_class_field_desc item.pcf_desc;
     Parsetree.pcf_loc= item.pcf_loc;
     Parsetree.pcf_attributes= to_parsetree_attributes item.pcf_attributes;
  }

and to_parsetree_class_field_kind = function
  | Cfk_virtual core_type ->
     let open Parsetree in
     Cfk_virtual (to_parsetree_core_type core_type)
  | Cfk_concrete (override_flag, expression) ->
     let open Parsetree in
     Cfk_concrete (to_parsetree_override_flag override_flag,
                   to_parsetree_expression expression)

and to_parsetree_class_field_desc = function
  | Pcf_inherit (override_flag, class_expr, string_loc_option) ->
     let open Parsetree in
     Pcf_inherit (to_parsetree_override_flag override_flag,
                  to_parsetree_class_expr class_expr,
                  map_option (fun str_loc -> str_loc.txt) string_loc_option)
  | Pcf_val (string_loc, mutable_flag, class_field_kind) ->
     let open Parsetree in
     Pcf_val (string_loc,
              to_parsetree_mutable_flag mutable_flag,
              to_parsetree_class_field_kind class_field_kind
             )

  | Pcf_method (string_loc, private_flag, class_field_kind) ->
     let open Parsetree in
     Pcf_method (string_loc,
                 to_parsetree_private_flag private_flag,
                 to_parsetree_class_field_kind class_field_kind
                )

  | Pcf_constraint (core_type1, core_type2) ->
     let open Parsetree in
     Pcf_constraint (to_parsetree_core_type core_type1,
                     to_parsetree_core_type core_type2
                    )

  | Pcf_initializer expression ->
     let open Parsetree in
     Pcf_initializer (to_parsetree_expression expression)

  | Pcf_attribute attribute ->
     let open Parsetree in
     Pcf_attribute (to_parsetree_attribute attribute)

  | Pcf_extension extension ->
     let open Parsetree in
     Pcf_extension (to_parsetree_extension extension)
        (* [%%id] *)

and to_parsetree_module_type_declaration item = {
     Parsetree.pmtd_name= item.pmtd_name;
     Parsetree.pmtd_type= map_option to_parsetree_module_type item.pmtd_type;
     Parsetree.pmtd_attributes= to_parsetree_attributes item.pmtd_attributes;
     Parsetree.pmtd_loc= item.pmtd_loc;
    }
and to_parsetree_open_description item = {
    Parsetree.popen_lid= item.popen_lid;
    Parsetree.popen_override= to_parsetree_override_flag item.popen_override;
    Parsetree.popen_loc= item.popen_loc;
    Parsetree.popen_attributes= to_parsetree_attributes item.popen_attributes;
  }

and to_parsetree_class_type_declaration item = {
     Parsetree.pci_virt= to_parsetree_virtual_flag item.pci_virt;
     Parsetree.pci_params= List.map (fun (core_type, variance) ->
                               to_parsetree_core_type core_type,
                               to_parsetree_variance variance
                             ) item.pci_params;
     Parsetree.pci_name= item.pci_name;
     Parsetree.pci_expr= to_parsetree_class_type item.pci_expr;
     Parsetree.pci_loc= item.pci_loc;
     Parsetree.pci_attributes= to_parsetree_attributes item.pci_attributes;
    }

and to_parsetree_class_description item = to_parsetree_class_type_declaration item

and to_parsetree_class_expr item = {
     Parsetree.pcl_desc= to_parsetree_class_expr_desc item.pcl_desc;
     Parsetree.pcl_loc= item.pcl_loc;
     Parsetree.pcl_attributes= to_parsetree_attributes item.pcl_attributes;
  }

and to_parsetree_class_expr_desc = function
  | Pcl_constr (longident_loc, core_type_list) ->
     let open Parsetree in
     Pcl_constr (longident_loc,
                 List.map to_parsetree_core_type core_type_list
                )

  | Pcl_structure class_structure ->
     let open Parsetree in
     Pcl_structure (to_parsetree_class_structure class_structure)

  | Pcl_fun (arg_label, expression_option, pattern, class_expr) ->
     let open Parsetree in
     Pcl_fun (to_parsetree_arg_label arg_label,
              map_option to_parsetree_expression expression_option,
              to_parsetree_pattern pattern,
              to_parsetree_class_expr class_expr
             )

  | Pcl_apply (class_expr, arg_label_expression_list) ->
     let open Parsetree in
     Pcl_apply (to_parsetree_class_expr class_expr,
                List.map (fun (arg_label, expression)->
                    (to_parsetree_arg_label arg_label,
                     to_parsetree_expression expression
                    )
                  )
                arg_label_expression_list)

  | Pcl_let (rec_flag, value_binding_list, class_expr) ->
     let open Parsetree in
     Pcl_let (to_parsetree_rec_flag rec_flag,
              List.map to_parsetree_value_binding value_binding_list,
              to_parsetree_class_expr class_expr
             )

  | Pcl_constraint (class_expr, class_type) ->
     let open Parsetree in
     Pcl_constraint (to_parsetree_class_expr class_expr,
                     to_parsetree_class_type class_type
                    )

  | Pcl_extension extension ->
     let open Parsetree in
     Pcl_extension (to_parsetree_extension extension)


and to_parsetree_class_declaration item = {
     Parsetree.pci_virt= to_parsetree_virtual_flag item.pci_virt;
     Parsetree.pci_params= List.map (fun (core_type, variance) ->
                               to_parsetree_core_type core_type,
                               to_parsetree_variance variance
                             ) item.pci_params;
     Parsetree.pci_name= item.pci_name;
     Parsetree.pci_expr= to_parsetree_class_expr item.pci_expr;
     Parsetree.pci_loc= item.pci_loc;
     Parsetree.pci_attributes= to_parsetree_attributes item.pci_attributes;
    }


and to_parsetree_module_type item = {
    Parsetree.pmty_desc= to_parsetree_module_type_desc item.pmty_desc;
    Parsetree.pmty_loc= item.pmty_loc;
    Parsetree.pmty_attributes= to_parsetree_attributes item.pmty_attributes;
  }

and to_parsetree_module_type_desc = function
  | Pmty_ident longident_loc ->
     let open Parsetree in
     Pmty_ident longident_loc

  | Pmty_signature signature ->
     let open Parsetree in
     Pmty_signature (to_parsetree_signature signature)

  | Pmty_functor (string_loc, module_type_option, module_type) ->
     let open Parsetree in
     Pmty_functor (string_loc,
                   map_option to_parsetree_module_type module_type_option,
                   to_parsetree_module_type module_type)

  | Pmty_with (module_type, with_constraint_list) ->
     let open Parsetree in
     Pmty_with (
         to_parsetree_module_type module_type,
         List.map to_parsetree_with_constraint with_constraint_list
       )

  | Pmty_typeof module_expr ->
     let open Parsetree in
     Pmty_typeof (to_parsetree_module_expr module_expr)

  | Pmty_extension extension ->
     let open Parsetree in
     Pmty_extension (
         to_parsetree_extension extension
       )
  | Pmty_alias longident_loc ->
     let open Parsetree in
     Pmty_alias longident_loc

and to_parsetree_signature items = List.map to_parsetree_signature_item items

and to_parsetree_signature_item item = {
     Parsetree.psig_desc= to_parsetree_signature_item_desc item.psig_desc;
     Parsetree.psig_loc= item.psig_loc;
    }

and to_parsetree_signature_item_desc = function
  | Psig_value value_description ->
     let open Parsetree in
     Psig_value (to_parsetree_value_description value_description)

  | Psig_type (rec_flag, type_declaration_list) ->
     let open Parsetree in
     Psig_type (List.map to_parsetree_type_declaration type_declaration_list)
  | Psig_typext type_extension ->
     let open Parsetree in
     Psig_typext (to_parsetree_type_extension type_extension)

  | Psig_exception extension_constructor ->
     let open Parsetree in
     Psig_exception (to_parsetree_extension_constructor extension_constructor)

  | Psig_module module_declaration ->
     let open Parsetree in
     Psig_module (to_parsetree_module_declaration module_declaration)

  | Psig_recmodule module_declaration_list ->
     let open Parsetree in
     Psig_recmodule (List.map to_parsetree_module_declaration module_declaration_list)

  | Psig_modtype module_type_declaration ->
     let open Parsetree in
     Psig_modtype (to_parsetree_module_type_declaration module_type_declaration)

  | Psig_open open_description ->
     let open Parsetree in
     Psig_open (to_parsetree_open_description open_description)

  | Psig_include include_description ->
     let open Parsetree in
     Psig_include (to_parsetree_include_description include_description)

  | Psig_class class_description_list ->
     let open Parsetree in
     Psig_class (List.map to_parsetree_class_description class_description_list)

  | Psig_class_type class_type_declaration_list ->
     let open Parsetree in
     Psig_class_type (List.map to_parsetree_class_type_declaration class_type_declaration_list)

  | Psig_attribute attribute ->
     let open Parsetree in
     Psig_attribute (to_parsetree_attribute attribute)

  | Psig_extension (extension, attributes) ->
     let open Parsetree in
     Psig_extension (to_parsetree_extension extension,
                     to_parsetree_attributes attributes
                    )

and to_parsetree_module_declaration item = {
     Parsetree.pmd_name= item.pmd_name;
     Parsetree.pmd_type= to_parsetree_module_type item.pmd_type;
     Parsetree.pmd_attributes= to_parsetree_attributes item.pmd_attributes;
     Parsetree.pmd_loc= item.pmd_loc;
}

and to_parsetree_include_description item = {
     Parsetree.pincl_mod= to_parsetree_module_type item.pincl_mod;
     Parsetree.pincl_loc= item.pincl_loc;
     Parsetree.pincl_attributes= to_parsetree_attributes item.pincl_attributes;
    }

and to_parsetree_include_declaration item = {
     Parsetree.pincl_mod= to_parsetree_module_expr item.pincl_mod;
     Parsetree.pincl_loc= item.pincl_loc;
     Parsetree.pincl_attributes= to_parsetree_attributes item.pincl_attributes;
    }

and to_parsetree_with_constraint = function
  | Pwith_type (longident_loc, type_declaration) ->
     let open Parsetree in
     Pwith_type (longident_loc,
                 to_parsetree_type_declaration type_declaration
                )

  | Pwith_module (longident_loc1, longident_loc2) ->
     let open Parsetree in
     Pwith_module (longident_loc1,
                   longident_loc2
                  )

  | Pwith_typesubst type_declaration ->
     let open Parsetree in
     Pwith_typesubst (to_parsetree_type_declaration type_declaration)

  | Pwith_modsubst (string_loc, longident_loc) ->
     let open Parsetree in
     Pwith_modsubst (string_loc, longident_loc)

and to_parsetree_module_expr item = {
    Parsetree.pmod_desc= to_parsetree_module_expr_desc item.pmod_desc;
    Parsetree.pmod_loc= item.pmod_loc;
    Parsetree.pmod_attributes= to_parsetree_attributes item.pmod_attributes;
  }

and to_parsetree_module_expr_desc = function
  | Pmod_ident longident_loc ->
     let open Parsetree in
     Pmod_ident longident_loc

  | Pmod_structure structure ->
     let open Parsetree in
     Pmod_structure (to_parsetree_structure structure)

  | Pmod_functor (string_loc, module_type_option, module_expr) ->
     let open Parsetree in
     Pmod_functor (string_loc,
                   map_option to_parsetree_module_type module_type_option,
                   to_parsetree_module_expr module_expr)

  | Pmod_apply (module_expr1, module_expr2) ->
     let open Parsetree in
     Pmod_apply (to_parsetree_module_expr module_expr1,
                 to_parsetree_module_expr module_expr2)

  | Pmod_constraint (module_expr, module_type) ->
     let open Parsetree in
     Pmod_constraint (to_parsetree_module_expr module_expr,
                      to_parsetree_module_type module_type)

  | Pmod_unpack expression ->
     let open Parsetree in
     Pmod_unpack (to_parsetree_expression expression)

  | Pmod_extension extension ->
     let open Parsetree in
     Pmod_extension (to_parsetree_extension extension)


and to_parsetree_structure structure: Parsetree.structure =
  List.map to_parsetree_structure_item structure

and to_parsetree_structure_item item = {
    Parsetree.pstr_desc= to_parsetree_item_desc item.pstr_desc;
    Parsetree.pstr_loc= item.pstr_loc;
  }

and to_parsetree_item_desc = function
  | Pstr_eval (expression, attributes) ->
     let open Parsetree in
     Pstr_eval (to_parsetree_expression expression, to_parsetree_attributes attributes)

  | Pstr_value (rec_flag, value_binding_list) ->
     let open Parsetree in
     Pstr_value (to_parsetree_rec_flag rec_flag, List.map to_parsetree_value_binding value_binding_list)

  | Pstr_primitive value_description ->
     let open Parsetree in
     Pstr_primitive (to_parsetree_value_description value_description)

  | Pstr_type (rec_flag, type_declaration_list) ->
     let open Parsetree in
     let add_flag type_declaration =
       let flag = to_parsetree_rec_flag rec_flag in
       {type_declaration with ptype_attributes = add_nonrec flag type_declaration.ptype_attributes}
     in
     Pstr_type (List.map to_parsetree_type_declaration type_declaration_list |> map_head add_flag)

  | Pstr_typext type_extension ->
     let open Parsetree in
     Pstr_typext (to_parsetree_type_extension type_extension)

  | Pstr_exception extension_constructor ->
     let open Parsetree in
     Pstr_exception (to_parsetree_extension_constructor extension_constructor)

  | Pstr_module module_binding ->
     let open Parsetree in
     Pstr_module (to_parsetree_module_binding module_binding)

  | Pstr_recmodule module_binding_list ->
     let open Parsetree in
     Pstr_recmodule (List.map to_parsetree_module_binding module_binding_list)

  | Pstr_modtype module_type_declaration ->
     let open Parsetree in
     Pstr_modtype (to_parsetree_module_type_declaration module_type_declaration)

  | Pstr_open open_description ->
     let open Parsetree in
     Pstr_open (to_parsetree_open_description open_description)

  | Pstr_class class_declaration_list ->
     let open Parsetree in
     Pstr_class (List.map to_parsetree_class_declaration class_declaration_list)

  | Pstr_class_type class_type_declaration_list ->
     let open Parsetree in
     Pstr_class_type (List.map to_parsetree_class_type_declaration class_type_declaration_list)

  | Pstr_include include_declaration ->
     let open Parsetree in
     Pstr_include (to_parsetree_include_declaration include_declaration)

  | Pstr_attribute attribute ->
     let open Parsetree in
     Pstr_attribute (to_parsetree_attribute attribute)

  | Pstr_extension (extension, attributes) ->
     let open Parsetree in
     Pstr_extension (to_parsetree_extension extension, to_parsetree_attributes attributes)

and to_parsetree_value_binding item = {
    Parsetree.pvb_pat= to_parsetree_pattern item.pvb_pat;
    Parsetree.pvb_expr= to_parsetree_expression item.pvb_expr;
    Parsetree.pvb_attributes= to_parsetree_attributes item.pvb_attributes;
    Parsetree.pvb_loc= item.pvb_loc;
  }

and to_parsetree_module_binding item = {
     Parsetree.pmb_name= item.pmb_name;
     Parsetree.pmb_expr= to_parsetree_module_expr item.pmb_expr;
     Parsetree.pmb_attributes= to_parsetree_attributes item.pmb_attributes;
     Parsetree.pmb_loc= item.pmb_loc;
    }

and to_parsetree_toplevel_phrase = function
  | Ptop_def structure ->
     let open Parsetree in
     Ptop_def (to_parsetree_structure structure)

  | Ptop_dir (string, directive_argument) ->
     let open Parsetree in
     Ptop_dir (string, to_parsetree_directive_argument directive_argument)

and to_parsetree_directive_argument = function
  | Pdir_none ->
     let open Parsetree in
     Pdir_none

  | Pdir_string string ->
     let open Parsetree in
     Pdir_string string

  | Pdir_int (i, _) ->
     let open Parsetree in
     Pdir_int (Int_literal_converter.int i)

  | Pdir_ident longident_t ->
     let open Parsetree in
     Pdir_ident longident_t

  | Pdir_bool bool ->
     let open Parsetree in
     Pdir_bool bool
