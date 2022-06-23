open Stdlib0
module From = Ast_500
module To = Ast_414
let rec copy_out_type_extension :
  Ast_500.Outcometree.out_type_extension ->
    Ast_414.Outcometree.out_type_extension
  =
  fun
    { Ast_500.Outcometree.otyext_name = otyext_name;
      Ast_500.Outcometree.otyext_params = otyext_params;
      Ast_500.Outcometree.otyext_constructors = otyext_constructors;
      Ast_500.Outcometree.otyext_private = otyext_private }
    ->
    {
      Ast_414.Outcometree.otyext_name = otyext_name;
      Ast_414.Outcometree.otyext_params =
        (List.map (fun x -> x) otyext_params);
      Ast_414.Outcometree.otyext_constructors =
        (List.map copy_out_constructor otyext_constructors);
      Ast_414.Outcometree.otyext_private = (copy_private_flag otyext_private)
    }
and copy_out_phrase :
  Ast_500.Outcometree.out_phrase -> Ast_414.Outcometree.out_phrase =
  function
  | Ast_500.Outcometree.Ophr_eval (x0, x1) ->
      Ast_414.Outcometree.Ophr_eval ((copy_out_value x0), (copy_out_type x1))
  | Ast_500.Outcometree.Ophr_signature x0 ->
      Ast_414.Outcometree.Ophr_signature
        (List.map
           (fun x ->
              let (x0, x1) = x in
              ((copy_out_sig_item x0), (Option.map copy_out_value x1))) x0)
  | Ast_500.Outcometree.Ophr_exception x0 ->
      Ast_414.Outcometree.Ophr_exception
        (let (x0, x1) = x0 in (x0, (copy_out_value x1)))
and copy_out_sig_item :
  Ast_500.Outcometree.out_sig_item -> Ast_414.Outcometree.out_sig_item =
  function
  | Ast_500.Outcometree.Osig_class (x0, x1, x2, x3, x4) ->
      Ast_414.Outcometree.Osig_class
        (x0, x1, (List.map copy_out_type_param x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | Ast_500.Outcometree.Osig_class_type (x0, x1, x2, x3, x4) ->
      Ast_414.Outcometree.Osig_class_type
        (x0, x1, (List.map copy_out_type_param x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | Ast_500.Outcometree.Osig_typext (x0, x1) ->
      Ast_414.Outcometree.Osig_typext
        ((copy_out_extension_constructor x0), (copy_out_ext_status x1))
  | Ast_500.Outcometree.Osig_modtype (x0, x1) ->
      Ast_414.Outcometree.Osig_modtype (x0, (copy_out_module_type x1))
  | Ast_500.Outcometree.Osig_module (x0, x1, x2) ->
      Ast_414.Outcometree.Osig_module
        (x0, (copy_out_module_type x1), (copy_out_rec_status x2))
  | Ast_500.Outcometree.Osig_type (x0, x1) ->
      Ast_414.Outcometree.Osig_type
        ((copy_out_type_decl x0), (copy_out_rec_status x1))
  | Ast_500.Outcometree.Osig_value x0 ->
      Ast_414.Outcometree.Osig_value (copy_out_val_decl x0)
  | Ast_500.Outcometree.Osig_ellipsis -> Ast_414.Outcometree.Osig_ellipsis
and copy_out_val_decl :
  Ast_500.Outcometree.out_val_decl -> Ast_414.Outcometree.out_val_decl =
  fun
    { Ast_500.Outcometree.oval_name = oval_name;
      Ast_500.Outcometree.oval_type = oval_type;
      Ast_500.Outcometree.oval_prims = oval_prims;
      Ast_500.Outcometree.oval_attributes = oval_attributes }
    ->
    {
      Ast_414.Outcometree.oval_name = oval_name;
      Ast_414.Outcometree.oval_type = (copy_out_type oval_type);
      Ast_414.Outcometree.oval_prims = (List.map (fun x -> x) oval_prims);
      Ast_414.Outcometree.oval_attributes =
        (List.map copy_out_attribute oval_attributes)
    }
and copy_out_type_decl :
  Ast_500.Outcometree.out_type_decl -> Ast_414.Outcometree.out_type_decl =
  fun
    { Ast_500.Outcometree.otype_name = otype_name;
      Ast_500.Outcometree.otype_params = otype_params;
      Ast_500.Outcometree.otype_type = otype_type;
      Ast_500.Outcometree.otype_private = otype_private;
      Ast_500.Outcometree.otype_immediate = otype_immediate;
      Ast_500.Outcometree.otype_unboxed = otype_unboxed;
      Ast_500.Outcometree.otype_cstrs = otype_cstrs }
    ->
    {
      Ast_414.Outcometree.otype_name = otype_name;
      Ast_414.Outcometree.otype_params =
        (List.map copy_out_type_param otype_params);
      Ast_414.Outcometree.otype_type = (copy_out_type otype_type);
      Ast_414.Outcometree.otype_private = (copy_private_flag otype_private);
      Ast_414.Outcometree.otype_immediate =
        (copy_Type_immediacy_t otype_immediate);
      Ast_414.Outcometree.otype_unboxed = otype_unboxed;
      Ast_414.Outcometree.otype_cstrs =
        (List.map
           (fun x ->
              let (x0, x1) = x in ((copy_out_type x0), (copy_out_type x1)))
           otype_cstrs)
    }
and copy_Type_immediacy_t :
  Ast_500.Type_immediacy.t -> Ast_414.Type_immediacy.t =
  function
  | Ast_500.Type_immediacy.Unknown -> Ast_414.Type_immediacy.Unknown
  | Ast_500.Type_immediacy.Always -> Ast_414.Type_immediacy.Always
  | Ast_500.Type_immediacy.Always_on_64bits ->
      Ast_414.Type_immediacy.Always_on_64bits
and copy_out_module_type :
  Ast_500.Outcometree.out_module_type -> Ast_414.Outcometree.out_module_type
  =
  function
  | Ast_500.Outcometree.Omty_abstract -> Ast_414.Outcometree.Omty_abstract
  | Ast_500.Outcometree.Omty_functor (x0, x1) ->
      Ast_414.Outcometree.Omty_functor
        ((Option.map
            (fun x ->
               let (x0, x1) = x in
               ((Option.map (fun x -> x) x0), (copy_out_module_type x1))) x0),
          (copy_out_module_type x1))
  | Ast_500.Outcometree.Omty_ident x0 ->
      Ast_414.Outcometree.Omty_ident (copy_out_ident x0)
  | Ast_500.Outcometree.Omty_signature x0 ->
      Ast_414.Outcometree.Omty_signature (List.map copy_out_sig_item x0)
  | Ast_500.Outcometree.Omty_alias x0 ->
      Ast_414.Outcometree.Omty_alias (copy_out_ident x0)
and copy_out_ext_status :
  Ast_500.Outcometree.out_ext_status -> Ast_414.Outcometree.out_ext_status =
  function
  | Ast_500.Outcometree.Oext_first -> Ast_414.Outcometree.Oext_first
  | Ast_500.Outcometree.Oext_next -> Ast_414.Outcometree.Oext_next
  | Ast_500.Outcometree.Oext_exception -> Ast_414.Outcometree.Oext_exception
and copy_out_extension_constructor :
  Ast_500.Outcometree.out_extension_constructor ->
    Ast_414.Outcometree.out_extension_constructor
  =
  fun
    { Ast_500.Outcometree.oext_name = oext_name;
      Ast_500.Outcometree.oext_type_name = oext_type_name;
      Ast_500.Outcometree.oext_type_params = oext_type_params;
      Ast_500.Outcometree.oext_args = oext_args;
      Ast_500.Outcometree.oext_ret_type = oext_ret_type;
      Ast_500.Outcometree.oext_private = oext_private }
    ->
    {
      Ast_414.Outcometree.oext_name = oext_name;
      Ast_414.Outcometree.oext_type_name = oext_type_name;
      Ast_414.Outcometree.oext_type_params =
        (List.map (fun x -> x) oext_type_params);
      Ast_414.Outcometree.oext_args = (List.map copy_out_type oext_args);
      Ast_414.Outcometree.oext_ret_type =
        (Option.map copy_out_type oext_ret_type);
      Ast_414.Outcometree.oext_private = (copy_private_flag oext_private)
    }
and copy_out_rec_status :
  Ast_500.Outcometree.out_rec_status -> Ast_414.Outcometree.out_rec_status =
  function
  | Ast_500.Outcometree.Orec_not -> Ast_414.Outcometree.Orec_not
  | Ast_500.Outcometree.Orec_first -> Ast_414.Outcometree.Orec_first
  | Ast_500.Outcometree.Orec_next -> Ast_414.Outcometree.Orec_next
and copy_out_class_type :
  Ast_500.Outcometree.out_class_type -> Ast_414.Outcometree.out_class_type =
  function
  | Ast_500.Outcometree.Octy_constr (x0, x1) ->
      Ast_414.Outcometree.Octy_constr
        ((copy_out_ident x0), (List.map copy_out_type x1))
  | Ast_500.Outcometree.Octy_arrow (x0, x1, x2) ->
      Ast_414.Outcometree.Octy_arrow
        (x0, (copy_out_type x1), (copy_out_class_type x2))
  | Ast_500.Outcometree.Octy_signature (x0, x1) ->
      Ast_414.Outcometree.Octy_signature
        ((Option.map copy_out_type x0),
          (List.map copy_out_class_sig_item x1))
and copy_out_class_sig_item :
  Ast_500.Outcometree.out_class_sig_item ->
    Ast_414.Outcometree.out_class_sig_item
  =
  function
  | Ast_500.Outcometree.Ocsg_constraint (x0, x1) ->
      Ast_414.Outcometree.Ocsg_constraint
        ((copy_out_type x0), (copy_out_type x1))
  | Ast_500.Outcometree.Ocsg_method (x0, x1, x2, x3) ->
      Ast_414.Outcometree.Ocsg_method (x0, x1, x2, (copy_out_type x3))
  | Ast_500.Outcometree.Ocsg_value (x0, x1, x2, x3) ->
      Ast_414.Outcometree.Ocsg_value (x0, x1, x2, (copy_out_type x3))
and copy_out_type_param :
  Ast_500.Outcometree.out_type_param -> Ast_414.Outcometree.out_type_param =
  fun x ->
    let (x0, x1) = x in
    (x0, (let (x0, x1) = x1 in ((copy_variance x0), (copy_injectivity x1))))
and copy_out_type :
  Ast_500.Outcometree.out_type -> Ast_414.Outcometree.out_type =
  function
  | Ast_500.Outcometree.Otyp_abstract -> Ast_414.Outcometree.Otyp_abstract
  | Ast_500.Outcometree.Otyp_open -> Ast_414.Outcometree.Otyp_open
  | Ast_500.Outcometree.Otyp_alias (x0, x1) ->
      Ast_414.Outcometree.Otyp_alias ((copy_out_type x0), x1)
  | Ast_500.Outcometree.Otyp_arrow (x0, x1, x2) ->
      Ast_414.Outcometree.Otyp_arrow
        (x0, (copy_out_type x1), (copy_out_type x2))
  | Ast_500.Outcometree.Otyp_class (x0, x1, x2) ->
      Ast_414.Outcometree.Otyp_class
        (x0, (copy_out_ident x1), (List.map copy_out_type x2))
  | Ast_500.Outcometree.Otyp_constr (x0, x1) ->
      Ast_414.Outcometree.Otyp_constr
        ((copy_out_ident x0), (List.map copy_out_type x1))
  | Ast_500.Outcometree.Otyp_manifest (x0, x1) ->
      Ast_414.Outcometree.Otyp_manifest
        ((copy_out_type x0), (copy_out_type x1))
  | Ast_500.Outcometree.Otyp_object (x0, x1) ->
      Ast_414.Outcometree.Otyp_object
        ((List.map (fun x -> let (x0, x1) = x in (x0, (copy_out_type x1))) x0),
          (Option.map (fun x -> x) x1))
  | Ast_500.Outcometree.Otyp_record x0 ->
      Ast_414.Outcometree.Otyp_record
        (List.map
           (fun x -> let (x0, x1, x2) = x in (x0, x1, (copy_out_type x2))) x0)
  | Ast_500.Outcometree.Otyp_stuff x0 -> Ast_414.Outcometree.Otyp_stuff x0
  | Ast_500.Outcometree.Otyp_sum x0 ->
      Ast_414.Outcometree.Otyp_sum (List.map copy_out_constructor x0)
  | Ast_500.Outcometree.Otyp_tuple x0 ->
      Ast_414.Outcometree.Otyp_tuple (List.map copy_out_type x0)
  | Ast_500.Outcometree.Otyp_var (x0, x1) ->
      Ast_414.Outcometree.Otyp_var (x0, x1)
  | Ast_500.Outcometree.Otyp_variant (x0, x1, x2, x3) ->
      Ast_414.Outcometree.Otyp_variant
        (x0, (copy_out_variant x1), x2,
          (Option.map (fun x -> List.map (fun x -> x) x) x3))
  | Ast_500.Outcometree.Otyp_poly (x0, x1) ->
      Ast_414.Outcometree.Otyp_poly
        ((List.map (fun x -> x) x0), (copy_out_type x1))
  | Ast_500.Outcometree.Otyp_module (x0, x1) ->
      Ast_414.Outcometree.Otyp_module
        ((copy_out_ident x0),
          (List.map (fun x -> let (x0, x1) = x in (x0, (copy_out_type x1)))
             x1))
  | Ast_500.Outcometree.Otyp_attribute (x0, x1) ->
      Ast_414.Outcometree.Otyp_attribute
        ((copy_out_type x0), (copy_out_attribute x1))
and copy_out_attribute :
  Ast_500.Outcometree.out_attribute -> Ast_414.Outcometree.out_attribute =
  fun { Ast_500.Outcometree.oattr_name = oattr_name } ->
    { Ast_414.Outcometree.oattr_name = oattr_name }
and copy_out_variant :
  Ast_500.Outcometree.out_variant -> Ast_414.Outcometree.out_variant =
  function
  | Ast_500.Outcometree.Ovar_fields x0 ->
      Ast_414.Outcometree.Ovar_fields
        (List.map
           (fun x ->
              let (x0, x1, x2) = x in (x0, x1, (List.map copy_out_type x2)))
           x0)
  | Ast_500.Outcometree.Ovar_typ x0 ->
      Ast_414.Outcometree.Ovar_typ (copy_out_type x0)
and copy_out_constructor :
  Ast_500.Outcometree.out_constructor -> Ast_414.Outcometree.out_constructor
  =
  fun
    { Ast_500.Outcometree.ocstr_name = ocstr_name;
      Ast_500.Outcometree.ocstr_args = ocstr_args;
      Ast_500.Outcometree.ocstr_return_type = ocstr_return_type }
    ->
    {
      Ast_414.Outcometree.ocstr_name = ocstr_name;
      Ast_414.Outcometree.ocstr_args = (List.map copy_out_type ocstr_args);
      Ast_414.Outcometree.ocstr_return_type =
        (Option.map copy_out_type ocstr_return_type)
    }
and copy_out_value :
  Ast_500.Outcometree.out_value -> Ast_414.Outcometree.out_value =
  function
  | Ast_500.Outcometree.Oval_array x0 ->
      Ast_414.Outcometree.Oval_array (List.map copy_out_value x0)
  | Ast_500.Outcometree.Oval_char x0 -> Ast_414.Outcometree.Oval_char x0
  | Ast_500.Outcometree.Oval_constr (x0, x1) ->
      Ast_414.Outcometree.Oval_constr
        ((copy_out_ident x0), (List.map copy_out_value x1))
  | Ast_500.Outcometree.Oval_ellipsis -> Ast_414.Outcometree.Oval_ellipsis
  | Ast_500.Outcometree.Oval_float x0 -> Ast_414.Outcometree.Oval_float x0
  | Ast_500.Outcometree.Oval_int x0 -> Ast_414.Outcometree.Oval_int x0
  | Ast_500.Outcometree.Oval_int32 x0 -> Ast_414.Outcometree.Oval_int32 x0
  | Ast_500.Outcometree.Oval_int64 x0 -> Ast_414.Outcometree.Oval_int64 x0
  | Ast_500.Outcometree.Oval_nativeint x0 ->
      Ast_414.Outcometree.Oval_nativeint x0
  | Ast_500.Outcometree.Oval_list x0 ->
      Ast_414.Outcometree.Oval_list (List.map copy_out_value x0)
  | Ast_500.Outcometree.Oval_printer x0 ->
      Ast_414.Outcometree.Oval_printer x0
  | Ast_500.Outcometree.Oval_record x0 ->
      Ast_414.Outcometree.Oval_record
        (List.map
           (fun x ->
              let (x0, x1) = x in ((copy_out_ident x0), (copy_out_value x1)))
           x0)
  | Ast_500.Outcometree.Oval_string (x0, x1, x2) ->
      Ast_414.Outcometree.Oval_string (x0, x1, (copy_out_string x2))
  | Ast_500.Outcometree.Oval_stuff x0 -> Ast_414.Outcometree.Oval_stuff x0
  | Ast_500.Outcometree.Oval_tuple x0 ->
      Ast_414.Outcometree.Oval_tuple (List.map copy_out_value x0)
  | Ast_500.Outcometree.Oval_variant (x0, x1) ->
      Ast_414.Outcometree.Oval_variant (x0, (Option.map copy_out_value x1))
and copy_out_string :
  Ast_500.Outcometree.out_string -> Ast_414.Outcometree.out_string =
  function
  | Ast_500.Outcometree.Ostr_string -> Ast_414.Outcometree.Ostr_string
  | Ast_500.Outcometree.Ostr_bytes -> Ast_414.Outcometree.Ostr_bytes
and copy_out_ident :
  Ast_500.Outcometree.out_ident -> Ast_414.Outcometree.out_ident =
  function
  | Ast_500.Outcometree.Oide_apply (x0, x1) ->
      Ast_414.Outcometree.Oide_apply
        ((copy_out_ident x0), (copy_out_ident x1))
  | Ast_500.Outcometree.Oide_dot (x0, x1) ->
      Ast_414.Outcometree.Oide_dot ((copy_out_ident x0), x1)
  | Ast_500.Outcometree.Oide_ident x0 ->
      Ast_414.Outcometree.Oide_ident (copy_out_name x0)
and copy_out_name :
  Ast_500.Outcometree.out_name -> Ast_414.Outcometree.out_name =
  fun { Ast_500.Outcometree.printed_name = printed_name } ->
    { Ast_414.Outcometree.printed_name = printed_name }
and copy_toplevel_phrase :
  Ast_500.Parsetree.toplevel_phrase -> Ast_414.Parsetree.toplevel_phrase =
  function
  | Ast_500.Parsetree.Ptop_def x0 ->
      Ast_414.Parsetree.Ptop_def (copy_structure x0)
  | Ast_500.Parsetree.Ptop_dir x0 ->
      Ast_414.Parsetree.Ptop_dir (copy_toplevel_directive x0)
and copy_toplevel_directive :
  Ast_500.Parsetree.toplevel_directive ->
    Ast_414.Parsetree.toplevel_directive
  =
  fun
    { Ast_500.Parsetree.pdir_name = pdir_name;
      Ast_500.Parsetree.pdir_arg = pdir_arg;
      Ast_500.Parsetree.pdir_loc = pdir_loc }
    ->
    {
      Ast_414.Parsetree.pdir_name = (copy_loc (fun x -> x) pdir_name);
      Ast_414.Parsetree.pdir_arg =
        (Option.map copy_directive_argument pdir_arg);
      Ast_414.Parsetree.pdir_loc = (copy_location pdir_loc)
    }
and copy_directive_argument :
  Ast_500.Parsetree.directive_argument ->
    Ast_414.Parsetree.directive_argument
  =
  fun
    { Ast_500.Parsetree.pdira_desc = pdira_desc;
      Ast_500.Parsetree.pdira_loc = pdira_loc }
    ->
    {
      Ast_414.Parsetree.pdira_desc =
        (copy_directive_argument_desc pdira_desc);
      Ast_414.Parsetree.pdira_loc = (copy_location pdira_loc)
    }
and copy_directive_argument_desc :
  Ast_500.Parsetree.directive_argument_desc ->
    Ast_414.Parsetree.directive_argument_desc
  =
  function
  | Ast_500.Parsetree.Pdir_string x0 -> Ast_414.Parsetree.Pdir_string x0
  | Ast_500.Parsetree.Pdir_int (x0, x1) ->
      Ast_414.Parsetree.Pdir_int (x0, (Option.map (fun x -> x) x1))
  | Ast_500.Parsetree.Pdir_ident x0 ->
      Ast_414.Parsetree.Pdir_ident (copy_Longident_t x0)
  | Ast_500.Parsetree.Pdir_bool x0 -> Ast_414.Parsetree.Pdir_bool x0
and copy_expression :
  Ast_500.Parsetree.expression -> Ast_414.Parsetree.expression =
  fun
    { Ast_500.Parsetree.pexp_desc = pexp_desc;
      Ast_500.Parsetree.pexp_loc = pexp_loc;
      Ast_500.Parsetree.pexp_loc_stack = pexp_loc_stack;
      Ast_500.Parsetree.pexp_attributes = pexp_attributes }
    ->
    {
      Ast_414.Parsetree.pexp_desc = (copy_expression_desc pexp_desc);
      Ast_414.Parsetree.pexp_loc = (copy_location pexp_loc);
      Ast_414.Parsetree.pexp_loc_stack = (copy_location_stack pexp_loc_stack);
      Ast_414.Parsetree.pexp_attributes = (copy_attributes pexp_attributes)
    }
and copy_expression_desc :
  Ast_500.Parsetree.expression_desc -> Ast_414.Parsetree.expression_desc =
  function
  | Ast_500.Parsetree.Pexp_ident x0 ->
      Ast_414.Parsetree.Pexp_ident (copy_loc copy_Longident_t x0)
  | Ast_500.Parsetree.Pexp_constant x0 ->
      Ast_414.Parsetree.Pexp_constant (copy_constant x0)
  | Ast_500.Parsetree.Pexp_let (x0, x1, x2) ->
      Ast_414.Parsetree.Pexp_let
        ((copy_rec_flag x0), (List.map copy_value_binding x1),
          (copy_expression x2))
  | Ast_500.Parsetree.Pexp_function x0 ->
      Ast_414.Parsetree.Pexp_function (List.map copy_case x0)
  | Ast_500.Parsetree.Pexp_fun (x0, x1, x2, x3) ->
      Ast_414.Parsetree.Pexp_fun
        ((copy_arg_label x0), (Option.map copy_expression x1),
          (copy_pattern x2), (copy_expression x3))
  | Ast_500.Parsetree.Pexp_apply (x0, x1) ->
      Ast_414.Parsetree.Pexp_apply
        ((copy_expression x0),
          (List.map
             (fun x ->
                let (x0, x1) = x in
                ((copy_arg_label x0), (copy_expression x1))) x1))
  | Ast_500.Parsetree.Pexp_match (x0, x1) ->
      Ast_414.Parsetree.Pexp_match
        ((copy_expression x0), (List.map copy_case x1))
  | Ast_500.Parsetree.Pexp_try (x0, x1) ->
      Ast_414.Parsetree.Pexp_try
        ((copy_expression x0), (List.map copy_case x1))
  | Ast_500.Parsetree.Pexp_tuple x0 ->
      Ast_414.Parsetree.Pexp_tuple (List.map copy_expression x0)
  | Ast_500.Parsetree.Pexp_construct (x0, x1) ->
      Ast_414.Parsetree.Pexp_construct
        ((copy_loc copy_Longident_t x0), (Option.map copy_expression x1))
  | Ast_500.Parsetree.Pexp_variant (x0, x1) ->
      Ast_414.Parsetree.Pexp_variant
        ((copy_label x0), (Option.map copy_expression x1))
  | Ast_500.Parsetree.Pexp_record (x0, x1) ->
      Ast_414.Parsetree.Pexp_record
        ((List.map
            (fun x ->
               let (x0, x1) = x in
               ((copy_loc copy_Longident_t x0), (copy_expression x1))) x0),
          (Option.map copy_expression x1))
  | Ast_500.Parsetree.Pexp_field (x0, x1) ->
      Ast_414.Parsetree.Pexp_field
        ((copy_expression x0), (copy_loc copy_Longident_t x1))
  | Ast_500.Parsetree.Pexp_setfield (x0, x1, x2) ->
      Ast_414.Parsetree.Pexp_setfield
        ((copy_expression x0), (copy_loc copy_Longident_t x1),
          (copy_expression x2))
  | Ast_500.Parsetree.Pexp_array x0 ->
      Ast_414.Parsetree.Pexp_array (List.map copy_expression x0)
  | Ast_500.Parsetree.Pexp_ifthenelse (x0, x1, x2) ->
      Ast_414.Parsetree.Pexp_ifthenelse
        ((copy_expression x0), (copy_expression x1),
          (Option.map copy_expression x2))
  | Ast_500.Parsetree.Pexp_sequence (x0, x1) ->
      Ast_414.Parsetree.Pexp_sequence
        ((copy_expression x0), (copy_expression x1))
  | Ast_500.Parsetree.Pexp_while (x0, x1) ->
      Ast_414.Parsetree.Pexp_while
        ((copy_expression x0), (copy_expression x1))
  | Ast_500.Parsetree.Pexp_for (x0, x1, x2, x3, x4) ->
      Ast_414.Parsetree.Pexp_for
        ((copy_pattern x0), (copy_expression x1), (copy_expression x2),
          (copy_direction_flag x3), (copy_expression x4))
  | Ast_500.Parsetree.Pexp_constraint (x0, x1) ->
      Ast_414.Parsetree.Pexp_constraint
        ((copy_expression x0), (copy_core_type x1))
  | Ast_500.Parsetree.Pexp_coerce (x0, x1, x2) ->
      Ast_414.Parsetree.Pexp_coerce
        ((copy_expression x0), (Option.map copy_core_type x1),
          (copy_core_type x2))
  | Ast_500.Parsetree.Pexp_send (x0, x1) ->
      Ast_414.Parsetree.Pexp_send
        ((copy_expression x0), (copy_loc copy_label x1))
  | Ast_500.Parsetree.Pexp_new x0 ->
      Ast_414.Parsetree.Pexp_new (copy_loc copy_Longident_t x0)
  | Ast_500.Parsetree.Pexp_setinstvar (x0, x1) ->
      Ast_414.Parsetree.Pexp_setinstvar
        ((copy_loc copy_label x0), (copy_expression x1))
  | Ast_500.Parsetree.Pexp_override x0 ->
      Ast_414.Parsetree.Pexp_override
        (List.map
           (fun x ->
              let (x0, x1) = x in
              ((copy_loc copy_label x0), (copy_expression x1))) x0)
  | Ast_500.Parsetree.Pexp_letmodule (x0, x1, x2) ->
      Ast_414.Parsetree.Pexp_letmodule
        ((copy_loc (fun x -> Option.map (fun x -> x) x) x0),
          (copy_module_expr x1), (copy_expression x2))
  | Ast_500.Parsetree.Pexp_letexception (x0, x1) ->
      Ast_414.Parsetree.Pexp_letexception
        ((copy_extension_constructor x0), (copy_expression x1))
  | Ast_500.Parsetree.Pexp_assert x0 ->
      Ast_414.Parsetree.Pexp_assert (copy_expression x0)
  | Ast_500.Parsetree.Pexp_lazy x0 ->
      Ast_414.Parsetree.Pexp_lazy (copy_expression x0)
  | Ast_500.Parsetree.Pexp_poly (x0, x1) ->
      Ast_414.Parsetree.Pexp_poly
        ((copy_expression x0), (Option.map copy_core_type x1))
  | Ast_500.Parsetree.Pexp_object x0 ->
      Ast_414.Parsetree.Pexp_object (copy_class_structure x0)
  | Ast_500.Parsetree.Pexp_newtype (x0, x1) ->
      Ast_414.Parsetree.Pexp_newtype
        ((copy_loc (fun x -> x) x0), (copy_expression x1))
  | Ast_500.Parsetree.Pexp_pack x0 ->
      Ast_414.Parsetree.Pexp_pack (copy_module_expr x0)
  | Ast_500.Parsetree.Pexp_open (x0, x1) ->
      Ast_414.Parsetree.Pexp_open
        ((copy_open_declaration x0), (copy_expression x1))
  | Ast_500.Parsetree.Pexp_letop x0 ->
      Ast_414.Parsetree.Pexp_letop (copy_letop x0)
  | Ast_500.Parsetree.Pexp_extension x0 ->
      Ast_414.Parsetree.Pexp_extension (copy_extension x0)
  | Ast_500.Parsetree.Pexp_unreachable -> Ast_414.Parsetree.Pexp_unreachable
and copy_letop : Ast_500.Parsetree.letop -> Ast_414.Parsetree.letop =
  fun
    { Ast_500.Parsetree.let_ = let_; Ast_500.Parsetree.ands = ands;
      Ast_500.Parsetree.body = body }
    ->
    {
      Ast_414.Parsetree.let_ = (copy_binding_op let_);
      Ast_414.Parsetree.ands = (List.map copy_binding_op ands);
      Ast_414.Parsetree.body = (copy_expression body)
    }
and copy_binding_op :
  Ast_500.Parsetree.binding_op -> Ast_414.Parsetree.binding_op =
  fun
    { Ast_500.Parsetree.pbop_op = pbop_op;
      Ast_500.Parsetree.pbop_pat = pbop_pat;
      Ast_500.Parsetree.pbop_exp = pbop_exp;
      Ast_500.Parsetree.pbop_loc = pbop_loc }
    ->
    {
      Ast_414.Parsetree.pbop_op = (copy_loc (fun x -> x) pbop_op);
      Ast_414.Parsetree.pbop_pat = (copy_pattern pbop_pat);
      Ast_414.Parsetree.pbop_exp = (copy_expression pbop_exp);
      Ast_414.Parsetree.pbop_loc = (copy_location pbop_loc)
    }
and copy_direction_flag :
  Ast_500.Asttypes.direction_flag -> Ast_414.Asttypes.direction_flag =
  function
  | Ast_500.Asttypes.Upto -> Ast_414.Asttypes.Upto
  | Ast_500.Asttypes.Downto -> Ast_414.Asttypes.Downto
and copy_case : Ast_500.Parsetree.case -> Ast_414.Parsetree.case =
  fun
    { Ast_500.Parsetree.pc_lhs = pc_lhs;
      Ast_500.Parsetree.pc_guard = pc_guard;
      Ast_500.Parsetree.pc_rhs = pc_rhs }
    ->
    {
      Ast_414.Parsetree.pc_lhs = (copy_pattern pc_lhs);
      Ast_414.Parsetree.pc_guard = (Option.map copy_expression pc_guard);
      Ast_414.Parsetree.pc_rhs = (copy_expression pc_rhs)
    }
and copy_value_binding :
  Ast_500.Parsetree.value_binding -> Ast_414.Parsetree.value_binding =
  fun
    { Ast_500.Parsetree.pvb_pat = pvb_pat;
      Ast_500.Parsetree.pvb_expr = pvb_expr;
      Ast_500.Parsetree.pvb_attributes = pvb_attributes;
      Ast_500.Parsetree.pvb_loc = pvb_loc }
    ->
    {
      Ast_414.Parsetree.pvb_pat = (copy_pattern pvb_pat);
      Ast_414.Parsetree.pvb_expr = (copy_expression pvb_expr);
      Ast_414.Parsetree.pvb_attributes = (copy_attributes pvb_attributes);
      Ast_414.Parsetree.pvb_loc = (copy_location pvb_loc)
    }
and copy_pattern : Ast_500.Parsetree.pattern -> Ast_414.Parsetree.pattern =
  fun
    { Ast_500.Parsetree.ppat_desc = ppat_desc;
      Ast_500.Parsetree.ppat_loc = ppat_loc;
      Ast_500.Parsetree.ppat_loc_stack = ppat_loc_stack;
      Ast_500.Parsetree.ppat_attributes = ppat_attributes }
    ->
    {
      Ast_414.Parsetree.ppat_desc = (copy_pattern_desc ppat_desc);
      Ast_414.Parsetree.ppat_loc = (copy_location ppat_loc);
      Ast_414.Parsetree.ppat_loc_stack = (copy_location_stack ppat_loc_stack);
      Ast_414.Parsetree.ppat_attributes = (copy_attributes ppat_attributes)
    }
and copy_pattern_desc :
  Ast_500.Parsetree.pattern_desc -> Ast_414.Parsetree.pattern_desc =
  function
  | Ast_500.Parsetree.Ppat_any -> Ast_414.Parsetree.Ppat_any
  | Ast_500.Parsetree.Ppat_var x0 ->
      Ast_414.Parsetree.Ppat_var (copy_loc (fun x -> x) x0)
  | Ast_500.Parsetree.Ppat_alias (x0, x1) ->
      Ast_414.Parsetree.Ppat_alias
        ((copy_pattern x0), (copy_loc (fun x -> x) x1))
  | Ast_500.Parsetree.Ppat_constant x0 ->
      Ast_414.Parsetree.Ppat_constant (copy_constant x0)
  | Ast_500.Parsetree.Ppat_interval (x0, x1) ->
      Ast_414.Parsetree.Ppat_interval
        ((copy_constant x0), (copy_constant x1))
  | Ast_500.Parsetree.Ppat_tuple x0 ->
      Ast_414.Parsetree.Ppat_tuple (List.map copy_pattern x0)
  | Ast_500.Parsetree.Ppat_construct (x0, x1) ->
      Ast_414.Parsetree.Ppat_construct
        ((copy_loc copy_Longident_t x0),
          (Option.map
             (fun x ->
                let (x0, x1) = x in
                ((List.map (fun x -> copy_loc (fun x -> x) x) x0),
                  (copy_pattern x1))) x1))
  | Ast_500.Parsetree.Ppat_variant (x0, x1) ->
      Ast_414.Parsetree.Ppat_variant
        ((copy_label x0), (Option.map copy_pattern x1))
  | Ast_500.Parsetree.Ppat_record (x0, x1) ->
      Ast_414.Parsetree.Ppat_record
        ((List.map
            (fun x ->
               let (x0, x1) = x in
               ((copy_loc copy_Longident_t x0), (copy_pattern x1))) x0),
          (copy_closed_flag x1))
  | Ast_500.Parsetree.Ppat_array x0 ->
      Ast_414.Parsetree.Ppat_array (List.map copy_pattern x0)
  | Ast_500.Parsetree.Ppat_or (x0, x1) ->
      Ast_414.Parsetree.Ppat_or ((copy_pattern x0), (copy_pattern x1))
  | Ast_500.Parsetree.Ppat_constraint (x0, x1) ->
      Ast_414.Parsetree.Ppat_constraint
        ((copy_pattern x0), (copy_core_type x1))
  | Ast_500.Parsetree.Ppat_type x0 ->
      Ast_414.Parsetree.Ppat_type (copy_loc copy_Longident_t x0)
  | Ast_500.Parsetree.Ppat_lazy x0 ->
      Ast_414.Parsetree.Ppat_lazy (copy_pattern x0)
  | Ast_500.Parsetree.Ppat_unpack x0 ->
      Ast_414.Parsetree.Ppat_unpack
        (copy_loc (fun x -> Option.map (fun x -> x) x) x0)
  | Ast_500.Parsetree.Ppat_exception x0 ->
      Ast_414.Parsetree.Ppat_exception (copy_pattern x0)
  | Ast_500.Parsetree.Ppat_extension x0 ->
      Ast_414.Parsetree.Ppat_extension (copy_extension x0)
  | Ast_500.Parsetree.Ppat_open (x0, x1) ->
      Ast_414.Parsetree.Ppat_open
        ((copy_loc copy_Longident_t x0), (copy_pattern x1))
and copy_core_type :
  Ast_500.Parsetree.core_type -> Ast_414.Parsetree.core_type =
  fun
    { Ast_500.Parsetree.ptyp_desc = ptyp_desc;
      Ast_500.Parsetree.ptyp_loc = ptyp_loc;
      Ast_500.Parsetree.ptyp_loc_stack = ptyp_loc_stack;
      Ast_500.Parsetree.ptyp_attributes = ptyp_attributes }
    ->
    {
      Ast_414.Parsetree.ptyp_desc = (copy_core_type_desc ptyp_desc);
      Ast_414.Parsetree.ptyp_loc = (copy_location ptyp_loc);
      Ast_414.Parsetree.ptyp_loc_stack = (copy_location_stack ptyp_loc_stack);
      Ast_414.Parsetree.ptyp_attributes = (copy_attributes ptyp_attributes)
    }
and copy_location_stack :
  Ast_500.Parsetree.location_stack -> Ast_414.Parsetree.location_stack =
  fun x -> List.map copy_location x
and copy_core_type_desc :
  Ast_500.Parsetree.core_type_desc -> Ast_414.Parsetree.core_type_desc =
  function
  | Ast_500.Parsetree.Ptyp_any -> Ast_414.Parsetree.Ptyp_any
  | Ast_500.Parsetree.Ptyp_var x0 -> Ast_414.Parsetree.Ptyp_var x0
  | Ast_500.Parsetree.Ptyp_arrow (x0, x1, x2) ->
      Ast_414.Parsetree.Ptyp_arrow
        ((copy_arg_label x0), (copy_core_type x1), (copy_core_type x2))
  | Ast_500.Parsetree.Ptyp_tuple x0 ->
      Ast_414.Parsetree.Ptyp_tuple (List.map copy_core_type x0)
  | Ast_500.Parsetree.Ptyp_constr (x0, x1) ->
      Ast_414.Parsetree.Ptyp_constr
        ((copy_loc copy_Longident_t x0), (List.map copy_core_type x1))
  | Ast_500.Parsetree.Ptyp_object (x0, x1) ->
      Ast_414.Parsetree.Ptyp_object
        ((List.map copy_object_field x0), (copy_closed_flag x1))
  | Ast_500.Parsetree.Ptyp_class (x0, x1) ->
      Ast_414.Parsetree.Ptyp_class
        ((copy_loc copy_Longident_t x0), (List.map copy_core_type x1))
  | Ast_500.Parsetree.Ptyp_alias (x0, x1) ->
      Ast_414.Parsetree.Ptyp_alias ((copy_core_type x0), x1)
  | Ast_500.Parsetree.Ptyp_variant (x0, x1, x2) ->
      Ast_414.Parsetree.Ptyp_variant
        ((List.map copy_row_field x0), (copy_closed_flag x1),
          (Option.map (fun x -> List.map copy_label x) x2))
  | Ast_500.Parsetree.Ptyp_poly (x0, x1) ->
      Ast_414.Parsetree.Ptyp_poly
        ((List.map (fun x -> copy_loc (fun x -> x) x) x0),
          (copy_core_type x1))
  | Ast_500.Parsetree.Ptyp_package x0 ->
      Ast_414.Parsetree.Ptyp_package (copy_package_type x0)
  | Ast_500.Parsetree.Ptyp_extension x0 ->
      Ast_414.Parsetree.Ptyp_extension (copy_extension x0)
and copy_package_type :
  Ast_500.Parsetree.package_type -> Ast_414.Parsetree.package_type =
  fun x ->
    let (x0, x1) = x in
    ((copy_loc copy_Longident_t x0),
      (List.map
         (fun x ->
            let (x0, x1) = x in
            ((copy_loc copy_Longident_t x0), (copy_core_type x1))) x1))
and copy_row_field :
  Ast_500.Parsetree.row_field -> Ast_414.Parsetree.row_field =
  fun
    { Ast_500.Parsetree.prf_desc = prf_desc;
      Ast_500.Parsetree.prf_loc = prf_loc;
      Ast_500.Parsetree.prf_attributes = prf_attributes }
    ->
    {
      Ast_414.Parsetree.prf_desc = (copy_row_field_desc prf_desc);
      Ast_414.Parsetree.prf_loc = (copy_location prf_loc);
      Ast_414.Parsetree.prf_attributes = (copy_attributes prf_attributes)
    }
and copy_row_field_desc :
  Ast_500.Parsetree.row_field_desc -> Ast_414.Parsetree.row_field_desc =
  function
  | Ast_500.Parsetree.Rtag (x0, x1, x2) ->
      Ast_414.Parsetree.Rtag
        ((copy_loc copy_label x0), x1, (List.map copy_core_type x2))
  | Ast_500.Parsetree.Rinherit x0 ->
      Ast_414.Parsetree.Rinherit (copy_core_type x0)
and copy_object_field :
  Ast_500.Parsetree.object_field -> Ast_414.Parsetree.object_field =
  fun
    { Ast_500.Parsetree.pof_desc = pof_desc;
      Ast_500.Parsetree.pof_loc = pof_loc;
      Ast_500.Parsetree.pof_attributes = pof_attributes }
    ->
    {
      Ast_414.Parsetree.pof_desc = (copy_object_field_desc pof_desc);
      Ast_414.Parsetree.pof_loc = (copy_location pof_loc);
      Ast_414.Parsetree.pof_attributes = (copy_attributes pof_attributes)
    }
and copy_attributes :
  Ast_500.Parsetree.attributes -> Ast_414.Parsetree.attributes =
  fun x -> List.map copy_attribute x
and copy_attribute :
  Ast_500.Parsetree.attribute -> Ast_414.Parsetree.attribute =
  fun
    { Ast_500.Parsetree.attr_name = attr_name;
      Ast_500.Parsetree.attr_payload = attr_payload;
      Ast_500.Parsetree.attr_loc = attr_loc }
    ->
    {
      Ast_414.Parsetree.attr_name = (copy_loc (fun x -> x) attr_name);
      Ast_414.Parsetree.attr_payload = (copy_payload attr_payload);
      Ast_414.Parsetree.attr_loc = (copy_location attr_loc)
    }
and copy_payload : Ast_500.Parsetree.payload -> Ast_414.Parsetree.payload =
  function
  | Ast_500.Parsetree.PStr x0 -> Ast_414.Parsetree.PStr (copy_structure x0)
  | Ast_500.Parsetree.PSig x0 -> Ast_414.Parsetree.PSig (copy_signature x0)
  | Ast_500.Parsetree.PTyp x0 -> Ast_414.Parsetree.PTyp (copy_core_type x0)
  | Ast_500.Parsetree.PPat (x0, x1) ->
      Ast_414.Parsetree.PPat
        ((copy_pattern x0), (Option.map copy_expression x1))
and copy_structure :
  Ast_500.Parsetree.structure -> Ast_414.Parsetree.structure =
  fun x -> List.map copy_structure_item x
and copy_structure_item :
  Ast_500.Parsetree.structure_item -> Ast_414.Parsetree.structure_item =
  fun
    { Ast_500.Parsetree.pstr_desc = pstr_desc;
      Ast_500.Parsetree.pstr_loc = pstr_loc }
    ->
    {
      Ast_414.Parsetree.pstr_desc = (copy_structure_item_desc pstr_desc);
      Ast_414.Parsetree.pstr_loc = (copy_location pstr_loc)
    }
and copy_structure_item_desc :
  Ast_500.Parsetree.structure_item_desc ->
    Ast_414.Parsetree.structure_item_desc
  =
  function
  | Ast_500.Parsetree.Pstr_eval (x0, x1) ->
      Ast_414.Parsetree.Pstr_eval
        ((copy_expression x0), (copy_attributes x1))
  | Ast_500.Parsetree.Pstr_value (x0, x1) ->
      Ast_414.Parsetree.Pstr_value
        ((copy_rec_flag x0), (List.map copy_value_binding x1))
  | Ast_500.Parsetree.Pstr_primitive x0 ->
      Ast_414.Parsetree.Pstr_primitive (copy_value_description x0)
  | Ast_500.Parsetree.Pstr_type (x0, x1) ->
      Ast_414.Parsetree.Pstr_type
        ((copy_rec_flag x0), (List.map copy_type_declaration x1))
  | Ast_500.Parsetree.Pstr_typext x0 ->
      Ast_414.Parsetree.Pstr_typext (copy_type_extension x0)
  | Ast_500.Parsetree.Pstr_exception x0 ->
      Ast_414.Parsetree.Pstr_exception (copy_type_exception x0)
  | Ast_500.Parsetree.Pstr_module x0 ->
      Ast_414.Parsetree.Pstr_module (copy_module_binding x0)
  | Ast_500.Parsetree.Pstr_recmodule x0 ->
      Ast_414.Parsetree.Pstr_recmodule (List.map copy_module_binding x0)
  | Ast_500.Parsetree.Pstr_modtype x0 ->
      Ast_414.Parsetree.Pstr_modtype (copy_module_type_declaration x0)
  | Ast_500.Parsetree.Pstr_open x0 ->
      Ast_414.Parsetree.Pstr_open (copy_open_declaration x0)
  | Ast_500.Parsetree.Pstr_class x0 ->
      Ast_414.Parsetree.Pstr_class (List.map copy_class_declaration x0)
  | Ast_500.Parsetree.Pstr_class_type x0 ->
      Ast_414.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | Ast_500.Parsetree.Pstr_include x0 ->
      Ast_414.Parsetree.Pstr_include (copy_include_declaration x0)
  | Ast_500.Parsetree.Pstr_attribute x0 ->
      Ast_414.Parsetree.Pstr_attribute (copy_attribute x0)
  | Ast_500.Parsetree.Pstr_extension (x0, x1) ->
      Ast_414.Parsetree.Pstr_extension
        ((copy_extension x0), (copy_attributes x1))
and copy_include_declaration :
  Ast_500.Parsetree.include_declaration ->
    Ast_414.Parsetree.include_declaration
  = fun x -> copy_include_infos copy_module_expr x
and copy_class_declaration :
  Ast_500.Parsetree.class_declaration -> Ast_414.Parsetree.class_declaration
  = fun x -> copy_class_infos copy_class_expr x
and copy_class_expr :
  Ast_500.Parsetree.class_expr -> Ast_414.Parsetree.class_expr =
  fun
    { Ast_500.Parsetree.pcl_desc = pcl_desc;
      Ast_500.Parsetree.pcl_loc = pcl_loc;
      Ast_500.Parsetree.pcl_attributes = pcl_attributes }
    ->
    {
      Ast_414.Parsetree.pcl_desc = (copy_class_expr_desc pcl_desc);
      Ast_414.Parsetree.pcl_loc = (copy_location pcl_loc);
      Ast_414.Parsetree.pcl_attributes = (copy_attributes pcl_attributes)
    }
and copy_class_expr_desc :
  Ast_500.Parsetree.class_expr_desc -> Ast_414.Parsetree.class_expr_desc =
  function
  | Ast_500.Parsetree.Pcl_constr (x0, x1) ->
      Ast_414.Parsetree.Pcl_constr
        ((copy_loc copy_Longident_t x0), (List.map copy_core_type x1))
  | Ast_500.Parsetree.Pcl_structure x0 ->
      Ast_414.Parsetree.Pcl_structure (copy_class_structure x0)
  | Ast_500.Parsetree.Pcl_fun (x0, x1, x2, x3) ->
      Ast_414.Parsetree.Pcl_fun
        ((copy_arg_label x0), (Option.map copy_expression x1),
          (copy_pattern x2), (copy_class_expr x3))
  | Ast_500.Parsetree.Pcl_apply (x0, x1) ->
      Ast_414.Parsetree.Pcl_apply
        ((copy_class_expr x0),
          (List.map
             (fun x ->
                let (x0, x1) = x in
                ((copy_arg_label x0), (copy_expression x1))) x1))
  | Ast_500.Parsetree.Pcl_let (x0, x1, x2) ->
      Ast_414.Parsetree.Pcl_let
        ((copy_rec_flag x0), (List.map copy_value_binding x1),
          (copy_class_expr x2))
  | Ast_500.Parsetree.Pcl_constraint (x0, x1) ->
      Ast_414.Parsetree.Pcl_constraint
        ((copy_class_expr x0), (copy_class_type x1))
  | Ast_500.Parsetree.Pcl_extension x0 ->
      Ast_414.Parsetree.Pcl_extension (copy_extension x0)
  | Ast_500.Parsetree.Pcl_open (x0, x1) ->
      Ast_414.Parsetree.Pcl_open
        ((copy_open_description x0), (copy_class_expr x1))
and copy_class_structure :
  Ast_500.Parsetree.class_structure -> Ast_414.Parsetree.class_structure =
  fun
    { Ast_500.Parsetree.pcstr_self = pcstr_self;
      Ast_500.Parsetree.pcstr_fields = pcstr_fields }
    ->
    {
      Ast_414.Parsetree.pcstr_self = (copy_pattern pcstr_self);
      Ast_414.Parsetree.pcstr_fields =
        (List.map copy_class_field pcstr_fields)
    }
and copy_class_field :
  Ast_500.Parsetree.class_field -> Ast_414.Parsetree.class_field =
  fun
    { Ast_500.Parsetree.pcf_desc = pcf_desc;
      Ast_500.Parsetree.pcf_loc = pcf_loc;
      Ast_500.Parsetree.pcf_attributes = pcf_attributes }
    ->
    {
      Ast_414.Parsetree.pcf_desc = (copy_class_field_desc pcf_desc);
      Ast_414.Parsetree.pcf_loc = (copy_location pcf_loc);
      Ast_414.Parsetree.pcf_attributes = (copy_attributes pcf_attributes)
    }
and copy_class_field_desc :
  Ast_500.Parsetree.class_field_desc -> Ast_414.Parsetree.class_field_desc =
  function
  | Ast_500.Parsetree.Pcf_inherit (x0, x1, x2) ->
      Ast_414.Parsetree.Pcf_inherit
        ((copy_override_flag x0), (copy_class_expr x1),
          (Option.map (fun x -> copy_loc (fun x -> x) x) x2))
  | Ast_500.Parsetree.Pcf_val x0 ->
      Ast_414.Parsetree.Pcf_val
        (let (x0, x1, x2) = x0 in
         ((copy_loc copy_label x0), (copy_mutable_flag x1),
           (copy_class_field_kind x2)))
  | Ast_500.Parsetree.Pcf_method x0 ->
      Ast_414.Parsetree.Pcf_method
        (let (x0, x1, x2) = x0 in
         ((copy_loc copy_label x0), (copy_private_flag x1),
           (copy_class_field_kind x2)))
  | Ast_500.Parsetree.Pcf_constraint x0 ->
      Ast_414.Parsetree.Pcf_constraint
        (let (x0, x1) = x0 in ((copy_core_type x0), (copy_core_type x1)))
  | Ast_500.Parsetree.Pcf_initializer x0 ->
      Ast_414.Parsetree.Pcf_initializer (copy_expression x0)
  | Ast_500.Parsetree.Pcf_attribute x0 ->
      Ast_414.Parsetree.Pcf_attribute (copy_attribute x0)
  | Ast_500.Parsetree.Pcf_extension x0 ->
      Ast_414.Parsetree.Pcf_extension (copy_extension x0)
and copy_class_field_kind :
  Ast_500.Parsetree.class_field_kind -> Ast_414.Parsetree.class_field_kind =
  function
  | Ast_500.Parsetree.Cfk_virtual x0 ->
      Ast_414.Parsetree.Cfk_virtual (copy_core_type x0)
  | Ast_500.Parsetree.Cfk_concrete (x0, x1) ->
      Ast_414.Parsetree.Cfk_concrete
        ((copy_override_flag x0), (copy_expression x1))
and copy_open_declaration :
  Ast_500.Parsetree.open_declaration -> Ast_414.Parsetree.open_declaration =
  fun x -> copy_open_infos copy_module_expr x
and copy_module_binding :
  Ast_500.Parsetree.module_binding -> Ast_414.Parsetree.module_binding =
  fun
    { Ast_500.Parsetree.pmb_name = pmb_name;
      Ast_500.Parsetree.pmb_expr = pmb_expr;
      Ast_500.Parsetree.pmb_attributes = pmb_attributes;
      Ast_500.Parsetree.pmb_loc = pmb_loc }
    ->
    {
      Ast_414.Parsetree.pmb_name =
        (copy_loc (fun x -> Option.map (fun x -> x) x) pmb_name);
      Ast_414.Parsetree.pmb_expr = (copy_module_expr pmb_expr);
      Ast_414.Parsetree.pmb_attributes = (copy_attributes pmb_attributes);
      Ast_414.Parsetree.pmb_loc = (copy_location pmb_loc)
    }
and copy_module_expr :
  Ast_500.Parsetree.module_expr -> Ast_414.Parsetree.module_expr =
  fun
    { Ast_500.Parsetree.pmod_desc = pmod_desc;
      Ast_500.Parsetree.pmod_loc = pmod_loc;
      Ast_500.Parsetree.pmod_attributes = pmod_attributes }
    ->
    {
      Ast_414.Parsetree.pmod_desc = (copy_module_expr_desc pmod_desc);
      Ast_414.Parsetree.pmod_loc = (copy_location pmod_loc);
      Ast_414.Parsetree.pmod_attributes = (copy_attributes pmod_attributes)
    }
and copy_module_expr_desc :
  Ast_500.Parsetree.module_expr_desc -> Ast_414.Parsetree.module_expr_desc =
  function
  | Ast_500.Parsetree.Pmod_ident x0 ->
      Ast_414.Parsetree.Pmod_ident (copy_loc copy_Longident_t x0)
  | Ast_500.Parsetree.Pmod_structure x0 ->
      Ast_414.Parsetree.Pmod_structure (copy_structure x0)
  | Ast_500.Parsetree.Pmod_functor (x0, x1) ->
      Ast_414.Parsetree.Pmod_functor
        ((copy_functor_parameter x0), (copy_module_expr x1))
  | Ast_500.Parsetree.Pmod_apply (x0, x1) ->
      Ast_414.Parsetree.Pmod_apply
        ((copy_module_expr x0), (copy_module_expr x1))
  | Ast_500.Parsetree.Pmod_constraint (x0, x1) ->
      Ast_414.Parsetree.Pmod_constraint
        ((copy_module_expr x0), (copy_module_type x1))
  | Ast_500.Parsetree.Pmod_unpack x0 ->
      Ast_414.Parsetree.Pmod_unpack (copy_expression x0)
  | Ast_500.Parsetree.Pmod_extension x0 ->
      Ast_414.Parsetree.Pmod_extension (copy_extension x0)
and copy_functor_parameter :
  Ast_500.Parsetree.functor_parameter -> Ast_414.Parsetree.functor_parameter
  =
  function
  | Ast_500.Parsetree.Unit -> Ast_414.Parsetree.Unit
  | Ast_500.Parsetree.Named (x0, x1) ->
      Ast_414.Parsetree.Named
        ((copy_loc (fun x -> Option.map (fun x -> x) x) x0),
          (copy_module_type x1))
and copy_module_type :
  Ast_500.Parsetree.module_type -> Ast_414.Parsetree.module_type =
  fun
    { Ast_500.Parsetree.pmty_desc = pmty_desc;
      Ast_500.Parsetree.pmty_loc = pmty_loc;
      Ast_500.Parsetree.pmty_attributes = pmty_attributes }
    ->
    {
      Ast_414.Parsetree.pmty_desc = (copy_module_type_desc pmty_desc);
      Ast_414.Parsetree.pmty_loc = (copy_location pmty_loc);
      Ast_414.Parsetree.pmty_attributes = (copy_attributes pmty_attributes)
    }
and copy_module_type_desc :
  Ast_500.Parsetree.module_type_desc -> Ast_414.Parsetree.module_type_desc =
  function
  | Ast_500.Parsetree.Pmty_ident x0 ->
      Ast_414.Parsetree.Pmty_ident (copy_loc copy_Longident_t x0)
  | Ast_500.Parsetree.Pmty_signature x0 ->
      Ast_414.Parsetree.Pmty_signature (copy_signature x0)
  | Ast_500.Parsetree.Pmty_functor (x0, x1) ->
      Ast_414.Parsetree.Pmty_functor
        ((copy_functor_parameter x0), (copy_module_type x1))
  | Ast_500.Parsetree.Pmty_with (x0, x1) ->
      Ast_414.Parsetree.Pmty_with
        ((copy_module_type x0), (List.map copy_with_constraint x1))
  | Ast_500.Parsetree.Pmty_typeof x0 ->
      Ast_414.Parsetree.Pmty_typeof (copy_module_expr x0)
  | Ast_500.Parsetree.Pmty_extension x0 ->
      Ast_414.Parsetree.Pmty_extension (copy_extension x0)
  | Ast_500.Parsetree.Pmty_alias x0 ->
      Ast_414.Parsetree.Pmty_alias (copy_loc copy_Longident_t x0)
and copy_with_constraint :
  Ast_500.Parsetree.with_constraint -> Ast_414.Parsetree.with_constraint =
  function
  | Ast_500.Parsetree.Pwith_type (x0, x1) ->
      Ast_414.Parsetree.Pwith_type
        ((copy_loc copy_Longident_t x0), (copy_type_declaration x1))
  | Ast_500.Parsetree.Pwith_module (x0, x1) ->
      Ast_414.Parsetree.Pwith_module
        ((copy_loc copy_Longident_t x0), (copy_loc copy_Longident_t x1))
  | Ast_500.Parsetree.Pwith_modtype (x0, x1) ->
      Ast_414.Parsetree.Pwith_modtype
        ((copy_loc copy_Longident_t x0), (copy_module_type x1))
  | Ast_500.Parsetree.Pwith_modtypesubst (x0, x1) ->
      Ast_414.Parsetree.Pwith_modtypesubst
        ((copy_loc copy_Longident_t x0), (copy_module_type x1))
  | Ast_500.Parsetree.Pwith_typesubst (x0, x1) ->
      Ast_414.Parsetree.Pwith_typesubst
        ((copy_loc copy_Longident_t x0), (copy_type_declaration x1))
  | Ast_500.Parsetree.Pwith_modsubst (x0, x1) ->
      Ast_414.Parsetree.Pwith_modsubst
        ((copy_loc copy_Longident_t x0), (copy_loc copy_Longident_t x1))
and copy_signature :
  Ast_500.Parsetree.signature -> Ast_414.Parsetree.signature =
  fun x -> List.map copy_signature_item x
and copy_signature_item :
  Ast_500.Parsetree.signature_item -> Ast_414.Parsetree.signature_item =
  fun
    { Ast_500.Parsetree.psig_desc = psig_desc;
      Ast_500.Parsetree.psig_loc = psig_loc }
    ->
    {
      Ast_414.Parsetree.psig_desc = (copy_signature_item_desc psig_desc);
      Ast_414.Parsetree.psig_loc = (copy_location psig_loc)
    }
and copy_signature_item_desc :
  Ast_500.Parsetree.signature_item_desc ->
    Ast_414.Parsetree.signature_item_desc
  =
  function
  | Ast_500.Parsetree.Psig_value x0 ->
      Ast_414.Parsetree.Psig_value (copy_value_description x0)
  | Ast_500.Parsetree.Psig_type (x0, x1) ->
      Ast_414.Parsetree.Psig_type
        ((copy_rec_flag x0), (List.map copy_type_declaration x1))
  | Ast_500.Parsetree.Psig_typesubst x0 ->
      Ast_414.Parsetree.Psig_typesubst (List.map copy_type_declaration x0)
  | Ast_500.Parsetree.Psig_typext x0 ->
      Ast_414.Parsetree.Psig_typext (copy_type_extension x0)
  | Ast_500.Parsetree.Psig_exception x0 ->
      Ast_414.Parsetree.Psig_exception (copy_type_exception x0)
  | Ast_500.Parsetree.Psig_module x0 ->
      Ast_414.Parsetree.Psig_module (copy_module_declaration x0)
  | Ast_500.Parsetree.Psig_modsubst x0 ->
      Ast_414.Parsetree.Psig_modsubst (copy_module_substitution x0)
  | Ast_500.Parsetree.Psig_recmodule x0 ->
      Ast_414.Parsetree.Psig_recmodule (List.map copy_module_declaration x0)
  | Ast_500.Parsetree.Psig_modtype x0 ->
      Ast_414.Parsetree.Psig_modtype (copy_module_type_declaration x0)
  | Ast_500.Parsetree.Psig_modtypesubst x0 ->
      Ast_414.Parsetree.Psig_modtypesubst (copy_module_type_declaration x0)
  | Ast_500.Parsetree.Psig_open x0 ->
      Ast_414.Parsetree.Psig_open (copy_open_description x0)
  | Ast_500.Parsetree.Psig_include x0 ->
      Ast_414.Parsetree.Psig_include (copy_include_description x0)
  | Ast_500.Parsetree.Psig_class x0 ->
      Ast_414.Parsetree.Psig_class (List.map copy_class_description x0)
  | Ast_500.Parsetree.Psig_class_type x0 ->
      Ast_414.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | Ast_500.Parsetree.Psig_attribute x0 ->
      Ast_414.Parsetree.Psig_attribute (copy_attribute x0)
  | Ast_500.Parsetree.Psig_extension (x0, x1) ->
      Ast_414.Parsetree.Psig_extension
        ((copy_extension x0), (copy_attributes x1))
and copy_class_type_declaration :
  Ast_500.Parsetree.class_type_declaration ->
    Ast_414.Parsetree.class_type_declaration
  = fun x -> copy_class_infos copy_class_type x
and copy_class_description :
  Ast_500.Parsetree.class_description -> Ast_414.Parsetree.class_description
  = fun x -> copy_class_infos copy_class_type x
and copy_class_type :
  Ast_500.Parsetree.class_type -> Ast_414.Parsetree.class_type =
  fun
    { Ast_500.Parsetree.pcty_desc = pcty_desc;
      Ast_500.Parsetree.pcty_loc = pcty_loc;
      Ast_500.Parsetree.pcty_attributes = pcty_attributes }
    ->
    {
      Ast_414.Parsetree.pcty_desc = (copy_class_type_desc pcty_desc);
      Ast_414.Parsetree.pcty_loc = (copy_location pcty_loc);
      Ast_414.Parsetree.pcty_attributes = (copy_attributes pcty_attributes)
    }
and copy_class_type_desc :
  Ast_500.Parsetree.class_type_desc -> Ast_414.Parsetree.class_type_desc =
  function
  | Ast_500.Parsetree.Pcty_constr (x0, x1) ->
      Ast_414.Parsetree.Pcty_constr
        ((copy_loc copy_Longident_t x0), (List.map copy_core_type x1))
  | Ast_500.Parsetree.Pcty_signature x0 ->
      Ast_414.Parsetree.Pcty_signature (copy_class_signature x0)
  | Ast_500.Parsetree.Pcty_arrow (x0, x1, x2) ->
      Ast_414.Parsetree.Pcty_arrow
        ((copy_arg_label x0), (copy_core_type x1), (copy_class_type x2))
  | Ast_500.Parsetree.Pcty_extension x0 ->
      Ast_414.Parsetree.Pcty_extension (copy_extension x0)
  | Ast_500.Parsetree.Pcty_open (x0, x1) ->
      Ast_414.Parsetree.Pcty_open
        ((copy_open_description x0), (copy_class_type x1))
and copy_class_signature :
  Ast_500.Parsetree.class_signature -> Ast_414.Parsetree.class_signature =
  fun
    { Ast_500.Parsetree.pcsig_self = pcsig_self;
      Ast_500.Parsetree.pcsig_fields = pcsig_fields }
    ->
    {
      Ast_414.Parsetree.pcsig_self = (copy_core_type pcsig_self);
      Ast_414.Parsetree.pcsig_fields =
        (List.map copy_class_type_field pcsig_fields)
    }
and copy_class_type_field :
  Ast_500.Parsetree.class_type_field -> Ast_414.Parsetree.class_type_field =
  fun
    { Ast_500.Parsetree.pctf_desc = pctf_desc;
      Ast_500.Parsetree.pctf_loc = pctf_loc;
      Ast_500.Parsetree.pctf_attributes = pctf_attributes }
    ->
    {
      Ast_414.Parsetree.pctf_desc = (copy_class_type_field_desc pctf_desc);
      Ast_414.Parsetree.pctf_loc = (copy_location pctf_loc);
      Ast_414.Parsetree.pctf_attributes = (copy_attributes pctf_attributes)
    }
and copy_class_type_field_desc :
  Ast_500.Parsetree.class_type_field_desc ->
    Ast_414.Parsetree.class_type_field_desc
  =
  function
  | Ast_500.Parsetree.Pctf_inherit x0 ->
      Ast_414.Parsetree.Pctf_inherit (copy_class_type x0)
  | Ast_500.Parsetree.Pctf_val x0 ->
      Ast_414.Parsetree.Pctf_val
        (let (x0, x1, x2, x3) = x0 in
         ((copy_loc copy_label x0), (copy_mutable_flag x1),
           (copy_virtual_flag x2), (copy_core_type x3)))
  | Ast_500.Parsetree.Pctf_method x0 ->
      Ast_414.Parsetree.Pctf_method
        (let (x0, x1, x2, x3) = x0 in
         ((copy_loc copy_label x0), (copy_private_flag x1),
           (copy_virtual_flag x2), (copy_core_type x3)))
  | Ast_500.Parsetree.Pctf_constraint x0 ->
      Ast_414.Parsetree.Pctf_constraint
        (let (x0, x1) = x0 in ((copy_core_type x0), (copy_core_type x1)))
  | Ast_500.Parsetree.Pctf_attribute x0 ->
      Ast_414.Parsetree.Pctf_attribute (copy_attribute x0)
  | Ast_500.Parsetree.Pctf_extension x0 ->
      Ast_414.Parsetree.Pctf_extension (copy_extension x0)
and copy_extension :
  Ast_500.Parsetree.extension -> Ast_414.Parsetree.extension =
  fun x ->
    let (x0, x1) = x in ((copy_loc (fun x -> x) x0), (copy_payload x1))
and copy_class_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 Ast_500.Parsetree.class_infos -> 'g0 Ast_414.Parsetree.class_infos
  =
  fun f0 ->
    fun
      { Ast_500.Parsetree.pci_virt = pci_virt;
        Ast_500.Parsetree.pci_params = pci_params;
        Ast_500.Parsetree.pci_name = pci_name;
        Ast_500.Parsetree.pci_expr = pci_expr;
        Ast_500.Parsetree.pci_loc = pci_loc;
        Ast_500.Parsetree.pci_attributes = pci_attributes }
      ->
      {
        Ast_414.Parsetree.pci_virt = (copy_virtual_flag pci_virt);
        Ast_414.Parsetree.pci_params =
          (List.map
             (fun x ->
                let (x0, x1) = x in
                ((copy_core_type x0),
                  (let (x0, x1) = x1 in
                   ((copy_variance x0), (copy_injectivity x1))))) pci_params);
        Ast_414.Parsetree.pci_name = (copy_loc (fun x -> x) pci_name);
        Ast_414.Parsetree.pci_expr = (f0 pci_expr);
        Ast_414.Parsetree.pci_loc = (copy_location pci_loc);
        Ast_414.Parsetree.pci_attributes = (copy_attributes pci_attributes)
      }
and copy_virtual_flag :
  Ast_500.Asttypes.virtual_flag -> Ast_414.Asttypes.virtual_flag =
  function
  | Ast_500.Asttypes.Virtual -> Ast_414.Asttypes.Virtual
  | Ast_500.Asttypes.Concrete -> Ast_414.Asttypes.Concrete
and copy_include_description :
  Ast_500.Parsetree.include_description ->
    Ast_414.Parsetree.include_description
  = fun x -> copy_include_infos copy_module_type x
and copy_include_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 Ast_500.Parsetree.include_infos ->
        'g0 Ast_414.Parsetree.include_infos
  =
  fun f0 ->
    fun
      { Ast_500.Parsetree.pincl_mod = pincl_mod;
        Ast_500.Parsetree.pincl_loc = pincl_loc;
        Ast_500.Parsetree.pincl_attributes = pincl_attributes }
      ->
      {
        Ast_414.Parsetree.pincl_mod = (f0 pincl_mod);
        Ast_414.Parsetree.pincl_loc = (copy_location pincl_loc);
        Ast_414.Parsetree.pincl_attributes =
          (copy_attributes pincl_attributes)
      }
and copy_open_description :
  Ast_500.Parsetree.open_description -> Ast_414.Parsetree.open_description =
  fun x -> copy_open_infos (fun x -> copy_loc copy_Longident_t x) x
and copy_open_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 Ast_500.Parsetree.open_infos -> 'g0 Ast_414.Parsetree.open_infos
  =
  fun f0 ->
    fun
      { Ast_500.Parsetree.popen_expr = popen_expr;
        Ast_500.Parsetree.popen_override = popen_override;
        Ast_500.Parsetree.popen_loc = popen_loc;
        Ast_500.Parsetree.popen_attributes = popen_attributes }
      ->
      {
        Ast_414.Parsetree.popen_expr = (f0 popen_expr);
        Ast_414.Parsetree.popen_override =
          (copy_override_flag popen_override);
        Ast_414.Parsetree.popen_loc = (copy_location popen_loc);
        Ast_414.Parsetree.popen_attributes =
          (copy_attributes popen_attributes)
      }
and copy_override_flag :
  Ast_500.Asttypes.override_flag -> Ast_414.Asttypes.override_flag =
  function
  | Ast_500.Asttypes.Override -> Ast_414.Asttypes.Override
  | Ast_500.Asttypes.Fresh -> Ast_414.Asttypes.Fresh
and copy_module_type_declaration :
  Ast_500.Parsetree.module_type_declaration ->
    Ast_414.Parsetree.module_type_declaration
  =
  fun
    { Ast_500.Parsetree.pmtd_name = pmtd_name;
      Ast_500.Parsetree.pmtd_type = pmtd_type;
      Ast_500.Parsetree.pmtd_attributes = pmtd_attributes;
      Ast_500.Parsetree.pmtd_loc = pmtd_loc }
    ->
    {
      Ast_414.Parsetree.pmtd_name = (copy_loc (fun x -> x) pmtd_name);
      Ast_414.Parsetree.pmtd_type = (Option.map copy_module_type pmtd_type);
      Ast_414.Parsetree.pmtd_attributes = (copy_attributes pmtd_attributes);
      Ast_414.Parsetree.pmtd_loc = (copy_location pmtd_loc)
    }
and copy_module_substitution :
  Ast_500.Parsetree.module_substitution ->
    Ast_414.Parsetree.module_substitution
  =
  fun
    { Ast_500.Parsetree.pms_name = pms_name;
      Ast_500.Parsetree.pms_manifest = pms_manifest;
      Ast_500.Parsetree.pms_attributes = pms_attributes;
      Ast_500.Parsetree.pms_loc = pms_loc }
    ->
    {
      Ast_414.Parsetree.pms_name = (copy_loc (fun x -> x) pms_name);
      Ast_414.Parsetree.pms_manifest =
        (copy_loc copy_Longident_t pms_manifest);
      Ast_414.Parsetree.pms_attributes = (copy_attributes pms_attributes);
      Ast_414.Parsetree.pms_loc = (copy_location pms_loc)
    }
and copy_module_declaration :
  Ast_500.Parsetree.module_declaration ->
    Ast_414.Parsetree.module_declaration
  =
  fun
    { Ast_500.Parsetree.pmd_name = pmd_name;
      Ast_500.Parsetree.pmd_type = pmd_type;
      Ast_500.Parsetree.pmd_attributes = pmd_attributes;
      Ast_500.Parsetree.pmd_loc = pmd_loc }
    ->
    {
      Ast_414.Parsetree.pmd_name =
        (copy_loc (fun x -> Option.map (fun x -> x) x) pmd_name);
      Ast_414.Parsetree.pmd_type = (copy_module_type pmd_type);
      Ast_414.Parsetree.pmd_attributes = (copy_attributes pmd_attributes);
      Ast_414.Parsetree.pmd_loc = (copy_location pmd_loc)
    }
and copy_type_exception :
  Ast_500.Parsetree.type_exception -> Ast_414.Parsetree.type_exception =
  fun
    { Ast_500.Parsetree.ptyexn_constructor = ptyexn_constructor;
      Ast_500.Parsetree.ptyexn_loc = ptyexn_loc;
      Ast_500.Parsetree.ptyexn_attributes = ptyexn_attributes }
    ->
    {
      Ast_414.Parsetree.ptyexn_constructor =
        (copy_extension_constructor ptyexn_constructor);
      Ast_414.Parsetree.ptyexn_loc = (copy_location ptyexn_loc);
      Ast_414.Parsetree.ptyexn_attributes =
        (copy_attributes ptyexn_attributes)
    }
and copy_type_extension :
  Ast_500.Parsetree.type_extension -> Ast_414.Parsetree.type_extension =
  fun
    { Ast_500.Parsetree.ptyext_path = ptyext_path;
      Ast_500.Parsetree.ptyext_params = ptyext_params;
      Ast_500.Parsetree.ptyext_constructors = ptyext_constructors;
      Ast_500.Parsetree.ptyext_private = ptyext_private;
      Ast_500.Parsetree.ptyext_loc = ptyext_loc;
      Ast_500.Parsetree.ptyext_attributes = ptyext_attributes }
    ->
    {
      Ast_414.Parsetree.ptyext_path = (copy_loc copy_Longident_t ptyext_path);
      Ast_414.Parsetree.ptyext_params =
        (List.map
           (fun x ->
              let (x0, x1) = x in
              ((copy_core_type x0),
                (let (x0, x1) = x1 in
                 ((copy_variance x0), (copy_injectivity x1))))) ptyext_params);
      Ast_414.Parsetree.ptyext_constructors =
        (List.map copy_extension_constructor ptyext_constructors);
      Ast_414.Parsetree.ptyext_private = (copy_private_flag ptyext_private);
      Ast_414.Parsetree.ptyext_loc = (copy_location ptyext_loc);
      Ast_414.Parsetree.ptyext_attributes =
        (copy_attributes ptyext_attributes)
    }
and copy_extension_constructor :
  Ast_500.Parsetree.extension_constructor ->
    Ast_414.Parsetree.extension_constructor
  =
  fun
    { Ast_500.Parsetree.pext_name = pext_name;
      Ast_500.Parsetree.pext_kind = pext_kind;
      Ast_500.Parsetree.pext_loc = pext_loc;
      Ast_500.Parsetree.pext_attributes = pext_attributes }
    ->
    {
      Ast_414.Parsetree.pext_name = (copy_loc (fun x -> x) pext_name);
      Ast_414.Parsetree.pext_kind =
        (copy_extension_constructor_kind pext_kind);
      Ast_414.Parsetree.pext_loc = (copy_location pext_loc);
      Ast_414.Parsetree.pext_attributes = (copy_attributes pext_attributes)
    }
and copy_extension_constructor_kind :
  Ast_500.Parsetree.extension_constructor_kind ->
    Ast_414.Parsetree.extension_constructor_kind
  =
  function
  | Ast_500.Parsetree.Pext_decl (x0, x1, x2) ->
      Ast_414.Parsetree.Pext_decl
        ((List.map (fun x -> copy_loc (fun x -> x) x) x0),
          (copy_constructor_arguments x1), (Option.map copy_core_type x2))
  | Ast_500.Parsetree.Pext_rebind x0 ->
      Ast_414.Parsetree.Pext_rebind (copy_loc copy_Longident_t x0)
and copy_type_declaration :
  Ast_500.Parsetree.type_declaration -> Ast_414.Parsetree.type_declaration =
  fun
    { Ast_500.Parsetree.ptype_name = ptype_name;
      Ast_500.Parsetree.ptype_params = ptype_params;
      Ast_500.Parsetree.ptype_cstrs = ptype_cstrs;
      Ast_500.Parsetree.ptype_kind = ptype_kind;
      Ast_500.Parsetree.ptype_private = ptype_private;
      Ast_500.Parsetree.ptype_manifest = ptype_manifest;
      Ast_500.Parsetree.ptype_attributes = ptype_attributes;
      Ast_500.Parsetree.ptype_loc = ptype_loc }
    ->
    {
      Ast_414.Parsetree.ptype_name = (copy_loc (fun x -> x) ptype_name);
      Ast_414.Parsetree.ptype_params =
        (List.map
           (fun x ->
              let (x0, x1) = x in
              ((copy_core_type x0),
                (let (x0, x1) = x1 in
                 ((copy_variance x0), (copy_injectivity x1))))) ptype_params);
      Ast_414.Parsetree.ptype_cstrs =
        (List.map
           (fun x ->
              let (x0, x1, x2) = x in
              ((copy_core_type x0), (copy_core_type x1), (copy_location x2)))
           ptype_cstrs);
      Ast_414.Parsetree.ptype_kind = (copy_type_kind ptype_kind);
      Ast_414.Parsetree.ptype_private = (copy_private_flag ptype_private);
      Ast_414.Parsetree.ptype_manifest =
        (Option.map copy_core_type ptype_manifest);
      Ast_414.Parsetree.ptype_attributes = (copy_attributes ptype_attributes);
      Ast_414.Parsetree.ptype_loc = (copy_location ptype_loc)
    }
and copy_private_flag :
  Ast_500.Asttypes.private_flag -> Ast_414.Asttypes.private_flag =
  function
  | Ast_500.Asttypes.Private -> Ast_414.Asttypes.Private
  | Ast_500.Asttypes.Public -> Ast_414.Asttypes.Public
and copy_type_kind :
  Ast_500.Parsetree.type_kind -> Ast_414.Parsetree.type_kind =
  function
  | Ast_500.Parsetree.Ptype_abstract -> Ast_414.Parsetree.Ptype_abstract
  | Ast_500.Parsetree.Ptype_variant x0 ->
      Ast_414.Parsetree.Ptype_variant
        (List.map copy_constructor_declaration x0)
  | Ast_500.Parsetree.Ptype_record x0 ->
      Ast_414.Parsetree.Ptype_record (List.map copy_label_declaration x0)
  | Ast_500.Parsetree.Ptype_open -> Ast_414.Parsetree.Ptype_open
and copy_constructor_declaration :
  Ast_500.Parsetree.constructor_declaration ->
    Ast_414.Parsetree.constructor_declaration
  =
  fun
    { Ast_500.Parsetree.pcd_name = pcd_name;
      Ast_500.Parsetree.pcd_vars = pcd_vars;
      Ast_500.Parsetree.pcd_args = pcd_args;
      Ast_500.Parsetree.pcd_res = pcd_res;
      Ast_500.Parsetree.pcd_loc = pcd_loc;
      Ast_500.Parsetree.pcd_attributes = pcd_attributes }
    ->
    {
      Ast_414.Parsetree.pcd_name = (copy_loc (fun x -> x) pcd_name);
      Ast_414.Parsetree.pcd_vars =
        (List.map (fun x -> copy_loc (fun x -> x) x) pcd_vars);
      Ast_414.Parsetree.pcd_args = (copy_constructor_arguments pcd_args);
      Ast_414.Parsetree.pcd_res = (Option.map copy_core_type pcd_res);
      Ast_414.Parsetree.pcd_loc = (copy_location pcd_loc);
      Ast_414.Parsetree.pcd_attributes = (copy_attributes pcd_attributes)
    }
and copy_constructor_arguments :
  Ast_500.Parsetree.constructor_arguments ->
    Ast_414.Parsetree.constructor_arguments
  =
  function
  | Ast_500.Parsetree.Pcstr_tuple x0 ->
      Ast_414.Parsetree.Pcstr_tuple (List.map copy_core_type x0)
  | Ast_500.Parsetree.Pcstr_record x0 ->
      Ast_414.Parsetree.Pcstr_record (List.map copy_label_declaration x0)
and copy_label_declaration :
  Ast_500.Parsetree.label_declaration -> Ast_414.Parsetree.label_declaration
  =
  fun
    { Ast_500.Parsetree.pld_name = pld_name;
      Ast_500.Parsetree.pld_mutable = pld_mutable;
      Ast_500.Parsetree.pld_type = pld_type;
      Ast_500.Parsetree.pld_loc = pld_loc;
      Ast_500.Parsetree.pld_attributes = pld_attributes }
    ->
    {
      Ast_414.Parsetree.pld_name = (copy_loc (fun x -> x) pld_name);
      Ast_414.Parsetree.pld_mutable = (copy_mutable_flag pld_mutable);
      Ast_414.Parsetree.pld_type = (copy_core_type pld_type);
      Ast_414.Parsetree.pld_loc = (copy_location pld_loc);
      Ast_414.Parsetree.pld_attributes = (copy_attributes pld_attributes)
    }
and copy_mutable_flag :
  Ast_500.Asttypes.mutable_flag -> Ast_414.Asttypes.mutable_flag =
  function
  | Ast_500.Asttypes.Immutable -> Ast_414.Asttypes.Immutable
  | Ast_500.Asttypes.Mutable -> Ast_414.Asttypes.Mutable
and copy_injectivity :
  Ast_500.Asttypes.injectivity -> Ast_414.Asttypes.injectivity =
  function
  | Ast_500.Asttypes.Injective -> Ast_414.Asttypes.Injective
  | Ast_500.Asttypes.NoInjectivity -> Ast_414.Asttypes.NoInjectivity
and copy_variance : Ast_500.Asttypes.variance -> Ast_414.Asttypes.variance =
  function
  | Ast_500.Asttypes.Covariant -> Ast_414.Asttypes.Covariant
  | Ast_500.Asttypes.Contravariant -> Ast_414.Asttypes.Contravariant
  | Ast_500.Asttypes.NoVariance -> Ast_414.Asttypes.NoVariance
and copy_value_description :
  Ast_500.Parsetree.value_description -> Ast_414.Parsetree.value_description
  =
  fun
    { Ast_500.Parsetree.pval_name = pval_name;
      Ast_500.Parsetree.pval_type = pval_type;
      Ast_500.Parsetree.pval_prim = pval_prim;
      Ast_500.Parsetree.pval_attributes = pval_attributes;
      Ast_500.Parsetree.pval_loc = pval_loc }
    ->
    {
      Ast_414.Parsetree.pval_name = (copy_loc (fun x -> x) pval_name);
      Ast_414.Parsetree.pval_type = (copy_core_type pval_type);
      Ast_414.Parsetree.pval_prim = (List.map (fun x -> x) pval_prim);
      Ast_414.Parsetree.pval_attributes = (copy_attributes pval_attributes);
      Ast_414.Parsetree.pval_loc = (copy_location pval_loc)
    }
and copy_object_field_desc :
  Ast_500.Parsetree.object_field_desc -> Ast_414.Parsetree.object_field_desc
  =
  function
  | Ast_500.Parsetree.Otag (x0, x1) ->
      Ast_414.Parsetree.Otag ((copy_loc copy_label x0), (copy_core_type x1))
  | Ast_500.Parsetree.Oinherit x0 ->
      Ast_414.Parsetree.Oinherit (copy_core_type x0)
and copy_arg_label : Ast_500.Asttypes.arg_label -> Ast_414.Asttypes.arg_label
  =
  function
  | Ast_500.Asttypes.Nolabel -> Ast_414.Asttypes.Nolabel
  | Ast_500.Asttypes.Labelled x0 -> Ast_414.Asttypes.Labelled x0
  | Ast_500.Asttypes.Optional x0 -> Ast_414.Asttypes.Optional x0
and copy_closed_flag :
  Ast_500.Asttypes.closed_flag -> Ast_414.Asttypes.closed_flag =
  function
  | Ast_500.Asttypes.Closed -> Ast_414.Asttypes.Closed
  | Ast_500.Asttypes.Open -> Ast_414.Asttypes.Open
and copy_label : Ast_500.Asttypes.label -> Ast_414.Asttypes.label =
  fun x -> x
and copy_rec_flag : Ast_500.Asttypes.rec_flag -> Ast_414.Asttypes.rec_flag =
  function
  | Ast_500.Asttypes.Nonrecursive -> Ast_414.Asttypes.Nonrecursive
  | Ast_500.Asttypes.Recursive -> Ast_414.Asttypes.Recursive
and copy_constant : Ast_500.Parsetree.constant -> Ast_414.Parsetree.constant
  =
  function
  | Ast_500.Parsetree.Pconst_integer (x0, x1) ->
      Ast_414.Parsetree.Pconst_integer (x0, (Option.map (fun x -> x) x1))
  | Ast_500.Parsetree.Pconst_char x0 -> Ast_414.Parsetree.Pconst_char x0
  | Ast_500.Parsetree.Pconst_string (x0, x1, x2) ->
      Ast_414.Parsetree.Pconst_string
        (x0, (copy_location x1), (Option.map (fun x -> x) x2))
  | Ast_500.Parsetree.Pconst_float (x0, x1) ->
      Ast_414.Parsetree.Pconst_float (x0, (Option.map (fun x -> x) x1))
and copy_Longident_t : Longident.t -> Longident.t =
  function
  | Longident.Lident x0 -> Longident.Lident x0
  | Longident.Ldot (x0, x1) -> Longident.Ldot ((copy_Longident_t x0), x1)
  | Longident.Lapply (x0, x1) ->
      Longident.Lapply ((copy_Longident_t x0), (copy_Longident_t x1))
and copy_loc :
  'f0 'g0 .
    ('f0 -> 'g0) -> 'f0 Ast_500.Asttypes.loc -> 'g0 Ast_414.Asttypes.loc
  =
  fun f0 ->
    fun { Ast_500.Asttypes.txt = txt; Ast_500.Asttypes.loc = loc } ->
      {
        Ast_414.Asttypes.txt = (f0 txt);
        Ast_414.Asttypes.loc = (copy_location loc)
      }
and copy_location : Location.t -> Location.t =
  fun
    { Location.loc_start = loc_start; Location.loc_end = loc_end;
      Location.loc_ghost = loc_ghost }
    ->
    {
      Location.loc_start = (copy_position loc_start);
      Location.loc_end = (copy_position loc_end);
      Location.loc_ghost = loc_ghost
    }
and copy_position : Lexing.position -> Lexing.position =
  fun
    { Lexing.pos_fname = pos_fname; Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol; Lexing.pos_cnum = pos_cnum }
    ->
    {
      Lexing.pos_fname = pos_fname;
      Lexing.pos_lnum = pos_lnum;
      Lexing.pos_bol = pos_bol;
      Lexing.pos_cnum = pos_cnum
    }
