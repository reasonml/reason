open Stdlib0
module From = Ast_411
module To = Ast_410
let rec copy_out_type_extension :
  Ast_411.Outcometree.out_type_extension ->
    Ast_410.Outcometree.out_type_extension
  =
  fun
    { Ast_411.Outcometree.otyext_name = otyext_name;
      Ast_411.Outcometree.otyext_params = otyext_params;
      Ast_411.Outcometree.otyext_constructors = otyext_constructors;
      Ast_411.Outcometree.otyext_private = otyext_private }
    ->
    {
      Ast_410.Outcometree.otyext_name = otyext_name;
      Ast_410.Outcometree.otyext_params =
        (List.map (fun x -> x) otyext_params);
      Ast_410.Outcometree.otyext_constructors =
        (List.map
           (fun x ->
              let (x0, x1, x2) = x in
              (x0, (List.map copy_out_type x1),
                (Option.map copy_out_type x2))) otyext_constructors);
      Ast_410.Outcometree.otyext_private = (copy_private_flag otyext_private)
    }
and copy_out_phrase :
  Ast_411.Outcometree.out_phrase -> Ast_410.Outcometree.out_phrase =
  function
  | Ast_411.Outcometree.Ophr_eval (x0, x1) ->
      Ast_410.Outcometree.Ophr_eval ((copy_out_value x0), (copy_out_type x1))
  | Ast_411.Outcometree.Ophr_signature x0 ->
      Ast_410.Outcometree.Ophr_signature
        (List.map
           (fun x ->
              let (x0, x1) = x in
              ((copy_out_sig_item x0), (Option.map copy_out_value x1))) x0)
  | Ast_411.Outcometree.Ophr_exception x0 ->
      Ast_410.Outcometree.Ophr_exception
        (let (x0, x1) = x0 in (x0, (copy_out_value x1)))
and copy_out_sig_item :
  Ast_411.Outcometree.out_sig_item -> Ast_410.Outcometree.out_sig_item =
  function
  | Ast_411.Outcometree.Osig_class (x0, x1, x2, x3, x4) ->
      Ast_410.Outcometree.Osig_class
        (x0, x1,
          (List.map
             (fun x ->
                let (x0, x1) = x in (x0, (let (x0, x1) = x1 in (x0, x1)))) x2),
          (copy_out_class_type x3), (copy_out_rec_status x4))
  | Ast_411.Outcometree.Osig_class_type (x0, x1, x2, x3, x4) ->
      Ast_410.Outcometree.Osig_class_type
        (x0, x1,
          (List.map
             (fun x ->
                let (x0, x1) = x in (x0, (let (x0, x1) = x1 in (x0, x1)))) x2),
          (copy_out_class_type x3), (copy_out_rec_status x4))
  | Ast_411.Outcometree.Osig_typext (x0, x1) ->
      Ast_410.Outcometree.Osig_typext
        ((copy_out_extension_constructor x0), (copy_out_ext_status x1))
  | Ast_411.Outcometree.Osig_modtype (x0, x1) ->
      Ast_410.Outcometree.Osig_modtype (x0, (copy_out_module_type x1))
  | Ast_411.Outcometree.Osig_module (x0, x1, x2) ->
      Ast_410.Outcometree.Osig_module
        (x0, (copy_out_module_type x1), (copy_out_rec_status x2))
  | Ast_411.Outcometree.Osig_type (x0, x1) ->
      Ast_410.Outcometree.Osig_type
        ((copy_out_type_decl x0), (copy_out_rec_status x1))
  | Ast_411.Outcometree.Osig_value x0 ->
      Ast_410.Outcometree.Osig_value (copy_out_val_decl x0)
  | Ast_411.Outcometree.Osig_ellipsis -> Ast_410.Outcometree.Osig_ellipsis
and copy_out_val_decl :
  Ast_411.Outcometree.out_val_decl -> Ast_410.Outcometree.out_val_decl =
  fun
    { Ast_411.Outcometree.oval_name = oval_name;
      Ast_411.Outcometree.oval_type = oval_type;
      Ast_411.Outcometree.oval_prims = oval_prims;
      Ast_411.Outcometree.oval_attributes = oval_attributes }
    ->
    {
      Ast_410.Outcometree.oval_name = oval_name;
      Ast_410.Outcometree.oval_type = (copy_out_type oval_type);
      Ast_410.Outcometree.oval_prims = (List.map (fun x -> x) oval_prims);
      Ast_410.Outcometree.oval_attributes =
        (List.map copy_out_attribute oval_attributes)
    }
and copy_out_type_decl :
  Ast_411.Outcometree.out_type_decl -> Ast_410.Outcometree.out_type_decl =
  fun
    { Ast_411.Outcometree.otype_name = otype_name;
      Ast_411.Outcometree.otype_params = otype_params;
      Ast_411.Outcometree.otype_type = otype_type;
      Ast_411.Outcometree.otype_private = otype_private;
      Ast_411.Outcometree.otype_immediate = otype_immediate;
      Ast_411.Outcometree.otype_unboxed = otype_unboxed;
      Ast_411.Outcometree.otype_cstrs = otype_cstrs }
    ->
    {
      Ast_410.Outcometree.otype_name = otype_name;
      Ast_410.Outcometree.otype_params =
        (List.map
           (fun x ->
              let (x0, x1) = x in (x0, (let (x0, x1) = x1 in (x0, x1))))
           otype_params);
      Ast_410.Outcometree.otype_type = (copy_out_type otype_type);
      Ast_410.Outcometree.otype_private = (copy_private_flag otype_private);
      Ast_410.Outcometree.otype_immediate =
        (copy_Type_immediacy_t otype_immediate);
      Ast_410.Outcometree.otype_unboxed = otype_unboxed;
      Ast_410.Outcometree.otype_cstrs =
        (List.map
           (fun x ->
              let (x0, x1) = x in ((copy_out_type x0), (copy_out_type x1)))
           otype_cstrs)
    }
and copy_Type_immediacy_t :
  Ast_411.Type_immediacy.t -> Ast_410.Type_immediacy.t =
  function
  | Ast_411.Type_immediacy.Unknown -> Ast_410.Type_immediacy.Unknown
  | Ast_411.Type_immediacy.Always -> Ast_410.Type_immediacy.Always
  | Ast_411.Type_immediacy.Always_on_64bits ->
      Ast_410.Type_immediacy.Always_on_64bits
and copy_out_module_type :
  Ast_411.Outcometree.out_module_type -> Ast_410.Outcometree.out_module_type
  =
  function
  | Ast_411.Outcometree.Omty_abstract -> Ast_410.Outcometree.Omty_abstract
  | Ast_411.Outcometree.Omty_functor (x0, x1) ->
      Ast_410.Outcometree.Omty_functor
        ((Option.map
            (fun x ->
               let (x0, x1) = x in
               ((Option.map (fun x -> x) x0), (copy_out_module_type x1))) x0),
          (copy_out_module_type x1))
  | Ast_411.Outcometree.Omty_ident x0 ->
      Ast_410.Outcometree.Omty_ident (copy_out_ident x0)
  | Ast_411.Outcometree.Omty_signature x0 ->
      Ast_410.Outcometree.Omty_signature (List.map copy_out_sig_item x0)
  | Ast_411.Outcometree.Omty_alias x0 ->
      Ast_410.Outcometree.Omty_alias (copy_out_ident x0)
and copy_out_ext_status :
  Ast_411.Outcometree.out_ext_status -> Ast_410.Outcometree.out_ext_status =
  function
  | Ast_411.Outcometree.Oext_first -> Ast_410.Outcometree.Oext_first
  | Ast_411.Outcometree.Oext_next -> Ast_410.Outcometree.Oext_next
  | Ast_411.Outcometree.Oext_exception -> Ast_410.Outcometree.Oext_exception
and copy_out_extension_constructor :
  Ast_411.Outcometree.out_extension_constructor ->
    Ast_410.Outcometree.out_extension_constructor
  =
  fun
    { Ast_411.Outcometree.oext_name = oext_name;
      Ast_411.Outcometree.oext_type_name = oext_type_name;
      Ast_411.Outcometree.oext_type_params = oext_type_params;
      Ast_411.Outcometree.oext_args = oext_args;
      Ast_411.Outcometree.oext_ret_type = oext_ret_type;
      Ast_411.Outcometree.oext_private = oext_private }
    ->
    {
      Ast_410.Outcometree.oext_name = oext_name;
      Ast_410.Outcometree.oext_type_name = oext_type_name;
      Ast_410.Outcometree.oext_type_params =
        (List.map (fun x -> x) oext_type_params);
      Ast_410.Outcometree.oext_args = (List.map copy_out_type oext_args);
      Ast_410.Outcometree.oext_ret_type =
        (Option.map copy_out_type oext_ret_type);
      Ast_410.Outcometree.oext_private = (copy_private_flag oext_private)
    }
and copy_out_rec_status :
  Ast_411.Outcometree.out_rec_status -> Ast_410.Outcometree.out_rec_status =
  function
  | Ast_411.Outcometree.Orec_not -> Ast_410.Outcometree.Orec_not
  | Ast_411.Outcometree.Orec_first -> Ast_410.Outcometree.Orec_first
  | Ast_411.Outcometree.Orec_next -> Ast_410.Outcometree.Orec_next
and copy_out_class_type :
  Ast_411.Outcometree.out_class_type -> Ast_410.Outcometree.out_class_type =
  function
  | Ast_411.Outcometree.Octy_constr (x0, x1) ->
      Ast_410.Outcometree.Octy_constr
        ((copy_out_ident x0), (List.map copy_out_type x1))
  | Ast_411.Outcometree.Octy_arrow (x0, x1, x2) ->
      Ast_410.Outcometree.Octy_arrow
        (x0, (copy_out_type x1), (copy_out_class_type x2))
  | Ast_411.Outcometree.Octy_signature (x0, x1) ->
      Ast_410.Outcometree.Octy_signature
        ((Option.map copy_out_type x0),
          (List.map copy_out_class_sig_item x1))
and copy_out_class_sig_item :
  Ast_411.Outcometree.out_class_sig_item ->
    Ast_410.Outcometree.out_class_sig_item
  =
  function
  | Ast_411.Outcometree.Ocsg_constraint (x0, x1) ->
      Ast_410.Outcometree.Ocsg_constraint
        ((copy_out_type x0), (copy_out_type x1))
  | Ast_411.Outcometree.Ocsg_method (x0, x1, x2, x3) ->
      Ast_410.Outcometree.Ocsg_method (x0, x1, x2, (copy_out_type x3))
  | Ast_411.Outcometree.Ocsg_value (x0, x1, x2, x3) ->
      Ast_410.Outcometree.Ocsg_value (x0, x1, x2, (copy_out_type x3))
and copy_out_type :
  Ast_411.Outcometree.out_type -> Ast_410.Outcometree.out_type =
  function
  | Ast_411.Outcometree.Otyp_abstract -> Ast_410.Outcometree.Otyp_abstract
  | Ast_411.Outcometree.Otyp_open -> Ast_410.Outcometree.Otyp_open
  | Ast_411.Outcometree.Otyp_alias (x0, x1) ->
      Ast_410.Outcometree.Otyp_alias ((copy_out_type x0), x1)
  | Ast_411.Outcometree.Otyp_arrow (x0, x1, x2) ->
      Ast_410.Outcometree.Otyp_arrow
        (x0, (copy_out_type x1), (copy_out_type x2))
  | Ast_411.Outcometree.Otyp_class (x0, x1, x2) ->
      Ast_410.Outcometree.Otyp_class
        (x0, (copy_out_ident x1), (List.map copy_out_type x2))
  | Ast_411.Outcometree.Otyp_constr (x0, x1) ->
      Ast_410.Outcometree.Otyp_constr
        ((copy_out_ident x0), (List.map copy_out_type x1))
  | Ast_411.Outcometree.Otyp_manifest (x0, x1) ->
      Ast_410.Outcometree.Otyp_manifest
        ((copy_out_type x0), (copy_out_type x1))
  | Ast_411.Outcometree.Otyp_object (x0, x1) ->
      Ast_410.Outcometree.Otyp_object
        ((List.map (fun x -> let (x0, x1) = x in (x0, (copy_out_type x1))) x0),
          (Option.map (fun x -> x) x1))
  | Ast_411.Outcometree.Otyp_record x0 ->
      Ast_410.Outcometree.Otyp_record
        (List.map
           (fun x -> let (x0, x1, x2) = x in (x0, x1, (copy_out_type x2))) x0)
  | Ast_411.Outcometree.Otyp_stuff x0 -> Ast_410.Outcometree.Otyp_stuff x0
  | Ast_411.Outcometree.Otyp_sum x0 ->
      Ast_410.Outcometree.Otyp_sum
        (List.map
           (fun x ->
              let (x0, x1, x2) = x in
              (x0, (List.map copy_out_type x1),
                (Option.map copy_out_type x2))) x0)
  | Ast_411.Outcometree.Otyp_tuple x0 ->
      Ast_410.Outcometree.Otyp_tuple (List.map copy_out_type x0)
  | Ast_411.Outcometree.Otyp_var (x0, x1) ->
      Ast_410.Outcometree.Otyp_var (x0, x1)
  | Ast_411.Outcometree.Otyp_variant (x0, x1, x2, x3) ->
      Ast_410.Outcometree.Otyp_variant
        (x0, (copy_out_variant x1), x2,
          (Option.map (fun x -> List.map (fun x -> x) x) x3))
  | Ast_411.Outcometree.Otyp_poly (x0, x1) ->
      Ast_410.Outcometree.Otyp_poly
        ((List.map (fun x -> x) x0), (copy_out_type x1))
  | Ast_411.Outcometree.Otyp_module (x0, x1, x2) ->
      Ast_410.Outcometree.Otyp_module
        ((copy_out_ident x0), (List.map (fun x -> x) x1),
          (List.map copy_out_type x2))
  | Ast_411.Outcometree.Otyp_attribute (x0, x1) ->
      Ast_410.Outcometree.Otyp_attribute
        ((copy_out_type x0), (copy_out_attribute x1))
and copy_out_attribute :
  Ast_411.Outcometree.out_attribute -> Ast_410.Outcometree.out_attribute =
  fun { Ast_411.Outcometree.oattr_name = oattr_name } ->
    { Ast_410.Outcometree.oattr_name = oattr_name }
and copy_out_variant :
  Ast_411.Outcometree.out_variant -> Ast_410.Outcometree.out_variant =
  function
  | Ast_411.Outcometree.Ovar_fields x0 ->
      Ast_410.Outcometree.Ovar_fields
        (List.map
           (fun x ->
              let (x0, x1, x2) = x in (x0, x1, (List.map copy_out_type x2)))
           x0)
  | Ast_411.Outcometree.Ovar_typ x0 ->
      Ast_410.Outcometree.Ovar_typ (copy_out_type x0)
and copy_out_value :
  Ast_411.Outcometree.out_value -> Ast_410.Outcometree.out_value =
  function
  | Ast_411.Outcometree.Oval_array x0 ->
      Ast_410.Outcometree.Oval_array (List.map copy_out_value x0)
  | Ast_411.Outcometree.Oval_char x0 -> Ast_410.Outcometree.Oval_char x0
  | Ast_411.Outcometree.Oval_constr (x0, x1) ->
      Ast_410.Outcometree.Oval_constr
        ((copy_out_ident x0), (List.map copy_out_value x1))
  | Ast_411.Outcometree.Oval_ellipsis -> Ast_410.Outcometree.Oval_ellipsis
  | Ast_411.Outcometree.Oval_float x0 -> Ast_410.Outcometree.Oval_float x0
  | Ast_411.Outcometree.Oval_int x0 -> Ast_410.Outcometree.Oval_int x0
  | Ast_411.Outcometree.Oval_int32 x0 -> Ast_410.Outcometree.Oval_int32 x0
  | Ast_411.Outcometree.Oval_int64 x0 -> Ast_410.Outcometree.Oval_int64 x0
  | Ast_411.Outcometree.Oval_nativeint x0 ->
      Ast_410.Outcometree.Oval_nativeint x0
  | Ast_411.Outcometree.Oval_list x0 ->
      Ast_410.Outcometree.Oval_list (List.map copy_out_value x0)
  | Ast_411.Outcometree.Oval_printer x0 ->
      Ast_410.Outcometree.Oval_printer x0
  | Ast_411.Outcometree.Oval_record x0 ->
      Ast_410.Outcometree.Oval_record
        (List.map
           (fun x ->
              let (x0, x1) = x in ((copy_out_ident x0), (copy_out_value x1)))
           x0)
  | Ast_411.Outcometree.Oval_string (x0, x1, x2) ->
      Ast_410.Outcometree.Oval_string (x0, x1, (copy_out_string x2))
  | Ast_411.Outcometree.Oval_stuff x0 -> Ast_410.Outcometree.Oval_stuff x0
  | Ast_411.Outcometree.Oval_tuple x0 ->
      Ast_410.Outcometree.Oval_tuple (List.map copy_out_value x0)
  | Ast_411.Outcometree.Oval_variant (x0, x1) ->
      Ast_410.Outcometree.Oval_variant (x0, (Option.map copy_out_value x1))
and copy_out_string :
  Ast_411.Outcometree.out_string -> Ast_410.Outcometree.out_string =
  function
  | Ast_411.Outcometree.Ostr_string -> Ast_410.Outcometree.Ostr_string
  | Ast_411.Outcometree.Ostr_bytes -> Ast_410.Outcometree.Ostr_bytes
and copy_out_ident :
  Ast_411.Outcometree.out_ident -> Ast_410.Outcometree.out_ident =
  function
  | Ast_411.Outcometree.Oide_apply (x0, x1) ->
      Ast_410.Outcometree.Oide_apply
        ((copy_out_ident x0), (copy_out_ident x1))
  | Ast_411.Outcometree.Oide_dot (x0, x1) ->
      Ast_410.Outcometree.Oide_dot ((copy_out_ident x0), x1)
  | Ast_411.Outcometree.Oide_ident x0 ->
      Ast_410.Outcometree.Oide_ident (copy_out_name x0)
and copy_out_name :
  Ast_411.Outcometree.out_name -> Ast_410.Outcometree.out_name =
  fun { Ast_411.Outcometree.printed_name = printed_name } ->
    { Ast_410.Outcometree.printed_name = printed_name }
and copy_toplevel_phrase :
  Ast_411.Parsetree.toplevel_phrase -> Ast_410.Parsetree.toplevel_phrase =
  function
  | Ast_411.Parsetree.Ptop_def x0 ->
      Ast_410.Parsetree.Ptop_def (copy_structure x0)
  | Ast_411.Parsetree.Ptop_dir x0 ->
      Ast_410.Parsetree.Ptop_dir (copy_toplevel_directive x0)
and copy_toplevel_directive :
  Ast_411.Parsetree.toplevel_directive ->
    Ast_410.Parsetree.toplevel_directive
  =
  fun
    { Ast_411.Parsetree.pdir_name = pdir_name;
      Ast_411.Parsetree.pdir_arg = pdir_arg;
      Ast_411.Parsetree.pdir_loc = pdir_loc }
    ->
    {
      Ast_410.Parsetree.pdir_name = (copy_loc (fun x -> x) pdir_name);
      Ast_410.Parsetree.pdir_arg =
        (Option.map copy_directive_argument pdir_arg);
      Ast_410.Parsetree.pdir_loc = (copy_location pdir_loc)
    }
and copy_directive_argument :
  Ast_411.Parsetree.directive_argument ->
    Ast_410.Parsetree.directive_argument
  =
  fun
    { Ast_411.Parsetree.pdira_desc = pdira_desc;
      Ast_411.Parsetree.pdira_loc = pdira_loc }
    ->
    {
      Ast_410.Parsetree.pdira_desc =
        (copy_directive_argument_desc pdira_desc);
      Ast_410.Parsetree.pdira_loc = (copy_location pdira_loc)
    }
and copy_directive_argument_desc :
  Ast_411.Parsetree.directive_argument_desc ->
    Ast_410.Parsetree.directive_argument_desc
  =
  function
  | Ast_411.Parsetree.Pdir_string x0 -> Ast_410.Parsetree.Pdir_string x0
  | Ast_411.Parsetree.Pdir_int (x0, x1) ->
      Ast_410.Parsetree.Pdir_int (x0, (Option.map (fun x -> x) x1))
  | Ast_411.Parsetree.Pdir_ident x0 ->
      Ast_410.Parsetree.Pdir_ident (copy_Longident_t x0)
  | Ast_411.Parsetree.Pdir_bool x0 -> Ast_410.Parsetree.Pdir_bool x0
and copy_expression :
  Ast_411.Parsetree.expression -> Ast_410.Parsetree.expression =
  fun
    { Ast_411.Parsetree.pexp_desc = pexp_desc;
      Ast_411.Parsetree.pexp_loc = pexp_loc;
      Ast_411.Parsetree.pexp_loc_stack = pexp_loc_stack;
      Ast_411.Parsetree.pexp_attributes = pexp_attributes }
    ->
    {
      Ast_410.Parsetree.pexp_desc = (copy_expression_desc pexp_desc);
      Ast_410.Parsetree.pexp_loc = (copy_location pexp_loc);
      Ast_410.Parsetree.pexp_loc_stack = (copy_location_stack pexp_loc_stack);
      Ast_410.Parsetree.pexp_attributes = (copy_attributes pexp_attributes)
    }
and copy_expr x = copy_expression x
and copy_expression_desc :
  Ast_411.Parsetree.expression_desc -> Ast_410.Parsetree.expression_desc =
  function
  | Ast_411.Parsetree.Pexp_ident x0 ->
      Ast_410.Parsetree.Pexp_ident (copy_loc copy_Longident_t x0)
  | Ast_411.Parsetree.Pexp_constant x0 ->
      Ast_410.Parsetree.Pexp_constant (copy_constant x0)
  | Ast_411.Parsetree.Pexp_let (x0, x1, x2) ->
      Ast_410.Parsetree.Pexp_let
        ((copy_rec_flag x0), (List.map copy_value_binding x1),
          (copy_expression x2))
  | Ast_411.Parsetree.Pexp_function x0 ->
      Ast_410.Parsetree.Pexp_function (List.map copy_case x0)
  | Ast_411.Parsetree.Pexp_fun (x0, x1, x2, x3) ->
      Ast_410.Parsetree.Pexp_fun
        ((copy_arg_label x0), (Option.map copy_expression x1),
          (copy_pattern x2), (copy_expression x3))
  | Ast_411.Parsetree.Pexp_apply (x0, x1) ->
      Ast_410.Parsetree.Pexp_apply
        ((copy_expression x0),
          (List.map
             (fun x ->
                let (x0, x1) = x in
                ((copy_arg_label x0), (copy_expression x1))) x1))
  | Ast_411.Parsetree.Pexp_match (x0, x1) ->
      Ast_410.Parsetree.Pexp_match
        ((copy_expression x0), (List.map copy_case x1))
  | Ast_411.Parsetree.Pexp_try (x0, x1) ->
      Ast_410.Parsetree.Pexp_try
        ((copy_expression x0), (List.map copy_case x1))
  | Ast_411.Parsetree.Pexp_tuple x0 ->
      Ast_410.Parsetree.Pexp_tuple (List.map copy_expression x0)
  | Ast_411.Parsetree.Pexp_construct (x0, x1) ->
      Ast_410.Parsetree.Pexp_construct
        ((copy_loc copy_Longident_t x0), (Option.map copy_expression x1))
  | Ast_411.Parsetree.Pexp_variant (x0, x1) ->
      Ast_410.Parsetree.Pexp_variant
        ((copy_label x0), (Option.map copy_expression x1))
  | Ast_411.Parsetree.Pexp_record (x0, x1) ->
      Ast_410.Parsetree.Pexp_record
        ((List.map
            (fun x ->
               let (x0, x1) = x in
               ((copy_loc copy_Longident_t x0), (copy_expression x1))) x0),
          (Option.map copy_expression x1))
  | Ast_411.Parsetree.Pexp_field (x0, x1) ->
      Ast_410.Parsetree.Pexp_field
        ((copy_expression x0), (copy_loc copy_Longident_t x1))
  | Ast_411.Parsetree.Pexp_setfield (x0, x1, x2) ->
      Ast_410.Parsetree.Pexp_setfield
        ((copy_expression x0), (copy_loc copy_Longident_t x1),
          (copy_expression x2))
  | Ast_411.Parsetree.Pexp_array x0 ->
      Ast_410.Parsetree.Pexp_array (List.map copy_expression x0)
  | Ast_411.Parsetree.Pexp_ifthenelse (x0, x1, x2) ->
      Ast_410.Parsetree.Pexp_ifthenelse
        ((copy_expression x0), (copy_expression x1),
          (Option.map copy_expression x2))
  | Ast_411.Parsetree.Pexp_sequence (x0, x1) ->
      Ast_410.Parsetree.Pexp_sequence
        ((copy_expression x0), (copy_expression x1))
  | Ast_411.Parsetree.Pexp_while (x0, x1) ->
      Ast_410.Parsetree.Pexp_while
        ((copy_expression x0), (copy_expression x1))
  | Ast_411.Parsetree.Pexp_for (x0, x1, x2, x3, x4) ->
      Ast_410.Parsetree.Pexp_for
        ((copy_pattern x0), (copy_expression x1), (copy_expression x2),
          (copy_direction_flag x3), (copy_expression x4))
  | Ast_411.Parsetree.Pexp_constraint (x0, x1) ->
      Ast_410.Parsetree.Pexp_constraint
        ((copy_expression x0), (copy_core_type x1))
  | Ast_411.Parsetree.Pexp_coerce (x0, x1, x2) ->
      Ast_410.Parsetree.Pexp_coerce
        ((copy_expression x0), (Option.map copy_core_type x1),
          (copy_core_type x2))
  | Ast_411.Parsetree.Pexp_send (x0, x1) ->
      Ast_410.Parsetree.Pexp_send
        ((copy_expression x0), (copy_loc copy_label x1))
  | Ast_411.Parsetree.Pexp_new x0 ->
      Ast_410.Parsetree.Pexp_new (copy_loc copy_Longident_t x0)
  | Ast_411.Parsetree.Pexp_setinstvar (x0, x1) ->
      Ast_410.Parsetree.Pexp_setinstvar
        ((copy_loc copy_label x0), (copy_expression x1))
  | Ast_411.Parsetree.Pexp_override x0 ->
      Ast_410.Parsetree.Pexp_override
        (List.map
           (fun x ->
              let (x0, x1) = x in
              ((copy_loc copy_label x0), (copy_expression x1))) x0)
  | Ast_411.Parsetree.Pexp_letmodule (x0, x1, x2) ->
      Ast_410.Parsetree.Pexp_letmodule
        ((copy_loc (fun x -> Option.map (fun x -> x) x) x0),
          (copy_module_expr x1), (copy_expression x2))
  | Ast_411.Parsetree.Pexp_letexception (x0, x1) ->
      Ast_410.Parsetree.Pexp_letexception
        ((copy_extension_constructor x0), (copy_expression x1))
  | Ast_411.Parsetree.Pexp_assert x0 ->
      Ast_410.Parsetree.Pexp_assert (copy_expression x0)
  | Ast_411.Parsetree.Pexp_lazy x0 ->
      Ast_410.Parsetree.Pexp_lazy (copy_expression x0)
  | Ast_411.Parsetree.Pexp_poly (x0, x1) ->
      Ast_410.Parsetree.Pexp_poly
        ((copy_expression x0), (Option.map copy_core_type x1))
  | Ast_411.Parsetree.Pexp_object x0 ->
      Ast_410.Parsetree.Pexp_object (copy_class_structure x0)
  | Ast_411.Parsetree.Pexp_newtype (x0, x1) ->
      Ast_410.Parsetree.Pexp_newtype
        ((copy_loc (fun x -> x) x0), (copy_expression x1))
  | Ast_411.Parsetree.Pexp_pack x0 ->
      Ast_410.Parsetree.Pexp_pack (copy_module_expr x0)
  | Ast_411.Parsetree.Pexp_open (x0, x1) ->
      Ast_410.Parsetree.Pexp_open
        ((copy_open_declaration x0), (copy_expression x1))
  | Ast_411.Parsetree.Pexp_letop x0 ->
      Ast_410.Parsetree.Pexp_letop (copy_letop x0)
  | Ast_411.Parsetree.Pexp_extension x0 ->
      Ast_410.Parsetree.Pexp_extension (copy_extension x0)
  | Ast_411.Parsetree.Pexp_unreachable -> Ast_410.Parsetree.Pexp_unreachable
and copy_letop : Ast_411.Parsetree.letop -> Ast_410.Parsetree.letop =
  fun
    { Ast_411.Parsetree.let_ = let_; Ast_411.Parsetree.ands = ands;
      Ast_411.Parsetree.body = body }
    ->
    {
      Ast_410.Parsetree.let_ = (copy_binding_op let_);
      Ast_410.Parsetree.ands = (List.map copy_binding_op ands);
      Ast_410.Parsetree.body = (copy_expression body)
    }
and copy_binding_op :
  Ast_411.Parsetree.binding_op -> Ast_410.Parsetree.binding_op =
  fun
    { Ast_411.Parsetree.pbop_op = pbop_op;
      Ast_411.Parsetree.pbop_pat = pbop_pat;
      Ast_411.Parsetree.pbop_exp = pbop_exp;
      Ast_411.Parsetree.pbop_loc = pbop_loc }
    ->
    {
      Ast_410.Parsetree.pbop_op = (copy_loc (fun x -> x) pbop_op);
      Ast_410.Parsetree.pbop_pat = (copy_pattern pbop_pat);
      Ast_410.Parsetree.pbop_exp = (copy_expression pbop_exp);
      Ast_410.Parsetree.pbop_loc = (copy_location pbop_loc)
    }
and copy_direction_flag :
  Ast_411.Asttypes.direction_flag -> Ast_410.Asttypes.direction_flag =
  function
  | Ast_411.Asttypes.Upto -> Ast_410.Asttypes.Upto
  | Ast_411.Asttypes.Downto -> Ast_410.Asttypes.Downto
and copy_case : Ast_411.Parsetree.case -> Ast_410.Parsetree.case =
  fun
    { Ast_411.Parsetree.pc_lhs = pc_lhs;
      Ast_411.Parsetree.pc_guard = pc_guard;
      Ast_411.Parsetree.pc_rhs = pc_rhs }
    ->
    {
      Ast_410.Parsetree.pc_lhs = (copy_pattern pc_lhs);
      Ast_410.Parsetree.pc_guard = (Option.map copy_expression pc_guard);
      Ast_410.Parsetree.pc_rhs = (copy_expression pc_rhs)
    }
and copy_cases : Ast_411.Parsetree.case list -> Ast_410.Parsetree.case list =
  fun x -> List.map copy_case x
and copy_value_binding :
  Ast_411.Parsetree.value_binding -> Ast_410.Parsetree.value_binding =
  fun
    { Ast_411.Parsetree.pvb_pat = pvb_pat;
      Ast_411.Parsetree.pvb_expr = pvb_expr;
      Ast_411.Parsetree.pvb_attributes = pvb_attributes;
      Ast_411.Parsetree.pvb_loc = pvb_loc }
    ->
    {
      Ast_410.Parsetree.pvb_pat = (copy_pattern pvb_pat);
      Ast_410.Parsetree.pvb_expr = (copy_expression pvb_expr);
      Ast_410.Parsetree.pvb_attributes = (copy_attributes pvb_attributes);
      Ast_410.Parsetree.pvb_loc = (copy_location pvb_loc)
    }
and copy_pattern : Ast_411.Parsetree.pattern -> Ast_410.Parsetree.pattern =
  fun
    { Ast_411.Parsetree.ppat_desc = ppat_desc;
      Ast_411.Parsetree.ppat_loc = ppat_loc;
      Ast_411.Parsetree.ppat_loc_stack = ppat_loc_stack;
      Ast_411.Parsetree.ppat_attributes = ppat_attributes }
    ->
    {
      Ast_410.Parsetree.ppat_desc = (copy_pattern_desc ppat_desc);
      Ast_410.Parsetree.ppat_loc = (copy_location ppat_loc);
      Ast_410.Parsetree.ppat_loc_stack = (copy_location_stack ppat_loc_stack);
      Ast_410.Parsetree.ppat_attributes = (copy_attributes ppat_attributes)
    }
and copy_pat x = copy_pattern x
and copy_pattern_desc :
  Ast_411.Parsetree.pattern_desc -> Ast_410.Parsetree.pattern_desc =
  function
  | Ast_411.Parsetree.Ppat_any -> Ast_410.Parsetree.Ppat_any
  | Ast_411.Parsetree.Ppat_var x0 ->
      Ast_410.Parsetree.Ppat_var (copy_loc (fun x -> x) x0)
  | Ast_411.Parsetree.Ppat_alias (x0, x1) ->
      Ast_410.Parsetree.Ppat_alias
        ((copy_pattern x0), (copy_loc (fun x -> x) x1))
  | Ast_411.Parsetree.Ppat_constant x0 ->
      Ast_410.Parsetree.Ppat_constant (copy_constant x0)
  | Ast_411.Parsetree.Ppat_interval (x0, x1) ->
      Ast_410.Parsetree.Ppat_interval
        ((copy_constant x0), (copy_constant x1))
  | Ast_411.Parsetree.Ppat_tuple x0 ->
      Ast_410.Parsetree.Ppat_tuple (List.map copy_pattern x0)
  | Ast_411.Parsetree.Ppat_construct (x0, x1) ->
      Ast_410.Parsetree.Ppat_construct
        ((copy_loc copy_Longident_t x0), (Option.map copy_pattern x1))
  | Ast_411.Parsetree.Ppat_variant (x0, x1) ->
      Ast_410.Parsetree.Ppat_variant
        ((copy_label x0), (Option.map copy_pattern x1))
  | Ast_411.Parsetree.Ppat_record (x0, x1) ->
      Ast_410.Parsetree.Ppat_record
        ((List.map
            (fun x ->
               let (x0, x1) = x in
               ((copy_loc copy_Longident_t x0), (copy_pattern x1))) x0),
          (copy_closed_flag x1))
  | Ast_411.Parsetree.Ppat_array x0 ->
      Ast_410.Parsetree.Ppat_array (List.map copy_pattern x0)
  | Ast_411.Parsetree.Ppat_or (x0, x1) ->
      Ast_410.Parsetree.Ppat_or ((copy_pattern x0), (copy_pattern x1))
  | Ast_411.Parsetree.Ppat_constraint (x0, x1) ->
      Ast_410.Parsetree.Ppat_constraint
        ((copy_pattern x0), (copy_core_type x1))
  | Ast_411.Parsetree.Ppat_type x0 ->
      Ast_410.Parsetree.Ppat_type (copy_loc copy_Longident_t x0)
  | Ast_411.Parsetree.Ppat_lazy x0 ->
      Ast_410.Parsetree.Ppat_lazy (copy_pattern x0)
  | Ast_411.Parsetree.Ppat_unpack x0 ->
      Ast_410.Parsetree.Ppat_unpack
        (copy_loc (fun x -> Option.map (fun x -> x) x) x0)
  | Ast_411.Parsetree.Ppat_exception x0 ->
      Ast_410.Parsetree.Ppat_exception (copy_pattern x0)
  | Ast_411.Parsetree.Ppat_extension x0 ->
      Ast_410.Parsetree.Ppat_extension (copy_extension x0)
  | Ast_411.Parsetree.Ppat_open (x0, x1) ->
      Ast_410.Parsetree.Ppat_open
        ((copy_loc copy_Longident_t x0), (copy_pattern x1))
and copy_core_type :
  Ast_411.Parsetree.core_type -> Ast_410.Parsetree.core_type =
  fun
    { Ast_411.Parsetree.ptyp_desc = ptyp_desc;
      Ast_411.Parsetree.ptyp_loc = ptyp_loc;
      Ast_411.Parsetree.ptyp_loc_stack = ptyp_loc_stack;
      Ast_411.Parsetree.ptyp_attributes = ptyp_attributes }
    ->
    {
      Ast_410.Parsetree.ptyp_desc = (copy_core_type_desc ptyp_desc);
      Ast_410.Parsetree.ptyp_loc = (copy_location ptyp_loc);
      Ast_410.Parsetree.ptyp_loc_stack = (copy_location_stack ptyp_loc_stack);
      Ast_410.Parsetree.ptyp_attributes = (copy_attributes ptyp_attributes)
    }
and copy_typ x = copy_core_type x
and copy_location_stack :
  Ast_411.Parsetree.location_stack -> Ast_410.Parsetree.location_stack =
  fun x -> List.map copy_location x
and copy_core_type_desc :
  Ast_411.Parsetree.core_type_desc -> Ast_410.Parsetree.core_type_desc =
  function
  | Ast_411.Parsetree.Ptyp_any -> Ast_410.Parsetree.Ptyp_any
  | Ast_411.Parsetree.Ptyp_var x0 -> Ast_410.Parsetree.Ptyp_var x0
  | Ast_411.Parsetree.Ptyp_arrow (x0, x1, x2) ->
      Ast_410.Parsetree.Ptyp_arrow
        ((copy_arg_label x0), (copy_core_type x1), (copy_core_type x2))
  | Ast_411.Parsetree.Ptyp_tuple x0 ->
      Ast_410.Parsetree.Ptyp_tuple (List.map copy_core_type x0)
  | Ast_411.Parsetree.Ptyp_constr (x0, x1) ->
      Ast_410.Parsetree.Ptyp_constr
        ((copy_loc copy_Longident_t x0), (List.map copy_core_type x1))
  | Ast_411.Parsetree.Ptyp_object (x0, x1) ->
      Ast_410.Parsetree.Ptyp_object
        ((List.map copy_object_field x0), (copy_closed_flag x1))
  | Ast_411.Parsetree.Ptyp_class (x0, x1) ->
      Ast_410.Parsetree.Ptyp_class
        ((copy_loc copy_Longident_t x0), (List.map copy_core_type x1))
  | Ast_411.Parsetree.Ptyp_alias (x0, x1) ->
      Ast_410.Parsetree.Ptyp_alias ((copy_core_type x0), x1)
  | Ast_411.Parsetree.Ptyp_variant (x0, x1, x2) ->
      Ast_410.Parsetree.Ptyp_variant
        ((List.map copy_row_field x0), (copy_closed_flag x1),
          (Option.map (fun x -> List.map copy_label x) x2))
  | Ast_411.Parsetree.Ptyp_poly (x0, x1) ->
      Ast_410.Parsetree.Ptyp_poly
        ((List.map (fun x -> copy_loc (fun x -> x) x) x0),
          (copy_core_type x1))
  | Ast_411.Parsetree.Ptyp_package x0 ->
      Ast_410.Parsetree.Ptyp_package (copy_package_type x0)
  | Ast_411.Parsetree.Ptyp_extension x0 ->
      Ast_410.Parsetree.Ptyp_extension (copy_extension x0)
and copy_package_type :
  Ast_411.Parsetree.package_type -> Ast_410.Parsetree.package_type =
  fun x ->
    let (x0, x1) = x in
    ((copy_loc copy_Longident_t x0),
      (List.map
         (fun x ->
            let (x0, x1) = x in
            ((copy_loc copy_Longident_t x0), (copy_core_type x1))) x1))
and copy_row_field :
  Ast_411.Parsetree.row_field -> Ast_410.Parsetree.row_field =
  fun
    { Ast_411.Parsetree.prf_desc = prf_desc;
      Ast_411.Parsetree.prf_loc = prf_loc;
      Ast_411.Parsetree.prf_attributes = prf_attributes }
    ->
    {
      Ast_410.Parsetree.prf_desc = (copy_row_field_desc prf_desc);
      Ast_410.Parsetree.prf_loc = (copy_location prf_loc);
      Ast_410.Parsetree.prf_attributes = (copy_attributes prf_attributes)
    }
and copy_row_field_desc :
  Ast_411.Parsetree.row_field_desc -> Ast_410.Parsetree.row_field_desc =
  function
  | Ast_411.Parsetree.Rtag (x0, x1, x2) ->
      Ast_410.Parsetree.Rtag
        ((copy_loc copy_label x0), x1, (List.map copy_core_type x2))
  | Ast_411.Parsetree.Rinherit x0 ->
      Ast_410.Parsetree.Rinherit (copy_core_type x0)
and copy_object_field :
  Ast_411.Parsetree.object_field -> Ast_410.Parsetree.object_field =
  fun
    { Ast_411.Parsetree.pof_desc = pof_desc;
      Ast_411.Parsetree.pof_loc = pof_loc;
      Ast_411.Parsetree.pof_attributes = pof_attributes }
    ->
    {
      Ast_410.Parsetree.pof_desc = (copy_object_field_desc pof_desc);
      Ast_410.Parsetree.pof_loc = (copy_location pof_loc);
      Ast_410.Parsetree.pof_attributes = (copy_attributes pof_attributes)
    }
and copy_attributes :
  Ast_411.Parsetree.attributes -> Ast_410.Parsetree.attributes =
  fun x -> List.map copy_attribute x
and copy_attribute :
  Ast_411.Parsetree.attribute -> Ast_410.Parsetree.attribute =
  fun
    { Ast_411.Parsetree.attr_name = attr_name;
      Ast_411.Parsetree.attr_payload = attr_payload;
      Ast_411.Parsetree.attr_loc = attr_loc }
    ->
    {
      Ast_410.Parsetree.attr_name = (copy_loc (fun x -> x) attr_name);
      Ast_410.Parsetree.attr_payload = (copy_payload attr_payload);
      Ast_410.Parsetree.attr_loc = (copy_location attr_loc)
    }
and copy_payload : Ast_411.Parsetree.payload -> Ast_410.Parsetree.payload =
  function
  | Ast_411.Parsetree.PStr x0 -> Ast_410.Parsetree.PStr (copy_structure x0)
  | Ast_411.Parsetree.PSig x0 -> Ast_410.Parsetree.PSig (copy_signature x0)
  | Ast_411.Parsetree.PTyp x0 -> Ast_410.Parsetree.PTyp (copy_core_type x0)
  | Ast_411.Parsetree.PPat (x0, x1) ->
      Ast_410.Parsetree.PPat
        ((copy_pattern x0), (Option.map copy_expression x1))
and copy_structure :
  Ast_411.Parsetree.structure -> Ast_410.Parsetree.structure =
  fun x -> List.map copy_structure_item x
and copy_structure_item :
  Ast_411.Parsetree.structure_item -> Ast_410.Parsetree.structure_item =
  fun
    { Ast_411.Parsetree.pstr_desc = pstr_desc;
      Ast_411.Parsetree.pstr_loc = pstr_loc }
    ->
    {
      Ast_410.Parsetree.pstr_desc = (copy_structure_item_desc pstr_desc);
      Ast_410.Parsetree.pstr_loc = (copy_location pstr_loc)
    }
and copy_structure_item_desc :
  Ast_411.Parsetree.structure_item_desc ->
    Ast_410.Parsetree.structure_item_desc
  =
  function
  | Ast_411.Parsetree.Pstr_eval (x0, x1) ->
      Ast_410.Parsetree.Pstr_eval
        ((copy_expression x0), (copy_attributes x1))
  | Ast_411.Parsetree.Pstr_value (x0, x1) ->
      Ast_410.Parsetree.Pstr_value
        ((copy_rec_flag x0), (List.map copy_value_binding x1))
  | Ast_411.Parsetree.Pstr_primitive x0 ->
      Ast_410.Parsetree.Pstr_primitive (copy_value_description x0)
  | Ast_411.Parsetree.Pstr_type (x0, x1) ->
      Ast_410.Parsetree.Pstr_type
        ((copy_rec_flag x0), (List.map copy_type_declaration x1))
  | Ast_411.Parsetree.Pstr_typext x0 ->
      Ast_410.Parsetree.Pstr_typext (copy_type_extension x0)
  | Ast_411.Parsetree.Pstr_exception x0 ->
      Ast_410.Parsetree.Pstr_exception (copy_type_exception x0)
  | Ast_411.Parsetree.Pstr_module x0 ->
      Ast_410.Parsetree.Pstr_module (copy_module_binding x0)
  | Ast_411.Parsetree.Pstr_recmodule x0 ->
      Ast_410.Parsetree.Pstr_recmodule (List.map copy_module_binding x0)
  | Ast_411.Parsetree.Pstr_modtype x0 ->
      Ast_410.Parsetree.Pstr_modtype (copy_module_type_declaration x0)
  | Ast_411.Parsetree.Pstr_open x0 ->
      Ast_410.Parsetree.Pstr_open (copy_open_declaration x0)
  | Ast_411.Parsetree.Pstr_class x0 ->
      Ast_410.Parsetree.Pstr_class (List.map copy_class_declaration x0)
  | Ast_411.Parsetree.Pstr_class_type x0 ->
      Ast_410.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | Ast_411.Parsetree.Pstr_include x0 ->
      Ast_410.Parsetree.Pstr_include (copy_include_declaration x0)
  | Ast_411.Parsetree.Pstr_attribute x0 ->
      Ast_410.Parsetree.Pstr_attribute (copy_attribute x0)
  | Ast_411.Parsetree.Pstr_extension (x0, x1) ->
      Ast_410.Parsetree.Pstr_extension
        ((copy_extension x0), (copy_attributes x1))
and copy_include_declaration :
  Ast_411.Parsetree.include_declaration ->
    Ast_410.Parsetree.include_declaration
  = fun x -> copy_include_infos copy_module_expr x
and copy_class_declaration :
  Ast_411.Parsetree.class_declaration -> Ast_410.Parsetree.class_declaration
  = fun x -> copy_class_infos copy_class_expr x
and copy_class_expr :
  Ast_411.Parsetree.class_expr -> Ast_410.Parsetree.class_expr =
  fun
    { Ast_411.Parsetree.pcl_desc = pcl_desc;
      Ast_411.Parsetree.pcl_loc = pcl_loc;
      Ast_411.Parsetree.pcl_attributes = pcl_attributes }
    ->
    {
      Ast_410.Parsetree.pcl_desc = (copy_class_expr_desc pcl_desc);
      Ast_410.Parsetree.pcl_loc = (copy_location pcl_loc);
      Ast_410.Parsetree.pcl_attributes = (copy_attributes pcl_attributes)
    }
and copy_class_expr_desc :
  Ast_411.Parsetree.class_expr_desc -> Ast_410.Parsetree.class_expr_desc =
  function
  | Ast_411.Parsetree.Pcl_constr (x0, x1) ->
      Ast_410.Parsetree.Pcl_constr
        ((copy_loc copy_Longident_t x0), (List.map copy_core_type x1))
  | Ast_411.Parsetree.Pcl_structure x0 ->
      Ast_410.Parsetree.Pcl_structure (copy_class_structure x0)
  | Ast_411.Parsetree.Pcl_fun (x0, x1, x2, x3) ->
      Ast_410.Parsetree.Pcl_fun
        ((copy_arg_label x0), (Option.map copy_expression x1),
          (copy_pattern x2), (copy_class_expr x3))
  | Ast_411.Parsetree.Pcl_apply (x0, x1) ->
      Ast_410.Parsetree.Pcl_apply
        ((copy_class_expr x0),
          (List.map
             (fun x ->
                let (x0, x1) = x in
                ((copy_arg_label x0), (copy_expression x1))) x1))
  | Ast_411.Parsetree.Pcl_let (x0, x1, x2) ->
      Ast_410.Parsetree.Pcl_let
        ((copy_rec_flag x0), (List.map copy_value_binding x1),
          (copy_class_expr x2))
  | Ast_411.Parsetree.Pcl_constraint (x0, x1) ->
      Ast_410.Parsetree.Pcl_constraint
        ((copy_class_expr x0), (copy_class_type x1))
  | Ast_411.Parsetree.Pcl_extension x0 ->
      Ast_410.Parsetree.Pcl_extension (copy_extension x0)
  | Ast_411.Parsetree.Pcl_open (x0, x1) ->
      Ast_410.Parsetree.Pcl_open
        ((copy_open_description x0), (copy_class_expr x1))
and copy_class_structure :
  Ast_411.Parsetree.class_structure -> Ast_410.Parsetree.class_structure =
  fun
    { Ast_411.Parsetree.pcstr_self = pcstr_self;
      Ast_411.Parsetree.pcstr_fields = pcstr_fields }
    ->
    {
      Ast_410.Parsetree.pcstr_self = (copy_pattern pcstr_self);
      Ast_410.Parsetree.pcstr_fields =
        (List.map copy_class_field pcstr_fields)
    }
and copy_class_field :
  Ast_411.Parsetree.class_field -> Ast_410.Parsetree.class_field =
  fun
    { Ast_411.Parsetree.pcf_desc = pcf_desc;
      Ast_411.Parsetree.pcf_loc = pcf_loc;
      Ast_411.Parsetree.pcf_attributes = pcf_attributes }
    ->
    {
      Ast_410.Parsetree.pcf_desc = (copy_class_field_desc pcf_desc);
      Ast_410.Parsetree.pcf_loc = (copy_location pcf_loc);
      Ast_410.Parsetree.pcf_attributes = (copy_attributes pcf_attributes)
    }
and copy_class_field_desc :
  Ast_411.Parsetree.class_field_desc -> Ast_410.Parsetree.class_field_desc =
  function
  | Ast_411.Parsetree.Pcf_inherit (x0, x1, x2) ->
      Ast_410.Parsetree.Pcf_inherit
        ((copy_override_flag x0), (copy_class_expr x1),
          (Option.map (fun x -> copy_loc (fun x -> x) x) x2))
  | Ast_411.Parsetree.Pcf_val x0 ->
      Ast_410.Parsetree.Pcf_val
        (let (x0, x1, x2) = x0 in
         ((copy_loc copy_label x0), (copy_mutable_flag x1),
           (copy_class_field_kind x2)))
  | Ast_411.Parsetree.Pcf_method x0 ->
      Ast_410.Parsetree.Pcf_method
        (let (x0, x1, x2) = x0 in
         ((copy_loc copy_label x0), (copy_private_flag x1),
           (copy_class_field_kind x2)))
  | Ast_411.Parsetree.Pcf_constraint x0 ->
      Ast_410.Parsetree.Pcf_constraint
        (let (x0, x1) = x0 in ((copy_core_type x0), (copy_core_type x1)))
  | Ast_411.Parsetree.Pcf_initializer x0 ->
      Ast_410.Parsetree.Pcf_initializer (copy_expression x0)
  | Ast_411.Parsetree.Pcf_attribute x0 ->
      Ast_410.Parsetree.Pcf_attribute (copy_attribute x0)
  | Ast_411.Parsetree.Pcf_extension x0 ->
      Ast_410.Parsetree.Pcf_extension (copy_extension x0)
and copy_class_field_kind :
  Ast_411.Parsetree.class_field_kind -> Ast_410.Parsetree.class_field_kind =
  function
  | Ast_411.Parsetree.Cfk_virtual x0 ->
      Ast_410.Parsetree.Cfk_virtual (copy_core_type x0)
  | Ast_411.Parsetree.Cfk_concrete (x0, x1) ->
      Ast_410.Parsetree.Cfk_concrete
        ((copy_override_flag x0), (copy_expression x1))
and copy_open_declaration :
  Ast_411.Parsetree.open_declaration -> Ast_410.Parsetree.open_declaration =
  fun x -> copy_open_infos copy_module_expr x
and copy_module_binding :
  Ast_411.Parsetree.module_binding -> Ast_410.Parsetree.module_binding =
  fun
    { Ast_411.Parsetree.pmb_name = pmb_name;
      Ast_411.Parsetree.pmb_expr = pmb_expr;
      Ast_411.Parsetree.pmb_attributes = pmb_attributes;
      Ast_411.Parsetree.pmb_loc = pmb_loc }
    ->
    {
      Ast_410.Parsetree.pmb_name =
        (copy_loc (fun x -> Option.map (fun x -> x) x) pmb_name);
      Ast_410.Parsetree.pmb_expr = (copy_module_expr pmb_expr);
      Ast_410.Parsetree.pmb_attributes = (copy_attributes pmb_attributes);
      Ast_410.Parsetree.pmb_loc = (copy_location pmb_loc)
    }
and copy_module_expr :
  Ast_411.Parsetree.module_expr -> Ast_410.Parsetree.module_expr =
  fun
    { Ast_411.Parsetree.pmod_desc = pmod_desc;
      Ast_411.Parsetree.pmod_loc = pmod_loc;
      Ast_411.Parsetree.pmod_attributes = pmod_attributes }
    ->
    {
      Ast_410.Parsetree.pmod_desc = (copy_module_expr_desc pmod_desc);
      Ast_410.Parsetree.pmod_loc = (copy_location pmod_loc);
      Ast_410.Parsetree.pmod_attributes = (copy_attributes pmod_attributes)
    }
and copy_module_expr_desc :
  Ast_411.Parsetree.module_expr_desc -> Ast_410.Parsetree.module_expr_desc =
  function
  | Ast_411.Parsetree.Pmod_ident x0 ->
      Ast_410.Parsetree.Pmod_ident (copy_loc copy_Longident_t x0)
  | Ast_411.Parsetree.Pmod_structure x0 ->
      Ast_410.Parsetree.Pmod_structure (copy_structure x0)
  | Ast_411.Parsetree.Pmod_functor (x0, x1) ->
      Ast_410.Parsetree.Pmod_functor
        ((copy_functor_parameter x0), (copy_module_expr x1))
  | Ast_411.Parsetree.Pmod_apply (x0, x1) ->
      Ast_410.Parsetree.Pmod_apply
        ((copy_module_expr x0), (copy_module_expr x1))
  | Ast_411.Parsetree.Pmod_constraint (x0, x1) ->
      Ast_410.Parsetree.Pmod_constraint
        ((copy_module_expr x0), (copy_module_type x1))
  | Ast_411.Parsetree.Pmod_unpack x0 ->
      Ast_410.Parsetree.Pmod_unpack (copy_expression x0)
  | Ast_411.Parsetree.Pmod_extension x0 ->
      Ast_410.Parsetree.Pmod_extension (copy_extension x0)
and copy_functor_parameter :
  Ast_411.Parsetree.functor_parameter -> Ast_410.Parsetree.functor_parameter
  =
  function
  | Ast_411.Parsetree.Unit -> Ast_410.Parsetree.Unit
  | Ast_411.Parsetree.Named (x0, x1) ->
      Ast_410.Parsetree.Named
        ((copy_loc (fun x -> Option.map (fun x -> x) x) x0),
          (copy_module_type x1))
and copy_module_type :
  Ast_411.Parsetree.module_type -> Ast_410.Parsetree.module_type =
  fun
    { Ast_411.Parsetree.pmty_desc = pmty_desc;
      Ast_411.Parsetree.pmty_loc = pmty_loc;
      Ast_411.Parsetree.pmty_attributes = pmty_attributes }
    ->
    {
      Ast_410.Parsetree.pmty_desc = (copy_module_type_desc pmty_desc);
      Ast_410.Parsetree.pmty_loc = (copy_location pmty_loc);
      Ast_410.Parsetree.pmty_attributes = (copy_attributes pmty_attributes)
    }
and copy_module_type_desc :
  Ast_411.Parsetree.module_type_desc -> Ast_410.Parsetree.module_type_desc =
  function
  | Ast_411.Parsetree.Pmty_ident x0 ->
      Ast_410.Parsetree.Pmty_ident (copy_loc copy_Longident_t x0)
  | Ast_411.Parsetree.Pmty_signature x0 ->
      Ast_410.Parsetree.Pmty_signature (copy_signature x0)
  | Ast_411.Parsetree.Pmty_functor (x0, x1) ->
      Ast_410.Parsetree.Pmty_functor
        ((copy_functor_parameter x0), (copy_module_type x1))
  | Ast_411.Parsetree.Pmty_with (x0, x1) ->
      Ast_410.Parsetree.Pmty_with
        ((copy_module_type x0), (List.map copy_with_constraint x1))
  | Ast_411.Parsetree.Pmty_typeof x0 ->
      Ast_410.Parsetree.Pmty_typeof (copy_module_expr x0)
  | Ast_411.Parsetree.Pmty_extension x0 ->
      Ast_410.Parsetree.Pmty_extension (copy_extension x0)
  | Ast_411.Parsetree.Pmty_alias x0 ->
      Ast_410.Parsetree.Pmty_alias (copy_loc copy_Longident_t x0)
and copy_with_constraint :
  Ast_411.Parsetree.with_constraint -> Ast_410.Parsetree.with_constraint =
  function
  | Ast_411.Parsetree.Pwith_type (x0, x1) ->
      Ast_410.Parsetree.Pwith_type
        ((copy_loc copy_Longident_t x0), (copy_type_declaration x1))
  | Ast_411.Parsetree.Pwith_module (x0, x1) ->
      Ast_410.Parsetree.Pwith_module
        ((copy_loc copy_Longident_t x0), (copy_loc copy_Longident_t x1))
  | Ast_411.Parsetree.Pwith_typesubst (x0, x1) ->
      Ast_410.Parsetree.Pwith_typesubst
        ((copy_loc copy_Longident_t x0), (copy_type_declaration x1))
  | Ast_411.Parsetree.Pwith_modsubst (x0, x1) ->
      Ast_410.Parsetree.Pwith_modsubst
        ((copy_loc copy_Longident_t x0), (copy_loc copy_Longident_t x1))
and copy_signature :
  Ast_411.Parsetree.signature -> Ast_410.Parsetree.signature =
  fun x -> List.map copy_signature_item x
and copy_signature_item :
  Ast_411.Parsetree.signature_item -> Ast_410.Parsetree.signature_item =
  fun
    { Ast_411.Parsetree.psig_desc = psig_desc;
      Ast_411.Parsetree.psig_loc = psig_loc }
    ->
    {
      Ast_410.Parsetree.psig_desc = (copy_signature_item_desc psig_desc);
      Ast_410.Parsetree.psig_loc = (copy_location psig_loc)
    }
and copy_signature_item_desc :
  Ast_411.Parsetree.signature_item_desc ->
    Ast_410.Parsetree.signature_item_desc
  =
  function
  | Ast_411.Parsetree.Psig_value x0 ->
      Ast_410.Parsetree.Psig_value (copy_value_description x0)
  | Ast_411.Parsetree.Psig_type (x0, x1) ->
      Ast_410.Parsetree.Psig_type
        ((copy_rec_flag x0), (List.map copy_type_declaration x1))
  | Ast_411.Parsetree.Psig_typesubst x0 ->
      Ast_410.Parsetree.Psig_typesubst (List.map copy_type_declaration x0)
  | Ast_411.Parsetree.Psig_typext x0 ->
      Ast_410.Parsetree.Psig_typext (copy_type_extension x0)
  | Ast_411.Parsetree.Psig_exception x0 ->
      Ast_410.Parsetree.Psig_exception (copy_type_exception x0)
  | Ast_411.Parsetree.Psig_module x0 ->
      Ast_410.Parsetree.Psig_module (copy_module_declaration x0)
  | Ast_411.Parsetree.Psig_modsubst x0 ->
      Ast_410.Parsetree.Psig_modsubst (copy_module_substitution x0)
  | Ast_411.Parsetree.Psig_recmodule x0 ->
      Ast_410.Parsetree.Psig_recmodule (List.map copy_module_declaration x0)
  | Ast_411.Parsetree.Psig_modtype x0 ->
      Ast_410.Parsetree.Psig_modtype (copy_module_type_declaration x0)
  | Ast_411.Parsetree.Psig_open x0 ->
      Ast_410.Parsetree.Psig_open (copy_open_description x0)
  | Ast_411.Parsetree.Psig_include x0 ->
      Ast_410.Parsetree.Psig_include (copy_include_description x0)
  | Ast_411.Parsetree.Psig_class x0 ->
      Ast_410.Parsetree.Psig_class (List.map copy_class_description x0)
  | Ast_411.Parsetree.Psig_class_type x0 ->
      Ast_410.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | Ast_411.Parsetree.Psig_attribute x0 ->
      Ast_410.Parsetree.Psig_attribute (copy_attribute x0)
  | Ast_411.Parsetree.Psig_extension (x0, x1) ->
      Ast_410.Parsetree.Psig_extension
        ((copy_extension x0), (copy_attributes x1))
and copy_class_type_declaration :
  Ast_411.Parsetree.class_type_declaration ->
    Ast_410.Parsetree.class_type_declaration
  = fun x -> copy_class_infos copy_class_type x
and copy_class_description :
  Ast_411.Parsetree.class_description -> Ast_410.Parsetree.class_description
  = fun x -> copy_class_infos copy_class_type x
and copy_class_type :
  Ast_411.Parsetree.class_type -> Ast_410.Parsetree.class_type =
  fun
    { Ast_411.Parsetree.pcty_desc = pcty_desc;
      Ast_411.Parsetree.pcty_loc = pcty_loc;
      Ast_411.Parsetree.pcty_attributes = pcty_attributes }
    ->
    {
      Ast_410.Parsetree.pcty_desc = (copy_class_type_desc pcty_desc);
      Ast_410.Parsetree.pcty_loc = (copy_location pcty_loc);
      Ast_410.Parsetree.pcty_attributes = (copy_attributes pcty_attributes)
    }
and copy_class_type_desc :
  Ast_411.Parsetree.class_type_desc -> Ast_410.Parsetree.class_type_desc =
  function
  | Ast_411.Parsetree.Pcty_constr (x0, x1) ->
      Ast_410.Parsetree.Pcty_constr
        ((copy_loc copy_Longident_t x0), (List.map copy_core_type x1))
  | Ast_411.Parsetree.Pcty_signature x0 ->
      Ast_410.Parsetree.Pcty_signature (copy_class_signature x0)
  | Ast_411.Parsetree.Pcty_arrow (x0, x1, x2) ->
      Ast_410.Parsetree.Pcty_arrow
        ((copy_arg_label x0), (copy_core_type x1), (copy_class_type x2))
  | Ast_411.Parsetree.Pcty_extension x0 ->
      Ast_410.Parsetree.Pcty_extension (copy_extension x0)
  | Ast_411.Parsetree.Pcty_open (x0, x1) ->
      Ast_410.Parsetree.Pcty_open
        ((copy_open_description x0), (copy_class_type x1))
and copy_class_signature :
  Ast_411.Parsetree.class_signature -> Ast_410.Parsetree.class_signature =
  fun
    { Ast_411.Parsetree.pcsig_self = pcsig_self;
      Ast_411.Parsetree.pcsig_fields = pcsig_fields }
    ->
    {
      Ast_410.Parsetree.pcsig_self = (copy_core_type pcsig_self);
      Ast_410.Parsetree.pcsig_fields =
        (List.map copy_class_type_field pcsig_fields)
    }
and copy_class_type_field :
  Ast_411.Parsetree.class_type_field -> Ast_410.Parsetree.class_type_field =
  fun
    { Ast_411.Parsetree.pctf_desc = pctf_desc;
      Ast_411.Parsetree.pctf_loc = pctf_loc;
      Ast_411.Parsetree.pctf_attributes = pctf_attributes }
    ->
    {
      Ast_410.Parsetree.pctf_desc = (copy_class_type_field_desc pctf_desc);
      Ast_410.Parsetree.pctf_loc = (copy_location pctf_loc);
      Ast_410.Parsetree.pctf_attributes = (copy_attributes pctf_attributes)
    }
and copy_class_type_field_desc :
  Ast_411.Parsetree.class_type_field_desc ->
    Ast_410.Parsetree.class_type_field_desc
  =
  function
  | Ast_411.Parsetree.Pctf_inherit x0 ->
      Ast_410.Parsetree.Pctf_inherit (copy_class_type x0)
  | Ast_411.Parsetree.Pctf_val x0 ->
      Ast_410.Parsetree.Pctf_val
        (let (x0, x1, x2, x3) = x0 in
         ((copy_loc copy_label x0), (copy_mutable_flag x1),
           (copy_virtual_flag x2), (copy_core_type x3)))
  | Ast_411.Parsetree.Pctf_method x0 ->
      Ast_410.Parsetree.Pctf_method
        (let (x0, x1, x2, x3) = x0 in
         ((copy_loc copy_label x0), (copy_private_flag x1),
           (copy_virtual_flag x2), (copy_core_type x3)))
  | Ast_411.Parsetree.Pctf_constraint x0 ->
      Ast_410.Parsetree.Pctf_constraint
        (let (x0, x1) = x0 in ((copy_core_type x0), (copy_core_type x1)))
  | Ast_411.Parsetree.Pctf_attribute x0 ->
      Ast_410.Parsetree.Pctf_attribute (copy_attribute x0)
  | Ast_411.Parsetree.Pctf_extension x0 ->
      Ast_410.Parsetree.Pctf_extension (copy_extension x0)
and copy_extension :
  Ast_411.Parsetree.extension -> Ast_410.Parsetree.extension =
  fun x ->
    let (x0, x1) = x in ((copy_loc (fun x -> x) x0), (copy_payload x1))
and copy_class_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 Ast_411.Parsetree.class_infos -> 'g0 Ast_410.Parsetree.class_infos
  =
  fun f0 ->
    fun
      { Ast_411.Parsetree.pci_virt = pci_virt;
        Ast_411.Parsetree.pci_params = pci_params;
        Ast_411.Parsetree.pci_name = pci_name;
        Ast_411.Parsetree.pci_expr = pci_expr;
        Ast_411.Parsetree.pci_loc = pci_loc;
        Ast_411.Parsetree.pci_attributes = pci_attributes }
      ->
      {
        Ast_410.Parsetree.pci_virt = (copy_virtual_flag pci_virt);
        Ast_410.Parsetree.pci_params =
          (List.map
             (fun x ->
                let (x0, x1) = x in ((copy_core_type x0), (copy_variance x1)))
             pci_params);
        Ast_410.Parsetree.pci_name = (copy_loc (fun x -> x) pci_name);
        Ast_410.Parsetree.pci_expr = (f0 pci_expr);
        Ast_410.Parsetree.pci_loc = (copy_location pci_loc);
        Ast_410.Parsetree.pci_attributes = (copy_attributes pci_attributes)
      }
and copy_virtual_flag :
  Ast_411.Asttypes.virtual_flag -> Ast_410.Asttypes.virtual_flag =
  function
  | Ast_411.Asttypes.Virtual -> Ast_410.Asttypes.Virtual
  | Ast_411.Asttypes.Concrete -> Ast_410.Asttypes.Concrete
and copy_include_description :
  Ast_411.Parsetree.include_description ->
    Ast_410.Parsetree.include_description
  = fun x -> copy_include_infos copy_module_type x
and copy_include_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 Ast_411.Parsetree.include_infos ->
        'g0 Ast_410.Parsetree.include_infos
  =
  fun f0 ->
    fun
      { Ast_411.Parsetree.pincl_mod = pincl_mod;
        Ast_411.Parsetree.pincl_loc = pincl_loc;
        Ast_411.Parsetree.pincl_attributes = pincl_attributes }
      ->
      {
        Ast_410.Parsetree.pincl_mod = (f0 pincl_mod);
        Ast_410.Parsetree.pincl_loc = (copy_location pincl_loc);
        Ast_410.Parsetree.pincl_attributes =
          (copy_attributes pincl_attributes)
      }
and copy_open_description :
  Ast_411.Parsetree.open_description -> Ast_410.Parsetree.open_description =
  fun x -> copy_open_infos (fun x -> copy_loc copy_Longident_t x) x
and copy_open_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 Ast_411.Parsetree.open_infos -> 'g0 Ast_410.Parsetree.open_infos
  =
  fun f0 ->
    fun
      { Ast_411.Parsetree.popen_expr = popen_expr;
        Ast_411.Parsetree.popen_override = popen_override;
        Ast_411.Parsetree.popen_loc = popen_loc;
        Ast_411.Parsetree.popen_attributes = popen_attributes }
      ->
      {
        Ast_410.Parsetree.popen_expr = (f0 popen_expr);
        Ast_410.Parsetree.popen_override =
          (copy_override_flag popen_override);
        Ast_410.Parsetree.popen_loc = (copy_location popen_loc);
        Ast_410.Parsetree.popen_attributes =
          (copy_attributes popen_attributes)
      }
and copy_override_flag :
  Ast_411.Asttypes.override_flag -> Ast_410.Asttypes.override_flag =
  function
  | Ast_411.Asttypes.Override -> Ast_410.Asttypes.Override
  | Ast_411.Asttypes.Fresh -> Ast_410.Asttypes.Fresh
and copy_module_type_declaration :
  Ast_411.Parsetree.module_type_declaration ->
    Ast_410.Parsetree.module_type_declaration
  =
  fun
    { Ast_411.Parsetree.pmtd_name = pmtd_name;
      Ast_411.Parsetree.pmtd_type = pmtd_type;
      Ast_411.Parsetree.pmtd_attributes = pmtd_attributes;
      Ast_411.Parsetree.pmtd_loc = pmtd_loc }
    ->
    {
      Ast_410.Parsetree.pmtd_name = (copy_loc (fun x -> x) pmtd_name);
      Ast_410.Parsetree.pmtd_type = (Option.map copy_module_type pmtd_type);
      Ast_410.Parsetree.pmtd_attributes = (copy_attributes pmtd_attributes);
      Ast_410.Parsetree.pmtd_loc = (copy_location pmtd_loc)
    }
and copy_module_substitution :
  Ast_411.Parsetree.module_substitution ->
    Ast_410.Parsetree.module_substitution
  =
  fun
    { Ast_411.Parsetree.pms_name = pms_name;
      Ast_411.Parsetree.pms_manifest = pms_manifest;
      Ast_411.Parsetree.pms_attributes = pms_attributes;
      Ast_411.Parsetree.pms_loc = pms_loc }
    ->
    {
      Ast_410.Parsetree.pms_name = (copy_loc (fun x -> x) pms_name);
      Ast_410.Parsetree.pms_manifest =
        (copy_loc copy_Longident_t pms_manifest);
      Ast_410.Parsetree.pms_attributes = (copy_attributes pms_attributes);
      Ast_410.Parsetree.pms_loc = (copy_location pms_loc)
    }
and copy_module_declaration :
  Ast_411.Parsetree.module_declaration ->
    Ast_410.Parsetree.module_declaration
  =
  fun
    { Ast_411.Parsetree.pmd_name = pmd_name;
      Ast_411.Parsetree.pmd_type = pmd_type;
      Ast_411.Parsetree.pmd_attributes = pmd_attributes;
      Ast_411.Parsetree.pmd_loc = pmd_loc }
    ->
    {
      Ast_410.Parsetree.pmd_name =
        (copy_loc (fun x -> Option.map (fun x -> x) x) pmd_name);
      Ast_410.Parsetree.pmd_type = (copy_module_type pmd_type);
      Ast_410.Parsetree.pmd_attributes = (copy_attributes pmd_attributes);
      Ast_410.Parsetree.pmd_loc = (copy_location pmd_loc)
    }
and copy_type_exception :
  Ast_411.Parsetree.type_exception -> Ast_410.Parsetree.type_exception =
  fun
    { Ast_411.Parsetree.ptyexn_constructor = ptyexn_constructor;
      Ast_411.Parsetree.ptyexn_loc = ptyexn_loc;
      Ast_411.Parsetree.ptyexn_attributes = ptyexn_attributes }
    ->
    {
      Ast_410.Parsetree.ptyexn_constructor =
        (copy_extension_constructor ptyexn_constructor);
      Ast_410.Parsetree.ptyexn_loc = (copy_location ptyexn_loc);
      Ast_410.Parsetree.ptyexn_attributes =
        (copy_attributes ptyexn_attributes)
    }
and copy_type_extension :
  Ast_411.Parsetree.type_extension -> Ast_410.Parsetree.type_extension =
  fun
    { Ast_411.Parsetree.ptyext_path = ptyext_path;
      Ast_411.Parsetree.ptyext_params = ptyext_params;
      Ast_411.Parsetree.ptyext_constructors = ptyext_constructors;
      Ast_411.Parsetree.ptyext_private = ptyext_private;
      Ast_411.Parsetree.ptyext_loc = ptyext_loc;
      Ast_411.Parsetree.ptyext_attributes = ptyext_attributes }
    ->
    {
      Ast_410.Parsetree.ptyext_path = (copy_loc copy_Longident_t ptyext_path);
      Ast_410.Parsetree.ptyext_params =
        (List.map
           (fun x ->
              let (x0, x1) = x in ((copy_core_type x0), (copy_variance x1)))
           ptyext_params);
      Ast_410.Parsetree.ptyext_constructors =
        (List.map copy_extension_constructor ptyext_constructors);
      Ast_410.Parsetree.ptyext_private = (copy_private_flag ptyext_private);
      Ast_410.Parsetree.ptyext_loc = (copy_location ptyext_loc);
      Ast_410.Parsetree.ptyext_attributes =
        (copy_attributes ptyext_attributes)
    }
and copy_extension_constructor :
  Ast_411.Parsetree.extension_constructor ->
    Ast_410.Parsetree.extension_constructor
  =
  fun
    { Ast_411.Parsetree.pext_name = pext_name;
      Ast_411.Parsetree.pext_kind = pext_kind;
      Ast_411.Parsetree.pext_loc = pext_loc;
      Ast_411.Parsetree.pext_attributes = pext_attributes }
    ->
    {
      Ast_410.Parsetree.pext_name = (copy_loc (fun x -> x) pext_name);
      Ast_410.Parsetree.pext_kind =
        (copy_extension_constructor_kind pext_kind);
      Ast_410.Parsetree.pext_loc = (copy_location pext_loc);
      Ast_410.Parsetree.pext_attributes = (copy_attributes pext_attributes)
    }
and copy_extension_constructor_kind :
  Ast_411.Parsetree.extension_constructor_kind ->
    Ast_410.Parsetree.extension_constructor_kind
  =
  function
  | Ast_411.Parsetree.Pext_decl (x0, x1) ->
      Ast_410.Parsetree.Pext_decl
        ((copy_constructor_arguments x0), (Option.map copy_core_type x1))
  | Ast_411.Parsetree.Pext_rebind x0 ->
      Ast_410.Parsetree.Pext_rebind (copy_loc copy_Longident_t x0)
and copy_type_declaration :
  Ast_411.Parsetree.type_declaration -> Ast_410.Parsetree.type_declaration =
  fun
    { Ast_411.Parsetree.ptype_name = ptype_name;
      Ast_411.Parsetree.ptype_params = ptype_params;
      Ast_411.Parsetree.ptype_cstrs = ptype_cstrs;
      Ast_411.Parsetree.ptype_kind = ptype_kind;
      Ast_411.Parsetree.ptype_private = ptype_private;
      Ast_411.Parsetree.ptype_manifest = ptype_manifest;
      Ast_411.Parsetree.ptype_attributes = ptype_attributes;
      Ast_411.Parsetree.ptype_loc = ptype_loc }
    ->
    {
      Ast_410.Parsetree.ptype_name = (copy_loc (fun x -> x) ptype_name);
      Ast_410.Parsetree.ptype_params =
        (List.map
           (fun x ->
              let (x0, x1) = x in ((copy_core_type x0), (copy_variance x1)))
           ptype_params);
      Ast_410.Parsetree.ptype_cstrs =
        (List.map
           (fun x ->
              let (x0, x1, x2) = x in
              ((copy_core_type x0), (copy_core_type x1), (copy_location x2)))
           ptype_cstrs);
      Ast_410.Parsetree.ptype_kind = (copy_type_kind ptype_kind);
      Ast_410.Parsetree.ptype_private = (copy_private_flag ptype_private);
      Ast_410.Parsetree.ptype_manifest =
        (Option.map copy_core_type ptype_manifest);
      Ast_410.Parsetree.ptype_attributes = (copy_attributes ptype_attributes);
      Ast_410.Parsetree.ptype_loc = (copy_location ptype_loc)
    }
and copy_private_flag :
  Ast_411.Asttypes.private_flag -> Ast_410.Asttypes.private_flag =
  function
  | Ast_411.Asttypes.Private -> Ast_410.Asttypes.Private
  | Ast_411.Asttypes.Public -> Ast_410.Asttypes.Public
and copy_type_kind :
  Ast_411.Parsetree.type_kind -> Ast_410.Parsetree.type_kind =
  function
  | Ast_411.Parsetree.Ptype_abstract -> Ast_410.Parsetree.Ptype_abstract
  | Ast_411.Parsetree.Ptype_variant x0 ->
      Ast_410.Parsetree.Ptype_variant
        (List.map copy_constructor_declaration x0)
  | Ast_411.Parsetree.Ptype_record x0 ->
      Ast_410.Parsetree.Ptype_record (List.map copy_label_declaration x0)
  | Ast_411.Parsetree.Ptype_open -> Ast_410.Parsetree.Ptype_open
and copy_constructor_declaration :
  Ast_411.Parsetree.constructor_declaration ->
    Ast_410.Parsetree.constructor_declaration
  =
  fun
    { Ast_411.Parsetree.pcd_name = pcd_name;
      Ast_411.Parsetree.pcd_args = pcd_args;
      Ast_411.Parsetree.pcd_res = pcd_res;
      Ast_411.Parsetree.pcd_loc = pcd_loc;
      Ast_411.Parsetree.pcd_attributes = pcd_attributes }
    ->
    {
      Ast_410.Parsetree.pcd_name = (copy_loc (fun x -> x) pcd_name);
      Ast_410.Parsetree.pcd_args = (copy_constructor_arguments pcd_args);
      Ast_410.Parsetree.pcd_res = (Option.map copy_core_type pcd_res);
      Ast_410.Parsetree.pcd_loc = (copy_location pcd_loc);
      Ast_410.Parsetree.pcd_attributes = (copy_attributes pcd_attributes)
    }
and copy_constructor_arguments :
  Ast_411.Parsetree.constructor_arguments ->
    Ast_410.Parsetree.constructor_arguments
  =
  function
  | Ast_411.Parsetree.Pcstr_tuple x0 ->
      Ast_410.Parsetree.Pcstr_tuple (List.map copy_core_type x0)
  | Ast_411.Parsetree.Pcstr_record x0 ->
      Ast_410.Parsetree.Pcstr_record (List.map copy_label_declaration x0)
and copy_label_declaration :
  Ast_411.Parsetree.label_declaration -> Ast_410.Parsetree.label_declaration
  =
  fun
    { Ast_411.Parsetree.pld_name = pld_name;
      Ast_411.Parsetree.pld_mutable = pld_mutable;
      Ast_411.Parsetree.pld_type = pld_type;
      Ast_411.Parsetree.pld_loc = pld_loc;
      Ast_411.Parsetree.pld_attributes = pld_attributes }
    ->
    {
      Ast_410.Parsetree.pld_name = (copy_loc (fun x -> x) pld_name);
      Ast_410.Parsetree.pld_mutable = (copy_mutable_flag pld_mutable);
      Ast_410.Parsetree.pld_type = (copy_core_type pld_type);
      Ast_410.Parsetree.pld_loc = (copy_location pld_loc);
      Ast_410.Parsetree.pld_attributes = (copy_attributes pld_attributes)
    }
and copy_mutable_flag :
  Ast_411.Asttypes.mutable_flag -> Ast_410.Asttypes.mutable_flag =
  function
  | Ast_411.Asttypes.Immutable -> Ast_410.Asttypes.Immutable
  | Ast_411.Asttypes.Mutable -> Ast_410.Asttypes.Mutable
and copy_variance : Ast_411.Asttypes.variance -> Ast_410.Asttypes.variance =
  function
  | Ast_411.Asttypes.Covariant -> Ast_410.Asttypes.Covariant
  | Ast_411.Asttypes.Contravariant -> Ast_410.Asttypes.Contravariant
  | Ast_411.Asttypes.Invariant -> Ast_410.Asttypes.Invariant
and copy_value_description :
  Ast_411.Parsetree.value_description -> Ast_410.Parsetree.value_description
  =
  fun
    { Ast_411.Parsetree.pval_name = pval_name;
      Ast_411.Parsetree.pval_type = pval_type;
      Ast_411.Parsetree.pval_prim = pval_prim;
      Ast_411.Parsetree.pval_attributes = pval_attributes;
      Ast_411.Parsetree.pval_loc = pval_loc }
    ->
    {
      Ast_410.Parsetree.pval_name = (copy_loc (fun x -> x) pval_name);
      Ast_410.Parsetree.pval_type = (copy_core_type pval_type);
      Ast_410.Parsetree.pval_prim = (List.map (fun x -> x) pval_prim);
      Ast_410.Parsetree.pval_attributes = (copy_attributes pval_attributes);
      Ast_410.Parsetree.pval_loc = (copy_location pval_loc)
    }
and copy_object_field_desc :
  Ast_411.Parsetree.object_field_desc -> Ast_410.Parsetree.object_field_desc
  =
  function
  | Ast_411.Parsetree.Otag (x0, x1) ->
      Ast_410.Parsetree.Otag ((copy_loc copy_label x0), (copy_core_type x1))
  | Ast_411.Parsetree.Oinherit x0 ->
      Ast_410.Parsetree.Oinherit (copy_core_type x0)
and copy_arg_label : Ast_411.Asttypes.arg_label -> Ast_410.Asttypes.arg_label
  =
  function
  | Ast_411.Asttypes.Nolabel -> Ast_410.Asttypes.Nolabel
  | Ast_411.Asttypes.Labelled x0 -> Ast_410.Asttypes.Labelled x0
  | Ast_411.Asttypes.Optional x0 -> Ast_410.Asttypes.Optional x0
and copy_closed_flag :
  Ast_411.Asttypes.closed_flag -> Ast_410.Asttypes.closed_flag =
  function
  | Ast_411.Asttypes.Closed -> Ast_410.Asttypes.Closed
  | Ast_411.Asttypes.Open -> Ast_410.Asttypes.Open
and copy_label : Ast_411.Asttypes.label -> Ast_410.Asttypes.label =
  fun x -> x
and copy_rec_flag : Ast_411.Asttypes.rec_flag -> Ast_410.Asttypes.rec_flag =
  function
  | Ast_411.Asttypes.Nonrecursive -> Ast_410.Asttypes.Nonrecursive
  | Ast_411.Asttypes.Recursive -> Ast_410.Asttypes.Recursive
and copy_constant : Ast_411.Parsetree.constant -> Ast_410.Parsetree.constant
  =
  function
  | Ast_411.Parsetree.Pconst_integer (x0, x1) ->
      Ast_410.Parsetree.Pconst_integer (x0, (Option.map (fun x -> x) x1))
  | Ast_411.Parsetree.Pconst_char x0 -> Ast_410.Parsetree.Pconst_char x0
  | Ast_411.Parsetree.Pconst_string (x0, _, x2) ->
      Ast_410.Parsetree.Pconst_string
        (x0, (Option.map (fun x -> x) x2))
  | Ast_411.Parsetree.Pconst_float (x0, x1) ->
      Ast_410.Parsetree.Pconst_float (x0, (Option.map (fun x -> x) x1))
and copy_Longident_t : Ast_411.Longident.t -> Ast_410.Longident.t =
  function
  | Ast_411.Longident.Lident x0 -> Ast_410.Longident.Lident x0
  | Ast_411.Longident.Ldot (x0, x1) ->
      Ast_410.Longident.Ldot ((copy_Longident_t x0), x1)
  | Ast_411.Longident.Lapply (x0, x1) ->
      Ast_410.Longident.Lapply ((copy_Longident_t x0), (copy_Longident_t x1))
and copy_loc :
  'f0 'g0 .
    ('f0 -> 'g0) -> 'f0 Ast_411.Asttypes.loc -> 'g0 Ast_410.Asttypes.loc
  =
  fun f0 ->
    fun { Ast_411.Asttypes.txt = txt; Ast_411.Asttypes.loc = loc } ->
      {
        Ast_410.Asttypes.txt = (f0 txt);
        Ast_410.Asttypes.loc = (copy_location loc)
      }
and copy_location : Ast_411.Location.t -> Ast_410.Location.t =
  fun
    { Ast_411.Location.loc_start = loc_start;
      Ast_411.Location.loc_end = loc_end;
      Ast_411.Location.loc_ghost = loc_ghost }
    ->
    {
      Ast_410.Location.loc_start = (copy_position loc_start);
      Ast_410.Location.loc_end = (copy_position loc_end);
      Ast_410.Location.loc_ghost = loc_ghost
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
