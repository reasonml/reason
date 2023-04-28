open Stdlib0
module From = Ast_414
module To = Ast_500
let rec copy_out_type_extension :
  Ast_414.Outcometree.out_type_extension ->
    Ast_500.Outcometree.out_type_extension
  =
  fun
    { Ast_414.Outcometree.otyext_name = otyext_name;
      Ast_414.Outcometree.otyext_params = otyext_params;
      Ast_414.Outcometree.otyext_constructors = otyext_constructors;
      Ast_414.Outcometree.otyext_private = otyext_private }
    ->
    {
      Ast_500.Outcometree.otyext_name = otyext_name;
      Ast_500.Outcometree.otyext_params =
        (List.map (fun x -> x) otyext_params);
      Ast_500.Outcometree.otyext_constructors =
        (List.map copy_out_constructor otyext_constructors);
      Ast_500.Outcometree.otyext_private = (copy_private_flag otyext_private)
    }
and copy_out_phrase :
  Ast_414.Outcometree.out_phrase -> Ast_500.Outcometree.out_phrase =
  function
  | Ast_414.Outcometree.Ophr_eval (x0, x1) ->
      Ast_500.Outcometree.Ophr_eval ((copy_out_value x0), (copy_out_type x1))
  | Ast_414.Outcometree.Ophr_signature x0 ->
      Ast_500.Outcometree.Ophr_signature
        (List.map
           (fun x ->
              let (x0, x1) = x in
              ((copy_out_sig_item x0), (Option.map copy_out_value x1))) x0)
  | Ast_414.Outcometree.Ophr_exception x0 ->
      Ast_500.Outcometree.Ophr_exception
        (let (x0, x1) = x0 in (x0, (copy_out_value x1)))
and copy_out_sig_item :
  Ast_414.Outcometree.out_sig_item -> Ast_500.Outcometree.out_sig_item =
  function
  | Ast_414.Outcometree.Osig_class (x0, x1, x2, x3, x4) ->
      Ast_500.Outcometree.Osig_class
        (x0, x1, (List.map copy_out_type_param x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | Ast_414.Outcometree.Osig_class_type (x0, x1, x2, x3, x4) ->
      Ast_500.Outcometree.Osig_class_type
        (x0, x1, (List.map copy_out_type_param x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | Ast_414.Outcometree.Osig_typext (x0, x1) ->
      Ast_500.Outcometree.Osig_typext
        ((copy_out_extension_constructor x0), (copy_out_ext_status x1))
  | Ast_414.Outcometree.Osig_modtype (x0, x1) ->
      Ast_500.Outcometree.Osig_modtype (x0, (copy_out_module_type x1))
  | Ast_414.Outcometree.Osig_module (x0, x1, x2) ->
      Ast_500.Outcometree.Osig_module
        (x0, (copy_out_module_type x1), (copy_out_rec_status x2))
  | Ast_414.Outcometree.Osig_type (x0, x1) ->
      Ast_500.Outcometree.Osig_type
        ((copy_out_type_decl x0), (copy_out_rec_status x1))
  | Ast_414.Outcometree.Osig_value x0 ->
      Ast_500.Outcometree.Osig_value (copy_out_val_decl x0)
  | Ast_414.Outcometree.Osig_ellipsis -> Ast_500.Outcometree.Osig_ellipsis
and copy_out_val_decl :
  Ast_414.Outcometree.out_val_decl -> Ast_500.Outcometree.out_val_decl =
  fun
    { Ast_414.Outcometree.oval_name = oval_name;
      Ast_414.Outcometree.oval_type = oval_type;
      Ast_414.Outcometree.oval_prims = oval_prims;
      Ast_414.Outcometree.oval_attributes = oval_attributes }
    ->
    {
      Ast_500.Outcometree.oval_name = oval_name;
      Ast_500.Outcometree.oval_type = (copy_out_type oval_type);
      Ast_500.Outcometree.oval_prims = (List.map (fun x -> x) oval_prims);
      Ast_500.Outcometree.oval_attributes =
        (List.map copy_out_attribute oval_attributes)
    }
and copy_out_type_decl :
  Ast_414.Outcometree.out_type_decl -> Ast_500.Outcometree.out_type_decl =
  fun
    { Ast_414.Outcometree.otype_name = otype_name;
      Ast_414.Outcometree.otype_params = otype_params;
      Ast_414.Outcometree.otype_type = otype_type;
      Ast_414.Outcometree.otype_private = otype_private;
      Ast_414.Outcometree.otype_immediate = otype_immediate;
      Ast_414.Outcometree.otype_unboxed = otype_unboxed;
      Ast_414.Outcometree.otype_cstrs = otype_cstrs }
    ->
    {
      Ast_500.Outcometree.otype_name = otype_name;
      Ast_500.Outcometree.otype_params =
        (List.map copy_out_type_param otype_params);
      Ast_500.Outcometree.otype_type = (copy_out_type otype_type);
      Ast_500.Outcometree.otype_private = (copy_private_flag otype_private);
      Ast_500.Outcometree.otype_immediate =
        (copy_Type_immediacy_t otype_immediate);
      Ast_500.Outcometree.otype_unboxed = otype_unboxed;
      Ast_500.Outcometree.otype_cstrs =
        (List.map
           (fun x ->
              let (x0, x1) = x in ((copy_out_type x0), (copy_out_type x1)))
           otype_cstrs)
    }
and copy_Type_immediacy_t :
  Ast_414.Type_immediacy.t -> Ast_500.Type_immediacy.t =
  function
  | Ast_414.Type_immediacy.Unknown -> Ast_500.Type_immediacy.Unknown
  | Ast_414.Type_immediacy.Always -> Ast_500.Type_immediacy.Always
  | Ast_414.Type_immediacy.Always_on_64bits ->
      Ast_500.Type_immediacy.Always_on_64bits
and copy_out_module_type :
  Ast_414.Outcometree.out_module_type -> Ast_500.Outcometree.out_module_type
  =
  function
  | Ast_414.Outcometree.Omty_abstract -> Ast_500.Outcometree.Omty_abstract
  | Ast_414.Outcometree.Omty_functor (x0, x1) ->
      Ast_500.Outcometree.Omty_functor
        ((Option.map
            (fun x ->
               let (x0, x1) = x in
               ((Option.map (fun x -> x) x0), (copy_out_module_type x1))) x0),
          (copy_out_module_type x1))
  | Ast_414.Outcometree.Omty_ident x0 ->
      Ast_500.Outcometree.Omty_ident (copy_out_ident x0)
  | Ast_414.Outcometree.Omty_signature x0 ->
      Ast_500.Outcometree.Omty_signature (List.map copy_out_sig_item x0)
  | Ast_414.Outcometree.Omty_alias x0 ->
      Ast_500.Outcometree.Omty_alias (copy_out_ident x0)
and copy_out_ext_status :
  Ast_414.Outcometree.out_ext_status -> Ast_500.Outcometree.out_ext_status =
  function
  | Ast_414.Outcometree.Oext_first -> Ast_500.Outcometree.Oext_first
  | Ast_414.Outcometree.Oext_next -> Ast_500.Outcometree.Oext_next
  | Ast_414.Outcometree.Oext_exception -> Ast_500.Outcometree.Oext_exception
and copy_out_extension_constructor :
  Ast_414.Outcometree.out_extension_constructor ->
    Ast_500.Outcometree.out_extension_constructor
  =
  fun
    { Ast_414.Outcometree.oext_name = oext_name;
      Ast_414.Outcometree.oext_type_name = oext_type_name;
      Ast_414.Outcometree.oext_type_params = oext_type_params;
      Ast_414.Outcometree.oext_args = oext_args;
      Ast_414.Outcometree.oext_ret_type = oext_ret_type;
      Ast_414.Outcometree.oext_private = oext_private }
    ->
    {
      Ast_500.Outcometree.oext_name = oext_name;
      Ast_500.Outcometree.oext_type_name = oext_type_name;
      Ast_500.Outcometree.oext_type_params =
        (List.map (fun x -> x) oext_type_params);
      Ast_500.Outcometree.oext_args = (List.map copy_out_type oext_args);
      Ast_500.Outcometree.oext_ret_type =
        (Option.map copy_out_type oext_ret_type);
      Ast_500.Outcometree.oext_private = (copy_private_flag oext_private)
    }
and copy_private_flag :
  Ast_414.Asttypes.private_flag -> Ast_500.Asttypes.private_flag =
  function
  | Ast_414.Asttypes.Private -> Ast_500.Asttypes.Private
  | Ast_414.Asttypes.Public -> Ast_500.Asttypes.Public
and copy_out_rec_status :
  Ast_414.Outcometree.out_rec_status -> Ast_500.Outcometree.out_rec_status =
  function
  | Ast_414.Outcometree.Orec_not -> Ast_500.Outcometree.Orec_not
  | Ast_414.Outcometree.Orec_first -> Ast_500.Outcometree.Orec_first
  | Ast_414.Outcometree.Orec_next -> Ast_500.Outcometree.Orec_next
and copy_out_class_type :
  Ast_414.Outcometree.out_class_type -> Ast_500.Outcometree.out_class_type =
  function
  | Ast_414.Outcometree.Octy_constr (x0, x1) ->
      Ast_500.Outcometree.Octy_constr
        ((copy_out_ident x0), (List.map copy_out_type x1))
  | Ast_414.Outcometree.Octy_arrow (x0, x1, x2) ->
      Ast_500.Outcometree.Octy_arrow
        (x0, (copy_out_type x1), (copy_out_class_type x2))
  | Ast_414.Outcometree.Octy_signature (x0, x1) ->
      Ast_500.Outcometree.Octy_signature
        ((Option.map copy_out_type x0),
          (List.map copy_out_class_sig_item x1))
and copy_out_class_sig_item :
  Ast_414.Outcometree.out_class_sig_item ->
    Ast_500.Outcometree.out_class_sig_item
  =
  function
  | Ast_414.Outcometree.Ocsg_constraint (x0, x1) ->
      Ast_500.Outcometree.Ocsg_constraint
        ((copy_out_type x0), (copy_out_type x1))
  | Ast_414.Outcometree.Ocsg_method (x0, x1, x2, x3) ->
      Ast_500.Outcometree.Ocsg_method (x0, x1, x2, (copy_out_type x3))
  | Ast_414.Outcometree.Ocsg_value (x0, x1, x2, x3) ->
      Ast_500.Outcometree.Ocsg_value (x0, x1, x2, (copy_out_type x3))
and copy_out_type_param :
  Ast_414.Outcometree.out_type_param -> Ast_500.Outcometree.out_type_param =
  fun x ->
    let (x0, x1) = x in
    (x0, (let (x0, x1) = x1 in ((copy_variance x0), (copy_injectivity x1))))
and copy_injectivity :
  Ast_414.Asttypes.injectivity -> Ast_500.Asttypes.injectivity =
  function
  | Ast_414.Asttypes.Injective -> Ast_500.Asttypes.Injective
  | Ast_414.Asttypes.NoInjectivity -> Ast_500.Asttypes.NoInjectivity
and copy_variance : Ast_414.Asttypes.variance -> Ast_500.Asttypes.variance =
  function
  | Ast_414.Asttypes.Covariant -> Ast_500.Asttypes.Covariant
  | Ast_414.Asttypes.Contravariant -> Ast_500.Asttypes.Contravariant
  | Ast_414.Asttypes.NoVariance -> Ast_500.Asttypes.NoVariance
and copy_out_type :
  Ast_414.Outcometree.out_type -> Ast_500.Outcometree.out_type =
  function
  | Ast_414.Outcometree.Otyp_abstract -> Ast_500.Outcometree.Otyp_abstract
  | Ast_414.Outcometree.Otyp_open -> Ast_500.Outcometree.Otyp_open
  | Ast_414.Outcometree.Otyp_alias (x0, x1) ->
      Ast_500.Outcometree.Otyp_alias ((copy_out_type x0), x1)
  | Ast_414.Outcometree.Otyp_arrow (x0, x1, x2) ->
      Ast_500.Outcometree.Otyp_arrow
        (x0, (copy_out_type x1), (copy_out_type x2))
  | Ast_414.Outcometree.Otyp_class (x0, x1, x2) ->
      Ast_500.Outcometree.Otyp_class
        (x0, (copy_out_ident x1), (List.map copy_out_type x2))
  | Ast_414.Outcometree.Otyp_constr (x0, x1) ->
      Ast_500.Outcometree.Otyp_constr
        ((copy_out_ident x0), (List.map copy_out_type x1))
  | Ast_414.Outcometree.Otyp_manifest (x0, x1) ->
      Ast_500.Outcometree.Otyp_manifest
        ((copy_out_type x0), (copy_out_type x1))
  | Ast_414.Outcometree.Otyp_object (x0, x1) ->
      Ast_500.Outcometree.Otyp_object
        ((List.map (fun x -> let (x0, x1) = x in (x0, (copy_out_type x1))) x0),
          (Option.map (fun x -> x) x1))
  | Ast_414.Outcometree.Otyp_record x0 ->
      Ast_500.Outcometree.Otyp_record
        (List.map
           (fun x -> let (x0, x1, x2) = x in (x0, x1, (copy_out_type x2))) x0)
  | Ast_414.Outcometree.Otyp_stuff x0 -> Ast_500.Outcometree.Otyp_stuff x0
  | Ast_414.Outcometree.Otyp_sum x0 ->
      Ast_500.Outcometree.Otyp_sum (List.map copy_out_constructor x0)
  | Ast_414.Outcometree.Otyp_tuple x0 ->
      Ast_500.Outcometree.Otyp_tuple (List.map copy_out_type x0)
  | Ast_414.Outcometree.Otyp_var (x0, x1) ->
      Ast_500.Outcometree.Otyp_var (x0, x1)
  | Ast_414.Outcometree.Otyp_variant (x0, x1, x2, x3) ->
      Ast_500.Outcometree.Otyp_variant
        (x0, (copy_out_variant x1), x2,
          (Option.map (fun x -> List.map (fun x -> x) x) x3))
  | Ast_414.Outcometree.Otyp_poly (x0, x1) ->
      Ast_500.Outcometree.Otyp_poly
        ((List.map (fun x -> x) x0), (copy_out_type x1))
  | Ast_414.Outcometree.Otyp_module (x0, x1) ->
      Ast_500.Outcometree.Otyp_module
        ((copy_out_ident x0),
          (List.map (fun x -> let (x0, x1) = x in (x0, (copy_out_type x1)))
             x1))
  | Ast_414.Outcometree.Otyp_attribute (x0, x1) ->
      Ast_500.Outcometree.Otyp_attribute
        ((copy_out_type x0), (copy_out_attribute x1))
and copy_out_attribute :
  Ast_414.Outcometree.out_attribute -> Ast_500.Outcometree.out_attribute =
  fun { Ast_414.Outcometree.oattr_name = oattr_name } ->
    { Ast_500.Outcometree.oattr_name = oattr_name }
and copy_out_variant :
  Ast_414.Outcometree.out_variant -> Ast_500.Outcometree.out_variant =
  function
  | Ast_414.Outcometree.Ovar_fields x0 ->
      Ast_500.Outcometree.Ovar_fields
        (List.map
           (fun x ->
              let (x0, x1, x2) = x in (x0, x1, (List.map copy_out_type x2)))
           x0)
  | Ast_414.Outcometree.Ovar_typ x0 ->
      Ast_500.Outcometree.Ovar_typ (copy_out_type x0)
and copy_out_constructor :
  Ast_414.Outcometree.out_constructor -> Ast_500.Outcometree.out_constructor
  =
  fun
    { Ast_414.Outcometree.ocstr_name = ocstr_name;
      Ast_414.Outcometree.ocstr_args = ocstr_args;
      Ast_414.Outcometree.ocstr_return_type = ocstr_return_type }
    ->
    {
      Ast_500.Outcometree.ocstr_name = ocstr_name;
      Ast_500.Outcometree.ocstr_args = (List.map copy_out_type ocstr_args);
      Ast_500.Outcometree.ocstr_return_type =
        (Option.map copy_out_type ocstr_return_type)
    }
and copy_out_value :
  Ast_414.Outcometree.out_value -> Ast_500.Outcometree.out_value =
  function
  | Ast_414.Outcometree.Oval_array x0 ->
      Ast_500.Outcometree.Oval_array (List.map copy_out_value x0)
  | Ast_414.Outcometree.Oval_char x0 -> Ast_500.Outcometree.Oval_char x0
  | Ast_414.Outcometree.Oval_constr (x0, x1) ->
      Ast_500.Outcometree.Oval_constr
        ((copy_out_ident x0), (List.map copy_out_value x1))
  | Ast_414.Outcometree.Oval_ellipsis -> Ast_500.Outcometree.Oval_ellipsis
  | Ast_414.Outcometree.Oval_float x0 -> Ast_500.Outcometree.Oval_float x0
  | Ast_414.Outcometree.Oval_int x0 -> Ast_500.Outcometree.Oval_int x0
  | Ast_414.Outcometree.Oval_int32 x0 -> Ast_500.Outcometree.Oval_int32 x0
  | Ast_414.Outcometree.Oval_int64 x0 -> Ast_500.Outcometree.Oval_int64 x0
  | Ast_414.Outcometree.Oval_nativeint x0 ->
      Ast_500.Outcometree.Oval_nativeint x0
  | Ast_414.Outcometree.Oval_list x0 ->
      Ast_500.Outcometree.Oval_list (List.map copy_out_value x0)
  | Ast_414.Outcometree.Oval_printer x0 ->
      Ast_500.Outcometree.Oval_printer x0
  | Ast_414.Outcometree.Oval_record x0 ->
      Ast_500.Outcometree.Oval_record
        (List.map
           (fun x ->
              let (x0, x1) = x in ((copy_out_ident x0), (copy_out_value x1)))
           x0)
  | Ast_414.Outcometree.Oval_string (x0, x1, x2) ->
      Ast_500.Outcometree.Oval_string (x0, x1, (copy_out_string x2))
  | Ast_414.Outcometree.Oval_stuff x0 -> Ast_500.Outcometree.Oval_stuff x0
  | Ast_414.Outcometree.Oval_tuple x0 ->
      Ast_500.Outcometree.Oval_tuple (List.map copy_out_value x0)
  | Ast_414.Outcometree.Oval_variant (x0, x1) ->
      Ast_500.Outcometree.Oval_variant (x0, (Option.map copy_out_value x1))
and copy_out_string :
  Ast_414.Outcometree.out_string -> Ast_500.Outcometree.out_string =
  function
  | Ast_414.Outcometree.Ostr_string -> Ast_500.Outcometree.Ostr_string
  | Ast_414.Outcometree.Ostr_bytes -> Ast_500.Outcometree.Ostr_bytes
and copy_out_ident :
  Ast_414.Outcometree.out_ident -> Ast_500.Outcometree.out_ident =
  function
  | Ast_414.Outcometree.Oide_apply (x0, x1) ->
      Ast_500.Outcometree.Oide_apply
        ((copy_out_ident x0), (copy_out_ident x1))
  | Ast_414.Outcometree.Oide_dot (x0, x1) ->
      Ast_500.Outcometree.Oide_dot ((copy_out_ident x0), x1)
  | Ast_414.Outcometree.Oide_ident x0 ->
      Ast_500.Outcometree.Oide_ident (copy_out_name x0)
and copy_out_name :
  Ast_414.Outcometree.out_name -> Ast_500.Outcometree.out_name =
  fun { Ast_414.Outcometree.printed_name = printed_name } ->
    { Ast_500.Outcometree.printed_name = printed_name }
