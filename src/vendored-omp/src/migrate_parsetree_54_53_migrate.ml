module From = Ast_54
module To = Ast_53
let rec (copy_out_type_extension :
          Ast_54.Outcometree.out_type_extension ->
            Ast_53.Outcometree.out_type_extension)
  =
  fun
    { Ast_54.Outcometree.otyext_name = otyext_name;
      Ast_54.Outcometree.otyext_params = otyext_params;
      Ast_54.Outcometree.otyext_constructors = otyext_constructors;
      Ast_54.Outcometree.otyext_private = otyext_private }
    ->
    {
      Ast_53.Outcometree.otyext_name = otyext_name;
      Ast_53.Outcometree.otyext_params =
        (List.map (fun x -> x) otyext_params);
      Ast_53.Outcometree.otyext_constructors =
        (List.map (fun x -> copy_out_constructor x) otyext_constructors);
      Ast_53.Outcometree.otyext_private = (copy_private_flag otyext_private)
    }
and (copy_out_phrase :
      Ast_54.Outcometree.out_phrase -> Ast_53.Outcometree.out_phrase)
  =
  function
  | Ast_54.Outcometree.Ophr_eval (x0, x1) ->
      Ast_53.Outcometree.Ophr_eval ((copy_out_value x0), (copy_out_type x1))
  | Ast_54.Outcometree.Ophr_signature x0 ->
      Ast_53.Outcometree.Ophr_signature
        (List.map
           (fun x ->
              let (x0, x1) = x in
              ((copy_out_sig_item x0),
                (Option.map (fun x -> copy_out_value x) x1))) x0)
  | Ast_54.Outcometree.Ophr_exception x0 ->
      Ast_53.Outcometree.Ophr_exception
        (let (x0, x1) = x0 in (x0, (copy_out_value x1)))
and (copy_out_sig_item :
      Ast_54.Outcometree.out_sig_item -> Ast_53.Outcometree.out_sig_item)
  =
  function
  | Ast_54.Outcometree.Osig_class (x0, x1, x2, x3, x4) ->
      Ast_53.Outcometree.Osig_class
        (x0, x1, (List.map (fun x -> copy_out_type_param x) x2),
          (copy_out_class_type x3), (copy_out_rec_status x4))
  | Ast_54.Outcometree.Osig_class_type (x0, x1, x2, x3, x4) ->
      Ast_53.Outcometree.Osig_class_type
        (x0, x1, (List.map (fun x -> copy_out_type_param x) x2),
          (copy_out_class_type x3), (copy_out_rec_status x4))
  | Ast_54.Outcometree.Osig_typext (x0, x1) ->
      Ast_53.Outcometree.Osig_typext
        ((copy_out_extension_constructor x0), (copy_out_ext_status x1))
  | Ast_54.Outcometree.Osig_modtype (x0, x1) ->
      Ast_53.Outcometree.Osig_modtype (x0, (copy_out_module_type x1))
  | Ast_54.Outcometree.Osig_module (x0, x1, x2) ->
      Ast_53.Outcometree.Osig_module
        (x0, (copy_out_module_type x1), (copy_out_rec_status x2))
  | Ast_54.Outcometree.Osig_type (x0, x1) ->
      Ast_53.Outcometree.Osig_type
        ((copy_out_type_decl x0), (copy_out_rec_status x1))
  | Ast_54.Outcometree.Osig_value x0 ->
      Ast_53.Outcometree.Osig_value (copy_out_val_decl x0)
  | Ast_54.Outcometree.Osig_ellipsis -> Ast_53.Outcometree.Osig_ellipsis
and (copy_out_val_decl :
      Ast_54.Outcometree.out_val_decl -> Ast_53.Outcometree.out_val_decl)
  =
  fun
    { Ast_54.Outcometree.oval_name = oval_name;
      Ast_54.Outcometree.oval_type = oval_type;
      Ast_54.Outcometree.oval_prims = oval_prims;
      Ast_54.Outcometree.oval_attributes = oval_attributes }
    ->
    {
      Ast_53.Outcometree.oval_name = oval_name;
      Ast_53.Outcometree.oval_type = (copy_out_type oval_type);
      Ast_53.Outcometree.oval_prims = (List.map (fun x -> x) oval_prims);
      Ast_53.Outcometree.oval_attributes =
        (List.map (fun x -> copy_out_attribute x) oval_attributes)
    }
and (copy_out_type_decl :
      Ast_54.Outcometree.out_type_decl -> Ast_53.Outcometree.out_type_decl)
  =
  fun
    { Ast_54.Outcometree.otype_name = otype_name;
      Ast_54.Outcometree.otype_params = otype_params;
      Ast_54.Outcometree.otype_type = otype_type;
      Ast_54.Outcometree.otype_private = otype_private;
      Ast_54.Outcometree.otype_immediate = otype_immediate;
      Ast_54.Outcometree.otype_unboxed = otype_unboxed;
      Ast_54.Outcometree.otype_cstrs = otype_cstrs }
    ->
    {
      Ast_53.Outcometree.otype_name = otype_name;
      Ast_53.Outcometree.otype_params =
        (List.map (fun x -> copy_out_type_param x) otype_params);
      Ast_53.Outcometree.otype_type = (copy_out_type otype_type);
      Ast_53.Outcometree.otype_private = (copy_private_flag otype_private);
      Ast_53.Outcometree.otype_immediate =
        (copy_Type_immediacy_t otype_immediate);
      Ast_53.Outcometree.otype_unboxed = otype_unboxed;
      Ast_53.Outcometree.otype_cstrs =
        (List.map
           (fun x ->
              let (x0, x1) = x in ((copy_out_type x0), (copy_out_type x1)))
           otype_cstrs)
    }
and (copy_Type_immediacy_t :
      Ast_54.Type_immediacy.t -> Ast_53.Type_immediacy.t)
  =
  function
  | Ast_54.Type_immediacy.Unknown -> Ast_53.Type_immediacy.Unknown
  | Ast_54.Type_immediacy.Always -> Ast_53.Type_immediacy.Always
  | Ast_54.Type_immediacy.Always_on_64bits ->
      Ast_53.Type_immediacy.Always_on_64bits
and (copy_out_module_type :
      Ast_54.Outcometree.out_module_type ->
        Ast_53.Outcometree.out_module_type)
  =
  function
  | Ast_54.Outcometree.Omty_abstract -> Ast_53.Outcometree.Omty_abstract
  | Ast_54.Outcometree.Omty_functor (x0, x1) ->
      Ast_53.Outcometree.Omty_functor
        ((Option.map
            (fun x ->
               let (x0, x1) = x in
               ((Option.map (fun x -> x) x0), (copy_out_module_type x1))) x0),
          (copy_out_module_type x1))
  | Ast_54.Outcometree.Omty_ident x0 ->
      Ast_53.Outcometree.Omty_ident (copy_out_ident x0)
  | Ast_54.Outcometree.Omty_signature x0 ->
      Ast_53.Outcometree.Omty_signature
        (List.map (fun x -> copy_out_sig_item x) x0)
  | Ast_54.Outcometree.Omty_alias x0 ->
      Ast_53.Outcometree.Omty_alias (copy_out_ident x0)
and (copy_out_ext_status :
      Ast_54.Outcometree.out_ext_status -> Ast_53.Outcometree.out_ext_status)
  =
  function
  | Ast_54.Outcometree.Oext_first -> Ast_53.Outcometree.Oext_first
  | Ast_54.Outcometree.Oext_next -> Ast_53.Outcometree.Oext_next
  | Ast_54.Outcometree.Oext_exception -> Ast_53.Outcometree.Oext_exception
and (copy_out_extension_constructor :
      Ast_54.Outcometree.out_extension_constructor ->
        Ast_53.Outcometree.out_extension_constructor)
  =
  fun
    { Ast_54.Outcometree.oext_name = oext_name;
      Ast_54.Outcometree.oext_type_name = oext_type_name;
      Ast_54.Outcometree.oext_type_params = oext_type_params;
      Ast_54.Outcometree.oext_args = oext_args;
      Ast_54.Outcometree.oext_ret_type = oext_ret_type;
      Ast_54.Outcometree.oext_private = oext_private }
    ->
    {
      Ast_53.Outcometree.oext_name = oext_name;
      Ast_53.Outcometree.oext_type_name = oext_type_name;
      Ast_53.Outcometree.oext_type_params =
        (List.map (fun x -> x) oext_type_params);
      Ast_53.Outcometree.oext_args =
        (List.map (fun x -> copy_out_type x) oext_args);
      Ast_53.Outcometree.oext_ret_type =
        (Option.map (fun x -> copy_out_type x) oext_ret_type);
      Ast_53.Outcometree.oext_private = (copy_private_flag oext_private)
    }
and (copy_private_flag :
      Ast_54.Asttypes.private_flag -> Ast_53.Asttypes.private_flag)
  =
  function
  | Ast_54.Asttypes.Private -> Ast_53.Asttypes.Private
  | Ast_54.Asttypes.Public -> Ast_53.Asttypes.Public
and (copy_out_rec_status :
      Ast_54.Outcometree.out_rec_status -> Ast_53.Outcometree.out_rec_status)
  =
  function
  | Ast_54.Outcometree.Orec_not -> Ast_53.Outcometree.Orec_not
  | Ast_54.Outcometree.Orec_first -> Ast_53.Outcometree.Orec_first
  | Ast_54.Outcometree.Orec_next -> Ast_53.Outcometree.Orec_next
and (copy_out_class_type :
      Ast_54.Outcometree.out_class_type -> Ast_53.Outcometree.out_class_type)
  =
  function
  | Ast_54.Outcometree.Octy_constr (x0, x1) ->
      Ast_53.Outcometree.Octy_constr
        ((copy_out_ident x0), (List.map (fun x -> copy_out_type x) x1))
  | Ast_54.Outcometree.Octy_arrow (x0, x1, x2) ->
      Ast_53.Outcometree.Octy_arrow
        ((copy_arg_label x0), (copy_out_type x1), (copy_out_class_type x2))
  | Ast_54.Outcometree.Octy_signature (x0, x1) ->
      Ast_53.Outcometree.Octy_signature
        ((Option.map (fun x -> copy_out_type x) x0),
          (List.map (fun x -> copy_out_class_sig_item x) x1))
and (copy_out_class_sig_item :
      Ast_54.Outcometree.out_class_sig_item ->
        Ast_53.Outcometree.out_class_sig_item)
  =
  function
  | Ast_54.Outcometree.Ocsg_constraint (x0, x1) ->
      Ast_53.Outcometree.Ocsg_constraint
        ((copy_out_type x0), (copy_out_type x1))
  | Ast_54.Outcometree.Ocsg_method (x0, x1, x2, x3) ->
      Ast_53.Outcometree.Ocsg_method (x0, x1, x2, (copy_out_type x3))
  | Ast_54.Outcometree.Ocsg_value (x0, x1, x2, x3) ->
      Ast_53.Outcometree.Ocsg_value (x0, x1, x2, (copy_out_type x3))
and (copy_out_type_param :
      Ast_54.Outcometree.out_type_param -> Ast_53.Outcometree.out_type_param)
  =
  fun
    { Ast_54.Outcometree.ot_non_gen = ot_non_gen;
      Ast_54.Outcometree.ot_name = ot_name;
      Ast_54.Outcometree.ot_variance = ot_variance }
    ->
    {
      Ast_53.Outcometree.ot_non_gen = ot_non_gen;
      Ast_53.Outcometree.ot_name = ot_name;
      Ast_53.Outcometree.ot_variance =
        (let (x0, x1) = ot_variance in
         ((copy_variance x0), (copy_injectivity x1)))
    }
and (copy_injectivity :
      Ast_54.Asttypes.injectivity -> Ast_53.Asttypes.injectivity)
  =
  function
  | Ast_54.Asttypes.Injective -> Ast_53.Asttypes.Injective
  | Ast_54.Asttypes.NoInjectivity -> Ast_53.Asttypes.NoInjectivity
and (copy_variance : Ast_54.Asttypes.variance -> Ast_53.Asttypes.variance) =
  function
  | Ast_54.Asttypes.Covariant -> Ast_53.Asttypes.Covariant
  | Ast_54.Asttypes.Contravariant -> Ast_53.Asttypes.Contravariant
  | Ast_54.Asttypes.NoVariance -> Ast_53.Asttypes.NoVariance
  | Ast_54.Asttypes.Bivariant ->
    (* TODO(anmonteiro): this is likely not correct? *)
    Ast_53.Asttypes.NoVariance
and (copy_out_type :
      Ast_54.Outcometree.out_type -> Ast_53.Outcometree.out_type)
  =
  function
  | Ast_54.Outcometree.Otyp_abstract -> Ast_53.Outcometree.Otyp_abstract
  | Ast_54.Outcometree.Otyp_open -> Ast_53.Outcometree.Otyp_open
  | Ast_54.Outcometree.Otyp_alias {non_gen; aliased; alias} ->
      Ast_53.Outcometree.Otyp_alias {non_gen; aliased=(copy_out_type aliased); alias}

  | Ast_54.Outcometree.Otyp_arrow (x0, x1, x2) ->
      Ast_53.Outcometree.Otyp_arrow
        ((copy_arg_label x0), (copy_out_type x1), (copy_out_type x2))
  | Ast_54.Outcometree.Otyp_class (x0, x1) ->
      Ast_53.Outcometree.Otyp_class
        ((copy_out_ident x0), (List.map (fun x -> copy_out_type x) x1))
  | Ast_54.Outcometree.Otyp_constr (x0, x1) ->
      Ast_53.Outcometree.Otyp_constr
        ((copy_out_ident x0), (List.map (fun x -> copy_out_type x) x1))
  | Ast_54.Outcometree.Otyp_manifest (x0, x1) ->
      Ast_53.Outcometree.Otyp_manifest
        ((copy_out_type x0), (copy_out_type x1))
  | Ast_54.Outcometree.Otyp_object { fields; open_row } ->
      Ast_53.Outcometree.Otyp_object
         { fields =
           ((List.map (fun x -> let (x0, x1) = x in (x0, (copy_out_type x1)))
           fields));
           open_row }
  | Ast_54.Outcometree.Otyp_record x0 ->
      Ast_53.Outcometree.Otyp_record
        (List.map (fun x -> copy_out_label x) x0)
  | Ast_54.Outcometree.Otyp_stuff x0 -> Ast_53.Outcometree.Otyp_stuff x0
  | Ast_54.Outcometree.Otyp_sum x0 ->
      Ast_53.Outcometree.Otyp_sum
        (List.map (fun x -> copy_out_constructor x) x0)
  | Ast_54.Outcometree.Otyp_tuple x0 ->
      Ast_53.Outcometree.Otyp_tuple
        (List.map
           (fun x ->
              let (_x0, x1) = x in
              (copy_out_type x1)) x0)
  | Ast_54.Outcometree.Otyp_var (x0, x1) ->
      Ast_53.Outcometree.Otyp_var (x0, x1)
  | Ast_54.Outcometree.Otyp_variant (x0, x1, x2) ->
      Ast_53.Outcometree.Otyp_variant
        ((copy_out_variant x0), x1,
          (Option.map (fun x -> List.map (fun x -> x) x) x2))
  | Ast_54.Outcometree.Otyp_poly (x0, x1) ->
      Ast_53.Outcometree.Otyp_poly
        ((List.map (fun x -> x) x0), (copy_out_type x1))
  | Ast_54.Outcometree.Otyp_module
    { Ast_54.Outcometree.opack_path = opack_path;
      Ast_54.Outcometree.opack_cstrs = opack_cstrs } ->
      Ast_53.Outcometree.Otyp_module
        ((copy_out_ident opack_path),
          (List.map (fun x -> let (x0, x1) = x in (x0, (copy_out_type x1)))
             opack_cstrs))
  | Ast_54.Outcometree.Otyp_attribute (x0, x1) ->
      Ast_53.Outcometree.Otyp_attribute
        ((copy_out_type x0), (copy_out_attribute x1))
and (copy_out_attribute :
      Ast_54.Outcometree.out_attribute -> Ast_53.Outcometree.out_attribute)
  =
  fun { Ast_54.Outcometree.oattr_name = oattr_name } ->
    { Ast_53.Outcometree.oattr_name = oattr_name }
and (copy_out_variant :
      Ast_54.Outcometree.out_variant -> Ast_53.Outcometree.out_variant)
  =
  function
  | Ast_54.Outcometree.Ovar_fields x0 ->
      Ast_53.Outcometree.Ovar_fields
        (List.map
           (fun x ->
              let (x0, x1, x2) = x in
              (x0, x1, (List.map (fun x -> copy_out_type x) x2))) x0)
  | Ast_54.Outcometree.Ovar_typ x0 ->
      Ast_53.Outcometree.Ovar_typ (copy_out_type x0)
and (copy_out_constructor :
      Ast_54.Outcometree.out_constructor ->
        Ast_53.Outcometree.out_constructor)
  =
  fun
    { Ast_54.Outcometree.ocstr_name = ocstr_name;
      Ast_54.Outcometree.ocstr_args = ocstr_args;
      Ast_54.Outcometree.ocstr_return_type = ocstr_return_type }
    ->
    {
      Ast_53.Outcometree.ocstr_name = ocstr_name;
      Ast_53.Outcometree.ocstr_args =
        (List.map (fun x -> copy_out_type x) ocstr_args);
      Ast_53.Outcometree.ocstr_return_type =
        (Option.map (fun x -> copy_out_type x) ocstr_return_type)
    }
and (copy_out_label :
      Ast_54.Outcometree.out_label -> Ast_53.Outcometree.out_label)
  =
  fun
    { Ast_54.Outcometree.olab_name = olab_name;
      Ast_54.Outcometree.olab_mut = olab_mut;
      Ast_54.Outcometree.olab_atomic = _;
      Ast_54.Outcometree.olab_type = olab_type }
    ->
    {
      Ast_53.Outcometree.olab_name = olab_name;
      Ast_53.Outcometree.olab_mut = (copy_mutable_flag olab_mut);
      Ast_53.Outcometree.olab_type = (copy_out_type olab_type)
    }
and (copy_arg_label : Ast_54.Asttypes.arg_label -> Ast_53.Asttypes.arg_label)
  =
  function
  | Ast_54.Asttypes.Nolabel -> Ast_53.Asttypes.Nolabel
  | Ast_54.Asttypes.Labelled x0 -> Ast_53.Asttypes.Labelled x0
  | Ast_54.Asttypes.Optional x0 -> Ast_53.Asttypes.Optional x0
and (copy_out_value :
      Ast_54.Outcometree.out_value -> Ast_53.Outcometree.out_value)
  =
  function
  | Ast_54.Outcometree.Oval_array (x0, _x1) ->
      Ast_53.Outcometree.Oval_array
        ((List.map (fun x -> copy_out_value x) x0))
  | Ast_54.Outcometree.Oval_char x0 -> Ast_53.Outcometree.Oval_char x0
  | Ast_54.Outcometree.Oval_constr (x0, x1) ->
      Ast_53.Outcometree.Oval_constr
        ((copy_out_ident x0), (List.map (fun x -> copy_out_value x) x1))
  | Ast_54.Outcometree.Oval_ellipsis -> Ast_53.Outcometree.Oval_ellipsis
  | Ast_54.Outcometree.Oval_float x0 -> Ast_53.Outcometree.Oval_float x0
  | Ast_54.Outcometree.Oval_int x0 -> Ast_53.Outcometree.Oval_int x0
  | Ast_54.Outcometree.Oval_int32 x0 -> Ast_53.Outcometree.Oval_int32 x0
  | Ast_54.Outcometree.Oval_int64 x0 -> Ast_53.Outcometree.Oval_int64 x0
  | Ast_54.Outcometree.Oval_nativeint x0 ->
      Ast_53.Outcometree.Oval_nativeint x0
  | Ast_54.Outcometree.Oval_list x0 ->
      Ast_53.Outcometree.Oval_list (List.map (fun x -> copy_out_value x) x0)
  | Ast_54.Outcometree.Oval_printer x0 -> Ast_53.Outcometree.Oval_printer x0
  | Ast_54.Outcometree.Oval_record x0 ->
      Ast_53.Outcometree.Oval_record
        (List.map
           (fun x ->
              let (x0, x1) = x in ((copy_out_ident x0), (copy_out_value x1)))
           x0)
  | Ast_54.Outcometree.Oval_string (x0, x1, x2) ->
      Ast_53.Outcometree.Oval_string (x0, x1, (copy_out_string x2))
  | Ast_54.Outcometree.Oval_stuff x0 -> Ast_53.Outcometree.Oval_stuff x0
  | Ast_54.Outcometree.Oval_tuple x0 ->
      Ast_53.Outcometree.Oval_tuple
        (List.map
           (fun x ->
              let (_x0, x1) = x in
              (copy_out_value x1)) x0)
  | Ast_54.Outcometree.Oval_variant (x0, x1) ->
      Ast_53.Outcometree.Oval_variant
        (x0, (Option.map (fun x -> copy_out_value x) x1))
  | Ast_54.Outcometree.Oval_lazy x0 ->
      Ast_53.Outcometree.Oval_lazy (copy_out_value x0)
  | Ast_54.Outcometree.Oval_floatarray x0 ->
      let migrate x0 acc n =
        match n with
        | 0 -> acc
        | i ->
          Ast_53.Outcometree.Oval_float (Array.Floatarray.get x0 (i - 1)) :: acc
      in
      Ast_53.Outcometree.Oval_array (migrate x0 [] (Array.Floatarray.length x0))
and (copy_out_string :
      Ast_54.Outcometree.out_string -> Ast_53.Outcometree.out_string)
  =
  function
  | Ast_54.Outcometree.Ostr_string -> Ast_53.Outcometree.Ostr_string
  | Ast_54.Outcometree.Ostr_bytes -> Ast_53.Outcometree.Ostr_bytes
and (copy_out_ident :
      Ast_54.Outcometree.out_ident -> Ast_53.Outcometree.out_ident)
  =
  function
  | Ast_54.Outcometree.Oide_apply (x0, x1) ->
      Ast_53.Outcometree.Oide_apply
        ((copy_out_ident x0), (copy_out_ident x1))
  | Ast_54.Outcometree.Oide_dot (x0, x1) ->
      Ast_53.Outcometree.Oide_dot ((copy_out_ident x0), x1)
  | Ast_54.Outcometree.Oide_ident x0 ->
      Ast_53.Outcometree.Oide_ident (copy_out_name x0)
and (copy_out_name :
      Ast_54.Outcometree.out_name -> Ast_53.Outcometree.out_name)
  =
  fun { Ast_54.Outcometree.printed_name = printed_name } ->
    { Ast_53.Outcometree.printed_name = printed_name }
and (copy_mutable_flag :
      Ast_54.Asttypes.mutable_flag -> Ast_53.Asttypes.mutable_flag)
  =
  function
  | Ast_54.Asttypes.Immutable -> Ast_53.Asttypes.Immutable
  | Ast_54.Asttypes.Mutable -> Ast_53.Asttypes.Mutable
