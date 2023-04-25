open Stdlib0
module From = Ast_412
module To = Ast_411
let rec copy_out_type_extension :
  Ast_412.Outcometree.out_type_extension ->
    Ast_411.Outcometree.out_type_extension
  =
  fun
    { Ast_412.Outcometree.otyext_name = otyext_name;
      Ast_412.Outcometree.otyext_params = otyext_params;
      Ast_412.Outcometree.otyext_constructors = otyext_constructors;
      Ast_412.Outcometree.otyext_private = otyext_private }
    ->
    {
      Ast_411.Outcometree.otyext_name = otyext_name;
      Ast_411.Outcometree.otyext_params =
        (List.map (fun x -> x) otyext_params);
      Ast_411.Outcometree.otyext_constructors =
        (List.map
           (fun x ->
              let (x0, x1, x2) = x in
              (x0, (List.map copy_out_type x1),
                (Option.map copy_out_type x2))) otyext_constructors);
      Ast_411.Outcometree.otyext_private = (copy_private_flag otyext_private)
    }
and copy_out_phrase :
  Ast_412.Outcometree.out_phrase -> Ast_411.Outcometree.out_phrase =
  function
  | Ast_412.Outcometree.Ophr_eval (x0, x1) ->
      Ast_411.Outcometree.Ophr_eval ((copy_out_value x0), (copy_out_type x1))
  | Ast_412.Outcometree.Ophr_signature x0 ->
      Ast_411.Outcometree.Ophr_signature
        (List.map
           (fun x ->
              let (x0, x1) = x in
              ((copy_out_sig_item x0), (Option.map copy_out_value x1))) x0)
  | Ast_412.Outcometree.Ophr_exception x0 ->
      Ast_411.Outcometree.Ophr_exception
        (let (x0, x1) = x0 in (x0, (copy_out_value x1)))
and copy_out_sig_item :
  Ast_412.Outcometree.out_sig_item -> Ast_411.Outcometree.out_sig_item =
  function
  | Ast_412.Outcometree.Osig_class (x0, x1, x2, x3, x4) ->
      Ast_411.Outcometree.Osig_class
        (x0, x1, (List.map copy_out_type_param x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | Ast_412.Outcometree.Osig_class_type (x0, x1, x2, x3, x4) ->
      Ast_411.Outcometree.Osig_class_type
        (x0, x1, (List.map copy_out_type_param x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | Ast_412.Outcometree.Osig_typext (x0, x1) ->
      Ast_411.Outcometree.Osig_typext
        ((copy_out_extension_constructor x0), (copy_out_ext_status x1))
  | Ast_412.Outcometree.Osig_modtype (x0, x1) ->
      Ast_411.Outcometree.Osig_modtype (x0, (copy_out_module_type x1))
  | Ast_412.Outcometree.Osig_module (x0, x1, x2) ->
      Ast_411.Outcometree.Osig_module
        (x0, (copy_out_module_type x1), (copy_out_rec_status x2))
  | Ast_412.Outcometree.Osig_type (x0, x1) ->
      Ast_411.Outcometree.Osig_type
        ((copy_out_type_decl x0), (copy_out_rec_status x1))
  | Ast_412.Outcometree.Osig_value x0 ->
      Ast_411.Outcometree.Osig_value (copy_out_val_decl x0)
  | Ast_412.Outcometree.Osig_ellipsis -> Ast_411.Outcometree.Osig_ellipsis
and copy_out_val_decl :
  Ast_412.Outcometree.out_val_decl -> Ast_411.Outcometree.out_val_decl =
  fun
    { Ast_412.Outcometree.oval_name = oval_name;
      Ast_412.Outcometree.oval_type = oval_type;
      Ast_412.Outcometree.oval_prims = oval_prims;
      Ast_412.Outcometree.oval_attributes = oval_attributes }
    ->
    {
      Ast_411.Outcometree.oval_name = oval_name;
      Ast_411.Outcometree.oval_type = (copy_out_type oval_type);
      Ast_411.Outcometree.oval_prims = (List.map (fun x -> x) oval_prims);
      Ast_411.Outcometree.oval_attributes =
        (List.map copy_out_attribute oval_attributes)
    }
and copy_out_type_decl :
  Ast_412.Outcometree.out_type_decl -> Ast_411.Outcometree.out_type_decl =
  fun
    { Ast_412.Outcometree.otype_name = otype_name;
      Ast_412.Outcometree.otype_params = otype_params;
      Ast_412.Outcometree.otype_type = otype_type;
      Ast_412.Outcometree.otype_private = otype_private;
      Ast_412.Outcometree.otype_immediate = otype_immediate;
      Ast_412.Outcometree.otype_unboxed = otype_unboxed;
      Ast_412.Outcometree.otype_cstrs = otype_cstrs }
    ->
    {
      Ast_411.Outcometree.otype_name = otype_name;
      Ast_411.Outcometree.otype_params =
        (List.map copy_out_type_param otype_params);
      Ast_411.Outcometree.otype_type = (copy_out_type otype_type);
      Ast_411.Outcometree.otype_private = (copy_private_flag otype_private);
      Ast_411.Outcometree.otype_immediate =
        (copy_Type_immediacy_t otype_immediate);
      Ast_411.Outcometree.otype_unboxed = otype_unboxed;
      Ast_411.Outcometree.otype_cstrs =
        (List.map
           (fun x ->
              let (x0, x1) = x in ((copy_out_type x0), (copy_out_type x1)))
           otype_cstrs)
    }
and copy_Type_immediacy_t :
  Ast_412.Type_immediacy.t -> Ast_411.Type_immediacy.t =
  function
  | Ast_412.Type_immediacy.Unknown -> Ast_411.Type_immediacy.Unknown
  | Ast_412.Type_immediacy.Always -> Ast_411.Type_immediacy.Always
  | Ast_412.Type_immediacy.Always_on_64bits ->
      Ast_411.Type_immediacy.Always_on_64bits
and copy_out_module_type :
  Ast_412.Outcometree.out_module_type -> Ast_411.Outcometree.out_module_type
  =
  function
  | Ast_412.Outcometree.Omty_abstract -> Ast_411.Outcometree.Omty_abstract
  | Ast_412.Outcometree.Omty_functor (x0, x1) ->
      Ast_411.Outcometree.Omty_functor
        ((Option.map
            (fun x ->
               let (x0, x1) = x in
               ((Option.map (fun x -> x) x0), (copy_out_module_type x1))) x0),
          (copy_out_module_type x1))
  | Ast_412.Outcometree.Omty_ident x0 ->
      Ast_411.Outcometree.Omty_ident (copy_out_ident x0)
  | Ast_412.Outcometree.Omty_signature x0 ->
      Ast_411.Outcometree.Omty_signature (List.map copy_out_sig_item x0)
  | Ast_412.Outcometree.Omty_alias x0 ->
      Ast_411.Outcometree.Omty_alias (copy_out_ident x0)
and copy_out_ext_status :
  Ast_412.Outcometree.out_ext_status -> Ast_411.Outcometree.out_ext_status =
  function
  | Ast_412.Outcometree.Oext_first -> Ast_411.Outcometree.Oext_first
  | Ast_412.Outcometree.Oext_next -> Ast_411.Outcometree.Oext_next
  | Ast_412.Outcometree.Oext_exception -> Ast_411.Outcometree.Oext_exception
and copy_out_extension_constructor :
  Ast_412.Outcometree.out_extension_constructor ->
    Ast_411.Outcometree.out_extension_constructor
  =
  fun
    { Ast_412.Outcometree.oext_name = oext_name;
      Ast_412.Outcometree.oext_type_name = oext_type_name;
      Ast_412.Outcometree.oext_type_params = oext_type_params;
      Ast_412.Outcometree.oext_args = oext_args;
      Ast_412.Outcometree.oext_ret_type = oext_ret_type;
      Ast_412.Outcometree.oext_private = oext_private }
    ->
    {
      Ast_411.Outcometree.oext_name = oext_name;
      Ast_411.Outcometree.oext_type_name = oext_type_name;
      Ast_411.Outcometree.oext_type_params =
        (List.map (fun x -> x) oext_type_params);
      Ast_411.Outcometree.oext_args = (List.map copy_out_type oext_args);
      Ast_411.Outcometree.oext_ret_type =
        (Option.map copy_out_type oext_ret_type);
      Ast_411.Outcometree.oext_private = (copy_private_flag oext_private)
    }
and copy_private_flag :
  Ast_412.Asttypes.private_flag -> Ast_411.Asttypes.private_flag =
  function
  | Ast_412.Asttypes.Private -> Ast_411.Asttypes.Private
  | Ast_412.Asttypes.Public -> Ast_411.Asttypes.Public
and copy_out_rec_status :
  Ast_412.Outcometree.out_rec_status -> Ast_411.Outcometree.out_rec_status =
  function
  | Ast_412.Outcometree.Orec_not -> Ast_411.Outcometree.Orec_not
  | Ast_412.Outcometree.Orec_first -> Ast_411.Outcometree.Orec_first
  | Ast_412.Outcometree.Orec_next -> Ast_411.Outcometree.Orec_next
and copy_out_class_type :
  Ast_412.Outcometree.out_class_type -> Ast_411.Outcometree.out_class_type =
  function
  | Ast_412.Outcometree.Octy_constr (x0, x1) ->
      Ast_411.Outcometree.Octy_constr
        ((copy_out_ident x0), (List.map copy_out_type x1))
  | Ast_412.Outcometree.Octy_arrow (x0, x1, x2) ->
      Ast_411.Outcometree.Octy_arrow
        (x0, (copy_out_type x1), (copy_out_class_type x2))
  | Ast_412.Outcometree.Octy_signature (x0, x1) ->
      Ast_411.Outcometree.Octy_signature
        ((Option.map copy_out_type x0),
          (List.map copy_out_class_sig_item x1))
and copy_out_class_sig_item :
  Ast_412.Outcometree.out_class_sig_item ->
    Ast_411.Outcometree.out_class_sig_item
  =
  function
  | Ast_412.Outcometree.Ocsg_constraint (x0, x1) ->
      Ast_411.Outcometree.Ocsg_constraint
        ((copy_out_type x0), (copy_out_type x1))
  | Ast_412.Outcometree.Ocsg_method (x0, x1, x2, x3) ->
      Ast_411.Outcometree.Ocsg_method (x0, x1, x2, (copy_out_type x3))
  | Ast_412.Outcometree.Ocsg_value (x0, x1, x2, x3) ->
      Ast_411.Outcometree.Ocsg_value (x0, x1, x2, (copy_out_type x3))
and copy_out_type_param : Ast_412.Outcometree.out_type_param -> string * (bool * bool) =
  function (str, (v,inj)) ->
    (match inj with
     | Ast_412.Asttypes.NoInjectivity -> ()
     | Ast_412.Asttypes.Injective ->
       (* ignoring [Injective] is not quite correct *)
       ()
    );
    let co, cn =
      match v with
      | Ast_412.Asttypes.Covariant -> (true, false)
      | Ast_412.Asttypes.Contravariant -> (false, true)
      | Ast_412.Asttypes.NoVariance -> (false, false)
    in
    str, (co, cn)
and copy_variance : Ast_412.Asttypes.variance -> Ast_411.Asttypes.variance =
  function
  | Ast_412.Asttypes.Covariant -> Ast_411.Asttypes.Covariant
  | Ast_412.Asttypes.Contravariant -> Ast_411.Asttypes.Contravariant
  | Ast_412.Asttypes.NoVariance -> Ast_411.Asttypes.Invariant
and copy_out_type :
  Ast_412.Outcometree.out_type -> Ast_411.Outcometree.out_type =
  function
  | Ast_412.Outcometree.Otyp_abstract -> Ast_411.Outcometree.Otyp_abstract
  | Ast_412.Outcometree.Otyp_open -> Ast_411.Outcometree.Otyp_open
  | Ast_412.Outcometree.Otyp_alias (x0, x1) ->
      Ast_411.Outcometree.Otyp_alias ((copy_out_type x0), x1)
  | Ast_412.Outcometree.Otyp_arrow (x0, x1, x2) ->
      Ast_411.Outcometree.Otyp_arrow
        (x0, (copy_out_type x1), (copy_out_type x2))
  | Ast_412.Outcometree.Otyp_class (x0, x1, x2) ->
      Ast_411.Outcometree.Otyp_class
        (x0, (copy_out_ident x1), (List.map copy_out_type x2))
  | Ast_412.Outcometree.Otyp_constr (x0, x1) ->
      Ast_411.Outcometree.Otyp_constr
        ((copy_out_ident x0), (List.map copy_out_type x1))
  | Ast_412.Outcometree.Otyp_manifest (x0, x1) ->
      Ast_411.Outcometree.Otyp_manifest
        ((copy_out_type x0), (copy_out_type x1))
  | Ast_412.Outcometree.Otyp_object (x0, x1) ->
      Ast_411.Outcometree.Otyp_object
        ((List.map (fun x -> let (x0, x1) = x in (x0, (copy_out_type x1))) x0),
          (Option.map (fun x -> x) x1))
  | Ast_412.Outcometree.Otyp_record x0 ->
      Ast_411.Outcometree.Otyp_record
        (List.map
           (fun x -> let (x0, x1, x2) = x in (x0, x1, (copy_out_type x2))) x0)
  | Ast_412.Outcometree.Otyp_stuff x0 -> Ast_411.Outcometree.Otyp_stuff x0
  | Ast_412.Outcometree.Otyp_sum x0 ->
      Ast_411.Outcometree.Otyp_sum
        (List.map
           (fun x ->
              let (x0, x1, x2) = x in
              (x0, (List.map copy_out_type x1),
                (Option.map copy_out_type x2))) x0)
  | Ast_412.Outcometree.Otyp_tuple x0 ->
      Ast_411.Outcometree.Otyp_tuple (List.map copy_out_type x0)
  | Ast_412.Outcometree.Otyp_var (x0, x1) ->
      Ast_411.Outcometree.Otyp_var (x0, x1)
  | Ast_412.Outcometree.Otyp_variant (x0, x1, x2, x3) ->
      Ast_411.Outcometree.Otyp_variant
        (x0, (copy_out_variant x1), x2,
          (Option.map (fun x -> List.map (fun x -> x) x) x3))
  | Ast_412.Outcometree.Otyp_poly (x0, x1) ->
      Ast_411.Outcometree.Otyp_poly
        ((List.map (fun x -> x) x0), (copy_out_type x1))
  | Ast_412.Outcometree.Otyp_module (x0, x1, x2) ->
      Ast_411.Outcometree.Otyp_module
        ((copy_out_ident x0), (List.map (fun x -> x) x1),
          (List.map copy_out_type x2))
  | Ast_412.Outcometree.Otyp_attribute (x0, x1) ->
      Ast_411.Outcometree.Otyp_attribute
        ((copy_out_type x0), (copy_out_attribute x1))
and copy_out_attribute :
  Ast_412.Outcometree.out_attribute -> Ast_411.Outcometree.out_attribute =
  fun { Ast_412.Outcometree.oattr_name = oattr_name } ->
    { Ast_411.Outcometree.oattr_name = oattr_name }
and copy_out_variant :
  Ast_412.Outcometree.out_variant -> Ast_411.Outcometree.out_variant =
  function
  | Ast_412.Outcometree.Ovar_fields x0 ->
      Ast_411.Outcometree.Ovar_fields
        (List.map
           (fun x ->
              let (x0, x1, x2) = x in (x0, x1, (List.map copy_out_type x2)))
           x0)
  | Ast_412.Outcometree.Ovar_typ x0 ->
      Ast_411.Outcometree.Ovar_typ (copy_out_type x0)
and copy_out_value :
  Ast_412.Outcometree.out_value -> Ast_411.Outcometree.out_value =
  function
  | Ast_412.Outcometree.Oval_array x0 ->
      Ast_411.Outcometree.Oval_array (List.map copy_out_value x0)
  | Ast_412.Outcometree.Oval_char x0 -> Ast_411.Outcometree.Oval_char x0
  | Ast_412.Outcometree.Oval_constr (x0, x1) ->
      Ast_411.Outcometree.Oval_constr
        ((copy_out_ident x0), (List.map copy_out_value x1))
  | Ast_412.Outcometree.Oval_ellipsis -> Ast_411.Outcometree.Oval_ellipsis
  | Ast_412.Outcometree.Oval_float x0 -> Ast_411.Outcometree.Oval_float x0
  | Ast_412.Outcometree.Oval_int x0 -> Ast_411.Outcometree.Oval_int x0
  | Ast_412.Outcometree.Oval_int32 x0 -> Ast_411.Outcometree.Oval_int32 x0
  | Ast_412.Outcometree.Oval_int64 x0 -> Ast_411.Outcometree.Oval_int64 x0
  | Ast_412.Outcometree.Oval_nativeint x0 ->
      Ast_411.Outcometree.Oval_nativeint x0
  | Ast_412.Outcometree.Oval_list x0 ->
      Ast_411.Outcometree.Oval_list (List.map copy_out_value x0)
  | Ast_412.Outcometree.Oval_printer x0 ->
      Ast_411.Outcometree.Oval_printer x0
  | Ast_412.Outcometree.Oval_record x0 ->
      Ast_411.Outcometree.Oval_record
        (List.map
           (fun x ->
              let (x0, x1) = x in ((copy_out_ident x0), (copy_out_value x1)))
           x0)
  | Ast_412.Outcometree.Oval_string (x0, x1, x2) ->
      Ast_411.Outcometree.Oval_string (x0, x1, (copy_out_string x2))
  | Ast_412.Outcometree.Oval_stuff x0 -> Ast_411.Outcometree.Oval_stuff x0
  | Ast_412.Outcometree.Oval_tuple x0 ->
      Ast_411.Outcometree.Oval_tuple (List.map copy_out_value x0)
  | Ast_412.Outcometree.Oval_variant (x0, x1) ->
      Ast_411.Outcometree.Oval_variant (x0, (Option.map copy_out_value x1))
and copy_out_string :
  Ast_412.Outcometree.out_string -> Ast_411.Outcometree.out_string =
  function
  | Ast_412.Outcometree.Ostr_string -> Ast_411.Outcometree.Ostr_string
  | Ast_412.Outcometree.Ostr_bytes -> Ast_411.Outcometree.Ostr_bytes
and copy_out_ident :
  Ast_412.Outcometree.out_ident -> Ast_411.Outcometree.out_ident =
  function
  | Ast_412.Outcometree.Oide_apply (x0, x1) ->
      Ast_411.Outcometree.Oide_apply
        ((copy_out_ident x0), (copy_out_ident x1))
  | Ast_412.Outcometree.Oide_dot (x0, x1) ->
      Ast_411.Outcometree.Oide_dot ((copy_out_ident x0), x1)
  | Ast_412.Outcometree.Oide_ident x0 ->
      Ast_411.Outcometree.Oide_ident (copy_out_name x0)
and copy_out_name :
  Ast_412.Outcometree.out_name -> Ast_411.Outcometree.out_name =
  fun { Ast_412.Outcometree.printed_name = printed_name } ->
    { Ast_411.Outcometree.printed_name = printed_name }
