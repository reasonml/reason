(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module From = Ast_403
module To = Ast_404

let rec copy_expression :
  From.Parsetree.expression ->
    To.Parsetree.expression
  =
  fun
    { From.Parsetree.pexp_desc = pexp_desc;
      From.Parsetree.pexp_loc = pexp_loc;
      From.Parsetree.pexp_attributes = pexp_attributes }
     ->
    {
      To.Parsetree.pexp_desc =
        (copy_expression_desc pexp_desc);
      To.Parsetree.pexp_loc =
        (copy_location pexp_loc);
      To.Parsetree.pexp_attributes =
        (copy_attributes pexp_attributes)
    }

and copy_expression_desc :
  From.Parsetree.expression_desc ->
    To.Parsetree.expression_desc
  =
  function
  | From.Parsetree.Pexp_ident x0 ->
      To.Parsetree.Pexp_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pexp_constant x0 ->
      To.Parsetree.Pexp_constant
        (copy_constant x0)
  | From.Parsetree.Pexp_let (x0,x1,x2) ->
      To.Parsetree.Pexp_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_function x0 ->
      To.Parsetree.Pexp_function
        (List.map copy_case x0)
  | From.Parsetree.Pexp_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pexp_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_expression x3))
  | From.Parsetree.Pexp_apply (x0,x1) ->
      To.Parsetree.Pexp_apply
        ((copy_expression x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pexp_match (x0,x1) ->
      To.Parsetree.Pexp_match
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_try (x0,x1) ->
      To.Parsetree.Pexp_try
        ((copy_expression x0),
          (List.map copy_case x1))
  | From.Parsetree.Pexp_tuple x0 ->
      To.Parsetree.Pexp_tuple
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_construct (x0,x1) ->
      To.Parsetree.Pexp_construct
        ((copy_loc
            copy_longident x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_variant (x0,x1) ->
      To.Parsetree.Pexp_variant
        ((copy_label x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_record (x0,x1) ->
      To.Parsetree.Pexp_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc
                   copy_longident x0),
                 (copy_expression x1))) x0),
          (copy_option copy_expression x1))
  | From.Parsetree.Pexp_field (x0,x1) ->
      To.Parsetree.Pexp_field
        ((copy_expression x0),
          (copy_loc
             copy_longident x1))
  | From.Parsetree.Pexp_setfield (x0,x1,x2) ->
      To.Parsetree.Pexp_setfield
        ((copy_expression x0),
          (copy_loc
             copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_array x0 ->
      To.Parsetree.Pexp_array
        (List.map copy_expression x0)
  | From.Parsetree.Pexp_ifthenelse (x0,x1,x2) ->
      To.Parsetree.Pexp_ifthenelse
        ((copy_expression x0),
          (copy_expression x1),
          (copy_option copy_expression x2))
  | From.Parsetree.Pexp_sequence (x0,x1) ->
      To.Parsetree.Pexp_sequence
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_while (x0,x1) ->
      To.Parsetree.Pexp_while
        ((copy_expression x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_for (x0,x1,x2,x3,x4) ->
      To.Parsetree.Pexp_for
        ((copy_pattern x0),
          (copy_expression x1),
          (copy_expression x2),
          (copy_direction_flag x3),
          (copy_expression x4))
  | From.Parsetree.Pexp_constraint (x0,x1) ->
      To.Parsetree.Pexp_constraint
        ((copy_expression x0),
          (copy_core_type x1))
  | From.Parsetree.Pexp_coerce (x0,x1,x2) ->
      To.Parsetree.Pexp_coerce
        ((copy_expression x0),
          (copy_option copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Pexp_send (x0,x1) ->
      To.Parsetree.Pexp_send
        ((copy_expression x0), x1)
  | From.Parsetree.Pexp_new x0 ->
      To.Parsetree.Pexp_new
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pexp_setinstvar (x0,x1) ->
      To.Parsetree.Pexp_setinstvar
        ((copy_loc (fun x  -> x) x0),
          (copy_expression x1))
  | From.Parsetree.Pexp_override x0 ->
      To.Parsetree.Pexp_override
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_loc (fun x  -> x) x0),
                (copy_expression x1))) x0)
  | From.Parsetree.Pexp_letmodule (x0,x1,x2) ->
      To.Parsetree.Pexp_letmodule
        ((copy_loc (fun x  -> x) x0),
          (copy_module_expr x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_assert x0 ->
      To.Parsetree.Pexp_assert
        (copy_expression x0)
  | From.Parsetree.Pexp_lazy x0 ->
      To.Parsetree.Pexp_lazy
        (copy_expression x0)
  | From.Parsetree.Pexp_poly (x0,x1) ->
      To.Parsetree.Pexp_poly
        ((copy_expression x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pexp_object x0 ->
      To.Parsetree.Pexp_object
        (copy_class_structure x0)
  | From.Parsetree.Pexp_newtype (x0,x1) ->
      To.Parsetree.Pexp_newtype
        (x0, (copy_expression x1))
  | From.Parsetree.Pexp_pack x0 ->
      To.Parsetree.Pexp_pack
        (copy_module_expr x0)
  | From.Parsetree.Pexp_open (x0,x1,x2) ->
      To.Parsetree.Pexp_open
        ((copy_override_flag x0),
          (copy_loc
             copy_longident x1),
          (copy_expression x2))
  | From.Parsetree.Pexp_extension x0 ->
      To.Parsetree.Pexp_extension
        (copy_extension x0)
  | From.Parsetree.Pexp_unreachable  ->
      To.Parsetree.Pexp_unreachable

and copy_direction_flag :
  From.Asttypes.direction_flag ->
    To.Asttypes.direction_flag
  =
  function
  | From.Asttypes.Upto  -> To.Asttypes.Upto
  | From.Asttypes.Downto  -> To.Asttypes.Downto

and copy_case :
  From.Parsetree.case -> To.Parsetree.case =
  fun
    { From.Parsetree.pc_lhs = pc_lhs;
      From.Parsetree.pc_guard = pc_guard;
      From.Parsetree.pc_rhs = pc_rhs }
     ->
    {
      To.Parsetree.pc_lhs =
        (copy_pattern pc_lhs);
      To.Parsetree.pc_guard =
        (copy_option copy_expression pc_guard);
      To.Parsetree.pc_rhs =
        (copy_expression pc_rhs)
    }

and copy_value_binding :
  From.Parsetree.value_binding ->
    To.Parsetree.value_binding
  =
  fun
    { From.Parsetree.pvb_pat = pvb_pat;
      From.Parsetree.pvb_expr = pvb_expr;
      From.Parsetree.pvb_attributes = pvb_attributes;
      From.Parsetree.pvb_loc = pvb_loc }
     ->
    {
      To.Parsetree.pvb_pat =
        (copy_pattern pvb_pat);
      To.Parsetree.pvb_expr =
        (copy_expression pvb_expr);
      To.Parsetree.pvb_attributes =
        (copy_attributes pvb_attributes);
      To.Parsetree.pvb_loc =
        (copy_location pvb_loc)
    }

and copy_pattern :
  From.Parsetree.pattern -> To.Parsetree.pattern =
  fun
    { From.Parsetree.ppat_desc = ppat_desc;
      From.Parsetree.ppat_loc = ppat_loc;
      From.Parsetree.ppat_attributes = ppat_attributes }
     ->
    {
      To.Parsetree.ppat_desc =
        (copy_pattern_desc ppat_desc);
      To.Parsetree.ppat_loc =
        (copy_location ppat_loc);
      To.Parsetree.ppat_attributes =
        (copy_attributes ppat_attributes)
    }

and copy_pattern_desc :
  From.Parsetree.pattern_desc ->
    To.Parsetree.pattern_desc
  =
  function
  | From.Parsetree.Ppat_any  ->
      To.Parsetree.Ppat_any
  | From.Parsetree.Ppat_var x0 ->
      To.Parsetree.Ppat_var
        (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_alias (x0,x1) ->
      To.Parsetree.Ppat_alias
        ((copy_pattern x0),
          (copy_loc (fun x  -> x) x1))
  | From.Parsetree.Ppat_constant x0 ->
      To.Parsetree.Ppat_constant
        (copy_constant x0)
  | From.Parsetree.Ppat_interval (x0,x1) ->
      To.Parsetree.Ppat_interval
        ((copy_constant x0),
          (copy_constant x1))
  | From.Parsetree.Ppat_tuple x0 ->
      To.Parsetree.Ppat_tuple
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_construct (x0,x1) ->
      To.Parsetree.Ppat_construct
        ((copy_loc
            copy_longident x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_variant (x0,x1) ->
      To.Parsetree.Ppat_variant
        ((copy_label x0),
          (copy_option copy_pattern x1))
  | From.Parsetree.Ppat_record (x0,x1) ->
      To.Parsetree.Ppat_record
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               ((copy_loc
                   copy_longident x0),
                 (copy_pattern x1))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ppat_array x0 ->
      To.Parsetree.Ppat_array
        (List.map copy_pattern x0)
  | From.Parsetree.Ppat_or (x0,x1) ->
      To.Parsetree.Ppat_or
        ((copy_pattern x0),
          (copy_pattern x1))
  | From.Parsetree.Ppat_constraint (x0,x1) ->
      To.Parsetree.Ppat_constraint
        ((copy_pattern x0),
          (copy_core_type x1))
  | From.Parsetree.Ppat_type x0 ->
      To.Parsetree.Ppat_type
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Ppat_lazy x0 ->
      To.Parsetree.Ppat_lazy
        (copy_pattern x0)
  | From.Parsetree.Ppat_unpack x0 ->
      To.Parsetree.Ppat_unpack
        (copy_loc (fun x  -> x) x0)
  | From.Parsetree.Ppat_exception x0 ->
      To.Parsetree.Ppat_exception
        (copy_pattern x0)
  | From.Parsetree.Ppat_extension x0 ->
      To.Parsetree.Ppat_extension
        (copy_extension x0)

and copy_core_type :
  From.Parsetree.core_type ->
    To.Parsetree.core_type
  =
  fun
    { From.Parsetree.ptyp_desc = ptyp_desc;
      From.Parsetree.ptyp_loc = ptyp_loc;
      From.Parsetree.ptyp_attributes = ptyp_attributes }
     ->
    {
      To.Parsetree.ptyp_desc =
        (copy_core_type_desc ptyp_desc);
      To.Parsetree.ptyp_loc =
        (copy_location ptyp_loc);
      To.Parsetree.ptyp_attributes =
        (copy_attributes ptyp_attributes)
    }

and copy_core_type_desc :
  From.Parsetree.core_type_desc ->
    To.Parsetree.core_type_desc
  =
  function
  | From.Parsetree.Ptyp_any  ->
      To.Parsetree.Ptyp_any
  | From.Parsetree.Ptyp_var x0 ->
      To.Parsetree.Ptyp_var x0
  | From.Parsetree.Ptyp_arrow (x0,x1,x2) ->
      To.Parsetree.Ptyp_arrow
        ((copy_arg_label x0),
          (copy_core_type x1),
          (copy_core_type x2))
  | From.Parsetree.Ptyp_tuple x0 ->
      To.Parsetree.Ptyp_tuple
        (List.map copy_core_type x0)
  | From.Parsetree.Ptyp_constr (x0,x1) ->
      To.Parsetree.Ptyp_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_object (x0,x1) ->
      To.Parsetree.Ptyp_object
        ((List.map
            (fun x  ->
               let (x0,x1,x2) = x  in
               (x0, (copy_attributes x1),
                 (copy_core_type x2))) x0),
          (copy_closed_flag x1))
  | From.Parsetree.Ptyp_class (x0,x1) ->
      To.Parsetree.Ptyp_class
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Ptyp_alias (x0,x1) ->
      To.Parsetree.Ptyp_alias
        ((copy_core_type x0), x1)
  | From.Parsetree.Ptyp_variant (x0,x1,x2) ->
      To.Parsetree.Ptyp_variant
        ((List.map copy_row_field x0),
          (copy_closed_flag x1),
          (copy_option
             (fun x  -> List.map copy_label x) x2))
  | From.Parsetree.Ptyp_poly (x0,x1) ->
      To.Parsetree.Ptyp_poly
        ((List.map (fun x  -> x) x0),
          (copy_core_type x1))
  | From.Parsetree.Ptyp_package x0 ->
      To.Parsetree.Ptyp_package
        (copy_package_type x0)
  | From.Parsetree.Ptyp_extension x0 ->
      To.Parsetree.Ptyp_extension
        (copy_extension x0)

and copy_package_type :
  From.Parsetree.package_type ->
    To.Parsetree.package_type
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc copy_longident x0),
      (List.map
         (fun x  ->
            let (x0,x1) = x  in
            ((copy_loc
                copy_longident x0),
              (copy_core_type x1))) x1))

and copy_row_field :
  From.Parsetree.row_field ->
    To.Parsetree.row_field
  =
  function
  | From.Parsetree.Rtag (x0,x1,x2,x3) ->
      To.Parsetree.Rtag
        ((copy_label x0),
          (copy_attributes x1), (copy_bool x2),
          (List.map copy_core_type x3))
  | From.Parsetree.Rinherit x0 ->
      To.Parsetree.Rinherit
        (copy_core_type x0)

and copy_attributes :
  From.Parsetree.attributes ->
    To.Parsetree.attributes
  = fun x  -> List.map copy_attribute x

and copy_attribute :
  From.Parsetree.attribute ->
    To.Parsetree.attribute
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload x1))

and copy_payload :
  From.Parsetree.payload -> To.Parsetree.payload =
  function
  | From.Parsetree.PStr x0 ->
      To.Parsetree.PStr
        (copy_structure x0)
  | From.Parsetree.PSig x0 ->
      To.Parsetree.PSig
        (copy_signature x0)
  | From.Parsetree.PTyp x0 ->
      To.Parsetree.PTyp
        (copy_core_type x0)
  | From.Parsetree.PPat (x0,x1) ->
      To.Parsetree.PPat
        ((copy_pattern x0),
          (copy_option copy_expression x1))

and copy_structure :
  From.Parsetree.structure ->
    To.Parsetree.structure
  = fun x  -> List.map copy_structure_item x

and copy_structure_item :
  From.Parsetree.structure_item ->
    To.Parsetree.structure_item
  =
  fun
    { From.Parsetree.pstr_desc = pstr_desc;
      From.Parsetree.pstr_loc = pstr_loc }
     ->
    {
      To.Parsetree.pstr_desc =
        (copy_structure_item_desc pstr_desc);
      To.Parsetree.pstr_loc =
        (copy_location pstr_loc)
    }

and copy_structure_item_desc :
  From.Parsetree.structure_item_desc ->
    To.Parsetree.structure_item_desc
  =
  function
  | From.Parsetree.Pstr_eval (x0,x1) ->
      To.Parsetree.Pstr_eval
        ((copy_expression x0),
          (copy_attributes x1))
  | From.Parsetree.Pstr_value (x0,x1) ->
      To.Parsetree.Pstr_value
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1))
  | From.Parsetree.Pstr_primitive x0 ->
      To.Parsetree.Pstr_primitive
        (copy_value_description x0)
  | From.Parsetree.Pstr_type (x0,x1) ->
      To.Parsetree.Pstr_type
        ((copy_rec_flag x0),
          (List.map copy_type_declaration x1))
  | From.Parsetree.Pstr_typext x0 ->
      To.Parsetree.Pstr_typext
        (copy_type_extension x0)
  | From.Parsetree.Pstr_exception x0 ->
      To.Parsetree.Pstr_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Pstr_module x0 ->
      To.Parsetree.Pstr_module
        (copy_module_binding x0)
  | From.Parsetree.Pstr_recmodule x0 ->
      To.Parsetree.Pstr_recmodule
        (List.map copy_module_binding x0)
  | From.Parsetree.Pstr_modtype x0 ->
      To.Parsetree.Pstr_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Pstr_open x0 ->
      To.Parsetree.Pstr_open
        (copy_open_description x0)
  | From.Parsetree.Pstr_class x0 ->
      To.Parsetree.Pstr_class
        (List.map copy_class_declaration x0)
  | From.Parsetree.Pstr_class_type x0 ->
      To.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Pstr_include x0 ->
      To.Parsetree.Pstr_include
        (copy_include_declaration x0)
  | From.Parsetree.Pstr_attribute x0 ->
      To.Parsetree.Pstr_attribute
        (copy_attribute x0)
  | From.Parsetree.Pstr_extension (x0,x1) ->
      To.Parsetree.Pstr_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_include_declaration :
  From.Parsetree.include_declaration ->
    To.Parsetree.include_declaration
  =
  fun x  ->
    copy_include_infos
      copy_module_expr x

and copy_class_declaration :
  From.Parsetree.class_declaration ->
    To.Parsetree.class_declaration
  =
  fun x  ->
    copy_class_infos
      copy_class_expr x

and copy_class_expr :
  From.Parsetree.class_expr ->
    To.Parsetree.class_expr
  =
  fun
    { From.Parsetree.pcl_desc = pcl_desc;
      From.Parsetree.pcl_loc = pcl_loc;
      From.Parsetree.pcl_attributes = pcl_attributes }
     ->
    {
      To.Parsetree.pcl_desc =
        (copy_class_expr_desc pcl_desc);
      To.Parsetree.pcl_loc =
        (copy_location pcl_loc);
      To.Parsetree.pcl_attributes =
        (copy_attributes pcl_attributes)
    }

and copy_class_expr_desc :
  From.Parsetree.class_expr_desc ->
    To.Parsetree.class_expr_desc
  =
  function
  | From.Parsetree.Pcl_constr (x0,x1) ->
      To.Parsetree.Pcl_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcl_structure x0 ->
      To.Parsetree.Pcl_structure
        (copy_class_structure x0)
  | From.Parsetree.Pcl_fun (x0,x1,x2,x3) ->
      To.Parsetree.Pcl_fun
        ((copy_arg_label x0),
          (copy_option copy_expression x1),
          (copy_pattern x2),
          (copy_class_expr x3))
  | From.Parsetree.Pcl_apply (x0,x1) ->
      To.Parsetree.Pcl_apply
        ((copy_class_expr x0),
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_arg_label x0),
                  (copy_expression x1))) x1))
  | From.Parsetree.Pcl_let (x0,x1,x2) ->
      To.Parsetree.Pcl_let
        ((copy_rec_flag x0),
          (List.map copy_value_binding x1),
          (copy_class_expr x2))
  | From.Parsetree.Pcl_constraint (x0,x1) ->
      To.Parsetree.Pcl_constraint
        ((copy_class_expr x0),
          (copy_class_type x1))
  | From.Parsetree.Pcl_extension x0 ->
      To.Parsetree.Pcl_extension
        (copy_extension x0)

and copy_class_structure :
  From.Parsetree.class_structure ->
    To.Parsetree.class_structure
  =
  fun
    { From.Parsetree.pcstr_self = pcstr_self;
      From.Parsetree.pcstr_fields = pcstr_fields }
     ->
    {
      To.Parsetree.pcstr_self =
        (copy_pattern pcstr_self);
      To.Parsetree.pcstr_fields =
        (List.map copy_class_field pcstr_fields)
    }

and copy_class_field :
  From.Parsetree.class_field ->
    To.Parsetree.class_field
  =
  fun
    { From.Parsetree.pcf_desc = pcf_desc;
      From.Parsetree.pcf_loc = pcf_loc;
      From.Parsetree.pcf_attributes = pcf_attributes }
     ->
    {
      To.Parsetree.pcf_desc =
        (copy_class_field_desc pcf_desc);
      To.Parsetree.pcf_loc =
        (copy_location pcf_loc);
      To.Parsetree.pcf_attributes =
        (copy_attributes pcf_attributes)
    }

and copy_class_field_desc :
  From.Parsetree.class_field_desc ->
    To.Parsetree.class_field_desc
  =
  function
  | From.Parsetree.Pcf_inherit (x0,x1,x2) ->
      To.Parsetree.Pcf_inherit
        ((copy_override_flag x0),
          (copy_class_expr x1),
          (copy_option (fun x  -> x) x2))
  | From.Parsetree.Pcf_val x0 ->
      To.Parsetree.Pcf_val
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_mutable_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_method x0 ->
      To.Parsetree.Pcf_method
        (let (x0,x1,x2) = x0  in
         ((copy_loc (fun x  -> x) x0),
           (copy_private_flag x1),
           (copy_class_field_kind x2)))
  | From.Parsetree.Pcf_constraint x0 ->
      To.Parsetree.Pcf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pcf_initializer x0 ->
      To.Parsetree.Pcf_initializer
        (copy_expression x0)
  | From.Parsetree.Pcf_attribute x0 ->
      To.Parsetree.Pcf_attribute
        (copy_attribute x0)
  | From.Parsetree.Pcf_extension x0 ->
      To.Parsetree.Pcf_extension
        (copy_extension x0)

and copy_class_field_kind :
  From.Parsetree.class_field_kind ->
    To.Parsetree.class_field_kind
  =
  function
  | From.Parsetree.Cfk_virtual x0 ->
      To.Parsetree.Cfk_virtual
        (copy_core_type x0)
  | From.Parsetree.Cfk_concrete (x0,x1) ->
      To.Parsetree.Cfk_concrete
        ((copy_override_flag x0),
          (copy_expression x1))

and copy_module_binding :
  From.Parsetree.module_binding ->
    To.Parsetree.module_binding
  =
  fun
    { From.Parsetree.pmb_name = pmb_name;
      From.Parsetree.pmb_expr = pmb_expr;
      From.Parsetree.pmb_attributes = pmb_attributes;
      From.Parsetree.pmb_loc = pmb_loc }
     ->
    {
      To.Parsetree.pmb_name =
        (copy_loc (fun x  -> x) pmb_name);
      To.Parsetree.pmb_expr =
        (copy_module_expr pmb_expr);
      To.Parsetree.pmb_attributes =
        (copy_attributes pmb_attributes);
      To.Parsetree.pmb_loc =
        (copy_location pmb_loc)
    }

and copy_module_expr :
  From.Parsetree.module_expr ->
    To.Parsetree.module_expr
  =
  fun
    { From.Parsetree.pmod_desc = pmod_desc;
      From.Parsetree.pmod_loc = pmod_loc;
      From.Parsetree.pmod_attributes = pmod_attributes }
     ->
    {
      To.Parsetree.pmod_desc =
        (copy_module_expr_desc pmod_desc);
      To.Parsetree.pmod_loc =
        (copy_location pmod_loc);
      To.Parsetree.pmod_attributes =
        (copy_attributes pmod_attributes)
    }

and copy_module_expr_desc :
  From.Parsetree.module_expr_desc ->
    To.Parsetree.module_expr_desc
  =
  function
  | From.Parsetree.Pmod_ident x0 ->
      To.Parsetree.Pmod_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pmod_structure x0 ->
      To.Parsetree.Pmod_structure
        (copy_structure x0)
  | From.Parsetree.Pmod_functor (x0,x1,x2) ->
      To.Parsetree.Pmod_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_expr x2))
  | From.Parsetree.Pmod_apply (x0,x1) ->
      To.Parsetree.Pmod_apply
        ((copy_module_expr x0),
          (copy_module_expr x1))
  | From.Parsetree.Pmod_constraint (x0,x1) ->
      To.Parsetree.Pmod_constraint
        ((copy_module_expr x0),
          (copy_module_type x1))
  | From.Parsetree.Pmod_unpack x0 ->
      To.Parsetree.Pmod_unpack
        (copy_expression x0)
  | From.Parsetree.Pmod_extension x0 ->
      To.Parsetree.Pmod_extension
        (copy_extension x0)

and copy_module_type :
  From.Parsetree.module_type ->
    To.Parsetree.module_type
  =
  fun
    { From.Parsetree.pmty_desc = pmty_desc;
      From.Parsetree.pmty_loc = pmty_loc;
      From.Parsetree.pmty_attributes = pmty_attributes }
     ->
    {
      To.Parsetree.pmty_desc =
        (copy_module_type_desc pmty_desc);
      To.Parsetree.pmty_loc =
        (copy_location pmty_loc);
      To.Parsetree.pmty_attributes =
        (copy_attributes pmty_attributes)
    }

and copy_module_type_desc :
  From.Parsetree.module_type_desc ->
    To.Parsetree.module_type_desc
  =
  function
  | From.Parsetree.Pmty_ident x0 ->
      To.Parsetree.Pmty_ident
        (copy_loc copy_longident
           x0)
  | From.Parsetree.Pmty_signature x0 ->
      To.Parsetree.Pmty_signature
        (copy_signature x0)
  | From.Parsetree.Pmty_functor (x0,x1,x2) ->
      To.Parsetree.Pmty_functor
        ((copy_loc (fun x  -> x) x0),
          (copy_option copy_module_type x1),
          (copy_module_type x2))
  | From.Parsetree.Pmty_with (x0,x1) ->
      To.Parsetree.Pmty_with
        ((copy_module_type x0),
          (List.map copy_with_constraint x1))
  | From.Parsetree.Pmty_typeof x0 ->
      To.Parsetree.Pmty_typeof
        (copy_module_expr x0)
  | From.Parsetree.Pmty_extension x0 ->
      To.Parsetree.Pmty_extension
        (copy_extension x0)
  | From.Parsetree.Pmty_alias x0 ->
      To.Parsetree.Pmty_alias
        (copy_loc copy_longident
           x0)

and copy_with_constraint :
  From.Parsetree.with_constraint ->
    To.Parsetree.with_constraint
  =
  function
  | From.Parsetree.Pwith_type (x0,x1) ->
      To.Parsetree.Pwith_type
        ((copy_loc
            copy_longident x0),
          (copy_type_declaration x1))
  | From.Parsetree.Pwith_module (x0,x1) ->
      To.Parsetree.Pwith_module
        ((copy_loc
            copy_longident x0),
          (copy_loc
             copy_longident x1))
  | From.Parsetree.Pwith_typesubst x0 ->
      To.Parsetree.Pwith_typesubst
        (copy_type_declaration x0)
  | From.Parsetree.Pwith_modsubst (x0,x1) ->
      To.Parsetree.Pwith_modsubst
        ((copy_loc (fun x  -> x) x0),
          (copy_loc
             copy_longident x1))

and copy_signature :
  From.Parsetree.signature ->
    To.Parsetree.signature
  = fun x  -> List.map copy_signature_item x

and copy_signature_item :
  From.Parsetree.signature_item ->
    To.Parsetree.signature_item
  =
  fun
    { From.Parsetree.psig_desc = psig_desc;
      From.Parsetree.psig_loc = psig_loc }
     ->
    {
      To.Parsetree.psig_desc =
        (copy_signature_item_desc psig_desc);
      To.Parsetree.psig_loc =
        (copy_location psig_loc)
    }

and copy_signature_item_desc :
  From.Parsetree.signature_item_desc ->
    To.Parsetree.signature_item_desc
  =
  function
  | From.Parsetree.Psig_value x0 ->
      To.Parsetree.Psig_value
        (copy_value_description x0)
  | From.Parsetree.Psig_type (x0,x1) ->
      To.Parsetree.Psig_type
        ((copy_rec_flag x0),
          (List.map copy_type_declaration x1))
  | From.Parsetree.Psig_typext x0 ->
      To.Parsetree.Psig_typext
        (copy_type_extension x0)
  | From.Parsetree.Psig_exception x0 ->
      To.Parsetree.Psig_exception
        (copy_extension_constructor x0)
  | From.Parsetree.Psig_module x0 ->
      To.Parsetree.Psig_module
        (copy_module_declaration x0)
  | From.Parsetree.Psig_recmodule x0 ->
      To.Parsetree.Psig_recmodule
        (List.map copy_module_declaration x0)
  | From.Parsetree.Psig_modtype x0 ->
      To.Parsetree.Psig_modtype
        (copy_module_type_declaration x0)
  | From.Parsetree.Psig_open x0 ->
      To.Parsetree.Psig_open
        (copy_open_description x0)
  | From.Parsetree.Psig_include x0 ->
      To.Parsetree.Psig_include
        (copy_include_description x0)
  | From.Parsetree.Psig_class x0 ->
      To.Parsetree.Psig_class
        (List.map copy_class_description x0)
  | From.Parsetree.Psig_class_type x0 ->
      To.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | From.Parsetree.Psig_attribute x0 ->
      To.Parsetree.Psig_attribute
        (copy_attribute x0)
  | From.Parsetree.Psig_extension (x0,x1) ->
      To.Parsetree.Psig_extension
        ((copy_extension x0),
          (copy_attributes x1))

and copy_class_type_declaration :
  From.Parsetree.class_type_declaration ->
    To.Parsetree.class_type_declaration
  =
  fun x  ->
    copy_class_infos
      copy_class_type x

and copy_class_description :
  From.Parsetree.class_description ->
    To.Parsetree.class_description
  =
  fun x  ->
    copy_class_infos
      copy_class_type x

and copy_class_type :
  From.Parsetree.class_type ->
    To.Parsetree.class_type
  =
  fun
    { From.Parsetree.pcty_desc = pcty_desc;
      From.Parsetree.pcty_loc = pcty_loc;
      From.Parsetree.pcty_attributes = pcty_attributes }
     ->
    {
      To.Parsetree.pcty_desc =
        (copy_class_type_desc pcty_desc);
      To.Parsetree.pcty_loc =
        (copy_location pcty_loc);
      To.Parsetree.pcty_attributes =
        (copy_attributes pcty_attributes)
    }

and copy_class_type_desc :
  From.Parsetree.class_type_desc ->
    To.Parsetree.class_type_desc
  =
  function
  | From.Parsetree.Pcty_constr (x0,x1) ->
      To.Parsetree.Pcty_constr
        ((copy_loc
            copy_longident x0),
          (List.map copy_core_type x1))
  | From.Parsetree.Pcty_signature x0 ->
      To.Parsetree.Pcty_signature
        (copy_class_signature x0)
  | From.Parsetree.Pcty_arrow (x0,x1,x2) ->
      To.Parsetree.Pcty_arrow
        ((copy_arg_label x0),
          (copy_core_type x1),
          (copy_class_type x2))
  | From.Parsetree.Pcty_extension x0 ->
      To.Parsetree.Pcty_extension
        (copy_extension x0)

and copy_class_signature :
  From.Parsetree.class_signature ->
    To.Parsetree.class_signature
  =
  fun
    { From.Parsetree.pcsig_self = pcsig_self;
      From.Parsetree.pcsig_fields = pcsig_fields }
     ->
    {
      To.Parsetree.pcsig_self =
        (copy_core_type pcsig_self);
      To.Parsetree.pcsig_fields =
        (List.map copy_class_type_field
           pcsig_fields)
    }

and copy_class_type_field :
  From.Parsetree.class_type_field ->
    To.Parsetree.class_type_field
  =
  fun
    { From.Parsetree.pctf_desc = pctf_desc;
      From.Parsetree.pctf_loc = pctf_loc;
      From.Parsetree.pctf_attributes = pctf_attributes }
     ->
    {
      To.Parsetree.pctf_desc =
        (copy_class_type_field_desc pctf_desc);
      To.Parsetree.pctf_loc =
        (copy_location pctf_loc);
      To.Parsetree.pctf_attributes =
        (copy_attributes pctf_attributes)
    }

and copy_class_type_field_desc :
  From.Parsetree.class_type_field_desc ->
    To.Parsetree.class_type_field_desc
  =
  function
  | From.Parsetree.Pctf_inherit x0 ->
      To.Parsetree.Pctf_inherit
        (copy_class_type x0)
  | From.Parsetree.Pctf_val x0 ->
      To.Parsetree.Pctf_val
        (let (x0,x1,x2,x3) = x0  in
         (x0, (copy_mutable_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_method x0 ->
      To.Parsetree.Pctf_method
        (let (x0,x1,x2,x3) = x0  in
         (x0, (copy_private_flag x1),
           (copy_virtual_flag x2),
           (copy_core_type x3)))
  | From.Parsetree.Pctf_constraint x0 ->
      To.Parsetree.Pctf_constraint
        (let (x0,x1) = x0  in
         ((copy_core_type x0),
           (copy_core_type x1)))
  | From.Parsetree.Pctf_attribute x0 ->
      To.Parsetree.Pctf_attribute
        (copy_attribute x0)
  | From.Parsetree.Pctf_extension x0 ->
      To.Parsetree.Pctf_extension
        (copy_extension x0)

and copy_extension :
  From.Parsetree.extension ->
    To.Parsetree.extension
  =
  fun x  ->
    let (x0,x1) = x  in
    ((copy_loc (fun x  -> x) x0),
      (copy_payload x1))

and copy_class_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.class_infos ->
        'g0 To.Parsetree.class_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pci_virt = pci_virt;
        From.Parsetree.pci_params = pci_params;
        From.Parsetree.pci_name = pci_name;
        From.Parsetree.pci_expr = pci_expr;
        From.Parsetree.pci_loc = pci_loc;
        From.Parsetree.pci_attributes = pci_attributes }
       ->
      {
        To.Parsetree.pci_virt =
          (copy_virtual_flag pci_virt);
        To.Parsetree.pci_params =
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                ((copy_core_type x0),
                  (copy_variance x1))) pci_params);
        To.Parsetree.pci_name =
          (copy_loc (fun x  -> x) pci_name);
        To.Parsetree.pci_expr = (f0 pci_expr);
        To.Parsetree.pci_loc =
          (copy_location pci_loc);
        To.Parsetree.pci_attributes =
          (copy_attributes pci_attributes)
      }

and copy_virtual_flag :
  From.Asttypes.virtual_flag ->
    To.Asttypes.virtual_flag
  =
  function
  | From.Asttypes.Virtual  -> To.Asttypes.Virtual
  | From.Asttypes.Concrete  -> To.Asttypes.Concrete

and copy_include_description :
  From.Parsetree.include_description ->
    To.Parsetree.include_description
  =
  fun x  ->
    copy_include_infos
      copy_module_type x

and copy_include_infos :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Parsetree.include_infos ->
        'g0 To.Parsetree.include_infos
  =
  fun f0  ->
    fun
      { From.Parsetree.pincl_mod = pincl_mod;
        From.Parsetree.pincl_loc = pincl_loc;
        From.Parsetree.pincl_attributes = pincl_attributes }
       ->
      {
        To.Parsetree.pincl_mod = (f0 pincl_mod);
        To.Parsetree.pincl_loc =
          (copy_location pincl_loc);
        To.Parsetree.pincl_attributes =
          (copy_attributes pincl_attributes)
      }

and copy_open_description :
  From.Parsetree.open_description ->
    To.Parsetree.open_description
  =
  fun
    { From.Parsetree.popen_lid = popen_lid;
      From.Parsetree.popen_override = popen_override;
      From.Parsetree.popen_loc = popen_loc;
      From.Parsetree.popen_attributes = popen_attributes }
     ->
    {
      To.Parsetree.popen_lid =
        (copy_loc copy_longident
           popen_lid);
      To.Parsetree.popen_override =
        (copy_override_flag popen_override);
      To.Parsetree.popen_loc =
        (copy_location popen_loc);
      To.Parsetree.popen_attributes =
        (copy_attributes popen_attributes)
    }

and copy_override_flag :
  From.Asttypes.override_flag ->
    To.Asttypes.override_flag
  =
  function
  | From.Asttypes.Override  -> To.Asttypes.Override
  | From.Asttypes.Fresh  -> To.Asttypes.Fresh

and copy_module_type_declaration :
  From.Parsetree.module_type_declaration ->
    To.Parsetree.module_type_declaration
  =
  fun
    { From.Parsetree.pmtd_name = pmtd_name;
      From.Parsetree.pmtd_type = pmtd_type;
      From.Parsetree.pmtd_attributes = pmtd_attributes;
      From.Parsetree.pmtd_loc = pmtd_loc }
     ->
    {
      To.Parsetree.pmtd_name =
        (copy_loc (fun x  -> x) pmtd_name);
      To.Parsetree.pmtd_type =
        (copy_option copy_module_type pmtd_type);
      To.Parsetree.pmtd_attributes =
        (copy_attributes pmtd_attributes);
      To.Parsetree.pmtd_loc =
        (copy_location pmtd_loc)
    }

and copy_module_declaration :
  From.Parsetree.module_declaration ->
    To.Parsetree.module_declaration
  =
  fun
    { From.Parsetree.pmd_name = pmd_name;
      From.Parsetree.pmd_type = pmd_type;
      From.Parsetree.pmd_attributes = pmd_attributes;
      From.Parsetree.pmd_loc = pmd_loc }
     ->
    {
      To.Parsetree.pmd_name =
        (copy_loc (fun x  -> x) pmd_name);
      To.Parsetree.pmd_type =
        (copy_module_type pmd_type);
      To.Parsetree.pmd_attributes =
        (copy_attributes pmd_attributes);
      To.Parsetree.pmd_loc =
        (copy_location pmd_loc)
    }

and copy_type_extension :
  From.Parsetree.type_extension ->
    To.Parsetree.type_extension
  =
  fun
    { From.Parsetree.ptyext_path = ptyext_path;
      From.Parsetree.ptyext_params = ptyext_params;
      From.Parsetree.ptyext_constructors = ptyext_constructors;
      From.Parsetree.ptyext_private = ptyext_private;
      From.Parsetree.ptyext_attributes = ptyext_attributes }
     ->
    {
      To.Parsetree.ptyext_path =
        (copy_loc copy_longident
           ptyext_path);
      To.Parsetree.ptyext_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptyext_params);
      To.Parsetree.ptyext_constructors =
        (List.map copy_extension_constructor
           ptyext_constructors);
      To.Parsetree.ptyext_private =
        (copy_private_flag ptyext_private);
      To.Parsetree.ptyext_attributes =
        (copy_attributes ptyext_attributes)
    }

and copy_extension_constructor :
  From.Parsetree.extension_constructor ->
    To.Parsetree.extension_constructor
  =
  fun
    { From.Parsetree.pext_name = pext_name;
      From.Parsetree.pext_kind = pext_kind;
      From.Parsetree.pext_loc = pext_loc;
      From.Parsetree.pext_attributes = pext_attributes }
     ->
    {
      To.Parsetree.pext_name =
        (copy_loc (fun x  -> x) pext_name);
      To.Parsetree.pext_kind =
        (copy_extension_constructor_kind pext_kind);
      To.Parsetree.pext_loc =
        (copy_location pext_loc);
      To.Parsetree.pext_attributes =
        (copy_attributes pext_attributes)
    }

and copy_extension_constructor_kind :
  From.Parsetree.extension_constructor_kind ->
    To.Parsetree.extension_constructor_kind
  =
  function
  | From.Parsetree.Pext_decl (x0,x1) ->
      To.Parsetree.Pext_decl
        ((copy_constructor_arguments x0),
          (copy_option copy_core_type x1))
  | From.Parsetree.Pext_rebind x0 ->
      To.Parsetree.Pext_rebind
        (copy_loc copy_longident
           x0)

and copy_type_declaration :
  From.Parsetree.type_declaration ->
    To.Parsetree.type_declaration
  =
  fun
    { From.Parsetree.ptype_name = ptype_name;
      From.Parsetree.ptype_params = ptype_params;
      From.Parsetree.ptype_cstrs = ptype_cstrs;
      From.Parsetree.ptype_kind = ptype_kind;
      From.Parsetree.ptype_private = ptype_private;
      From.Parsetree.ptype_manifest = ptype_manifest;
      From.Parsetree.ptype_attributes = ptype_attributes;
      From.Parsetree.ptype_loc = ptype_loc }
     ->
    {
      To.Parsetree.ptype_name =
        (copy_loc (fun x  -> x) ptype_name);
      To.Parsetree.ptype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_core_type x0),
                (copy_variance x1))) ptype_params);
      To.Parsetree.ptype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              ((copy_core_type x0),
                (copy_core_type x1),
                (copy_location x2))) ptype_cstrs);
      To.Parsetree.ptype_kind =
        (copy_type_kind ptype_kind);
      To.Parsetree.ptype_private =
        (copy_private_flag ptype_private);
      To.Parsetree.ptype_manifest =
        (copy_option copy_core_type ptype_manifest);
      To.Parsetree.ptype_attributes =
        (copy_attributes ptype_attributes);
      To.Parsetree.ptype_loc =
        (copy_location ptype_loc)
    }

and copy_private_flag :
  From.Asttypes.private_flag ->
    To.Asttypes.private_flag
  =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_type_kind :
  From.Parsetree.type_kind ->
    To.Parsetree.type_kind
  =
  function
  | From.Parsetree.Ptype_abstract  ->
      To.Parsetree.Ptype_abstract
  | From.Parsetree.Ptype_variant x0 ->
      To.Parsetree.Ptype_variant
        (List.map copy_constructor_declaration x0)
  | From.Parsetree.Ptype_record x0 ->
      To.Parsetree.Ptype_record
        (List.map copy_label_declaration x0)
  | From.Parsetree.Ptype_open  ->
      To.Parsetree.Ptype_open

and copy_constructor_declaration :
  From.Parsetree.constructor_declaration ->
    To.Parsetree.constructor_declaration
  =
  fun
    { From.Parsetree.pcd_name = pcd_name;
      From.Parsetree.pcd_args = pcd_args;
      From.Parsetree.pcd_res = pcd_res;
      From.Parsetree.pcd_loc = pcd_loc;
      From.Parsetree.pcd_attributes = pcd_attributes }
     ->
    {
      To.Parsetree.pcd_name =
        (copy_loc (fun x  -> x) pcd_name);
      To.Parsetree.pcd_args =
        (copy_constructor_arguments pcd_args);
      To.Parsetree.pcd_res =
        (copy_option copy_core_type pcd_res);
      To.Parsetree.pcd_loc =
        (copy_location pcd_loc);
      To.Parsetree.pcd_attributes =
        (copy_attributes pcd_attributes)
    }

and copy_constructor_arguments :
  From.Parsetree.constructor_arguments ->
    To.Parsetree.constructor_arguments
  =
  function
  | From.Parsetree.Pcstr_tuple x0 ->
      To.Parsetree.Pcstr_tuple
        (List.map copy_core_type x0)
  | From.Parsetree.Pcstr_record x0 ->
      To.Parsetree.Pcstr_record
        (List.map copy_label_declaration x0)

and copy_label_declaration :
  From.Parsetree.label_declaration ->
    To.Parsetree.label_declaration
  =
  fun
    { From.Parsetree.pld_name = pld_name;
      From.Parsetree.pld_mutable = pld_mutable;
      From.Parsetree.pld_type = pld_type;
      From.Parsetree.pld_loc = pld_loc;
      From.Parsetree.pld_attributes = pld_attributes }
     ->
    {
      To.Parsetree.pld_name =
        (copy_loc (fun x  -> x) pld_name);
      To.Parsetree.pld_mutable =
        (copy_mutable_flag pld_mutable);
      To.Parsetree.pld_type =
        (copy_core_type pld_type);
      To.Parsetree.pld_loc =
        (copy_location pld_loc);
      To.Parsetree.pld_attributes =
        (copy_attributes pld_attributes)
    }

and copy_mutable_flag :
  From.Asttypes.mutable_flag ->
    To.Asttypes.mutable_flag
  =
  function
  | From.Asttypes.Immutable  ->
      To.Asttypes.Immutable
  | From.Asttypes.Mutable  -> To.Asttypes.Mutable

and copy_variance :
  From.Asttypes.variance -> To.Asttypes.variance =
  function
  | From.Asttypes.Covariant  ->
      To.Asttypes.Covariant
  | From.Asttypes.Contravariant  ->
      To.Asttypes.Contravariant
  | From.Asttypes.Invariant  ->
      To.Asttypes.Invariant

and copy_value_description :
  From.Parsetree.value_description ->
    To.Parsetree.value_description
  =
  fun
    { From.Parsetree.pval_name = pval_name;
      From.Parsetree.pval_type = pval_type;
      From.Parsetree.pval_prim = pval_prim;
      From.Parsetree.pval_attributes = pval_attributes;
      From.Parsetree.pval_loc = pval_loc }
     ->
    {
      To.Parsetree.pval_name =
        (copy_loc (fun x  -> x) pval_name);
      To.Parsetree.pval_type =
        (copy_core_type pval_type);
      To.Parsetree.pval_prim =
        (List.map (fun x  -> x) pval_prim);
      To.Parsetree.pval_attributes =
        (copy_attributes pval_attributes);
      To.Parsetree.pval_loc =
        (copy_location pval_loc)
    }

and copy_arg_label :
  From.Asttypes.arg_label -> To.Asttypes.arg_label
  =
  function
  | From.Asttypes.Nolabel  -> To.Asttypes.Nolabel
  | From.Asttypes.Labelled x0 ->
      To.Asttypes.Labelled x0
  | From.Asttypes.Optional x0 ->
      To.Asttypes.Optional x0

and copy_closed_flag :
  From.Asttypes.closed_flag ->
    To.Asttypes.closed_flag
  =
  function
  | From.Asttypes.Closed  -> To.Asttypes.Closed
  | From.Asttypes.Open  -> To.Asttypes.Open

and copy_label :
  From.Asttypes.label -> To.Asttypes.label =
  fun x  -> x

and copy_rec_flag :
  From.Asttypes.rec_flag -> To.Asttypes.rec_flag =
  function
  | From.Asttypes.Nonrecursive  ->
      To.Asttypes.Nonrecursive
  | From.Asttypes.Recursive  ->
      To.Asttypes.Recursive

and copy_constant :
  From.Parsetree.constant -> To.Parsetree.constant
  =
  function
  | From.Parsetree.Pconst_integer (x0,x1) ->
      To.Parsetree.Pconst_integer
        (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pconst_char x0 ->
      To.Parsetree.Pconst_char x0
  | From.Parsetree.Pconst_string (x0,x1) ->
      To.Parsetree.Pconst_string
        (x0, (copy_option (fun x  -> x) x1))
  | From.Parsetree.Pconst_float (x0,x1) ->
      To.Parsetree.Pconst_float
        (x0, (copy_option (fun x  -> x) x1))

and copy_option : 'f0 'g0 . ('f0 -> 'g0) -> 'f0 option -> 'g0 option =
  fun f0  -> function | None  -> None | Some x0 -> Some (f0 x0)

and copy_longident :
  From.Longident.t -> To.Longident.t =
  function
  | From.Longident.Lident x0 ->
      To.Longident.Lident x0
  | From.Longident.Ldot (x0,x1) ->
      To.Longident.Ldot
        ((copy_longident x0), x1)
  | From.Longident.Lapply (x0,x1) ->
      To.Longident.Lapply
        ((copy_longident x0), (copy_longident x1))

and copy_loc :
  'f0 'g0 .
    ('f0 -> 'g0) ->
      'f0 From.Asttypes.loc -> 'g0 To.Asttypes.loc
  =
  fun f0  ->
    fun
      { From.Asttypes.txt = txt;
        From.Asttypes.loc = loc }
       ->
      {
        To.Asttypes.txt = (f0 txt);
        To.Asttypes.loc = (copy_location loc)
      }

and copy_location :
  From.Location.t -> To.Location.t =
  fun
    { From.Location.loc_start = loc_start;
      From.Location.loc_end = loc_end;
      From.Location.loc_ghost = loc_ghost }
     ->
    {
      To.Location.loc_start = (copy_Lexing_position loc_start);
      To.Location.loc_end = (copy_Lexing_position loc_end);
      To.Location.loc_ghost = (copy_bool loc_ghost)
    }

and copy_bool : bool -> bool = function | false  -> false | true  -> true

and copy_Lexing_position : Lexing.position -> Lexing.position =
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

let rec copy_out_phrase :
  From.Outcometree.out_phrase -> To.Outcometree.out_phrase =
  function
  | From.Outcometree.Ophr_eval (x0,x1) ->
      To.Outcometree.Ophr_eval
        ((copy_out_value x0),
          (copy_out_type x1))
  | From.Outcometree.Ophr_signature x0 ->
      To.Outcometree.Ophr_signature
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_sig_item x0),
                (copy_option copy_out_value x1))) x0)
  | From.Outcometree.Ophr_exception x0 ->
      To.Outcometree.Ophr_exception
        (let (x0,x1) = x0  in
         ((copy_exn x0), (copy_out_value x1)))

and copy_exn : exn -> exn = fun x  -> x

and copy_out_sig_item :
  From.Outcometree.out_sig_item -> To.Outcometree.out_sig_item =
  function
  | From.Outcometree.Osig_class (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_class_type (x0,x1,x2,x3,x4) ->
      To.Outcometree.Osig_class_type
        ((copy_bool x0), x1,
          (List.map
             (fun x  ->
                let (x0,x1) = x  in
                (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
             x2), (copy_out_class_type x3),
          (copy_out_rec_status x4))
  | From.Outcometree.Osig_typext (x0,x1) ->
      To.Outcometree.Osig_typext
        ((copy_out_extension_constructor x0),
          (copy_out_ext_status x1))
  | From.Outcometree.Osig_modtype (x0,x1) ->
      To.Outcometree.Osig_modtype
        (x0, (copy_out_module_type x1))
  | From.Outcometree.Osig_module (x0,x1,x2) ->
      To.Outcometree.Osig_module
        (x0, (copy_out_module_type x1),
          (copy_out_rec_status x2))
  | From.Outcometree.Osig_type (x0,x1) ->
      To.Outcometree.Osig_type
        ((copy_out_type_decl x0),
          (copy_out_rec_status x1))
  | From.Outcometree.Osig_value x0 ->
      To.Outcometree.Osig_value
        (copy_out_val_decl x0)
  | From.Outcometree.Osig_ellipsis  -> To.Outcometree.Osig_ellipsis

and copy_out_val_decl :
  From.Outcometree.out_val_decl -> To.Outcometree.out_val_decl =
  fun
    { From.Outcometree.oval_name = oval_name;
      From.Outcometree.oval_type = oval_type;
      From.Outcometree.oval_prims = oval_prims;
      From.Outcometree.oval_attributes = oval_attributes }
     ->
    {
      To.Outcometree.oval_name = oval_name;
      To.Outcometree.oval_type =
        (copy_out_type oval_type);
      To.Outcometree.oval_prims = (List.map (fun x  -> x) oval_prims);
      To.Outcometree.oval_attributes =
        (List.map copy_out_attribute oval_attributes)
    }

and copy_out_type_decl :
  From.Outcometree.out_type_decl -> To.Outcometree.out_type_decl =
  fun
    { From.Outcometree.otype_name = otype_name;
      From.Outcometree.otype_params = otype_params;
      From.Outcometree.otype_type = otype_type;
      From.Outcometree.otype_private = otype_private;
      From.Outcometree.otype_immediate = otype_immediate;
      From.Outcometree.otype_cstrs = otype_cstrs }
     ->
    {
      To.Outcometree.otype_name = otype_name;
      To.Outcometree.otype_params =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              (x0, (let (x0,x1) = x1  in ((copy_bool x0), (copy_bool x1)))))
           otype_params);
      To.Outcometree.otype_type =
        (copy_out_type otype_type);
      To.Outcometree.otype_private =
        (copy_From_Asttypes_private_flag otype_private);
      To.Outcometree.otype_immediate = (copy_bool otype_immediate);
      To.Outcometree.otype_unboxed = false;
      To.Outcometree.otype_cstrs =
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_type x0),
                (copy_out_type x1))) otype_cstrs)
    }

and copy_out_module_type :
  From.Outcometree.out_module_type -> To.Outcometree.out_module_type
  =
  function
  | From.Outcometree.Omty_abstract  -> To.Outcometree.Omty_abstract
  | From.Outcometree.Omty_functor (x0,x1,x2) ->
      To.Outcometree.Omty_functor
        (x0, (copy_option copy_out_module_type x1),
          (copy_out_module_type x2))
  | From.Outcometree.Omty_ident x0 ->
      To.Outcometree.Omty_ident (copy_out_ident x0)
  | From.Outcometree.Omty_signature x0 ->
      To.Outcometree.Omty_signature
        (List.map copy_out_sig_item x0)
  | From.Outcometree.Omty_alias x0 ->
      To.Outcometree.Omty_alias (copy_out_ident x0)

and copy_out_ext_status :
  From.Outcometree.out_ext_status -> To.Outcometree.out_ext_status =
  function
  | From.Outcometree.Oext_first  -> To.Outcometree.Oext_first
  | From.Outcometree.Oext_next  -> To.Outcometree.Oext_next
  | From.Outcometree.Oext_exception  -> To.Outcometree.Oext_exception

and copy_out_extension_constructor :
  From.Outcometree.out_extension_constructor ->
    To.Outcometree.out_extension_constructor
  =
  fun
    { From.Outcometree.oext_name = oext_name;
      From.Outcometree.oext_type_name = oext_type_name;
      From.Outcometree.oext_type_params = oext_type_params;
      From.Outcometree.oext_args = oext_args;
      From.Outcometree.oext_ret_type = oext_ret_type;
      From.Outcometree.oext_private = oext_private }
     ->
    {
      To.Outcometree.oext_name = oext_name;
      To.Outcometree.oext_type_name = oext_type_name;
      To.Outcometree.oext_type_params =
        (List.map (fun x  -> x) oext_type_params);
      To.Outcometree.oext_args =
        (List.map copy_out_type oext_args);
      To.Outcometree.oext_ret_type =
        (copy_option copy_out_type oext_ret_type);
      To.Outcometree.oext_private =
        (copy_From_Asttypes_private_flag oext_private)
    }

and copy_From_Asttypes_private_flag :
  From.Asttypes.private_flag -> To.Asttypes.private_flag =
  function
  | From.Asttypes.Private  -> To.Asttypes.Private
  | From.Asttypes.Public  -> To.Asttypes.Public

and copy_out_rec_status :
  From.Outcometree.out_rec_status -> To.Outcometree.out_rec_status =
  function
  | From.Outcometree.Orec_not  -> To.Outcometree.Orec_not
  | From.Outcometree.Orec_first  -> To.Outcometree.Orec_first
  | From.Outcometree.Orec_next  -> To.Outcometree.Orec_next

and copy_out_class_type :
  From.Outcometree.out_class_type -> To.Outcometree.out_class_type =
  function
  | From.Outcometree.Octy_constr (x0,x1) ->
      To.Outcometree.Octy_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Octy_arrow (x0,x1,x2) ->
      To.Outcometree.Octy_arrow
        (x0, (copy_out_type x1),
          (copy_out_class_type x2))
  | From.Outcometree.Octy_signature (x0,x1) ->
      To.Outcometree.Octy_signature
        ((copy_option copy_out_type x0),
          (List.map copy_out_class_sig_item x1))

and copy_out_class_sig_item :
  From.Outcometree.out_class_sig_item ->
    To.Outcometree.out_class_sig_item
  =
  function
  | From.Outcometree.Ocsg_constraint (x0,x1) ->
      To.Outcometree.Ocsg_constraint
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Ocsg_method (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_method
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))
  | From.Outcometree.Ocsg_value (x0,x1,x2,x3) ->
      To.Outcometree.Ocsg_value
        (x0, (copy_bool x1), (copy_bool x2),
          (copy_out_type x3))

and copy_out_type :
  From.Outcometree.out_type -> To.Outcometree.out_type =
  function
  | From.Outcometree.Otyp_abstract  -> To.Outcometree.Otyp_abstract
  | From.Outcometree.Otyp_open  -> To.Outcometree.Otyp_open
  | From.Outcometree.Otyp_alias (x0,x1) ->
      To.Outcometree.Otyp_alias
        ((copy_out_type x0), x1)
  | From.Outcometree.Otyp_arrow (x0,x1,x2) ->
      To.Outcometree.Otyp_arrow
        (x0, (copy_out_type x1),
          (copy_out_type x2))
  | From.Outcometree.Otyp_class (x0,x1,x2) ->
      To.Outcometree.Otyp_class
        ((copy_bool x0), (copy_out_ident x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_constr (x0,x1) ->
      To.Outcometree.Otyp_constr
        ((copy_out_ident x0),
          (List.map copy_out_type x1))
  | From.Outcometree.Otyp_manifest (x0,x1) ->
      To.Outcometree.Otyp_manifest
        ((copy_out_type x0),
          (copy_out_type x1))
  | From.Outcometree.Otyp_object (x0,x1) ->
      To.Outcometree.Otyp_object
        ((List.map
            (fun x  ->
               let (x0,x1) = x  in
               (x0, (copy_out_type x1))) x0),
          (copy_option copy_bool x1))
  | From.Outcometree.Otyp_record x0 ->
      To.Outcometree.Otyp_record
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1), (copy_out_type x2)))
           x0)
  | From.Outcometree.Otyp_stuff x0 -> To.Outcometree.Otyp_stuff x0
  | From.Outcometree.Otyp_sum x0 ->
      To.Outcometree.Otyp_sum
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2))) x0)
  | From.Outcometree.Otyp_tuple x0 ->
      To.Outcometree.Otyp_tuple
        (List.map copy_out_type x0)
  | From.Outcometree.Otyp_var (x0,x1) ->
      To.Outcometree.Otyp_var ((copy_bool x0), x1)
  | From.Outcometree.Otyp_variant (x0,x1,x2,x3) ->
      To.Outcometree.Otyp_variant
        ((copy_bool x0), (copy_out_variant x1),
          (copy_bool x2),
          (copy_option (fun x  -> List.map (fun x  -> x) x) x3))
  | From.Outcometree.Otyp_poly (x0,x1) ->
      To.Outcometree.Otyp_poly
        ((List.map (fun x  -> x) x0), (copy_out_type x1))
  | From.Outcometree.Otyp_module (x0,x1,x2) ->
      To.Outcometree.Otyp_module
        (x0, (List.map (fun x  -> x) x1),
          (List.map copy_out_type x2))
  | From.Outcometree.Otyp_attribute (x0,x1) ->
      To.Outcometree.Otyp_attribute
        ((copy_out_type x0),
          (copy_out_attribute x1))

and copy_out_attribute :
  From.Outcometree.out_attribute -> To.Outcometree.out_attribute =
  fun { From.Outcometree.oattr_name = oattr_name }  ->
    { To.Outcometree.oattr_name = oattr_name }

and copy_out_variant :
  From.Outcometree.out_variant -> To.Outcometree.out_variant =
  function
  | From.Outcometree.Ovar_fields x0 ->
      To.Outcometree.Ovar_fields
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (copy_bool x1),
                (List.map copy_out_type x2))) x0)
  | From.Outcometree.Ovar_name (x0,x1) ->
      To.Outcometree.Ovar_name
        ((copy_out_ident x0),
          (List.map copy_out_type x1))

and copy_out_value :
  From.Outcometree.out_value -> To.Outcometree.out_value =
  function
  | From.Outcometree.Oval_array x0 ->
      To.Outcometree.Oval_array
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_char x0 -> To.Outcometree.Oval_char x0
  | From.Outcometree.Oval_constr (x0,x1) ->
      To.Outcometree.Oval_constr
        ((copy_out_ident x0),
          (List.map copy_out_value x1))
  | From.Outcometree.Oval_ellipsis  -> To.Outcometree.Oval_ellipsis
  | From.Outcometree.Oval_float x0 ->
      To.Outcometree.Oval_float (copy_float x0)
  | From.Outcometree.Oval_int x0 -> To.Outcometree.Oval_int x0
  | From.Outcometree.Oval_int32 x0 -> To.Outcometree.Oval_int32 x0
  | From.Outcometree.Oval_int64 x0 -> To.Outcometree.Oval_int64 x0
  | From.Outcometree.Oval_nativeint x0 ->
      To.Outcometree.Oval_nativeint x0
  | From.Outcometree.Oval_list x0 ->
      To.Outcometree.Oval_list
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_printer x0 ->
      To.Outcometree.Oval_printer x0
  | From.Outcometree.Oval_record x0 ->
      To.Outcometree.Oval_record
        (List.map
           (fun x  ->
              let (x0,x1) = x  in
              ((copy_out_ident x0),
                (copy_out_value x1))) x0)
  | From.Outcometree.Oval_string x0 -> To.Outcometree.Oval_string x0
  | From.Outcometree.Oval_stuff x0 -> To.Outcometree.Oval_stuff x0
  | From.Outcometree.Oval_tuple x0 ->
      To.Outcometree.Oval_tuple
        (List.map copy_out_value x0)
  | From.Outcometree.Oval_variant (x0,x1) ->
      To.Outcometree.Oval_variant
        (x0, (copy_option copy_out_value x1))

and copy_float : float -> float = fun x  -> x

and copy_out_ident :
  From.Outcometree.out_ident -> To.Outcometree.out_ident =
  function
  | From.Outcometree.Oide_apply (x0,x1) ->
      To.Outcometree.Oide_apply ((copy_out_ident x0), (copy_out_ident x1))
  | From.Outcometree.Oide_dot (x0,x1) ->
      To.Outcometree.Oide_dot ((copy_out_ident x0), x1)
  | From.Outcometree.Oide_ident x0 -> To.Outcometree.Oide_ident x0

let rec copy_toplevel_phrase :
  From.Parsetree.toplevel_phrase -> To.Parsetree.toplevel_phrase =
  function
  | From.Parsetree.Ptop_def x0 ->
      To.Parsetree.Ptop_def (copy_structure x0)
  | From.Parsetree.Ptop_dir (x0,x1) ->
      To.Parsetree.Ptop_dir (x0, copy_directive_argument x1)

and copy_directive_argument :
  From.Parsetree.directive_argument -> To.Parsetree.directive_argument =
  function
  | From.Parsetree.Pdir_none  -> To.Parsetree.Pdir_none
  | From.Parsetree.Pdir_string x0 -> To.Parsetree.Pdir_string x0
  | From.Parsetree.Pdir_int (x0,x1) ->
      To.Parsetree.Pdir_int (x0, copy_option (fun x  -> x) x1)
  | From.Parsetree.Pdir_ident x0 ->
      To.Parsetree.Pdir_ident (copy_longident x0)
  | From.Parsetree.Pdir_bool x0 ->
      To.Parsetree.Pdir_bool (copy_bool x0)

let copy_out_type_extension :
  From.Outcometree.out_type_extension -> To.Outcometree.out_type_extension =
  fun
    { From.Outcometree.otyext_name = otyext_name;
      From.Outcometree.otyext_params = otyext_params;
      From.Outcometree.otyext_constructors = otyext_constructors;
      From.Outcometree.otyext_private = otyext_private }
     ->
    {
      To.Outcometree.otyext_name = otyext_name;
      To.Outcometree.otyext_params =
        (List.map (fun x  -> x) otyext_params);
      To.Outcometree.otyext_constructors =
        (List.map
           (fun x  ->
              let (x0,x1,x2) = x  in
              (x0, (List.map copy_out_type x1),
                (copy_option copy_out_type x2)))
           otyext_constructors);
      To.Outcometree.otyext_private =
        (copy_private_flag otyext_private)
    }

let copy_cases x = List.map copy_case x
let copy_pat = copy_pattern
let copy_expr = copy_expression
let copy_typ = copy_core_type
