open Ast_404

class virtual ['res] lifter =
  object (this)
    method lift_Parsetree_expression : Parsetree.expression -> 'res=
      (fun
         { Parsetree.pexp_desc = pexp_desc; Parsetree.pexp_loc = pexp_loc;
           Parsetree.pexp_attributes = pexp_attributes }
          ->
         this#record "Ast_404.Parsetree.expression"
           [("pexp_desc", (this#lift_Parsetree_expression_desc pexp_desc));
           ("pexp_loc", (this#lift_Location_t pexp_loc));
           ("pexp_attributes",
             (this#lift_Parsetree_attributes pexp_attributes))] : Parsetree.expression
                                                                    ->
                                                                    'res)
    method lift_Parsetree_expression_desc :
      Parsetree.expression_desc -> 'res=
      (function
       | Parsetree.Pexp_ident x0 ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_ident",
               [this#lift_Asttypes_loc this#lift_Longident_t x0])
       | Parsetree.Pexp_constant x0 ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_constant", [this#lift_Parsetree_constant x0])
       | Parsetree.Pexp_let (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_let",
               [this#lift_Asttypes_rec_flag x0;
               this#list (List.map this#lift_Parsetree_value_binding x1);
               this#lift_Parsetree_expression x2])
       | Parsetree.Pexp_function x0 ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_function",
               [this#list (List.map this#lift_Parsetree_case x0)])
       | Parsetree.Pexp_fun (x0,x1,x2,x3) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_fun",
               [this#lift_Asttypes_arg_label x0;
               this#lift_option this#lift_Parsetree_expression x1;
               this#lift_Parsetree_pattern x2;
               this#lift_Parsetree_expression x3])
       | Parsetree.Pexp_apply (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_apply",
               [this#lift_Parsetree_expression x0;
               this#list
                 (List.map
                    (fun x  ->
                       let (x0,x1) = x  in
                       this#tuple
                         [this#lift_Asttypes_arg_label x0;
                         this#lift_Parsetree_expression x1]) x1)])
       | Parsetree.Pexp_match (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_match",
               [this#lift_Parsetree_expression x0;
               this#list (List.map this#lift_Parsetree_case x1)])
       | Parsetree.Pexp_try (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_try",
               [this#lift_Parsetree_expression x0;
               this#list (List.map this#lift_Parsetree_case x1)])
       | Parsetree.Pexp_tuple x0 ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_tuple",
               [this#list (List.map this#lift_Parsetree_expression x0)])
       | Parsetree.Pexp_construct (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_construct",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#lift_option this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_variant (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_variant",
               [this#lift_Asttypes_label x0;
               this#lift_option this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_record (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_record",
               [this#list
                  (List.map
                     (fun x  ->
                        let (x0,x1) = x  in
                        this#tuple
                          [this#lift_Asttypes_loc this#lift_Longident_t x0;
                          this#lift_Parsetree_expression x1]) x0);
               this#lift_option this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_field (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_field",
               [this#lift_Parsetree_expression x0;
               this#lift_Asttypes_loc this#lift_Longident_t x1])
       | Parsetree.Pexp_setfield (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_setfield",
               [this#lift_Parsetree_expression x0;
               this#lift_Asttypes_loc this#lift_Longident_t x1;
               this#lift_Parsetree_expression x2])
       | Parsetree.Pexp_array x0 ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_array",
               [this#list (List.map this#lift_Parsetree_expression x0)])
       | Parsetree.Pexp_ifthenelse (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_ifthenelse",
               [this#lift_Parsetree_expression x0;
               this#lift_Parsetree_expression x1;
               this#lift_option this#lift_Parsetree_expression x2])
       | Parsetree.Pexp_sequence (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_sequence",
               [this#lift_Parsetree_expression x0;
               this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_while (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_while",
               [this#lift_Parsetree_expression x0;
               this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_for (x0,x1,x2,x3,x4) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_for",
               [this#lift_Parsetree_pattern x0;
               this#lift_Parsetree_expression x1;
               this#lift_Parsetree_expression x2;
               this#lift_Asttypes_direction_flag x3;
               this#lift_Parsetree_expression x4])
       | Parsetree.Pexp_constraint (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_constraint",
               [this#lift_Parsetree_expression x0;
               this#lift_Parsetree_core_type x1])
       | Parsetree.Pexp_coerce (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_coerce",
               [this#lift_Parsetree_expression x0;
               this#lift_option this#lift_Parsetree_core_type x1;
               this#lift_Parsetree_core_type x2])
       | Parsetree.Pexp_send (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_send",
               [this#lift_Parsetree_expression x0; this#string x1])
       | Parsetree.Pexp_new x0 ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_new", [this#lift_Asttypes_loc this#lift_Longident_t x0])
       | Parsetree.Pexp_setinstvar (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_setinstvar",
               [this#lift_Asttypes_loc this#string x0;
               this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_override x0 ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_override",
               [this#list
                  (List.map
                     (fun x  ->
                        let (x0,x1) = x  in
                        this#tuple
                          [this#lift_Asttypes_loc this#string x0;
                          this#lift_Parsetree_expression x1]) x0)])
       | Parsetree.Pexp_letmodule (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_letmodule",
               [this#lift_Asttypes_loc this#string x0;
               this#lift_Parsetree_module_expr x1;
               this#lift_Parsetree_expression x2])
       | Parsetree.Pexp_letexception (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_letexception",
               [this#lift_Parsetree_extension_constructor x0;
               this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_assert x0 ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_assert", [this#lift_Parsetree_expression x0])
       | Parsetree.Pexp_lazy x0 ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_lazy", [this#lift_Parsetree_expression x0])
       | Parsetree.Pexp_poly (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_poly",
               [this#lift_Parsetree_expression x0;
               this#lift_option this#lift_Parsetree_core_type x1])
       | Parsetree.Pexp_object x0 ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_object", [this#lift_Parsetree_class_structure x0])
       | Parsetree.Pexp_newtype (x0,x1) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_newtype",
               [this#string x0; this#lift_Parsetree_expression x1])
       | Parsetree.Pexp_pack x0 ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_pack", [this#lift_Parsetree_module_expr x0])
       | Parsetree.Pexp_open (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_open",
               [this#lift_Asttypes_override_flag x0;
               this#lift_Asttypes_loc this#lift_Longident_t x1;
               this#lift_Parsetree_expression x2])
       | Parsetree.Pexp_extension x0 ->
           this#constr "Ast_404.Parsetree.expression_desc"
             ("Pexp_extension", [this#lift_Parsetree_extension x0])
       | Parsetree.Pexp_unreachable  ->
           this#constr "Ast_404.Parsetree.expression_desc" ("Pexp_unreachable", []) :
      Parsetree.expression_desc -> 'res)
    method lift_Asttypes_direction_flag : Asttypes.direction_flag -> 'res=
      (function
       | Asttypes.Upto  -> this#constr "Ast_404.Asttypes.direction_flag" ("Upto", [])
       | Asttypes.Downto  ->
           this#constr "Ast_404.Asttypes.direction_flag" ("Downto", []) : Asttypes.direction_flag
                                                                    ->
                                                                    'res)
    method lift_Parsetree_case : Parsetree.case -> 'res=
      (fun
         { Parsetree.pc_lhs = pc_lhs; Parsetree.pc_guard = pc_guard;
           Parsetree.pc_rhs = pc_rhs }
          ->
         this#record "Ast_404.Parsetree.case"
           [("pc_lhs", (this#lift_Parsetree_pattern pc_lhs));
           ("pc_guard",
             (this#lift_option this#lift_Parsetree_expression pc_guard));
           ("pc_rhs", (this#lift_Parsetree_expression pc_rhs))] : Parsetree.case
                                                                    ->
                                                                    'res)
    method lift_Parsetree_value_binding : Parsetree.value_binding -> 'res=
      (fun
         { Parsetree.pvb_pat = pvb_pat; Parsetree.pvb_expr = pvb_expr;
           Parsetree.pvb_attributes = pvb_attributes;
           Parsetree.pvb_loc = pvb_loc }
          ->
         this#record "Ast_404.Parsetree.value_binding"
           [("pvb_pat", (this#lift_Parsetree_pattern pvb_pat));
           ("pvb_expr", (this#lift_Parsetree_expression pvb_expr));
           ("pvb_attributes",
             (this#lift_Parsetree_attributes pvb_attributes));
           ("pvb_loc", (this#lift_Location_t pvb_loc))] : Parsetree.value_binding
                                                            -> 'res)
    method lift_Parsetree_pattern : Parsetree.pattern -> 'res=
      (fun
         { Parsetree.ppat_desc = ppat_desc; Parsetree.ppat_loc = ppat_loc;
           Parsetree.ppat_attributes = ppat_attributes }
          ->
         this#record "Ast_404.Parsetree.pattern"
           [("ppat_desc", (this#lift_Parsetree_pattern_desc ppat_desc));
           ("ppat_loc", (this#lift_Location_t ppat_loc));
           ("ppat_attributes",
             (this#lift_Parsetree_attributes ppat_attributes))] : Parsetree.pattern
                                                                    ->
                                                                    'res)
    method lift_Parsetree_pattern_desc : Parsetree.pattern_desc -> 'res=
      (function
       | Parsetree.Ppat_any  ->
           this#constr "Ast_404.Parsetree.pattern_desc" ("Ppat_any", [])
       | Parsetree.Ppat_var x0 ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_var", [this#lift_Asttypes_loc this#string x0])
       | Parsetree.Ppat_alias (x0,x1) ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_alias",
               [this#lift_Parsetree_pattern x0;
               this#lift_Asttypes_loc this#string x1])
       | Parsetree.Ppat_constant x0 ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_constant", [this#lift_Parsetree_constant x0])
       | Parsetree.Ppat_interval (x0,x1) ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_interval",
               [this#lift_Parsetree_constant x0;
               this#lift_Parsetree_constant x1])
       | Parsetree.Ppat_tuple x0 ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_tuple",
               [this#list (List.map this#lift_Parsetree_pattern x0)])
       | Parsetree.Ppat_construct (x0,x1) ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_construct",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#lift_option this#lift_Parsetree_pattern x1])
       | Parsetree.Ppat_variant (x0,x1) ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_variant",
               [this#lift_Asttypes_label x0;
               this#lift_option this#lift_Parsetree_pattern x1])
       | Parsetree.Ppat_record (x0,x1) ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_record",
               [this#list
                  (List.map
                     (fun x  ->
                        let (x0,x1) = x  in
                        this#tuple
                          [this#lift_Asttypes_loc this#lift_Longident_t x0;
                          this#lift_Parsetree_pattern x1]) x0);
               this#lift_Asttypes_closed_flag x1])
       | Parsetree.Ppat_array x0 ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_array",
               [this#list (List.map this#lift_Parsetree_pattern x0)])
       | Parsetree.Ppat_or (x0,x1) ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_or",
               [this#lift_Parsetree_pattern x0;
               this#lift_Parsetree_pattern x1])
       | Parsetree.Ppat_constraint (x0,x1) ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_constraint",
               [this#lift_Parsetree_pattern x0;
               this#lift_Parsetree_core_type x1])
       | Parsetree.Ppat_type x0 ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_type", [this#lift_Asttypes_loc this#lift_Longident_t x0])
       | Parsetree.Ppat_lazy x0 ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_lazy", [this#lift_Parsetree_pattern x0])
       | Parsetree.Ppat_unpack x0 ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_unpack", [this#lift_Asttypes_loc this#string x0])
       | Parsetree.Ppat_exception x0 ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_exception", [this#lift_Parsetree_pattern x0])
       | Parsetree.Ppat_extension x0 ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_extension", [this#lift_Parsetree_extension x0])
       | Parsetree.Ppat_open (x0,x1) ->
           this#constr "Ast_404.Parsetree.pattern_desc"
             ("Ppat_open",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#lift_Parsetree_pattern x1]) : Parsetree.pattern_desc ->
                                                    'res)
    method lift_Parsetree_core_type : Parsetree.core_type -> 'res=
      (fun
         { Parsetree.ptyp_desc = ptyp_desc; Parsetree.ptyp_loc = ptyp_loc;
           Parsetree.ptyp_attributes = ptyp_attributes }
          ->
         this#record "Ast_404.Parsetree.core_type"
           [("ptyp_desc", (this#lift_Parsetree_core_type_desc ptyp_desc));
           ("ptyp_loc", (this#lift_Location_t ptyp_loc));
           ("ptyp_attributes",
             (this#lift_Parsetree_attributes ptyp_attributes))] : Parsetree.core_type
                                                                    ->
                                                                    'res)
    method lift_Parsetree_core_type_desc : Parsetree.core_type_desc -> 'res=
      (function
       | Parsetree.Ptyp_any  ->
           this#constr "Ast_404.Parsetree.core_type_desc" ("Ptyp_any", [])
       | Parsetree.Ptyp_var x0 ->
           this#constr "Ast_404.Parsetree.core_type_desc"
             ("Ptyp_var", [this#string x0])
       | Parsetree.Ptyp_arrow (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.core_type_desc"
             ("Ptyp_arrow",
               [this#lift_Asttypes_arg_label x0;
               this#lift_Parsetree_core_type x1;
               this#lift_Parsetree_core_type x2])
       | Parsetree.Ptyp_tuple x0 ->
           this#constr "Ast_404.Parsetree.core_type_desc"
             ("Ptyp_tuple",
               [this#list (List.map this#lift_Parsetree_core_type x0)])
       | Parsetree.Ptyp_constr (x0,x1) ->
           this#constr "Ast_404.Parsetree.core_type_desc"
             ("Ptyp_constr",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#list (List.map this#lift_Parsetree_core_type x1)])
       | Parsetree.Ptyp_object (x0,x1) ->
           this#constr "Ast_404.Parsetree.core_type_desc"
             ("Ptyp_object",
               [this#list
                  (List.map
                     (fun x  ->
                        let (x0,x1,x2) = x  in
                        this#tuple
                          [this#string x0;
                          this#lift_Parsetree_attributes x1;
                          this#lift_Parsetree_core_type x2]) x0);
               this#lift_Asttypes_closed_flag x1])
       | Parsetree.Ptyp_class (x0,x1) ->
           this#constr "Ast_404.Parsetree.core_type_desc"
             ("Ptyp_class",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#list (List.map this#lift_Parsetree_core_type x1)])
       | Parsetree.Ptyp_alias (x0,x1) ->
           this#constr "Ast_404.Parsetree.core_type_desc"
             ("Ptyp_alias",
               [this#lift_Parsetree_core_type x0; this#string x1])
       | Parsetree.Ptyp_variant (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.core_type_desc"
             ("Ptyp_variant",
               [this#list (List.map this#lift_Parsetree_row_field x0);
               this#lift_Asttypes_closed_flag x1;
               this#lift_option
                 (fun x  -> this#list (List.map this#lift_Asttypes_label x))
                 x2])
       | Parsetree.Ptyp_poly (x0,x1) ->
           this#constr "Ast_404.Parsetree.core_type_desc"
             ("Ptyp_poly",
               [this#list (List.map this#string x0);
               this#lift_Parsetree_core_type x1])
       | Parsetree.Ptyp_package x0 ->
           this#constr "Ast_404.Parsetree.core_type_desc"
             ("Ptyp_package", [this#lift_Parsetree_package_type x0])
       | Parsetree.Ptyp_extension x0 ->
           this#constr "Ast_404.Parsetree.core_type_desc"
             ("Ptyp_extension", [this#lift_Parsetree_extension x0]) :
      Parsetree.core_type_desc -> 'res)
    method lift_Parsetree_package_type : Parsetree.package_type -> 'res=
      (fun x  ->
         let (x0,x1) = x  in
         this#tuple
           [this#lift_Asttypes_loc this#lift_Longident_t x0;
           this#list
             (List.map
                (fun x  ->
                   let (x0,x1) = x  in
                   this#tuple
                     [this#lift_Asttypes_loc this#lift_Longident_t x0;
                     this#lift_Parsetree_core_type x1]) x1)] : Parsetree.package_type
                                                                 -> 'res)
    method lift_Parsetree_row_field : Parsetree.row_field -> 'res=
      (function
       | Parsetree.Rtag (x0,x1,x2,x3) ->
           this#constr "Ast_404.Parsetree.row_field"
             ("Rtag",
               [this#lift_Asttypes_label x0;
               this#lift_Parsetree_attributes x1;
               this#lift_bool x2;
               this#list (List.map this#lift_Parsetree_core_type x3)])
       | Parsetree.Rinherit x0 ->
           this#constr "Ast_404.Parsetree.row_field"
             ("Rinherit", [this#lift_Parsetree_core_type x0]) : Parsetree.row_field
                                                                  ->
                                                                  'res)
    method lift_Parsetree_attributes : Parsetree.attributes -> 'res=
      (fun x  -> this#list (List.map this#lift_Parsetree_attribute x) :
      Parsetree.attributes -> 'res)
    method lift_Parsetree_attribute : Parsetree.attribute -> 'res=
      (fun x  ->
         let (x0,x1) = x  in
         this#tuple
           [this#lift_Asttypes_loc this#string x0;
           this#lift_Parsetree_payload x1] : Parsetree.attribute -> 'res)
    method lift_Parsetree_payload : Parsetree.payload -> 'res=
      (function
       | Parsetree.PStr x0 ->
           this#constr "Ast_404.Parsetree.payload"
             ("PStr", [this#lift_Parsetree_structure x0])
       | Parsetree.PSig x0 ->
           this#constr "Ast_404.Parsetree.payload"
             ("PSig", [this#lift_Parsetree_signature x0])
       | Parsetree.PTyp x0 ->
           this#constr "Ast_404.Parsetree.payload"
             ("PTyp", [this#lift_Parsetree_core_type x0])
       | Parsetree.PPat (x0,x1) ->
           this#constr "Ast_404.Parsetree.payload"
             ("PPat",
               [this#lift_Parsetree_pattern x0;
               this#lift_option this#lift_Parsetree_expression x1]) :
      Parsetree.payload -> 'res)
    method lift_Parsetree_structure : Parsetree.structure -> 'res=
      (fun x  -> this#list (List.map this#lift_Parsetree_structure_item x) :
      Parsetree.structure -> 'res)
    method lift_Parsetree_structure_item : Parsetree.structure_item -> 'res=
      (fun { Parsetree.pstr_desc = pstr_desc; Parsetree.pstr_loc = pstr_loc }
          ->
         this#record "Ast_404.Parsetree.structure_item"
           [("pstr_desc",
              (this#lift_Parsetree_structure_item_desc pstr_desc));
           ("pstr_loc", (this#lift_Location_t pstr_loc))] : Parsetree.structure_item
                                                              -> 'res)
    method lift_Parsetree_structure_item_desc :
      Parsetree.structure_item_desc -> 'res=
      (function
       | Parsetree.Pstr_eval (x0,x1) ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_eval",
               [this#lift_Parsetree_expression x0;
               this#lift_Parsetree_attributes x1])
       | Parsetree.Pstr_value (x0,x1) ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_value",
               [this#lift_Asttypes_rec_flag x0;
               this#list (List.map this#lift_Parsetree_value_binding x1)])
       | Parsetree.Pstr_primitive x0 ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_primitive", [this#lift_Parsetree_value_description x0])
       | Parsetree.Pstr_type (x0,x1) ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_type",
               [this#lift_Asttypes_rec_flag x0;
               this#list (List.map this#lift_Parsetree_type_declaration x1)])
       | Parsetree.Pstr_typext x0 ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_typext", [this#lift_Parsetree_type_extension x0])
       | Parsetree.Pstr_exception x0 ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_exception",
               [this#lift_Parsetree_extension_constructor x0])
       | Parsetree.Pstr_module x0 ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_module", [this#lift_Parsetree_module_binding x0])
       | Parsetree.Pstr_recmodule x0 ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_recmodule",
               [this#list (List.map this#lift_Parsetree_module_binding x0)])
       | Parsetree.Pstr_modtype x0 ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_modtype",
               [this#lift_Parsetree_module_type_declaration x0])
       | Parsetree.Pstr_open x0 ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_open", [this#lift_Parsetree_open_description x0])
       | Parsetree.Pstr_class x0 ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_class",
               [this#list (List.map this#lift_Parsetree_class_declaration x0)])
       | Parsetree.Pstr_class_type x0 ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_class_type",
               [this#list
                  (List.map this#lift_Parsetree_class_type_declaration x0)])
       | Parsetree.Pstr_include x0 ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_include", [this#lift_Parsetree_include_declaration x0])
       | Parsetree.Pstr_attribute x0 ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_attribute", [this#lift_Parsetree_attribute x0])
       | Parsetree.Pstr_extension (x0,x1) ->
           this#constr "Ast_404.Parsetree.structure_item_desc"
             ("Pstr_extension",
               [this#lift_Parsetree_extension x0;
               this#lift_Parsetree_attributes x1]) : Parsetree.structure_item_desc
                                                       -> 'res)
    method lift_Parsetree_include_declaration :
      Parsetree.include_declaration -> 'res=
      (fun x  ->
         this#lift_Parsetree_include_infos this#lift_Parsetree_module_expr x :
      Parsetree.include_declaration -> 'res)
    method lift_Parsetree_class_declaration :
      Parsetree.class_declaration -> 'res=
      (fun x  ->
         this#lift_Parsetree_class_infos this#lift_Parsetree_class_expr x :
      Parsetree.class_declaration -> 'res)
    method lift_Parsetree_class_expr : Parsetree.class_expr -> 'res=
      (fun
         { Parsetree.pcl_desc = pcl_desc; Parsetree.pcl_loc = pcl_loc;
           Parsetree.pcl_attributes = pcl_attributes }
          ->
         this#record "Ast_404.Parsetree.class_expr"
           [("pcl_desc", (this#lift_Parsetree_class_expr_desc pcl_desc));
           ("pcl_loc", (this#lift_Location_t pcl_loc));
           ("pcl_attributes",
             (this#lift_Parsetree_attributes pcl_attributes))] : Parsetree.class_expr
                                                                   ->
                                                                   'res)
    method lift_Parsetree_class_expr_desc :
      Parsetree.class_expr_desc -> 'res=
      (function
       | Parsetree.Pcl_constr (x0,x1) ->
           this#constr "Ast_404.Parsetree.class_expr_desc"
             ("Pcl_constr",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#list (List.map this#lift_Parsetree_core_type x1)])
       | Parsetree.Pcl_structure x0 ->
           this#constr "Ast_404.Parsetree.class_expr_desc"
             ("Pcl_structure", [this#lift_Parsetree_class_structure x0])
       | Parsetree.Pcl_fun (x0,x1,x2,x3) ->
           this#constr "Ast_404.Parsetree.class_expr_desc"
             ("Pcl_fun",
               [this#lift_Asttypes_arg_label x0;
               this#lift_option this#lift_Parsetree_expression x1;
               this#lift_Parsetree_pattern x2;
               this#lift_Parsetree_class_expr x3])
       | Parsetree.Pcl_apply (x0,x1) ->
           this#constr "Ast_404.Parsetree.class_expr_desc"
             ("Pcl_apply",
               [this#lift_Parsetree_class_expr x0;
               this#list
                 (List.map
                    (fun x  ->
                       let (x0,x1) = x  in
                       this#tuple
                         [this#lift_Asttypes_arg_label x0;
                         this#lift_Parsetree_expression x1]) x1)])
       | Parsetree.Pcl_let (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.class_expr_desc"
             ("Pcl_let",
               [this#lift_Asttypes_rec_flag x0;
               this#list (List.map this#lift_Parsetree_value_binding x1);
               this#lift_Parsetree_class_expr x2])
       | Parsetree.Pcl_constraint (x0,x1) ->
           this#constr "Ast_404.Parsetree.class_expr_desc"
             ("Pcl_constraint",
               [this#lift_Parsetree_class_expr x0;
               this#lift_Parsetree_class_type x1])
       | Parsetree.Pcl_extension x0 ->
           this#constr "Ast_404.Parsetree.class_expr_desc"
             ("Pcl_extension", [this#lift_Parsetree_extension x0]) :
      Parsetree.class_expr_desc -> 'res)
    method lift_Parsetree_class_structure :
      Parsetree.class_structure -> 'res=
      (fun
         { Parsetree.pcstr_self = pcstr_self;
           Parsetree.pcstr_fields = pcstr_fields }
          ->
         this#record "Ast_404.Parsetree.class_structure"
           [("pcstr_self", (this#lift_Parsetree_pattern pcstr_self));
           ("pcstr_fields",
             (this#list
                (List.map this#lift_Parsetree_class_field pcstr_fields)))] :
      Parsetree.class_structure -> 'res)
    method lift_Parsetree_class_field : Parsetree.class_field -> 'res=
      (fun
         { Parsetree.pcf_desc = pcf_desc; Parsetree.pcf_loc = pcf_loc;
           Parsetree.pcf_attributes = pcf_attributes }
          ->
         this#record "Ast_404.Parsetree.class_field"
           [("pcf_desc", (this#lift_Parsetree_class_field_desc pcf_desc));
           ("pcf_loc", (this#lift_Location_t pcf_loc));
           ("pcf_attributes",
             (this#lift_Parsetree_attributes pcf_attributes))] : Parsetree.class_field
                                                                   ->
                                                                   'res)
    method lift_Parsetree_class_field_desc :
      Parsetree.class_field_desc -> 'res=
      (function
       | Parsetree.Pcf_inherit (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.class_field_desc"
             ("Pcf_inherit",
               [this#lift_Asttypes_override_flag x0;
               this#lift_Parsetree_class_expr x1;
               this#lift_option this#string x2])
       | Parsetree.Pcf_val x0 ->
           this#constr "Ast_404.Parsetree.class_field_desc"
             ("Pcf_val",
               [(let (x0,x1,x2) = x0  in
                 this#tuple
                   [this#lift_Asttypes_loc this#string x0;
                   this#lift_Asttypes_mutable_flag x1;
                   this#lift_Parsetree_class_field_kind x2])])
       | Parsetree.Pcf_method x0 ->
           this#constr "Ast_404.Parsetree.class_field_desc"
             ("Pcf_method",
               [(let (x0,x1,x2) = x0  in
                 this#tuple
                   [this#lift_Asttypes_loc this#string x0;
                   this#lift_Asttypes_private_flag x1;
                   this#lift_Parsetree_class_field_kind x2])])
       | Parsetree.Pcf_constraint x0 ->
           this#constr "Ast_404.Parsetree.class_field_desc"
             ("Pcf_constraint",
               [(let (x0,x1) = x0  in
                 this#tuple
                   [this#lift_Parsetree_core_type x0;
                   this#lift_Parsetree_core_type x1])])
       | Parsetree.Pcf_initializer x0 ->
           this#constr "Ast_404.Parsetree.class_field_desc"
             ("Pcf_initializer", [this#lift_Parsetree_expression x0])
       | Parsetree.Pcf_attribute x0 ->
           this#constr "Ast_404.Parsetree.class_field_desc"
             ("Pcf_attribute", [this#lift_Parsetree_attribute x0])
       | Parsetree.Pcf_extension x0 ->
           this#constr "Ast_404.Parsetree.class_field_desc"
             ("Pcf_extension", [this#lift_Parsetree_extension x0]) :
      Parsetree.class_field_desc -> 'res)
    method lift_Parsetree_class_field_kind :
      Parsetree.class_field_kind -> 'res=
      (function
       | Parsetree.Cfk_virtual x0 ->
           this#constr "Ast_404.Parsetree.class_field_kind"
             ("Cfk_virtual", [this#lift_Parsetree_core_type x0])
       | Parsetree.Cfk_concrete (x0,x1) ->
           this#constr "Ast_404.Parsetree.class_field_kind"
             ("Cfk_concrete",
               [this#lift_Asttypes_override_flag x0;
               this#lift_Parsetree_expression x1]) : Parsetree.class_field_kind
                                                       -> 'res)
    method lift_Parsetree_module_binding : Parsetree.module_binding -> 'res=
      (fun
         { Parsetree.pmb_name = pmb_name; Parsetree.pmb_expr = pmb_expr;
           Parsetree.pmb_attributes = pmb_attributes;
           Parsetree.pmb_loc = pmb_loc }
          ->
         this#record "Ast_404.Parsetree.module_binding"
           [("pmb_name", (this#lift_Asttypes_loc this#string pmb_name));
           ("pmb_expr", (this#lift_Parsetree_module_expr pmb_expr));
           ("pmb_attributes",
             (this#lift_Parsetree_attributes pmb_attributes));
           ("pmb_loc", (this#lift_Location_t pmb_loc))] : Parsetree.module_binding
                                                            -> 'res)
    method lift_Parsetree_module_expr : Parsetree.module_expr -> 'res=
      (fun
         { Parsetree.pmod_desc = pmod_desc; Parsetree.pmod_loc = pmod_loc;
           Parsetree.pmod_attributes = pmod_attributes }
          ->
         this#record "Ast_404.Parsetree.module_expr"
           [("pmod_desc", (this#lift_Parsetree_module_expr_desc pmod_desc));
           ("pmod_loc", (this#lift_Location_t pmod_loc));
           ("pmod_attributes",
             (this#lift_Parsetree_attributes pmod_attributes))] : Parsetree.module_expr
                                                                    ->
                                                                    'res)
    method lift_Parsetree_module_expr_desc :
      Parsetree.module_expr_desc -> 'res=
      (function
       | Parsetree.Pmod_ident x0 ->
           this#constr "Ast_404.Parsetree.module_expr_desc"
             ("Pmod_ident",
               [this#lift_Asttypes_loc this#lift_Longident_t x0])
       | Parsetree.Pmod_structure x0 ->
           this#constr "Ast_404.Parsetree.module_expr_desc"
             ("Pmod_structure", [this#lift_Parsetree_structure x0])
       | Parsetree.Pmod_functor (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.module_expr_desc"
             ("Pmod_functor",
               [this#lift_Asttypes_loc this#string x0;
               this#lift_option this#lift_Parsetree_module_type x1;
               this#lift_Parsetree_module_expr x2])
       | Parsetree.Pmod_apply (x0,x1) ->
           this#constr "Ast_404.Parsetree.module_expr_desc"
             ("Pmod_apply",
               [this#lift_Parsetree_module_expr x0;
               this#lift_Parsetree_module_expr x1])
       | Parsetree.Pmod_constraint (x0,x1) ->
           this#constr "Ast_404.Parsetree.module_expr_desc"
             ("Pmod_constraint",
               [this#lift_Parsetree_module_expr x0;
               this#lift_Parsetree_module_type x1])
       | Parsetree.Pmod_unpack x0 ->
           this#constr "Ast_404.Parsetree.module_expr_desc"
             ("Pmod_unpack", [this#lift_Parsetree_expression x0])
       | Parsetree.Pmod_extension x0 ->
           this#constr "Ast_404.Parsetree.module_expr_desc"
             ("Pmod_extension", [this#lift_Parsetree_extension x0]) :
      Parsetree.module_expr_desc -> 'res)
    method lift_Parsetree_module_type : Parsetree.module_type -> 'res=
      (fun
         { Parsetree.pmty_desc = pmty_desc; Parsetree.pmty_loc = pmty_loc;
           Parsetree.pmty_attributes = pmty_attributes }
          ->
         this#record "Ast_404.Parsetree.module_type"
           [("pmty_desc", (this#lift_Parsetree_module_type_desc pmty_desc));
           ("pmty_loc", (this#lift_Location_t pmty_loc));
           ("pmty_attributes",
             (this#lift_Parsetree_attributes pmty_attributes))] : Parsetree.module_type
                                                                    ->
                                                                    'res)
    method lift_Parsetree_module_type_desc :
      Parsetree.module_type_desc -> 'res=
      (function
       | Parsetree.Pmty_ident x0 ->
           this#constr "Ast_404.Parsetree.module_type_desc"
             ("Pmty_ident",
               [this#lift_Asttypes_loc this#lift_Longident_t x0])
       | Parsetree.Pmty_signature x0 ->
           this#constr "Ast_404.Parsetree.module_type_desc"
             ("Pmty_signature", [this#lift_Parsetree_signature x0])
       | Parsetree.Pmty_functor (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.module_type_desc"
             ("Pmty_functor",
               [this#lift_Asttypes_loc this#string x0;
               this#lift_option this#lift_Parsetree_module_type x1;
               this#lift_Parsetree_module_type x2])
       | Parsetree.Pmty_with (x0,x1) ->
           this#constr "Ast_404.Parsetree.module_type_desc"
             ("Pmty_with",
               [this#lift_Parsetree_module_type x0;
               this#list (List.map this#lift_Parsetree_with_constraint x1)])
       | Parsetree.Pmty_typeof x0 ->
           this#constr "Ast_404.Parsetree.module_type_desc"
             ("Pmty_typeof", [this#lift_Parsetree_module_expr x0])
       | Parsetree.Pmty_extension x0 ->
           this#constr "Ast_404.Parsetree.module_type_desc"
             ("Pmty_extension", [this#lift_Parsetree_extension x0])
       | Parsetree.Pmty_alias x0 ->
           this#constr "Ast_404.Parsetree.module_type_desc"
             ("Pmty_alias",
               [this#lift_Asttypes_loc this#lift_Longident_t x0]) : Parsetree.module_type_desc
                                                                    ->
                                                                    'res)
    method lift_Parsetree_with_constraint :
      Parsetree.with_constraint -> 'res=
      (function
       | Parsetree.Pwith_type (x0,x1) ->
           this#constr "Ast_404.Parsetree.with_constraint"
             ("Pwith_type",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#lift_Parsetree_type_declaration x1])
       | Parsetree.Pwith_module (x0,x1) ->
           this#constr "Ast_404.Parsetree.with_constraint"
             ("Pwith_module",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#lift_Asttypes_loc this#lift_Longident_t x1])
       | Parsetree.Pwith_typesubst x0 ->
           this#constr "Ast_404.Parsetree.with_constraint"
             ("Pwith_typesubst", [this#lift_Parsetree_type_declaration x0])
       | Parsetree.Pwith_modsubst (x0,x1) ->
           this#constr "Ast_404.Parsetree.with_constraint"
             ("Pwith_modsubst",
               [this#lift_Asttypes_loc this#string x0;
               this#lift_Asttypes_loc this#lift_Longident_t x1]) : Parsetree.with_constraint
                                                                    ->
                                                                    'res)
    method lift_Parsetree_signature : Parsetree.signature -> 'res=
      (fun x  -> this#list (List.map this#lift_Parsetree_signature_item x) :
      Parsetree.signature -> 'res)
    method lift_Parsetree_signature_item : Parsetree.signature_item -> 'res=
      (fun { Parsetree.psig_desc = psig_desc; Parsetree.psig_loc = psig_loc }
          ->
         this#record "Ast_404.Parsetree.signature_item"
           [("psig_desc",
              (this#lift_Parsetree_signature_item_desc psig_desc));
           ("psig_loc", (this#lift_Location_t psig_loc))] : Parsetree.signature_item
                                                              -> 'res)
    method lift_Parsetree_signature_item_desc :
      Parsetree.signature_item_desc -> 'res=
      (function
       | Parsetree.Psig_value x0 ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_value", [this#lift_Parsetree_value_description x0])
       | Parsetree.Psig_type (x0,x1) ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_type",
               [this#lift_Asttypes_rec_flag x0;
               this#list (List.map this#lift_Parsetree_type_declaration x1)])
       | Parsetree.Psig_typext x0 ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_typext", [this#lift_Parsetree_type_extension x0])
       | Parsetree.Psig_exception x0 ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_exception",
               [this#lift_Parsetree_extension_constructor x0])
       | Parsetree.Psig_module x0 ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_module", [this#lift_Parsetree_module_declaration x0])
       | Parsetree.Psig_recmodule x0 ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_recmodule",
               [this#list
                  (List.map this#lift_Parsetree_module_declaration x0)])
       | Parsetree.Psig_modtype x0 ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_modtype",
               [this#lift_Parsetree_module_type_declaration x0])
       | Parsetree.Psig_open x0 ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_open", [this#lift_Parsetree_open_description x0])
       | Parsetree.Psig_include x0 ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_include", [this#lift_Parsetree_include_description x0])
       | Parsetree.Psig_class x0 ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_class",
               [this#list (List.map this#lift_Parsetree_class_description x0)])
       | Parsetree.Psig_class_type x0 ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_class_type",
               [this#list
                  (List.map this#lift_Parsetree_class_type_declaration x0)])
       | Parsetree.Psig_attribute x0 ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_attribute", [this#lift_Parsetree_attribute x0])
       | Parsetree.Psig_extension (x0,x1) ->
           this#constr "Ast_404.Parsetree.signature_item_desc"
             ("Psig_extension",
               [this#lift_Parsetree_extension x0;
               this#lift_Parsetree_attributes x1]) : Parsetree.signature_item_desc
                                                       -> 'res)
    method lift_Parsetree_class_type_declaration :
      Parsetree.class_type_declaration -> 'res=
      (fun x  ->
         this#lift_Parsetree_class_infos this#lift_Parsetree_class_type x :
      Parsetree.class_type_declaration -> 'res)
    method lift_Parsetree_class_description :
      Parsetree.class_description -> 'res=
      (fun x  ->
         this#lift_Parsetree_class_infos this#lift_Parsetree_class_type x :
      Parsetree.class_description -> 'res)
    method lift_Parsetree_class_type : Parsetree.class_type -> 'res=
      (fun
         { Parsetree.pcty_desc = pcty_desc; Parsetree.pcty_loc = pcty_loc;
           Parsetree.pcty_attributes = pcty_attributes }
          ->
         this#record "Ast_404.Parsetree.class_type"
           [("pcty_desc", (this#lift_Parsetree_class_type_desc pcty_desc));
           ("pcty_loc", (this#lift_Location_t pcty_loc));
           ("pcty_attributes",
             (this#lift_Parsetree_attributes pcty_attributes))] : Parsetree.class_type
                                                                    ->
                                                                    'res)
    method lift_Parsetree_class_type_desc :
      Parsetree.class_type_desc -> 'res=
      (function
       | Parsetree.Pcty_constr (x0,x1) ->
           this#constr "Ast_404.Parsetree.class_type_desc"
             ("Pcty_constr",
               [this#lift_Asttypes_loc this#lift_Longident_t x0;
               this#list (List.map this#lift_Parsetree_core_type x1)])
       | Parsetree.Pcty_signature x0 ->
           this#constr "Ast_404.Parsetree.class_type_desc"
             ("Pcty_signature", [this#lift_Parsetree_class_signature x0])
       | Parsetree.Pcty_arrow (x0,x1,x2) ->
           this#constr "Ast_404.Parsetree.class_type_desc"
             ("Pcty_arrow",
               [this#lift_Asttypes_arg_label x0;
               this#lift_Parsetree_core_type x1;
               this#lift_Parsetree_class_type x2])
       | Parsetree.Pcty_extension x0 ->
           this#constr "Ast_404.Parsetree.class_type_desc"
             ("Pcty_extension", [this#lift_Parsetree_extension x0]) :
      Parsetree.class_type_desc -> 'res)
    method lift_Parsetree_class_signature :
      Parsetree.class_signature -> 'res=
      (fun
         { Parsetree.pcsig_self = pcsig_self;
           Parsetree.pcsig_fields = pcsig_fields }
          ->
         this#record "Ast_404.Parsetree.class_signature"
           [("pcsig_self", (this#lift_Parsetree_core_type pcsig_self));
           ("pcsig_fields",
             (this#list
                (List.map this#lift_Parsetree_class_type_field pcsig_fields)))] :
      Parsetree.class_signature -> 'res)
    method lift_Parsetree_class_type_field :
      Parsetree.class_type_field -> 'res=
      (fun
         { Parsetree.pctf_desc = pctf_desc; Parsetree.pctf_loc = pctf_loc;
           Parsetree.pctf_attributes = pctf_attributes }
          ->
         this#record "Ast_404.Parsetree.class_type_field"
           [("pctf_desc",
              (this#lift_Parsetree_class_type_field_desc pctf_desc));
           ("pctf_loc", (this#lift_Location_t pctf_loc));
           ("pctf_attributes",
             (this#lift_Parsetree_attributes pctf_attributes))] : Parsetree.class_type_field
                                                                    ->
                                                                    'res)
    method lift_Parsetree_class_type_field_desc :
      Parsetree.class_type_field_desc -> 'res=
      (function
       | Parsetree.Pctf_inherit x0 ->
           this#constr "Ast_404.Parsetree.class_type_field_desc"
             ("Pctf_inherit", [this#lift_Parsetree_class_type x0])
       | Parsetree.Pctf_val x0 ->
           this#constr "Ast_404.Parsetree.class_type_field_desc"
             ("Pctf_val",
               [(let (x0,x1,x2,x3) = x0  in
                 this#tuple
                   [this#string x0;
                   this#lift_Asttypes_mutable_flag x1;
                   this#lift_Asttypes_virtual_flag x2;
                   this#lift_Parsetree_core_type x3])])
       | Parsetree.Pctf_method x0 ->
           this#constr "Ast_404.Parsetree.class_type_field_desc"
             ("Pctf_method",
               [(let (x0,x1,x2,x3) = x0  in
                 this#tuple
                   [this#string x0;
                   this#lift_Asttypes_private_flag x1;
                   this#lift_Asttypes_virtual_flag x2;
                   this#lift_Parsetree_core_type x3])])
       | Parsetree.Pctf_constraint x0 ->
           this#constr "Ast_404.Parsetree.class_type_field_desc"
             ("Pctf_constraint",
               [(let (x0,x1) = x0  in
                 this#tuple
                   [this#lift_Parsetree_core_type x0;
                   this#lift_Parsetree_core_type x1])])
       | Parsetree.Pctf_attribute x0 ->
           this#constr "Ast_404.Parsetree.class_type_field_desc"
             ("Pctf_attribute", [this#lift_Parsetree_attribute x0])
       | Parsetree.Pctf_extension x0 ->
           this#constr "Ast_404.Parsetree.class_type_field_desc"
             ("Pctf_extension", [this#lift_Parsetree_extension x0]) :
      Parsetree.class_type_field_desc -> 'res)
    method lift_Parsetree_extension : Parsetree.extension -> 'res=
      (fun x  ->
         let (x0,x1) = x  in
         this#tuple
           [this#lift_Asttypes_loc this#string x0;
           this#lift_Parsetree_payload x1] : Parsetree.extension -> 'res)
    method lift_Parsetree_class_infos :
      'f0 . ('f0 -> 'res) -> 'f0 Parsetree.class_infos -> 'res= fun (type f0)
      ->
      (fun f0  ->
         fun
           { Parsetree.pci_virt = pci_virt;
             Parsetree.pci_params = pci_params;
             Parsetree.pci_name = pci_name; Parsetree.pci_expr = pci_expr;
             Parsetree.pci_loc = pci_loc;
             Parsetree.pci_attributes = pci_attributes }
            ->
           this#record "Ast_404.Parsetree.class_infos"
             [("pci_virt", (this#lift_Asttypes_virtual_flag pci_virt));
             ("pci_params",
               (this#list
                  (List.map
                     (fun x  ->
                        let (x0,x1) = x  in
                        this#tuple
                          [this#lift_Parsetree_core_type x0;
                          this#lift_Asttypes_variance x1]) pci_params)));
             ("pci_name", (this#lift_Asttypes_loc this#string pci_name));
             ("pci_expr", (f0 pci_expr));
             ("pci_loc", (this#lift_Location_t pci_loc));
             ("pci_attributes",
               (this#lift_Parsetree_attributes pci_attributes))] : (f0 ->
                                                                    'res) ->
                                                                    f0
                                                                    Parsetree.class_infos
                                                                    ->
                                                                    'res)
    method lift_Asttypes_virtual_flag : Asttypes.virtual_flag -> 'res=
      (function
       | Asttypes.Virtual  ->
           this#constr "Ast_404.Asttypes.virtual_flag" ("Virtual", [])
       | Asttypes.Concrete  ->
           this#constr "Ast_404.Asttypes.virtual_flag" ("Concrete", []) : Asttypes.virtual_flag
                                                                    ->
                                                                    'res)
    method lift_Parsetree_include_description :
      Parsetree.include_description -> 'res=
      (fun x  ->
         this#lift_Parsetree_include_infos this#lift_Parsetree_module_type x :
      Parsetree.include_description -> 'res)
    method lift_Parsetree_include_infos :
      'f0 . ('f0 -> 'res) -> 'f0 Parsetree.include_infos -> 'res= fun (type
      f0) ->
      (fun f0  ->
         fun
           { Parsetree.pincl_mod = pincl_mod;
             Parsetree.pincl_loc = pincl_loc;
             Parsetree.pincl_attributes = pincl_attributes }
            ->
           this#record "Ast_404.Parsetree.include_infos"
             [("pincl_mod", (f0 pincl_mod));
             ("pincl_loc", (this#lift_Location_t pincl_loc));
             ("pincl_attributes",
               (this#lift_Parsetree_attributes pincl_attributes))] :
      (f0 -> 'res) -> f0 Parsetree.include_infos -> 'res)
    method lift_Parsetree_open_description :
      Parsetree.open_description -> 'res=
      (fun
         { Parsetree.popen_lid = popen_lid;
           Parsetree.popen_override = popen_override;
           Parsetree.popen_loc = popen_loc;
           Parsetree.popen_attributes = popen_attributes }
          ->
         this#record "Ast_404.Parsetree.open_description"
           [("popen_lid",
              (this#lift_Asttypes_loc this#lift_Longident_t popen_lid));
           ("popen_override",
             (this#lift_Asttypes_override_flag popen_override));
           ("popen_loc", (this#lift_Location_t popen_loc));
           ("popen_attributes",
             (this#lift_Parsetree_attributes popen_attributes))] : Parsetree.open_description
                                                                    ->
                                                                    'res)
    method lift_Asttypes_override_flag : Asttypes.override_flag -> 'res=
      (function
       | Asttypes.Override  ->
           this#constr "Ast_404.Asttypes.override_flag" ("Override", [])
       | Asttypes.Fresh  ->
           this#constr "Ast_404.Asttypes.override_flag" ("Fresh", []) : Asttypes.override_flag
                                                                  ->
                                                                  'res)
    method lift_Parsetree_module_type_declaration :
      Parsetree.module_type_declaration -> 'res=
      (fun
         { Parsetree.pmtd_name = pmtd_name; Parsetree.pmtd_type = pmtd_type;
           Parsetree.pmtd_attributes = pmtd_attributes;
           Parsetree.pmtd_loc = pmtd_loc }
          ->
         this#record "Ast_404.Parsetree.module_type_declaration"
           [("pmtd_name", (this#lift_Asttypes_loc this#string pmtd_name));
           ("pmtd_type",
             (this#lift_option this#lift_Parsetree_module_type pmtd_type));
           ("pmtd_attributes",
             (this#lift_Parsetree_attributes pmtd_attributes));
           ("pmtd_loc", (this#lift_Location_t pmtd_loc))] : Parsetree.module_type_declaration
                                                              -> 'res)
    method lift_Parsetree_module_declaration :
      Parsetree.module_declaration -> 'res=
      (fun
         { Parsetree.pmd_name = pmd_name; Parsetree.pmd_type = pmd_type;
           Parsetree.pmd_attributes = pmd_attributes;
           Parsetree.pmd_loc = pmd_loc }
          ->
         this#record "Ast_404.Parsetree.module_declaration"
           [("pmd_name", (this#lift_Asttypes_loc this#string pmd_name));
           ("pmd_type", (this#lift_Parsetree_module_type pmd_type));
           ("pmd_attributes",
             (this#lift_Parsetree_attributes pmd_attributes));
           ("pmd_loc", (this#lift_Location_t pmd_loc))] : Parsetree.module_declaration
                                                            -> 'res)
    method lift_Parsetree_type_extension : Parsetree.type_extension -> 'res=
      (fun
         { Parsetree.ptyext_path = ptyext_path;
           Parsetree.ptyext_params = ptyext_params;
           Parsetree.ptyext_constructors = ptyext_constructors;
           Parsetree.ptyext_private = ptyext_private;
           Parsetree.ptyext_attributes = ptyext_attributes }
          ->
         this#record "Ast_404.Parsetree.type_extension"
           [("ptyext_path",
              (this#lift_Asttypes_loc this#lift_Longident_t ptyext_path));
           ("ptyext_params",
             (this#list
                (List.map
                   (fun x  ->
                      let (x0,x1) = x  in
                      this#tuple
                        [this#lift_Parsetree_core_type x0;
                        this#lift_Asttypes_variance x1]) ptyext_params)));
           ("ptyext_constructors",
             (this#list
                (List.map this#lift_Parsetree_extension_constructor
                   ptyext_constructors)));
           ("ptyext_private",
             (this#lift_Asttypes_private_flag ptyext_private));
           ("ptyext_attributes",
             (this#lift_Parsetree_attributes ptyext_attributes))] : Parsetree.type_extension
                                                                    ->
                                                                    'res)
    method lift_Parsetree_extension_constructor :
      Parsetree.extension_constructor -> 'res=
      (fun
         { Parsetree.pext_name = pext_name; Parsetree.pext_kind = pext_kind;
           Parsetree.pext_loc = pext_loc;
           Parsetree.pext_attributes = pext_attributes }
          ->
         this#record "Ast_404.Parsetree.extension_constructor"
           [("pext_name", (this#lift_Asttypes_loc this#string pext_name));
           ("pext_kind",
             (this#lift_Parsetree_extension_constructor_kind pext_kind));
           ("pext_loc", (this#lift_Location_t pext_loc));
           ("pext_attributes",
             (this#lift_Parsetree_attributes pext_attributes))] : Parsetree.extension_constructor
                                                                    ->
                                                                    'res)
    method lift_Parsetree_extension_constructor_kind :
      Parsetree.extension_constructor_kind -> 'res=
      (function
       | Parsetree.Pext_decl (x0,x1) ->
           this#constr "Ast_404.Parsetree.extension_constructor_kind"
             ("Pext_decl",
               [this#lift_Parsetree_constructor_arguments x0;
               this#lift_option this#lift_Parsetree_core_type x1])
       | Parsetree.Pext_rebind x0 ->
           this#constr "Ast_404.Parsetree.extension_constructor_kind"
             ("Pext_rebind",
               [this#lift_Asttypes_loc this#lift_Longident_t x0]) : Parsetree.extension_constructor_kind
                                                                    ->
                                                                    'res)
    method lift_Parsetree_type_declaration :
      Parsetree.type_declaration -> 'res=
      (fun
         { Parsetree.ptype_name = ptype_name;
           Parsetree.ptype_params = ptype_params;
           Parsetree.ptype_cstrs = ptype_cstrs;
           Parsetree.ptype_kind = ptype_kind;
           Parsetree.ptype_private = ptype_private;
           Parsetree.ptype_manifest = ptype_manifest;
           Parsetree.ptype_attributes = ptype_attributes;
           Parsetree.ptype_loc = ptype_loc }
          ->
         this#record "Ast_404.Parsetree.type_declaration"
           [("ptype_name", (this#lift_Asttypes_loc this#string ptype_name));
           ("ptype_params",
             (this#list
                (List.map
                   (fun x  ->
                      let (x0,x1) = x  in
                      this#tuple
                        [this#lift_Parsetree_core_type x0;
                        this#lift_Asttypes_variance x1]) ptype_params)));
           ("ptype_cstrs",
             (this#list
                (List.map
                   (fun x  ->
                      let (x0,x1,x2) = x  in
                      this#tuple
                        [this#lift_Parsetree_core_type x0;
                        this#lift_Parsetree_core_type x1;
                        this#lift_Location_t x2]) ptype_cstrs)));
           ("ptype_kind", (this#lift_Parsetree_type_kind ptype_kind));
           ("ptype_private", (this#lift_Asttypes_private_flag ptype_private));
           ("ptype_manifest",
             (this#lift_option this#lift_Parsetree_core_type ptype_manifest));
           ("ptype_attributes",
             (this#lift_Parsetree_attributes ptype_attributes));
           ("ptype_loc", (this#lift_Location_t ptype_loc))] : Parsetree.type_declaration
                                                                -> 'res)
    method lift_Asttypes_private_flag : Asttypes.private_flag -> 'res=
      (function
       | Asttypes.Private  ->
           this#constr "Ast_404.Asttypes.private_flag" ("Private", [])
       | Asttypes.Public  ->
           this#constr "Ast_404.Asttypes.private_flag" ("Public", []) : Asttypes.private_flag
                                                                  ->
                                                                  'res)
    method lift_Parsetree_type_kind : Parsetree.type_kind -> 'res=
      (function
       | Parsetree.Ptype_abstract  ->
           this#constr "Ast_404.Parsetree.type_kind" ("Ptype_abstract", [])
       | Parsetree.Ptype_variant x0 ->
           this#constr "Ast_404.Parsetree.type_kind"
             ("Ptype_variant",
               [this#list
                  (List.map this#lift_Parsetree_constructor_declaration x0)])
       | Parsetree.Ptype_record x0 ->
           this#constr "Ast_404.Parsetree.type_kind"
             ("Ptype_record",
               [this#list (List.map this#lift_Parsetree_label_declaration x0)])
       | Parsetree.Ptype_open  ->
           this#constr "Ast_404.Parsetree.type_kind" ("Ptype_open", []) : Parsetree.type_kind
                                                                    ->
                                                                    'res)
    method lift_Parsetree_constructor_declaration :
      Parsetree.constructor_declaration -> 'res=
      (fun
         { Parsetree.pcd_name = pcd_name; Parsetree.pcd_args = pcd_args;
           Parsetree.pcd_res = pcd_res; Parsetree.pcd_loc = pcd_loc;
           Parsetree.pcd_attributes = pcd_attributes }
          ->
         this#record "Ast_404.Parsetree.constructor_declaration"
           [("pcd_name", (this#lift_Asttypes_loc this#string pcd_name));
           ("pcd_args", (this#lift_Parsetree_constructor_arguments pcd_args));
           ("pcd_res",
             (this#lift_option this#lift_Parsetree_core_type pcd_res));
           ("pcd_loc", (this#lift_Location_t pcd_loc));
           ("pcd_attributes",
             (this#lift_Parsetree_attributes pcd_attributes))] : Parsetree.constructor_declaration
                                                                   ->
                                                                   'res)
    method lift_Parsetree_constructor_arguments :
      Parsetree.constructor_arguments -> 'res=
      (function
       | Parsetree.Pcstr_tuple x0 ->
           this#constr "Ast_404.Parsetree.constructor_arguments"
             ("Pcstr_tuple",
               [this#list (List.map this#lift_Parsetree_core_type x0)])
       | Parsetree.Pcstr_record x0 ->
           this#constr "Ast_404.Parsetree.constructor_arguments"
             ("Pcstr_record",
               [this#list (List.map this#lift_Parsetree_label_declaration x0)]) :
      Parsetree.constructor_arguments -> 'res)
    method lift_Parsetree_label_declaration :
      Parsetree.label_declaration -> 'res=
      (fun
         { Parsetree.pld_name = pld_name;
           Parsetree.pld_mutable = pld_mutable;
           Parsetree.pld_type = pld_type; Parsetree.pld_loc = pld_loc;
           Parsetree.pld_attributes = pld_attributes }
          ->
         this#record "Ast_404.Parsetree.label_declaration"
           [("pld_name", (this#lift_Asttypes_loc this#string pld_name));
           ("pld_mutable", (this#lift_Asttypes_mutable_flag pld_mutable));
           ("pld_type", (this#lift_Parsetree_core_type pld_type));
           ("pld_loc", (this#lift_Location_t pld_loc));
           ("pld_attributes",
             (this#lift_Parsetree_attributes pld_attributes))] : Parsetree.label_declaration
                                                                   ->
                                                                   'res)
    method lift_Asttypes_mutable_flag : Asttypes.mutable_flag -> 'res=
      (function
       | Asttypes.Immutable  ->
           this#constr "Ast_404.Asttypes.mutable_flag" ("Immutable", [])
       | Asttypes.Mutable  ->
           this#constr "Ast_404.Asttypes.mutable_flag" ("Mutable", []) : Asttypes.mutable_flag
                                                                   ->
                                                                   'res)
    method lift_Asttypes_variance : Asttypes.variance -> 'res=
      (function
       | Asttypes.Covariant  ->
           this#constr "Ast_404.Asttypes.variance" ("Covariant", [])
       | Asttypes.Contravariant  ->
           this#constr "Ast_404.Asttypes.variance" ("Contravariant", [])
       | Asttypes.Invariant  ->
           this#constr "Ast_404.Asttypes.variance" ("Invariant", []) : Asttypes.variance
                                                                 -> 'res)
    method lift_Parsetree_value_description :
      Parsetree.value_description -> 'res=
      (fun
         { Parsetree.pval_name = pval_name; Parsetree.pval_type = pval_type;
           Parsetree.pval_prim = pval_prim;
           Parsetree.pval_attributes = pval_attributes;
           Parsetree.pval_loc = pval_loc }
          ->
         this#record "Ast_404.Parsetree.value_description"
           [("pval_name", (this#lift_Asttypes_loc this#string pval_name));
           ("pval_type", (this#lift_Parsetree_core_type pval_type));
           ("pval_prim", (this#list (List.map this#string pval_prim)));
           ("pval_attributes",
             (this#lift_Parsetree_attributes pval_attributes));
           ("pval_loc", (this#lift_Location_t pval_loc))] : Parsetree.value_description
                                                              -> 'res)
    method lift_Asttypes_arg_label : Asttypes.arg_label -> 'res=
      (function
       | Asttypes.Nolabel  ->
           this#constr "Ast_404.Asttypes.arg_label" ("Nolabel", [])
       | Asttypes.Labelled x0 ->
           this#constr "Ast_404.Asttypes.arg_label" ("Labelled", [this#string x0])
       | Asttypes.Optional x0 ->
           this#constr "Ast_404.Asttypes.arg_label" ("Optional", [this#string x0]) :
      Asttypes.arg_label -> 'res)
    method lift_Asttypes_closed_flag : Asttypes.closed_flag -> 'res=
      (function
       | Asttypes.Closed  ->
           this#constr "Ast_404.Asttypes.closed_flag" ("Closed", [])
       | Asttypes.Open  -> this#constr "Ast_404.Asttypes.closed_flag" ("Open", []) :
      Asttypes.closed_flag -> 'res)
    method lift_Asttypes_label : Asttypes.label -> 'res=
      (this#string : Asttypes.label -> 'res)
    method lift_Asttypes_rec_flag : Asttypes.rec_flag -> 'res=
      (function
       | Asttypes.Nonrecursive  ->
           this#constr "Ast_404.Asttypes.rec_flag" ("Nonrecursive", [])
       | Asttypes.Recursive  ->
           this#constr "Ast_404.Asttypes.rec_flag" ("Recursive", []) : Asttypes.rec_flag
                                                                 -> 'res)
    method lift_Parsetree_constant : Parsetree.constant -> 'res=
      (function
       | Parsetree.Pconst_integer (x0,x1) ->
           this#constr "Ast_404.Parsetree.constant"
             ("Pconst_integer",
               [this#string x0; this#lift_option this#char x1])
       | Parsetree.Pconst_char x0 ->
           this#constr "Ast_404.Parsetree.constant" ("Pconst_char", [this#char x0])
       | Parsetree.Pconst_string (x0,x1) ->
           this#constr "Ast_404.Parsetree.constant"
             ("Pconst_string",
               [this#string x0; this#lift_option this#string x1])
       | Parsetree.Pconst_float (x0,x1) ->
           this#constr "Ast_404.Parsetree.constant"
             ("Pconst_float",
               [this#string x0; this#lift_option this#char x1]) : Parsetree.constant
                                                                    ->
                                                                    'res)
    method lift_option : 'f0 . ('f0 -> 'res) -> 'f0 option -> 'res= fun (type
      f0) ->
      (fun f0  ->
         function
         | None  -> this#constr "option" ("None", [])
         | Some x0 -> this#constr "option" ("Some", [f0 x0]) : (f0 -> 'res)
                                                                 ->
                                                                 f0 option ->
                                                                   'res)
    method lift_Longident_t : Longident.t -> 'res=
      (function
       | Longident.Lident x0 ->
           this#constr "Ast_404.Longident.t" ("Lident", [this#string x0])
       | Longident.Ldot (x0,x1) ->
           this#constr "Ast_404.Longident.t"
             ("Ldot", [this#lift_Longident_t x0; this#string x1])
       | Longident.Lapply (x0,x1) ->
           this#constr "Ast_404.Longident.t"
             ("Lapply", [this#lift_Longident_t x0; this#lift_Longident_t x1]) :
      Longident.t -> 'res)
    method lift_Asttypes_loc :
      'f0 . ('f0 -> 'res) -> 'f0 Asttypes.loc -> 'res= fun (type f0) ->
      (fun f0  ->
         fun { Asttypes.txt = txt; Asttypes.loc = loc }  ->
           this#record "Ast_404.Asttypes.loc"
             [("txt", (f0 txt)); ("loc", (this#lift_Location_t loc))] :
      (f0 -> 'res) -> f0 Asttypes.loc -> 'res)
    method lift_Location_t : Location.t -> 'res=
      (fun
         { Location.loc_start = loc_start; Location.loc_end = loc_end;
           Location.loc_ghost = loc_ghost }
          ->
         this#record "Ast_404.Location.t"
           [("loc_start", (this#lift_Lexing_position loc_start));
           ("loc_end", (this#lift_Lexing_position loc_end));
           ("loc_ghost", (this#lift_bool loc_ghost))] : Location.t -> 'res)
    method lift_bool : bool -> 'res=
      (function
       | false  -> this#constr "bool" ("false", [])
       | true  -> this#constr "bool" ("true", []) : bool -> 'res)
    method lift_Lexing_position : Lexing.position -> 'res=
      (fun
         { Lexing.pos_fname = pos_fname; Lexing.pos_lnum = pos_lnum;
           Lexing.pos_bol = pos_bol; Lexing.pos_cnum = pos_cnum }
          ->
         this#record "Ast_404.Lexing.position"
           [("pos_fname", (this#string pos_fname));
           ("pos_lnum", (this#int pos_lnum));
           ("pos_bol", (this#int pos_bol));
           ("pos_cnum", (this#int pos_cnum))] : Lexing.position -> 'res)
  end
