open Ast_403

(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

(** Class-based customizable mapper *)

open Parsetree
open Asttypes
open Ast_helper

let map_fst f (x, y) = (f x, y)
let map_snd f (x, y) = (x, f y)
let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
let map_opt f = function None -> None | Some x -> Some (f x)

let map_loc sub {loc; txt} = {loc = sub # location loc; txt}

module T = struct
  (* Type expressions for the core language *)

  let row_field sub = function
    | Rtag (l, attrs, b, tl) ->
        Rtag (l, sub # attributes attrs, b, List.map (sub # typ) tl)
    | Rinherit t -> Rinherit (sub # typ t)

  let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
    let open Typ in
    let loc = sub # location loc in
    let attrs = sub # attributes attrs in
    match desc with
    | Ptyp_any -> any ~loc ~attrs ()
    | Ptyp_var s -> var ~loc ~attrs s
    | Ptyp_arrow (lab, t1, t2) ->
        arrow ~loc ~attrs lab (sub # typ t1) (sub # typ t2)
    | Ptyp_tuple tyl -> tuple ~loc ~attrs (List.map (sub # typ) tyl)
    | Ptyp_constr (lid, tl) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub # typ) tl)
    | Ptyp_object (l, o) ->
        let f (s, a, t) = (s, sub # attributes a, sub # typ t) in
        object_ ~loc ~attrs (List.map f l) o
    | Ptyp_class (lid, tl) ->
        class_ ~loc ~attrs (map_loc sub lid) (List.map (sub # typ) tl)
    | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub # typ t) s
    | Ptyp_variant (rl, b, ll) ->
        variant ~loc ~attrs (List.map (row_field sub) rl) b ll
    | Ptyp_poly (sl, t) -> poly ~loc ~attrs sl (sub # typ t)
    | Ptyp_package (lid, l) ->
        package ~loc ~attrs (map_loc sub lid)
          (List.map (map_tuple (map_loc sub) (sub # typ)) l)
    | Ptyp_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_type_declaration sub
      {ptype_name; ptype_params; ptype_cstrs;
       ptype_kind;
       ptype_private;
       ptype_manifest;
       ptype_attributes;
       ptype_loc} =
    Type.mk (map_loc sub ptype_name)
      ~params:(List.map (map_fst (sub # typ)) ptype_params)
      ~priv:ptype_private
      ~cstrs:(List.map (map_tuple3 (sub # typ) (sub # typ) (sub # location))
                ptype_cstrs)
      ~kind:(sub # type_kind ptype_kind)
      ?manifest:(map_opt (sub # typ) ptype_manifest)
      ~loc:(sub # location ptype_loc)
      ~attrs:(sub # attributes ptype_attributes)

  let map_type_kind sub = function
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant l ->
        Ptype_variant (List.map (sub # constructor_declaration) l)
    | Ptype_record l -> Ptype_record (List.map (sub # label_declaration) l)
    | Ptype_open -> Ptype_open

  let map_type_extension sub
      {ptyext_path; ptyext_params;
       ptyext_constructors;
       ptyext_private;
       ptyext_attributes} =
    Te.mk
      (map_loc sub ptyext_path)
      (List.map (sub # extension_constructor) ptyext_constructors)
      ~params:(List.map (map_fst (sub # typ)) ptyext_params)
      ~priv:ptyext_private
      ~attrs:(sub # attributes ptyext_attributes)

  let map_extension_constructor_kind sub = function
      Pext_decl(ctl, cto) ->
        Pext_decl(sub # constructor_arguments ctl, map_opt (sub # typ) cto)
    | Pext_rebind li ->
        Pext_rebind (map_loc sub li)

  let map_extension_constructor sub
      {pext_name;
       pext_kind;
       pext_loc;
       pext_attributes} =
    Te.constructor
      (map_loc sub pext_name)
      (map_extension_constructor_kind sub pext_kind)
      ~loc:(sub # location pext_loc)
      ~attrs:(sub # attributes pext_attributes)


end

module CT = struct
  (* Type expressions for the class language *)

  let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
    let open Cty in
    let loc = sub # location loc in
    match desc with
    | Pcty_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub # typ) tys)
    | Pcty_signature x -> signature ~loc ~attrs (sub # class_signature x)
    | Pcty_arrow (lab, t, ct) ->
        arrow ~loc ~attrs lab (sub # typ t) (sub # class_type ct)
    | Pcty_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
    =
    let open Ctf in
    let loc = sub # location loc in
    match desc with
    | Pctf_inherit ct -> inherit_ ~loc ~attrs (sub # class_type ct)
    | Pctf_val (s, m, v, t) -> val_ ~loc ~attrs s m v (sub # typ t)
    | Pctf_method (s, p, v, t) -> method_ ~loc ~attrs s p v (sub # typ t)
    | Pctf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub # typ t1) (sub # typ t2)
    | Pctf_attribute x -> attribute ~loc (sub # attribute x)
    | Pctf_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_signature sub {pcsig_self; pcsig_fields} =
    Csig.mk
      (sub # typ pcsig_self)
      (List.map (sub # class_type_field) pcsig_fields)
end

module MT = struct
  (* Type expressions for the module language *)

  let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    let open Mty in
    let loc = sub # location loc in
    let attrs = sub # attributes attrs in
    match desc with
    | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
    | Pmty_alias s -> alias ~loc ~attrs (map_loc sub s)
    | Pmty_signature sg -> signature ~loc ~attrs (sub # signature sg)
    | Pmty_functor (s, mt1, mt2) ->
        functor_ ~loc ~attrs (map_loc sub s)
          (map_opt (sub # module_type) mt1)
          (sub # module_type mt2)
    | Pmty_with (mt, l) ->
        with_ ~loc ~attrs (sub # module_type mt)
          (List.map (sub # with_constraint) l)
    | Pmty_typeof me -> typeof_ ~loc ~attrs (sub # module_expr me)
    | Pmty_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_with_constraint sub = function
    | Pwith_type (lid, d) ->
        Pwith_type (map_loc sub lid, sub # type_declaration d)
    | Pwith_module (lid, lid2) ->
        Pwith_module (map_loc sub lid, map_loc sub lid2)
    | Pwith_typesubst d -> Pwith_typesubst (sub # type_declaration d)
    | Pwith_modsubst (s, lid) ->
        Pwith_modsubst (map_loc sub s, map_loc sub lid)

  let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
    let open Sig in
    let loc = sub # location loc in
    match desc with
    | Psig_value vd -> value ~loc (sub # value_description vd)
    | Psig_type (rf, l) -> type_ ~loc rf (List.map (sub # type_declaration) l)
    | Psig_typext te -> type_extension ~loc (sub # type_extension te)
    | Psig_exception ed -> exception_ ~loc (sub # extension_constructor ed)
    | Psig_module x -> module_ ~loc (sub # module_declaration x)
    | Psig_recmodule l ->
        rec_module ~loc (List.map (sub # module_declaration) l)
    | Psig_modtype x -> modtype ~loc (sub # module_type_declaration x)
    | Psig_open od -> open_ ~loc (sub # open_description od)
    | Psig_include x -> include_ ~loc (sub # include_description x)
    | Psig_class l -> class_ ~loc (List.map (sub # class_description) l)
    | Psig_class_type l ->
        class_type ~loc (List.map (sub # class_type_declaration) l)
    | Psig_extension (x, attrs) ->
        extension ~loc (sub # extension x) ~attrs:(sub # attributes attrs)
    | Psig_attribute x -> attribute ~loc (sub # attribute x)
end


module M = struct
  (* Value expressions for the module language *)

  let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    let open Mod in
    let loc = sub # location loc in
    let attrs = sub # attributes attrs in
    match desc with
    | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pmod_structure str -> structure ~loc ~attrs (sub # structure str)
    | Pmod_functor (arg, arg_ty, body) ->
        functor_ ~loc ~attrs (map_loc sub arg)
          (map_opt (sub # module_type) arg_ty)
          (sub # module_expr body)
    | Pmod_apply (m1, m2) ->
        apply ~loc ~attrs (sub # module_expr m1) (sub # module_expr m2)
    | Pmod_constraint (m, mty) ->
        constraint_ ~loc ~attrs (sub # module_expr m) (sub # module_type mty)
    | Pmod_unpack e -> unpack ~loc ~attrs (sub # expr e)
    | Pmod_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    let open Str in
    let loc = sub # location loc in
    match desc with
    | Pstr_eval (x, attrs) ->
        eval ~loc ~attrs:(sub # attributes attrs) (sub # expr x)
    | Pstr_value (r, vbs) -> value ~loc r (List.map (sub # value_binding) vbs)
    | Pstr_primitive vd -> primitive ~loc (sub # value_description vd)
    | Pstr_type (rf, l) -> type_ ~loc rf (List.map (sub # type_declaration) l)
    | Pstr_typext te -> type_extension ~loc (sub # type_extension te)
    | Pstr_exception ed -> exception_ ~loc (sub # extension_constructor ed)
    | Pstr_module x -> module_ ~loc (sub # module_binding x)
    | Pstr_recmodule l -> rec_module ~loc (List.map (sub # module_binding) l)
    | Pstr_modtype x -> modtype ~loc (sub # module_type_declaration x)
    | Pstr_open od -> open_ ~loc (sub # open_description od)
    | Pstr_class l -> class_ ~loc (List.map (sub # class_declaration) l)
    | Pstr_class_type l ->
        class_type ~loc (List.map (sub # class_type_declaration) l)
    | Pstr_include x -> include_ ~loc (sub # include_declaration x)
    | Pstr_extension (x, attrs) ->
        extension ~loc (sub # extension x) ~attrs:(sub # attributes attrs)
    | Pstr_attribute x -> attribute ~loc (sub # attribute x)
end

module E = struct
  (* Value expressions for the core language *)

  let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
    let open Exp in
    let loc = sub # location loc in
    let attrs = sub # attributes attrs in
    match desc with
    | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pexp_constant x -> constant ~loc ~attrs x
    | Pexp_let (r, vbs, e) ->
        let_ ~loc ~attrs r (List.map (sub # value_binding) vbs) (sub # expr e)
    | Pexp_fun (lab, def, p, e) ->
        fun_ ~loc ~attrs lab (map_opt (sub # expr) def) (sub # pat p)
          (sub # expr e)
    | Pexp_function pel -> function_ ~loc ~attrs (sub # cases pel)
    | Pexp_apply (e, l) ->
        apply ~loc ~attrs (sub # expr e) (List.map (map_snd (sub # expr)) l)
    | Pexp_match (e, pel) -> match_ ~loc ~attrs (sub # expr e) (sub # cases pel)
    | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub # expr e) (sub # cases pel)
    | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub # expr) el)
    | Pexp_construct (lid, arg) ->
        construct ~loc ~attrs (map_loc sub lid) (map_opt (sub # expr) arg)
    | Pexp_variant (lab, eo) ->
        variant ~loc ~attrs lab (map_opt (sub # expr) eo)
    | Pexp_record (l, eo) ->
        record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub # expr)) l)
          (map_opt (sub # expr) eo)
    | Pexp_field (e, lid) -> field ~loc ~attrs (sub # expr e) (map_loc sub lid)
    | Pexp_setfield (e1, lid, e2) ->
        setfield ~loc ~attrs (sub # expr e1) (map_loc sub lid) (sub # expr e2)
    | Pexp_array el -> array ~loc ~attrs (List.map (sub # expr) el)
    | Pexp_ifthenelse (e1, e2, e3) ->
        ifthenelse ~loc ~attrs (sub # expr e1) (sub # expr e2)
          (map_opt (sub # expr) e3)
    | Pexp_sequence (e1, e2) ->
        sequence ~loc ~attrs (sub # expr e1) (sub # expr e2)
    | Pexp_while (e1, e2) -> while_ ~loc ~attrs (sub # expr e1) (sub # expr e2)
    | Pexp_for (p, e1, e2, d, e3) ->
        for_ ~loc ~attrs (sub # pat p) (sub # expr e1) (sub # expr e2) d
          (sub # expr e3)
    | Pexp_coerce (e, t1, t2) ->
        coerce ~loc ~attrs (sub # expr e) (map_opt (sub # typ) t1)
          (sub # typ t2)
    | Pexp_constraint (e, t) ->
        constraint_ ~loc ~attrs (sub # expr e) (sub # typ t)
    | Pexp_send (e, s) -> send ~loc ~attrs (sub # expr e) s
    | Pexp_new lid -> new_ ~loc ~attrs (map_loc sub lid)
    | Pexp_setinstvar (s, e) ->
        setinstvar ~loc ~attrs (map_loc sub s) (sub # expr e)
    | Pexp_override sel ->
        override ~loc ~attrs
          (List.map (map_tuple (map_loc sub) (sub # expr)) sel)
    | Pexp_letmodule (s, me, e) ->
        letmodule ~loc ~attrs (map_loc sub s) (sub # module_expr me)
          (sub # expr e)
    | Pexp_assert e -> assert_ ~loc ~attrs (sub # expr e)
    | Pexp_lazy e -> lazy_ ~loc ~attrs (sub # expr e)
    | Pexp_poly (e, t) ->
        poly ~loc ~attrs (sub # expr e) (map_opt (sub # typ) t)
    | Pexp_object cls -> object_ ~loc ~attrs (sub # class_structure cls)
    | Pexp_newtype (s, e) -> newtype ~loc ~attrs s (sub # expr e)
    | Pexp_pack me -> pack ~loc ~attrs (sub # module_expr me)
    | Pexp_open (ovf, lid, e) ->
        open_ ~loc ~attrs ovf (map_loc sub lid) (sub # expr e)
    | Pexp_extension x -> extension ~loc ~attrs (sub # extension x)
    | Pexp_unreachable -> unreachable ~loc ~attrs ()
end

module P = struct
  (* Patterns *)

  let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
    let open Pat in
    let loc = sub # location loc in
    let attrs = sub # attributes attrs in
    match desc with
    | Ppat_any -> any ~loc ~attrs ()
    | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
    | Ppat_alias (p, s) -> alias ~loc ~attrs (sub # pat p) (map_loc sub s)
    | Ppat_constant c -> constant ~loc ~attrs c
    | Ppat_interval (c1, c2) -> interval ~loc ~attrs c1 c2
    | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub # pat) pl)
    | Ppat_construct (l, p) ->
        construct ~loc ~attrs (map_loc sub l) (map_opt (sub # pat) p)
    | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub # pat) p)
    | Ppat_record (lpl, cf) ->
        record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub # pat)) lpl)
          cf
    | Ppat_array pl -> array ~loc ~attrs (List.map (sub # pat) pl)
    | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub # pat p1) (sub # pat p2)
    | Ppat_constraint (p, t) ->
        constraint_ ~loc ~attrs (sub # pat p) (sub # typ t)
    | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
    | Ppat_lazy p -> lazy_ ~loc ~attrs (sub # pat p)
    | Ppat_unpack s -> unpack ~loc ~attrs (map_loc sub s)
    | Ppat_exception p -> exception_ ~loc ~attrs (sub # pat p)
    | Ppat_extension x -> extension ~loc ~attrs (sub # extension x)
end

module CE = struct
  (* Value expressions for the class language *)

  let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
    let open Cl in
    let loc = sub # location loc in
    match desc with
    | Pcl_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub # typ) tys)
    | Pcl_structure s ->
        structure ~loc ~attrs (sub # class_structure s)
    | Pcl_fun (lab, e, p, ce) ->
        fun_ ~loc ~attrs lab
          (map_opt (sub # expr) e)
          (sub # pat p)
          (sub # class_expr ce)
    | Pcl_apply (ce, l) ->
        apply ~loc ~attrs (sub # class_expr ce)
          (List.map (map_snd (sub # expr)) l)
    | Pcl_let (r, vbs, ce) ->
        let_ ~loc ~attrs r (List.map (sub # value_binding) vbs)
          (sub # class_expr ce)
    | Pcl_constraint (ce, ct) ->
        constraint_ ~loc ~attrs (sub # class_expr ce) (sub # class_type ct)
    | Pcl_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_kind sub = function
    | Cfk_concrete (o, e) -> Cfk_concrete (o, sub # expr e)
    | Cfk_virtual t -> Cfk_virtual (sub # typ t)

  let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
    let open Cf in
    let loc = sub # location loc in
    match desc with
    | Pcf_inherit (o, ce, s) -> inherit_ ~loc ~attrs o (sub # class_expr ce) s
    | Pcf_val (s, m, k) -> val_ ~loc ~attrs (map_loc sub s) m (map_kind sub k)
    | Pcf_method (s, p, k) ->
        method_ ~loc ~attrs (map_loc sub s) p (map_kind sub k)
    | Pcf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub # typ t1) (sub # typ t2)
    | Pcf_initializer e -> initializer_ ~loc ~attrs (sub # expr e)
    | Pcf_attribute x -> attribute ~loc (sub # attribute x)
    | Pcf_extension x -> extension ~loc ~attrs (sub # extension x)

  let map_structure sub {pcstr_self; pcstr_fields} =
    {
      pcstr_self = sub # pat pcstr_self;
      pcstr_fields = List.map (sub # class_field) pcstr_fields;
    }

  let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                         pci_loc; pci_attributes} =
    Ci.mk
      ~virt:pci_virt
      ~params:(List.map (map_fst (sub # typ)) pl)
      (map_loc sub pci_name)
      (f pci_expr)
      ~loc:(sub # location pci_loc)
      ~attrs:(sub # attributes pci_attributes)
end

(* Now, a generic AST mapper class, to be extended to cover all kinds
   and cases of the OCaml grammar.  The default behavior of the mapper
   is the identity. *)

class mapper =
  object(this)
    method structure l = List.map (this # structure_item) l
    method structure_item si = M.map_structure_item this si
    method module_expr = M.map this

    method signature l = List.map (this # signature_item) l
    method signature_item si = MT.map_signature_item this si
    method module_type = MT.map this
    method with_constraint c = MT.map_with_constraint this c

    method class_declaration = CE.class_infos this (this # class_expr)
    method class_expr = CE.map this
    method class_field = CE.map_field this
    method class_structure = CE.map_structure this

    method class_type = CT.map this
    method class_type_field = CT.map_field this
    method class_signature = CT.map_signature this

    method class_type_declaration = CE.class_infos this (this # class_type)
    method class_description = CE.class_infos this (this # class_type)

    method type_declaration = T.map_type_declaration this
    method type_kind = T.map_type_kind this
    method typ = T.map this

    method type_extension = T.map_type_extension this
    method extension_constructor = T.map_extension_constructor this

    method value_description {pval_name; pval_type; pval_prim; pval_loc;
                              pval_attributes} =
      Val.mk
        (map_loc this pval_name)
        (this # typ pval_type)
        ~attrs:(this # attributes pval_attributes)
        ~loc:(this # location pval_loc)
        ~prim:pval_prim

    method pat = P.map this
    method expr = E.map this

    method module_declaration {pmd_name; pmd_type; pmd_attributes; pmd_loc} =
      Md.mk
        (map_loc this pmd_name)
        (this # module_type pmd_type)
        ~attrs:(this # attributes pmd_attributes)
        ~loc:(this # location pmd_loc)

    method module_type_declaration {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} =
      Mtd.mk
        (map_loc this pmtd_name)
        ?typ:(map_opt (this # module_type) pmtd_type)
        ~attrs:(this # attributes pmtd_attributes)
        ~loc:(this # location pmtd_loc)

    method module_binding {pmb_name; pmb_expr; pmb_attributes; pmb_loc} =
      Mb.mk (map_loc this pmb_name) (this # module_expr pmb_expr)
        ~attrs:(this # attributes pmb_attributes)
        ~loc:(this # location pmb_loc)

    method value_binding {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} =
      Vb.mk
        (this # pat pvb_pat)
        (this # expr pvb_expr)
        ~attrs:(this # attributes pvb_attributes)
        ~loc:(this # location pvb_loc)

    method constructor_arguments = function
      | Pcstr_tuple (tys) -> Pcstr_tuple (List.map (this # typ) tys)
      | Pcstr_record (ls) -> Pcstr_record (List.map (this # label_declaration) ls)

    method constructor_declaration {pcd_name; pcd_args; pcd_res; pcd_loc;
                                    pcd_attributes} =
      Type.constructor
        (map_loc this pcd_name)
        ~args:(this # constructor_arguments pcd_args)
        ?res:(map_opt (this # typ) pcd_res)
        ~loc:(this # location pcd_loc)
        ~attrs:(this # attributes pcd_attributes)

    method label_declaration {pld_name; pld_type; pld_loc; pld_mutable;
                              pld_attributes} =
      Type.field
        (map_loc this pld_name)
        (this # typ pld_type)
        ~mut:pld_mutable
        ~loc:(this # location pld_loc)
        ~attrs:(this # attributes pld_attributes)


    method cases l = List.map (this # case) l
    method case {pc_lhs; pc_guard; pc_rhs} =
      {
        pc_lhs = this # pat pc_lhs;
        pc_guard = map_opt (this # expr) pc_guard;
        pc_rhs = this # expr pc_rhs;
      }

    method open_description
        {popen_lid; popen_override; popen_attributes; popen_loc} =
      Opn.mk (map_loc this popen_lid)
        ~override:popen_override
        ~loc:(this # location popen_loc)
        ~attrs:(this # attributes popen_attributes)

    method include_description
        {pincl_mod; pincl_attributes; pincl_loc} =
      Incl.mk (this # module_type pincl_mod)
        ~loc:(this # location pincl_loc)
        ~attrs:(this # attributes pincl_attributes)

    method include_declaration
        {pincl_mod; pincl_attributes; pincl_loc} =
      Incl.mk (this # module_expr pincl_mod)
        ~loc:(this # location pincl_loc)
        ~attrs:(this # attributes pincl_attributes)

    method location l = l

    method extension (s, e) = (map_loc this s, this # payload e)
    method attribute (s, e) = (map_loc this s, this # payload e)
    method attributes l = List.map (this # attribute) l
    method payload = function
      | PStr x -> PStr (this # structure x)
      | PTyp x -> PTyp (this # typ x)
      | PPat (x, g) -> PPat (this # pat x, map_opt (this # expr) g)
      | PSig x -> PSig (this # signature x)
  end


let to_mapper this =
  let open Ast_mapper in
  {
    attribute = (fun _ -> this # attribute);
    attributes = (fun _ -> this # attributes);
    case = (fun _ -> this # case);
    cases = (fun _ -> this # cases);
    class_declaration = (fun _ -> this # class_declaration);
    class_description = (fun _ -> this # class_description);
    class_expr = (fun _ -> this # class_expr);
    class_field = (fun _ -> this # class_field);
    class_signature = (fun _ -> this # class_signature);
    class_structure = (fun _ -> this # class_structure);
    class_type = (fun _ -> this # class_type);
    class_type_declaration = (fun _ -> this # class_type_declaration);
    class_type_field = (fun _ -> this # class_type_field);
    constructor_declaration = (fun _ -> this # constructor_declaration);
    expr = (fun _ -> this # expr);
    extension = (fun _ -> this # extension);
    extension_constructor = (fun _ -> this # extension_constructor);
    include_declaration = (fun _ -> this # include_declaration);
    include_description = (fun _ -> this # include_description);
    label_declaration = (fun _ -> this # label_declaration);
    location = (fun _ -> this # location);
    module_binding = (fun _ -> this # module_binding);
    module_declaration = (fun _ -> this # module_declaration);
    module_expr = (fun _ -> this # module_expr);
    module_type = (fun _ -> this # module_type);
    module_type_declaration = (fun _ -> this # module_type_declaration);
    open_description = (fun _ -> this # open_description);
    pat = (fun _ -> this # pat);
    payload = (fun _ -> this # payload);
    signature = (fun _ -> this # signature);
    signature_item = (fun _ -> this # signature_item);
    structure = (fun _ -> this # structure);
    structure_item = (fun _ -> this # structure_item);
    typ = (fun _ -> this # typ);
    type_declaration = (fun _ -> this # type_declaration);
    type_extension = (fun _ -> this # type_extension);
    type_kind = (fun _ -> this # type_kind);
    value_binding = (fun _ -> this # value_binding);
    value_description = (fun _ -> this # value_description);
    with_constraint = (fun _ -> this # with_constraint);
  }
