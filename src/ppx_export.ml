open Migrate_parsetree
open Ast_404

open Parsetree
open Asttypes
open Ast_mapper
open Ast_helper

let fail loc txt = raise (Location.Error (Location.error ~loc txt))

let rec process_arguments func loc =
  match func with
  | Pexp_fun (arg_label, eo, {ppat_desc = Ppat_constraint (_, ct); ppat_loc}, exp) ->
    Typ.arrow arg_label ct (process_arguments exp.pexp_desc exp.pexp_loc)
  (* An unlabeled () unit argument *)
  | Pexp_fun (arg_label, eo, {ppat_desc = Ppat_construct (({txt = Longident.Lident "()"; _}), None)}, exp) ->
    let unit_type = (Typ.constr (Location.mkloc (Longident.Lident "unit") (Location.symbol_gloc ())) []) in
    Typ.arrow arg_label unit_type (process_arguments exp.pexp_desc exp.pexp_loc)
  | Pexp_fun (arg_label, eo, {ppat_loc}, exp) ->
    fail ppat_loc "All arguments must have type annotations"
  | Pexp_constraint (_, ct) ->
    let (const_type, c) =
      match ct.ptyp_desc with
      | Ptyp_constr (loc, ct) -> (loc.txt, ct)
      | _ -> fail ct.ptyp_loc "Function return value must be annotated"
    in
    (Typ.constr (Location.mkloc const_type (Location.symbol_gloc ())) c)
  | _ -> 
    fail loc "Function return value must be annotated"

let loc_of_constant const =
  let const_type =
    match const with
    | Pconst_integer _ -> "int"
    | Pconst_char _ -> "char"
    | Pconst_string _ -> "string"
    | Pconst_float _ -> "float"
  in
  (Location.mkloc (Longident.Lident const_type) (Location.symbol_gloc ()))

let type_of_constant const = (Typ.constr (loc_of_constant const) [])

let structure_of_value_binding {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} = 
  match pvb_pat.ppat_desc with
    | Ppat_var loc -> (
      match pvb_expr.pexp_desc with
      (* let%export a = 2 -- unannotated export of a constant *)
      | Pexp_constant const ->
        let value = Val.mk ~attrs:pvb_attributes loc (type_of_constant const)
        in Sig.mk (Psig_value value)

      (* let%export a:int = 2 -- an annotated value export *)
      | Pexp_constraint (_, const) -> (
          let (const_type, c) =
            match const.ptyp_desc with
            | Ptyp_constr (loc, ct) -> (loc.txt, ct)
            (* TODO(jaredly): why do we only accept one form of type? *)
            | _ -> fail pvb_loc "Invalid constraint"
        in
        let value = Val.mk
          ~attrs:pvb_attributes
          loc
          (Typ.constr (Location.mkloc const_type (Location.symbol_gloc ())) c)
        in Sig.mk (Psig_value value)
      )

      (* let%export a (b:int) (c:char) : string = "boe" -- function export *)
      | Pexp_fun _ as arrow ->
        Sig.mk (Psig_value (Val.mk ~attrs:pvb_attributes loc (process_arguments arrow loc.loc)))

      | _ -> fail loc.loc "Non-constant exports must be annotated"
    )
    (* What would it mean to export complex patterns? *)
    | _ -> fail pvb_loc "Let export only supports simple patterns"


let rec functor_to_type expr loc = (
  match expr with
  | Pmod_functor (name, type_, {pmod_desc; pmod_loc}) ->
    Mty.functor_ name type_ (functor_to_type pmod_desc pmod_loc)
  | Pmod_constraint (expr, type_) -> type_
  | _ -> fail loc "Exported functors must be annotated"
)

let rec fun_type label {ppat_desc; ppat_loc} {pexp_desc; pexp_loc; pexp_attributes} = (
  let pattern_type = match ppat_desc with
  | Ppat_constraint (_, type_) -> type_
  | _ -> fail ppat_loc "All arguments must be annotated"
  in
  let result_type = match pexp_desc with
  | Pexp_fun (label, _, pattern, expr) -> {
    ptyp_desc=fun_type label pattern expr;
    ptyp_loc=pexp_loc;
    ptyp_attributes=pexp_attributes
  }
  | Pexp_constraint (_, type_) -> type_
  | _ -> fail pexp_loc "Return value must be constrained"
  in
  Ptyp_arrow (label, pattern_type, result_type)
)

let fold_optionals items = List.fold_right (fun x y -> match x with | None -> y | Some x -> x::y) items []

let class_desc_to_type desc = (
  match desc with
  | Pcf_val (name, mutable_flag, class_field_kind) -> (
    let (is_virtual, type_) = match class_field_kind with
    | Cfk_virtual t -> (Virtual, t)
    | Cfk_concrete (override, {pexp_desc; pexp_loc}) -> (match pexp_desc with
      | Pexp_constraint (expr, type_) -> (Concrete, type_)
      | Pexp_constant const -> (Concrete, type_of_constant const)
      | _ -> fail pexp_loc "Exported class value must be constrained"
    )
    in
    Some (Pctf_val (name.txt, mutable_flag, is_virtual, type_))
  )

  | Pcf_method (name, private_flag, class_field_kind) -> 
    let (is_virtual, type_) = match class_field_kind with
      | Cfk_virtual t -> (Virtual, t)
      | Cfk_concrete (override, {pexp_desc; pexp_loc}) -> match pexp_desc with
        | Pexp_poly (expr, Some type_) -> (Concrete, type_)
        | Pexp_poly ({pexp_desc=Pexp_fun (label, _, pattern, expr); pexp_loc; pexp_attributes}, None) ->
          (Concrete, {
            ptyp_desc=fun_type label pattern expr;
            ptyp_loc=pexp_loc;
            ptyp_attributes=pexp_attributes
          })
        | _ -> fail pexp_loc "Exported class methods must be fully annotated"
    in
    Some (Pctf_method (name.txt, private_flag, is_virtual, type_))

  | Pcf_inherit (override_flag, {pcl_desc; pcl_loc; pcl_attributes}, maybe_rename) -> (
    let res = match pcl_desc with
    | Pcl_constraint (_, type_) -> Pctf_inherit type_

    | Pcl_apply ({pcl_desc=Pcl_constr (name, types)}, _)
    | Pcl_constr (name, types) -> Pctf_inherit ({
      pcty_desc=Pcty_constr (name, types);
      pcty_loc=pcl_loc;
      pcty_attributes=pcl_attributes
    })
    | _ -> fail pcl_loc "Inheritance must be type annotated"
    in Some res
  )

  | Pcf_constraint (t1, t2) -> Some (Pctf_constraint (t1, t2))
  | Pcf_attribute attr -> Some (Pctf_attribute attr)
  | Pcf_extension ext -> Some (Pctf_extension ext)

  | Pcf_initializer expr -> None
)

let class_structure_to_class_signature {pcstr_self={ppat_desc; ppat_loc; ppat_attributes}; pcstr_fields} = (
  {
    pcsig_self={ptyp_desc=Ptyp_any; ptyp_loc=ppat_loc; ptyp_attributes=ppat_attributes}; (* TODO figure out when this is ever not any *)
    pcsig_fields=List.map (fun {pcf_desc; pcf_loc; pcf_attributes} ->
      match (class_desc_to_type pcf_desc) with
      | None -> None
      | Some desc ->
      Some {
        pctf_desc=desc;
        pctf_loc=pcf_loc;
        pctf_attributes=pcf_attributes
      }
    ) pcstr_fields |> fold_optionals
  }
)

let rec class_fun_type label {ppat_desc; ppat_loc} result = (
  let argtype = match ppat_desc with
  | Ppat_constraint (_, type_) -> type_
  | _ -> fail ppat_loc "All arguments of exported class must be constrained"
  in
  let result_type = class_expr_to_class_type result
  in
  Pcty_arrow (label, argtype, result_type)
)

and class_expr_to_class_type {pcl_desc; pcl_loc; pcl_attributes}: class_type = (
  let desc = match pcl_desc with
  | Pcl_constr (name, types) -> (Pcty_constr (name, types))
  | Pcl_structure body -> (Pcty_signature (class_structure_to_class_signature body))
  | Pcl_fun (name, _, argument, expr) -> (class_fun_type name argument expr)
  | Pcl_constraint (_, {pcty_desc}) -> pcty_desc
  | Pcl_extension ext -> Pcty_extension ext
  | Pcl_let (_, _, expr) -> 
    let {pcty_desc} = class_expr_to_class_type expr in pcty_desc

  | Pcl_apply _ -> fail pcl_loc "Class application expressions must be type annotated"
  in 
  {pcty_desc=desc; pcty_loc=pcl_loc; pcty_attributes=pcl_attributes}
)

let class_declaration_to_class_description {pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes}: class_description =
  let description = class_expr_to_class_type pci_expr in
  {pci_virt; pci_params; pci_name; pci_expr=description; pci_loc; pci_attributes}

let get_class_descriptions declarations =
  (List.map class_declaration_to_class_description declarations)

let rec extract structures : signature_item list * structure_item list = (
  let rec inner structures current_signatures current_structures = (
    match structures with
    | [] -> (current_signatures, current_structures)
    | next :: tail ->
      let (new_signatures, new_structures) = (
      match next with
        (* -- modifies structure -- *)

        (* an unconstrained module with a body *)
        | {pstr_desc = Pstr_module {pmb_name; pmb_expr = {pmod_desc = Pmod_structure inner_structures; pmod_attributes; pmod_loc}}} ->
          let (child_signatures, child_structures) = process inner_structures [] [] in
          let module_expr = Mod.structure child_structures ~loc:pmod_loc ~attrs:pmod_attributes in
          let module_ = Str.module_ (Mb.mk pmb_name module_expr) in
          let sigModule_ = Sig.module_ (Md.mk pmb_name (Mty.signature child_signatures)) in
          (* let _ = {ex with pmb_expr = a} in *)
          ([sigModule_], [module_])

        | doesnt_modify_structure ->
          let new_signatures = (
            match doesnt_modify_structure with
              (* -- multiple exports -- *)
              | {pstr_desc = Pstr_value (rec_flag, value_bindings)} -> List.map structure_of_value_binding value_bindings

              (* -- single export -- *)
              | has_single_signature ->
                let new_signature = (
                  match has_single_signature with 
                    | {pstr_desc = Pstr_type (r, t)} -> Sig.mk (Psig_type (r, t))
                    | {pstr_desc = Pstr_typext te} -> Sig.mk (Psig_typext te)
                    | {pstr_desc = Pstr_exception e} -> Sig.mk (Psig_exception e)
                    | {pstr_desc = Pstr_modtype mt} -> Sig.mk (Psig_modtype mt)
                    | {pstr_desc = Pstr_class_type e} -> Sig.mk (Psig_class_type e)
                    | {pstr_desc = Pstr_attribute a} -> Sig.mk (Psig_attribute a)
                    | {pstr_desc = Pstr_extension (e, a)} -> Sig.mk (Psig_extension (e, a))
                    | {pstr_desc = Pstr_class c} -> Sig.mk (Psig_class (get_class_descriptions c))

                    (* a set of recursive modules *)
                    | {pstr_desc = Pstr_recmodule bindings; pstr_loc} ->
                      Sig.rec_module (List.map (fun {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
                        match pmb_expr.pmod_desc with
                        | Pmod_constraint (_, type_) ->
                          {
                            pmd_name=pmb_name;
                            pmd_type=type_;
                            pmd_attributes=pmb_attributes;
                            pmd_loc=pmb_loc
                          }
                        | _ -> fail pmb_expr.pmod_loc "Exported modules must have type annotations"
                      ) bindings)

                    | {pstr_desc = Pstr_module {pmb_name; pmb_loc; pmb_expr = {pmod_desc = Pmod_constraint (_, {pmty_desc = Pmty_ident loc}) }}} ->
                      (* a constrained module `module X: Type = ...` *)
                      Sig.module_ (Md.mk pmb_name (Mty.ident { txt = loc.txt; loc = pmb_loc }))
                    | {pstr_desc = Pstr_module {pmb_name; pmb_expr = {pmod_desc = Pmod_functor (arg, type_, {pmod_desc; pmod_loc}) }}} ->
                      (* a functor! module W (X: Y) : Z = ... *)
                      Sig.module_ (Md.mk pmb_name (Mty.functor_ arg type_ (functor_to_type pmod_desc pmod_loc)))

                    (* Invalid uses of export *)
                    | {pstr_desc = Pstr_module _; pstr_loc} ->
                      (* a functor! *)
                      fail pstr_loc "Invalid module export - must be constrained or literal"

                    | {pstr_loc} -> fail pstr_loc "Cannot export this item"
                )
                in
                [new_signature]
          ) in
          (new_signatures, [next])
      ) in
      inner tail (current_signatures @ new_signatures) (current_structures @ new_structures)
    )
    in inner structures [] []
)

and process_extension structure_item =
  match structure_item with
  | {pstr_desc = Pstr_extension (({txt = "export"; loc}, PStr str), _)} -> extract str
  | _ -> ([], [structure_item])
and process structure str_ sig_ =
  match structure with
  | hd :: tl ->
      let (sigl, strl) = process_extension hd in
      process tl (str_ @ strl) (sig_ @ sigl)
  | [] -> (sig_, str_)

let export =
  Reason_toolchain.To_current.copy_mapper
  {
    Ast_mapper.default_mapper with
    structure = (fun mapper structure  ->
      let (sig_, str_) = process structure [] []
      in
        if List.length sig_ > 0 then
          let processed_structure = Str.include_  {
            pincl_mod = Mod.constraint_
              (Mod.structure str_)
              (Mty.signature sig_);
              pincl_loc = (Location.symbol_rloc ());
              pincl_attributes = [];
            }
          in
          Ast_mapper.default_mapper.structure mapper [processed_structure]
        else
          Ast_mapper.default_mapper.structure mapper str_
      );
    }

  let _ = Compiler_libs.Ast_mapper.register "export"
      (fun _argv -> export)
