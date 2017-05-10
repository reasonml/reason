open Migrate_parsetree
open Ast_404

open Parsetree
open Asttypes
open Ast_mapper
open Ast_helper

let rec process_arguments func =
  match func with
  | Pexp_fun (arg_label, eo, {ppat_desc = Ppat_constraint (_, ct)}, exp) ->
    Typ.arrow arg_label ct (process_arguments exp.pexp_desc)
  (* An unlabeled () unit argument *)
  | Pexp_fun (arg_label, eo, {ppat_desc = Ppat_construct (({txt = Longident.Lident "()"; _}), None)}, exp) ->
    let unit_type = (Typ.constr (Location.mkloc (Longident.Lident "unit") (Location.symbol_gloc ())) []) in
    Typ.arrow arg_label unit_type (process_arguments exp.pexp_desc)
  | Pexp_constraint (_, ct) ->
    let (const_type, c) =
      match ct.ptyp_desc with
      | Ptyp_constr (loc, ct) -> (loc.txt, ct)
      | _ -> print_string "2. Give a proper error that a constraint is expected"; assert false
    in
    (Typ.constr (Location.mkloc const_type (Location.symbol_gloc ())) c)
  | _ -> print_string "Function exports must be fully constrained"; assert false

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

let structure_of_value_binding value_binding = 
  match value_binding with

  (* let%export a = 2 -- unannotated export of a constant *)
  | { pvb_pat = {ppat_desc = Ppat_var loc; _}; pvb_expr = {pexp_desc = Pexp_constant const; _}; pvb_attributes; pvb_loc } ->
    let value = Val.mk ~attrs:pvb_attributes loc (type_of_constant const)
    in Sig.mk (Psig_value value)

  (* let%export a:int = 2 -- an annotated value export *)
  | { pvb_pat = {ppat_desc = Ppat_var loc; _}; pvb_expr = { pexp_desc = Pexp_constraint (_, const)}; pvb_attributes; pvb_loc} -> (
      let (const_type, c) =
        match const.ptyp_desc with
        | Ptyp_constr (loc, ct) -> (loc.txt, ct)
        | _ -> print_string "1. Give a proper error that a constraint is expected"; assert false
    in
    let value = Val.mk
      ~attrs:pvb_attributes
      loc
      (Typ.constr (Location.mkloc const_type (Location.symbol_gloc ())) c)
    in Sig.mk (Psig_value value)
    )

  (* let%export a (b:int) (c:char) : string = "boe" -- function export *)
  | { pvb_pat = {ppat_desc = Ppat_var loc; _}; pvb_expr = { pexp_desc = Pexp_fun _ as arrow }; pvb_attributes; pvb_loc} ->
    Sig.mk (Psig_value (Val.mk ~attrs:pvb_attributes loc (process_arguments arrow)))
  | _ -> print_string "Unsupported usage of export"; assert false

let rec functor_to_type expr = (
  match expr with
  | Pmod_functor (name, type_, {pmod_desc}) ->
    Mty.functor_ name type_ (functor_to_type pmod_desc)
  | Pmod_constraint (expr, type_) -> type_
  | _ -> print_string "Unsupported functor"; assert false
)

let rec fun_type label {ppat_desc} {pexp_desc; pexp_loc; pexp_attributes} = (
  let pattern_type = match ppat_desc with
  | Ppat_constraint (_, type_) -> type_
  | _ -> print_string "All arguments must be constrained"; assert false
  in
  let result_type = match pexp_desc with
  | Pexp_fun (label, _, pattern, expr) -> {
    ptyp_desc=fun_type label pattern expr;
    ptyp_loc=pexp_loc;
    ptyp_attributes=pexp_attributes
  }
  | Pexp_constraint (_, type_) -> type_
  | _ -> print_string "Return value must be constrained"; assert false
  in
  Ptyp_arrow (label, pattern_type, result_type)
)

let class_desc_to_type desc = (
  match desc with
  | Pcf_inherit (override_flag, class_expr, maybe_rename) ->
    assert false
        (* inherit CE
           inherit CE as x
           inherit! CE
           inherit! CE as x
         *)
  | Pcf_val (name, mutable_flag, class_field_kind) -> (
    let (is_virtual, type_) = match class_field_kind with
    | Cfk_virtual t -> (Virtual, t)
    | Cfk_concrete (override, {pexp_desc}) -> (match pexp_desc with
      | Pexp_constraint (expr, type_) -> (Concrete, type_)
      | Pexp_constant const -> (Concrete, type_of_constant const)
      | _ -> assert false
    )
    in
    Pctf_val (name.txt, mutable_flag, is_virtual, type_)
  )
        (* val x = E
           val virtual x: T
         *)
  | Pcf_method (name, private_flag, class_field_kind) -> 
    let (is_virtual, type_) = match class_field_kind with
      | Cfk_virtual t -> (Virtual, t)
      | Cfk_concrete (override, {pexp_desc}) -> match pexp_desc with
        | Pexp_poly (expr, Some type_) -> (Concrete, type_)
        | Pexp_poly ({pexp_desc=Pexp_fun (label, _, pattern, expr); pexp_loc; pexp_attributes}, None) ->
          (Concrete, {
            ptyp_desc=fun_type label pattern expr;
            ptyp_loc=pexp_loc;
            ptyp_attributes=pexp_attributes
          })
        | _ -> print_string "Methods must be typed"; assert false
    in
    Pctf_method (name.txt, private_flag, is_virtual, type_)
  | Pcf_initializer expr -> assert false
        (* initializer E *)
  | Pcf_constraint (t1, t2) -> Pctf_constraint (t1, t2)
        (* constraint T1 = T2 *)
  | Pcf_attribute attr -> Pctf_attribute attr
        (* [@@@id] *)
  | Pcf_extension ext -> Pctf_extension ext
        (* [%%id] *)

(*
  | Pctf_inherit of class_type
        (* inherit CT *)
  | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
        (* val x: T *)
  | Pctf_method  of (string * private_flag * virtual_flag * core_type)
        (* method x: T
           Note: T can be a Ptyp_poly.
         *)
  | Pctf_constraint  of (core_type * core_type)
        (* constraint T1 = T2 *)
  | Pctf_attribute of attribute
        (* [@@@id] *)
  | Pctf_extension of extension
  *)
)

let class_structure_to_class_signature {pcstr_self={ppat_desc; ppat_loc; ppat_attributes}; pcstr_fields} = (
  {
    pcsig_self={ptyp_desc=Ptyp_any; ptyp_loc=ppat_loc; ptyp_attributes=ppat_attributes}; (* TODO figure out when this is ever not any *)
    pcsig_fields=List.map (fun {pcf_desc; pcf_loc; pcf_attributes} ->
      {
        pctf_desc=class_desc_to_type pcf_desc;
        pctf_loc=pcf_loc;
        pctf_attributes=pcf_attributes
      }
    ) pcstr_fields
  }
)

let class_expr_to_class_type {pcl_desc; pcl_loc; pcl_attributes}: class_type = (
  let desc = match pcl_desc with
  | Pcl_constr (name, types) -> Pcty_constr (name, types)
  | Pcl_structure body -> Pcty_signature (class_structure_to_class_signature body)

  | Pcl_fun (name, default_value, argument, expr) -> assert false
  | Pcl_apply (expr, args) -> assert false
  | Pcl_let (isrec, values, expr) -> assert false 
  | Pcl_constraint (expr, type_) -> assert false
  | Pcl_extension ext -> assert false
  in {pcty_desc=desc; pcty_loc=pcl_loc; pcty_attributes=pcl_attributes}
)
(*
    | Pcl_constr of Longident.t loc * core_type list
          (* c
            ['a1, ..., 'an] c *)
    | Pcl_structure of class_structure
          (* object ... end *)
    | Pcl_fun of label * expression option * pattern * class_expr
          (* fun P -> CE                          (lab = "", None)
            fun ~l:P -> CE                       (lab = "l", None)
            fun ?l:P -> CE                       (lab = "?l", None)
            fun ?l:(P = E0) -> CE                (lab = "?l", Some E0)
          *)
    | Pcl_apply of class_expr * (label * expression) list
          (* CE ~l1:E1 ... ~ln:En
            li can be empty (non labeled argument) or start with '?'
            (optional argument).
            Invariant: n > 0
          *)
    | Pcl_let of rec_flag * value_binding list * class_expr
          (* let P1 = E1 and ... and Pn = EN in CE      (flag = Nonrecursive)
            let rec P1 = E1 and ... and Pn = EN in CE  (flag = Recursive)
          *)
    | Pcl_constraint of class_expr * class_type
          (* (CE : CT) *)
    | Pcl_extension of extension
          (* [%id] *)
*)

let class_declaration_to_class_description {pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes}: class_description =
  let description = class_expr_to_class_type pci_expr in
  {pci_virt; pci_params; pci_name; pci_expr=description; pci_loc; pci_attributes}
  (*let rec acc2 class_declaration current_signatures =
    match class_declaration with
    (*| {pcl_desc = }*)
    
    | Pcty_constr of Longident.t loc * core_type list
          (* c
              ['a1, ..., 'an] c *)
    | Pcty_signature body
          (* object ... end *)
    | Pcty_arrow of arg_label * core_type * class_type
          (* T -> CT       Simple
              ~l:T -> CT    Labelled l
              ?l:T -> CT    Optional l
            *)
    | Pcty_extension of extension
          (* [%id] *)
    in acc2 cd []*)

let rec extract str : signature_item list * structure_item list = (
  let rec acc str current_signatures current_structures = (
      match str with
      | [] -> (current_signatures, current_structures)
      | next :: tail ->
        let (new_signatures, new_structures) = (
        match next with
          (* -- modifies structure -- *)

          (* an unconstrained module with a body *)
          | {pstr_desc = Pstr_module {pmb_name; pmb_expr = {pmod_desc = Pmod_structure str; pmod_attributes; pmod_loc}}} ->
            let (s, s2) = process str [] [] in
            let module_expr = Mod.structure s2 ~loc:pmod_loc ~attrs:pmod_attributes in
            let module_ = Str.module_ (Mb.mk pmb_name module_expr) in
            let sigModule_ = Sig.module_ (Md.mk pmb_name (Mty.signature s)) in
            (* let _ = {ex with pmb_expr = a} in *)
            ([sigModule_], [module_])

          (* a set of recursive modules *)
          | {pstr_desc = Pstr_recmodule e} ->
            print_string "Recursive modules not yet supported"; assert false
            (* acc tail (current_signatures @ [Sig.mk (Psig_module e)]) *)

          | doesnt_modify_structure ->
            let new_signatures = (
              match doesnt_modify_structure with
                (* -- multiple exports -- *)
                | {pstr_desc = Pstr_value (rec_flag, value_bindings)} -> List.map structure_of_value_binding value_bindings

                (* -- single export -- *)
                | has_single_signature ->
                  let new_signature = (
                    match has_single_signature with 
                      | {pstr_desc = Pstr_type (r, t)} ->
                        Sig.mk (Psig_type (r, t))
                      | {pstr_desc = Pstr_typext te} ->
                        Sig.mk (Psig_typext te)
                      | {pstr_desc = Pstr_exception e} ->
                        Sig.mk (Psig_exception e)
                      | {pstr_desc = Pstr_module {pmb_name; pmb_loc; pmb_expr = {pmod_desc = Pmod_constraint (_, {pmty_desc = Pmty_ident loc}) }}} ->
                        (* a constrained module `module X: Type = ...` *)
                        Sig.module_ (Md.mk pmb_name (Mty.ident { txt = loc.txt; loc = pmb_loc }))
                      | {pstr_desc = Pstr_modtype mt} ->
                        (* a module type *)
                        Sig.mk (Psig_modtype mt)
                      | {pstr_desc = Pstr_class_type e} ->
                        Sig.mk (Psig_class_type e)
                      | {pstr_desc = Pstr_attribute a} ->
                        Sig.mk (Psig_attribute a)
                      | {pstr_desc = Pstr_extension (e, a)} ->
                        Sig.mk (Psig_extension (e, a))
                      | {pstr_desc = Pstr_module {pmb_name; pmb_expr = {pmod_desc = Pmod_functor (arg, type_, {pmod_desc; _}) }}} ->
                        (* a functor! module W (X: Y) : Z = ... *)
                        Sig.module_ (Md.mk pmb_name (Mty.functor_ arg type_ (functor_to_type pmod_desc)))


                      | {pstr_desc = Pstr_class c} ->
                        Sig.mk (Psig_class (List.map class_declaration_to_class_description c))
                        (* a class *)

                      (* Invalid uses of export *)
                      | {pstr_desc = Pstr_module _} ->
                        (* a functor! *)
                        print_string "Invalid module export - must be constrained or literal"; assert false

                      | _ -> print_string "Unhandled structure type"; assert false
                  )
                  in
                  [new_signature]
            ) in
            (new_signatures, [next]) (* or str? *)
        ) in
        acc tail (current_signatures @ new_signatures) (current_structures @ new_structures)





    )
    in acc str [] []
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
