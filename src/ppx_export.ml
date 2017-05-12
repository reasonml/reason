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

let is_abstract_attr ({txt}, _) = txt = "abstract"
let is_my_attribute ({txt}, _) = txt = "export"

(*let is_my_attribute ({txt; loc}, payload) = match (txt, payload) with
  | ("as", PTyp _) -> true
  | ("as", _) -> fail loc "@as attributes must be types (put a colon after as)"
  | ("abstract", PStr []) -> true
  | ("abstract", _) -> fail loc "@abstract takes no arguments"
  | _ -> false*)

let is_not_my_attribute a = not (is_my_attribute a)
let as_replacement_attribute ({txt; loc}, payload) = match (txt, payload) with
  | ("as", PTyp t) -> Some t
  | ("as", _) -> fail loc "@as attributes must be types (put a colon after 'as')"
  | _ -> None

let filter_attrs = List.filter is_not_my_attribute

let disallow_my_attributes = List.filter (fun (loc, payload) ->
  if (is_my_attribute (loc, payload)) then
    fail loc.loc "Export not allowed here"
  else 
    false
  )

let rec find_maybe f l = match l with
  | [] -> None
  | x::rest -> (
    match (f x) with
    | Some r -> Some r
    | None -> find_maybe f rest
  )

let signature_of_value_binding {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} = 
  (* TODO just pass in *)
  let other_attrs = filter_attrs pvb_attributes in
  match pvb_pat.ppat_desc with
    | Ppat_var loc -> (
      match pvb_expr.pexp_desc with
      (* let%export a = 2 -- unannotated export of a constant *)
      | Pexp_constant const ->
        let value = Val.mk ~attrs:other_attrs loc (type_of_constant const)
        in Sig.mk (Psig_value value)

      (* let%export a:int = 2 -- an annotated value export *)
      | Pexp_constraint (_, const) -> (
        let value = Val.mk
          ~attrs:other_attrs
          loc
          const
        in Sig.mk (Psig_value value)
      )

      (* let%export a (b:int) (c:char) : string = "boe" -- function export *)
      | Pexp_fun _ as arrow ->
        Sig.mk (Psig_value (Val.mk ~attrs:other_attrs loc (process_arguments arrow loc.loc)))

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

let strip_attributes type_ = {
  type_ with
  ptype_attributes=List.filter is_not_my_attribute type_.ptype_attributes
}

let with_attributes type_ = (
  let {ptype_attributes} = type_ in
  let other_attributes = List.filter is_not_my_attribute ptype_attributes in
  if (List.exists is_abstract_attr ptype_attributes) then
  {
    (* fully abstract *)
    type_ with
    ptype_attributes=other_attributes;
    ptype_kind=Ptype_abstract;
    ptype_manifest=None;
    ptype_cstrs=[];
    ptype_params=[]
  }
  else
    let replacement = find_maybe as_replacement_attribute ptype_attributes in
    match replacement with
    | Some t -> {type_ with ptype_manifest=Some t;ptype_attributes=other_attributes}
    | None -> type_
)

let _strip_attributes structure =
  let {pstr_desc} = structure in
  let new_desc = match pstr_desc with
  | Pstr_eval (e, attrs) -> Pstr_eval (e, filter_attrs attrs)
  | Pstr_value (r, bindings) -> Pstr_value (r, List.map (fun b -> {b with pvb_attributes=filter_attrs b.pvb_attributes}) bindings)
  | Pstr_primitive desc -> Pstr_primitive {desc with pval_attributes=filter_attrs desc.pval_attributes}
  | Pstr_type (r, t) -> Pstr_type (r, List.map strip_attributes t)
  | Pstr_typext t -> Pstr_typext {t with ptyext_attributes=filter_attrs t.ptyext_attributes}
  | Pstr_exception e -> Pstr_exception {e with pext_attributes=filter_attrs e.pext_attributes}
  | Pstr_module m -> Pstr_module {m with pmb_attributes=filter_attrs m.pmb_attributes}
  | Pstr_recmodule m -> Pstr_recmodule (List.map (fun m -> {m with pmb_attributes=filter_attrs m.pmb_attributes}) m)
  | Pstr_modtype mt -> Pstr_modtype {mt with pmtd_attributes=filter_attrs mt.pmtd_attributes}
  | Pstr_class cls -> Pstr_class (List.map (fun cl -> {cl with pci_attributes=filter_attrs cl.pci_attributes}) cls)
  | Pstr_class_type clt -> Pstr_class_type (List.map (fun cl -> {cl with pci_attributes=filter_attrs cl.pci_attributes}) clt)
  | Pstr_open _
  | Pstr_include _
  | Pstr_attribute _
  | Pstr_extension _ -> pstr_desc
  in
  {structure with pstr_desc=new_desc}

(*let rec extract structures : signature_item list * structure_item list = (
  let rec inner structures current_signatures current_structures = (
    match structures with
    | [] -> (current_signatures, current_structures)
    | next :: tail ->
      let (new_signatures, new_structures) = (
      match next with
        (* -- modifies structure -- *)

        (* an unconstrained module with a body *)
        | {pstr_desc = Pstr_module {pmb_name; pmb_expr = {pmod_desc = Pmod_structure inner_structures; pmod_attributes; pmod_loc}}} ->
          let (child_signatures, child_structures) = process_structures inner_structures [] [] in
          let module_expr = Mod.structure child_structures ~loc:pmod_loc ~attrs:pmod_attributes in
          let module_ = Str.module_ (Mb.mk pmb_name module_expr) in
          let sigModule_ = Sig.module_ (Md.mk pmb_name (Mty.signature child_signatures)) in
          (* let _ = {ex with pmb_expr = a} in *)
          ([sigModule_], [module_])

        | doesnt_modify_structure ->
          let new_signatures = (
            match doesnt_modify_structure with
              (* -- multiple exports -- *)
              | {pstr_desc = Pstr_value (rec_flag, value_bindings)} -> List.map signature_of_value_binding value_bindings

              (* -- single export -- *)
              | has_single_signature ->
                let new_signature = (
                  match has_single_signature.pstr_desc with 
                    (*| Pstr_type (isrec, types) -> Sig.mk (Psig_type (isrec, List.map with_attributes types))
                    | Pstr_typext te -> Sig.mk (Psig_typext te)
                    | Pstr_exception e -> Sig.mk (Psig_exception e)
                    | Pstr_modtype mt -> Sig.mk (Psig_modtype mt)
                    | Pstr_class_type e -> Sig.mk (Psig_class_type e)
                    | Pstr_attribute a -> Sig.mk (Psig_attribute a)
                    | Pstr_extension (e, a) -> Sig.mk (Psig_extension (e, a))
                    | Pstr_class c -> Sig.mk (Psig_class (get_class_descriptions c))*)

                    (* a set of recursive modules *)
                    | Pstr_recmodule bindings ->
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

                    | Pstr_module {pmb_name; pmb_loc; pmb_expr = {pmod_desc = Pmod_constraint (_, {pmty_desc = Pmty_ident loc}) }} ->
                      (* a constrained module `module X: Type = ...` *)
                      Sig.module_ (Md.mk pmb_name (Mty.ident { txt = loc.txt; loc = pmb_loc }))
                    | Pstr_module {pmb_name; pmb_expr = {pmod_desc = Pmod_functor (arg, type_, {pmod_desc; pmod_loc}) }} ->
                      (* a functor! module W (X: Y) : Z = ... *)
                      Sig.module_ (Md.mk pmb_name (Mty.functor_ arg type_ (functor_to_type pmod_desc pmod_loc)))

                    (* Invalid uses of export *)
                    | Pstr_module _ ->
                      (* a functor! *)
                      fail has_single_signature.pstr_loc "Invalid module export - must be constrained or literal"

                    | _ -> fail has_single_signature.pstr_loc "Cannot export this item"
                )
                in
                [new_signature]
          ) in
          (new_signatures, [_strip_attributes next])
      ) in
      inner tail (current_signatures @ new_signatures) (current_structures @ new_structures)
    )
    in inner structures [] []
)*)

(*and process_extension structure_item =
  match structure_item with
  | {pstr_desc = Pstr_extension (({txt = "export"; loc}, PStr str), _)} -> extract str
  | _ -> ([], [structure_item])
and process_structures structure str_ sig_ =
  match structure with
  | hd :: tl ->
      let (sigl, strl) = process_extension hd in
      process_structures tl (str_ @ strl) (sig_ @ sigl)
  | [] -> (sig_, str_)*)






















(** The new stuff **)

type exportT =
  | NotExported
  | Exported
  | Abstract
  | ExportedAsType of core_type
  | ExportedAsSig of signature_item

let attribute_to_export ({txt; loc}, str) =
  if txt <> "export" then
    NotExported
  else
    match str with
    | PStr [] -> Exported (* [@export] *)
    | PSig [] -> Exported (* [@export:] *)
    (* [@export abstract] *)
    | PStr [{pstr_desc=Pstr_eval ({
        pexp_desc=Pexp_ident {txt=Longident.Lident "abstract"}
      }, _)}] -> Abstract
    | PStr _ -> fail loc "export with a value must be followed by a colon"
    | PTyp t -> ExportedAsType t (* [@export: t] *)
    | PSig [sig_] -> ExportedAsSig sig_ (* [@export: val m: t] *)
    (* TODO we could relax this if it makes sense? *)
    | PSig _ -> fail loc "can only export a single signature"
    | PPat _ -> fail loc "cannot export patterns"

let rec get_export attributes =
  match attributes with
  | [] -> (NotExported, [])
  | hd::tail ->
    let new_exp = attribute_to_export hd in
    let (final, others) = get_export tail in
    match (new_exp, final) with
    | (NotExported, _) -> (final, hd::others)
    | (_, NotExported) -> (new_exp, others)
    | _ -> let ({loc}, _) = hd in fail loc "Cannot have multiple export annotations"




let double_fold fn items =
  let rec inner items a b = (
    match items with
    | [] -> (a, b)
    | hd::tail ->
      let (a1, b1) = fn hd in
      inner tail (a @ a1) (b @ b1)
  ) in
  inner items [] []

(* TODO *)
let process_binding binding =
  let (export, attrs) = get_export binding.pvb_attributes in
  let sigs = match export with
  | Exported -> [signature_of_value_binding binding]
  | NotExported -> []
  | ExportedAsSig sig_ -> [sig_]
  | Abstract -> fail binding.pvb_loc "Cannot export value as abstract"
  | ExportedAsType t ->
    let name = match binding.pvb_pat.ppat_desc with
    | Ppat_var {txt} -> txt
    | _ -> fail binding.pvb_loc "let pattern must be a simple name when specifying the export type"
    in
    [Sig.mk (Psig_value (Val.mk ~attrs:attrs {loc=t.ptyp_loc;txt=name} t))]
  in
  (sigs, [{binding with pvb_attributes=attrs}])

let process_type type_ =
  let (export, attrs) = get_export type_.ptype_attributes in
  let sigtypes = match export with 
  | Exported -> [{type_ with ptype_attributes=attrs}]
  | NotExported -> []
  | ExportedAsSig sig_ -> (match sig_.psig_desc with
    | Psig_type (_, t) -> t
    | _ -> fail sig_.psig_loc "Type exported signature must be a type"
  )
  | Abstract -> 
    [{
      (* fully abstract *)
      type_ with
      ptype_attributes=attrs;
      ptype_kind=Ptype_abstract;
      ptype_manifest=None;
      ptype_cstrs=[];
      ptype_params=[]
    }]
  | ExportedAsType t -> [{
      type_ with
      ptype_manifest=Some t;
      ptype_attributes=attrs;
      ptype_cstrs=[];
      ptype_params=[]
    }]
  in
  (sigtypes, [{type_ with ptype_attributes=attrs}])

let process_typext t = ([], [t])

let module_sig module_ get_signatures =
  match module_.pmb_expr.pmod_desc with
  | Pmod_structure inner_structures ->
    let {pmb_name; pmb_expr={pmod_loc; pmod_attributes}} = module_ in
    let (child_signatures, child_structures) = double_fold get_signatures inner_structures in

    let module_expr = Mod.structure child_structures ~loc:pmod_loc ~attrs:pmod_attributes in
    let module_ = (Mb.mk pmb_name module_expr) in
    let sigModule_ = Sig.module_ (Md.mk pmb_name (Mty.signature child_signatures)) in
    (* let _ = {ex with pmb_expr = a} in *)
    ([sigModule_], module_)
  | _ -> fail module_.pmb_loc "Cannot determine a type for this exported module"

let process_class cl = ([], [cl])

let process_class_type clt = ([], [clt])



let process_module m get_signatures =
  let (export, attrs) = get_export m.pmb_attributes in
  let newmod = {m with pmb_attributes=attrs} in
  match export with
  | Exported -> module_sig m get_signatures
  | NotExported -> ([], newmod)
  | ExportedAsSig sig_ -> ([sig_], newmod)
  | ExportedAsType t -> fail m.pmb_loc "Module types don't work as types"
  | Abstract -> fail m.pmb_loc "Module cannot be exported as abstract"

let rec get_signatures structure =
  let (sigs, new_desc) = match structure.pstr_desc with
  (* Multiple exportables *)
  | Pstr_value (r, bindings) ->
    let (signatures, bindings) = double_fold process_binding bindings in
    (signatures, Pstr_value (r, bindings))

  | Pstr_class cls -> 
    let (signatures, classes) = double_fold process_class cls in
    (signatures, Pstr_class classes)

  | Pstr_class_type clt -> 
    let (signatures, class_types) = double_fold process_class_type clt in
    (signatures, Pstr_class_type class_types)

  | Pstr_type (r, types) ->
    let (sigtypes, types) = double_fold process_type types in
    (
      match sigtypes with 
      | [] -> ([], Pstr_type (r, types))
      | _ -> ([Sig.mk (Psig_type (r, sigtypes))], Pstr_type (r, types))
    )

  | Pstr_recmodule modules -> 
    let (signatures, modules) = double_fold (fun m ->
      let (sigs, m) = process_module m get_signatures in (sigs, [m])
     ) modules in
    (signatures, Pstr_recmodule modules)

  (* Single exportable *)
  | Pstr_module m ->
    let (sigs, m) = (process_module m get_signatures) in
    (sigs, Pstr_module m)

  | Pstr_typext t ->
    let (export, attrs) = (get_export t.ptyext_attributes) in
    let sigs = match export with
    | Exported -> [Sig.mk (Psig_typext {t with ptyext_attributes=attrs})]
    | NotExported -> []
    | ExportedAsSig sig_ -> [sig_]
    (* TODO? *)
    | Abstract -> fail t.ptyext_path.loc "Cannot export typext as abstract"
    | ExportedAsType _ -> fail t.ptyext_path.loc "Cannot export typext as type"
    in
    (sigs, Pstr_typext {t with ptyext_attributes=attrs})

  | Pstr_exception e -> 
    let (export, attrs) = (get_export e.pext_attributes) in
    let sigs = match export with
    | Exported -> [Sig.mk (Psig_exception {e with pext_attributes=attrs})]
    | NotExported -> []
    | ExportedAsSig sig_ -> [sig_]
    (* TODO? *)
    | Abstract
    | ExportedAsType _ -> fail e.pext_loc "Invalid exception export"
    in
    (sigs, Pstr_exception {e with pext_attributes=attrs})

  | Pstr_modtype mt ->
    let (export, attrs) = (get_export mt.pmtd_attributes) in
    let sigs = match export with
    | Exported -> [Sig.mk (Psig_modtype mt)]
    | NotExported -> []
    | ExportedAsSig sig_ -> [sig_]
    | Abstract -> fail mt.pmtd_loc "Module type cannot be abstract" (* TODO? *)
    | ExportedAsType _ -> fail mt.pmtd_loc "Must be a module type"
    in
    (sigs, Pstr_modtype {mt with pmtd_attributes=attrs})

  | Pstr_open _
  | Pstr_include _
  | Pstr_attribute _
  | Pstr_extension _ -> ([], structure.pstr_desc)
  | Pstr_eval (e, attrs) -> ([], Pstr_eval (e, disallow_my_attributes attrs))

  (* TODO maybe support primitives? *)
  | Pstr_primitive desc ->
    ([], Pstr_primitive {desc with pval_attributes=disallow_my_attributes desc.pval_attributes})
  in
  (sigs, [{structure with pstr_desc=new_desc}])

let export =
  Reason_toolchain.To_current.copy_mapper
  {
    Ast_mapper.default_mapper with
    structure = (fun mapper structure  ->
      let (sig_, str_) = double_fold get_signatures structure
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
