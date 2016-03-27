(***********************************************************************)
(*                                                                     *)
(*                                Reason                               *)
(*                                                                     *)
(***********************************************************************)
(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

open Format
open Outcometree

exception Ellipsis

let cautious f ppf arg =
  try f ppf arg with
    Ellipsis -> fprintf ppf "..."

let rec print_ident ppf =
  function
    Oide_ident s -> pp_print_string ppf s
  | Oide_dot (id, s) ->
      print_ident ppf id; pp_print_char ppf '.'; pp_print_string ppf s
  | Oide_apply (id1, id2) ->
      fprintf ppf "%a(%a)" print_ident id1 print_ident id2

let parenthesized_ident name =
  (List.mem name ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"])
  ||
  (match name.[0] with
      'a'..'z' | 'A'..'Z' | '\223'..'\246' | '\248'..'\255' | '_' ->
        false
    | _ -> true)

let value_ident ppf name =
  if parenthesized_ident name then
    fprintf ppf "( %s )" name
  else
    pp_print_string ppf name

(* Values *)

let valid_float_lexeme s =
  let l = String.length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match s.[i] with
    | '0' .. '9' | '-' -> loop (i+1)
    | _ -> s
  in loop 0

let float_repres f =
  match classify_float f with
    FP_nan -> "nan"
  | FP_infinite ->
      if f < 0.0 then "neg_infinity" else "infinity"
  | _ ->
      let float_val =
        let s1 = Printf.sprintf "%.12g" f in
        if f = float_of_string s1 then s1 else
        let s2 = Printf.sprintf "%.15g" f in
        if f = float_of_string s2 then s2 else
        Printf.sprintf "%.18g" f
      in valid_float_lexeme float_val

let parenthesize_if_neg ppf fmt v isneg =
  if isneg then pp_print_char ppf '(';
  fprintf ppf fmt v;
  if isneg then pp_print_char ppf ')'

let print_out_value ppf tree =
  let rec print_tree_1 ppf =
    function
    | Oval_constr (name, [param]) ->
        fprintf ppf "@[<1>%a@ %a@]" print_ident name print_constr_param param
    | Oval_constr (name, (_ :: _ as params)) ->
        fprintf ppf "@[<1>%a@ (%a)@]" print_ident name
          (print_tree_list print_tree_1 ",") params
    | Oval_variant (name, Some param) ->
        fprintf ppf "@[<2>`%s@ %a@]" name print_constr_param param
    | tree -> print_simple_tree ppf tree
  and print_constr_param ppf = function
    | Oval_int i -> parenthesize_if_neg ppf "%i" i (i < 0)
    | Oval_int32 i -> parenthesize_if_neg ppf "%lil" i (i < 0l)
    | Oval_int64 i -> parenthesize_if_neg ppf "%LiL" i (i < 0L)
    | Oval_nativeint i -> parenthesize_if_neg ppf "%nin" i (i < 0n)
    | Oval_float f -> parenthesize_if_neg ppf "%s" (float_repres f) (f < 0.0)
    | tree -> print_simple_tree ppf tree
  and print_simple_tree ppf =
    function
      Oval_int i -> fprintf ppf "%i" i
    | Oval_int32 i -> fprintf ppf "%lil" i
    | Oval_int64 i -> fprintf ppf "%LiL" i
    | Oval_nativeint i -> fprintf ppf "%nin" i
    | Oval_float f -> pp_print_string ppf (float_repres f)
    | Oval_char c -> fprintf ppf "%C" c
    | Oval_string s ->
        begin try fprintf ppf "%S" s with
          Invalid_argument "String.create" -> fprintf ppf "<huge string>"
        end
    | Oval_list tl ->
        fprintf ppf "@[<1>[%a]@]" (print_tree_list print_tree_1 ",") tl
    | Oval_array tl ->
        fprintf ppf "@[<2>[|%a|]@]" (print_tree_list print_tree_1 ",") tl
    | Oval_constr (name, []) -> print_ident ppf name
    | Oval_variant (name, None) -> fprintf ppf "`%s" name
    | Oval_stuff s -> pp_print_string ppf s
    | Oval_record fel ->
        fprintf ppf "@[<1>{%a}@]" (cautious (print_fields true)) fel
    | Oval_ellipsis -> raise Ellipsis
    | Oval_printer f -> f ppf
    | Oval_tuple tree_list ->
        fprintf ppf "@[<1>(%a)@]" (print_tree_list print_tree_1 ",") tree_list
    | tree -> fprintf ppf "@[<1>(%a)@]" (cautious print_tree_1) tree
  and print_fields first ppf =
    function
      [] -> ()
    | (name, tree) :: fields ->
        if not first then fprintf ppf ",@ ";
        fprintf ppf "@[<1>%a@ :@ %a@]" print_ident name (cautious print_tree_1)
          tree;
        print_fields false ppf fields
  and print_tree_list print_item sep ppf tree_list =
    let rec print_list first ppf =
      function
        [] -> ()
      | tree :: tree_list ->
          if not first then fprintf ppf "%s@ " sep;
          print_item ppf tree;
          print_list false ppf tree_list
    in
    cautious (print_list true) ppf tree_list
  in
  cautious print_tree_1 ppf tree

(* Types *)

let rec print_list_init pr sep ppf =
  function
    [] -> ()
  | a :: l -> sep ppf; pr ppf a; print_list_init pr sep ppf l

let rec print_list pr sep ppf =
  function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l

let pr_present =
  print_list (fun ppf s -> fprintf ppf "`%s" s) (fun ppf -> fprintf ppf "@ ")

let pr_vars =
  print_list (fun ppf s -> fprintf ppf "'%s" s) (fun ppf -> fprintf ppf "@ ")

let rec print_out_type ppf =
  function
  | Otyp_alias (ty, s) ->
      fprintf ppf "@[%a@ as '%s@]" print_out_type ty s
  | Otyp_poly (sl, ty) ->
      fprintf ppf "@[<hov 2>%a.@ %a@]"
        pr_vars sl
        print_out_type ty
  | ty ->
      print_out_type_1 ppf ty

and print_out_type_1 ppf =
  function
    Otyp_arrow (lab, ty1, ty2) ->
      pp_open_box ppf 0;
      if lab <> "" then (pp_print_string ppf lab; pp_print_char ppf ':');
      print_out_type_2 ppf ty1;
      pp_print_string ppf " =>";
      pp_print_space ppf ();
      print_out_type_1 ppf ty2;
      pp_close_box ppf ()
  | ty -> print_out_type_2 ppf ty
and print_out_type_2 ppf =
  function
    Otyp_tuple tyl ->
      fprintf ppf "@[<0>%a@]" (print_typlist print_simple_out_type ",") tyl
  | ty -> print_simple_out_type ppf ty
and print_simple_out_type ppf =
  function
    Otyp_class (ng, id, tyl) ->
      fprintf ppf "@[%a%s#%a@]" print_typargs tyl (if ng then "_" else "")
        print_ident id
  | Otyp_constr (id, tyl) ->
      pp_open_box ppf 0;
      print_ident ppf id;
      print_typargs ppf tyl;
      pp_close_box ppf ()
  | Otyp_object (fields, rest) ->
      fprintf ppf "@[<2>< %a >@]" (print_fields rest) fields
  | Otyp_stuff s -> pp_print_string ppf s
  | Otyp_var (ng, s) -> fprintf ppf "'%s%s" (if ng then "_" else "") s
  | Otyp_variant (non_gen, row_fields, closed, tags) ->
      let print_present ppf =
        function
          None | Some [] -> ()
        | Some l -> fprintf ppf "@;<1 -2>> @[<hov>%a@]" pr_present l
      in
      let print_fields ppf =
        function
          Ovar_fields fields ->
            print_list print_row_field (fun ppf -> fprintf ppf "@;<1 -2>| ")
              ppf fields
        | Ovar_name (id, tyl) ->
            fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id
      in
      fprintf ppf "%s[%s@[<hv>@[<hv>%a@]%a ]@]" (if non_gen then "_" else "")
        (if closed then if tags = None then " " else "< "
         else if tags = None then "> " else "? ")
        print_fields row_fields
        print_present tags
  | Otyp_alias _ | Otyp_poly _ | Otyp_arrow _ | Otyp_tuple _ as ty ->
      pp_open_box ppf 1;
      pp_print_char ppf '(';
      print_out_type ppf ty;
      pp_print_char ppf ')';
      pp_close_box ppf ()
  | Otyp_abstract | Otyp_open
  | Otyp_sum _ | Otyp_record _ | Otyp_manifest (_, _) -> ()
  | Otyp_module (p, n, tyl) ->
      fprintf ppf "@[<1>(module %s" p;
      let first = ref true in
      List.iter2
        (fun s t ->
          let sep = if !first then (first := false; "with") else "and" in
          fprintf ppf " %s type %s = %a" sep s print_out_type t
        )
        n tyl;
      fprintf ppf ")@]"
and print_fields rest ppf =
  function
    [] ->
      begin match rest with
        Some non_gen -> fprintf ppf "%s.." (if non_gen then "_" else "")
      | None -> ()
      end
  | [s, t] ->
      fprintf ppf "%s : %a" s print_out_type t;
      begin match rest with
        Some _ -> fprintf ppf ";@ "
      | None -> ()
      end;
      print_fields rest ppf []
  | (s, t) :: l ->
      fprintf ppf "%s : %a;@ %a" s print_out_type t (print_fields rest) l
and print_row_field ppf (l, opt_amp, tyl) =
  let pr_of ppf =
    if opt_amp then fprintf ppf " of@ &@ "
    else if tyl <> [] then fprintf ppf " of@ "
    else fprintf ppf ""
  in
  fprintf ppf "@[<hv 2>`%s%t%a@]" l pr_of (print_typlist print_out_type " &")
    tyl
and print_typlist print_elem sep ppf =
  function
    [] -> ()
  | [ty] -> print_elem ppf ty
  | ty :: tyl ->
      print_elem ppf ty;
      pp_print_string ppf sep;
      pp_print_space ppf ();
      print_typlist print_elem sep ppf tyl
and print_typargs ppf =
  function
    [] -> ()
  | [ty1] -> pp_print_space ppf (); print_simple_out_type ppf ty1
  | tyl ->
      pp_print_space ppf ();
      pp_open_box ppf 1;
      print_typlist print_out_type "" ppf tyl;
      pp_close_box ppf ()

(* Class types *)

let type_parameter ppf (ty, (co, cn)) =
  fprintf ppf "%s%s"
    (if not cn then "+" else if not co then "-" else "")
    (if ty = "_" then ty else "'"^ty)

let print_out_class_params ppf =
  function
    [] -> ()
  | tyl ->
      fprintf ppf "@[<1>[%a]@]@ "
        (print_list type_parameter (fun ppf -> fprintf ppf ", "))
        tyl

let rec print_out_class_type ppf =
  function
    Octy_constr (id, tyl) ->
      let pr_tyl ppf =
        function
          [] -> ()
        | tyl ->
            fprintf ppf "@[<1>[%a]@]@ " (print_typlist print_out_type ",") tyl
      in
      fprintf ppf "@[%a%a@]" pr_tyl tyl print_ident id
  | Octy_arrow (lab, ty, cty) ->
      fprintf ppf "@[%s%a ->@ %a@]" (if lab <> "" then lab ^ ":" else "")
        print_out_type_2 ty print_out_class_type cty
  | Octy_signature (self_ty, csil) ->
      let pr_param ppf =
        function
          Some ty -> fprintf ppf "@ @[(%a)@]" print_out_type ty
        | None -> ()
      in
      fprintf ppf "@[<hv 2>@[<2>object%a@]@ %a@;<1 -2>end@]" pr_param self_ty
        (print_list print_out_class_sig_item (fun ppf -> fprintf ppf "@ "))
        csil
and print_out_class_sig_item ppf =
  function
    Ocsg_constraint (ty1, ty2) ->
      fprintf ppf "@[<2>constraint %a =@ %a@]" print_out_type ty1
        print_out_type ty2
  | Ocsg_method (name, priv, virt, ty) ->
      fprintf ppf "@[<2>method %s%s%s :@ %a@]"
        (if priv then "private " else "") (if virt then "virtual " else "")
        name print_out_type ty
  | Ocsg_value (name, mut, vr, ty) ->
      fprintf ppf "@[<2>val %s%s%s :@ %a@]"
        (if mut then "mutable " else "")
        (if vr then "virtual " else "")
        name print_out_type ty

(* Signature *)

let rec print_out_functor ppf =
  function
    Omty_functor (_, None, mty_res) ->
      fprintf ppf "() %a" print_out_functor mty_res
  | Omty_functor (name , Some mty_arg, mty_res) ->
      fprintf ppf "(%s : %a) %a" name
        print_out_module_type mty_arg print_out_functor mty_res
  | m -> fprintf ppf "->@ %a" print_out_module_type m
and print_out_module_type ppf =
  function
    Omty_abstract -> ()
  | Omty_functor _ as t ->
      fprintf ppf "@[<2>functor@ %a@]" print_out_functor t
  | Omty_ident id -> fprintf ppf "%a" print_ident id
  | Omty_signature sg ->
      fprintf ppf "@[<hv 2>sig@ %a@;<1 -2>end@]" print_out_signature sg
  | Omty_alias id -> fprintf ppf "(module %a)" print_ident id
and print_out_signature ppf =
  function
    [] -> ()
  | [item] -> print_out_sig_item ppf item
  | Osig_typext(ext, Oext_first) :: items ->
      (* Gather together the extension constructors *)
      let rec gather_extensions acc items =
        match items with
            Osig_typext(ext, Oext_next) :: items ->
              gather_extensions
                ((ext.oext_name, ext.oext_args, ext.oext_ret_type) :: acc)
                items
          | _ -> (List.rev acc, items)
      in
      let exts, items =
        gather_extensions
          [(ext.oext_name, ext.oext_args, ext.oext_ret_type)]
          items
      in
      let te =
        { otyext_name = ext.oext_type_name;
          otyext_params = ext.oext_type_params;
          otyext_constructors = exts;
          otyext_private = ext.oext_private }
      in
        fprintf ppf "%a@ %a" print_out_type_extension te print_out_signature items
  | item :: items ->
      fprintf ppf "%a@ %a" print_out_sig_item item print_out_signature items
and print_out_sig_item ppf =
  function
    Osig_class (vir_flag, name, params, clt, rs) ->
      fprintf ppf "@[<2>%s%s@ %a%s@ :@ %a@]"
        (if rs = Orec_next then "and" else "class")
        (if vir_flag then " virtual" else "") print_out_class_params params
        name print_out_class_type clt
  | Osig_class_type (vir_flag, name, params, clt, rs) ->
      fprintf ppf "@[<2>%s%s@ %a%s@ =@ %a@]"
        (if rs = Orec_next then "and" else "class type")
        (if vir_flag then " virtual" else "") print_out_class_params params
        name print_out_class_type clt
  | Osig_typext (ext, Oext_exception) ->
      fprintf ppf "@[<2>exception %a@]"
        print_out_constr (ext.oext_name, ext.oext_args, ext.oext_ret_type)
  | Osig_typext (ext, es) ->
      print_out_extension_constructor ppf ext
  | Osig_modtype (name, Omty_abstract) ->
      fprintf ppf "@[<2>module type %s@]" name
  | Osig_modtype (name, mty) ->
      fprintf ppf "@[<2>module type %s =@ %a@]" name print_out_module_type mty
  | Osig_module (name, Omty_alias id, _) ->
      fprintf ppf "@[<2>module %s =@ %a@]" name print_ident id
  | Osig_module (name, mty, rs) ->
      fprintf ppf "@[<2>%s %s :@ %a@]"
        (match rs with Orec_not -> "module"
                     | Orec_first -> "module rec"
                     | Orec_next -> "and")
        name print_out_module_type mty
  | Osig_type(td, rs) ->
        print_out_type_decl
          (match rs with
           | Orec_not   -> "type nonrec"
           | Orec_first -> "type"
           | Orec_next  -> "and")
          ppf td
  | Osig_value (name, ty, prims) ->
      let kwd = if prims = [] then "val" else "external" in
      let pr_prims ppf =
        function
          [] -> ()
        | s :: sl ->
            fprintf ppf "@ = \"%s\"" s;
            List.iter (fun s -> fprintf ppf "@ \"%s\"" s) sl
      in
      fprintf ppf "@[<2>%s %a :@ %a%a@]" kwd value_ident name print_out_type
        ty pr_prims prims

and print_out_type_decl kwd ppf td =
  let print_constraints ppf =
    List.iter
      (fun (ty1, ty2) ->
         fprintf ppf "@ @[<2>constraint %a =@ %a@]" print_out_type ty1
           print_out_type ty2)
      td.otype_cstrs
  in
  let type_defined ppf =
    match td.otype_params with
      [] -> pp_print_string ppf td.otype_name
    | [param] -> fprintf ppf "@[%s@ %a@]" td.otype_name type_parameter param
    | _ ->
        fprintf ppf "@[%s@ @[%a@]@]"
          td.otype_name
          (print_list type_parameter (fun ppf -> fprintf ppf "@ "))
          td.otype_params
  in
  let print_manifest ppf =
    function
      Otyp_manifest (ty, _) -> fprintf ppf " =@ %a" print_out_type ty
    | _ -> ()
  in
  let print_name_params ppf =
    fprintf ppf "%s %t%a" kwd type_defined print_manifest td.otype_type
  in
  let ty =
    match td.otype_type with
      Otyp_manifest (_, ty) -> ty
    | _ -> td.otype_type
  in
  let print_private ppf = function
    Asttypes.Private -> fprintf ppf " private"
  | Asttypes.Public -> ()
  in
  let print_out_tkind ppf = function
  | Otyp_abstract -> ()
  | Otyp_record lbls ->
      fprintf ppf " =%a {%a@;<1 -2>}"
        print_private td.otype_private
        (print_list_init print_out_label (fun ppf -> fprintf ppf "@ ")) lbls
  | Otyp_sum constrs ->
      fprintf ppf " =%a@;<1 2>%a"
        print_private td.otype_private
        (print_list print_out_constr (fun ppf -> fprintf ppf "@ | ")) constrs
  | Otyp_open ->
      fprintf ppf " = .."
  | ty ->
      fprintf ppf " =%a@;<1 2>%a"
        print_private td.otype_private
        print_out_type ty
  in
  fprintf ppf "@[<2>@[<hv 2>%t%a@]%t@]"
    print_name_params
    print_out_tkind ty
    print_constraints

and print_out_constr ppf (name, tyl,ret_type_opt) =
  match ret_type_opt with
  | None ->
      begin match tyl with
      | [] ->
          pp_print_string ppf name
      | _ ->
          fprintf ppf "@[<2>%s of@ %a@]" name
            (print_typlist print_simple_out_type "") tyl
      end
  | Some ret_type ->
      begin match tyl with
      | [] ->
          fprintf ppf "@[<2>%s of@ %a@]" name print_simple_out_type  ret_type
      | _ ->
          fprintf ppf "@[<2>%s of@ %a :%a@]" name
            (print_typlist print_simple_out_type "")
            tyl print_simple_out_type ret_type
      end


and print_out_label ppf (name, mut, arg) =
  fprintf ppf "@[<2>%s%s :@ %a@]," (if mut then "mutable " else "") name
    print_out_type arg

and print_out_extension_constructor ppf ext =
  let print_extended_type ppf =
    let print_type_parameter ppf ty =
      fprintf ppf "%s"
        (if ty = "_" then ty else "'"^ty)
    in
      match ext.oext_type_params with
        [] -> fprintf ppf "%s" ext.oext_type_name
      | [ty_param] ->
        fprintf ppf "@[%a@ %s@]"
          print_type_parameter
          ty_param
          ext.oext_type_name
      | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list print_type_parameter (fun ppf -> fprintf ppf ",@ "))
          ext.oext_type_params
          ext.oext_type_name
  in
  fprintf ppf "@[<hv 2>type %t +=%s@;<1 2>%a@]"
    print_extended_type
    (if ext.oext_private = Asttypes.Private then " private" else "")
    print_out_constr (ext.oext_name, ext.oext_args, ext.oext_ret_type)

and print_out_type_extension ppf te =
  let print_extended_type ppf =
    let print_type_parameter ppf ty =
      fprintf ppf "%s"
        (if ty = "_" then ty else "'"^ty)
    in
    match te.otyext_params with
      [] -> fprintf ppf "%s" te.otyext_name
    | [param] ->
      fprintf ppf "@[%a@ %s@]"
        print_type_parameter param
        te.otyext_name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (print_list print_type_parameter (fun ppf -> fprintf ppf ",@ "))
          te.otyext_params
          te.otyext_name
  in
  fprintf ppf "@[<hv 2>type %t +=%s@;<1 2>%a@]"
    print_extended_type
    (if te.otyext_private = Asttypes.Private then " private" else "")
    (print_list print_out_constr (fun ppf -> fprintf ppf "@ | "))
    te.otyext_constructors

(* Phrases *)

let print_out_exception ppf exn outv =
  match exn with
    Sys.Break -> fprintf ppf "Interrupted.@."
  | Out_of_memory -> fprintf ppf "Out of memory during evaluation.@."
  | Stack_overflow ->
      fprintf ppf "Stack overflow during evaluation (looping recursion?).@."
  | _ -> fprintf ppf "@[Exception:@ %a.@]@." print_out_value outv

let rec print_items ppf =
  function
    [] -> ()
  | (Osig_typext(ext, Oext_first), None) :: items ->
      (* Gather together extension constructors *)
      let rec gather_extensions acc items =
        match items with
            (Osig_typext(ext, Oext_next), None) :: items ->
              gather_extensions
                ((ext.oext_name, ext.oext_args, ext.oext_ret_type) :: acc)
                items
          | _ -> (List.rev acc, items)
      in
      let exts, items =
        gather_extensions
          [(ext.oext_name, ext.oext_args, ext.oext_ret_type)]
          items
      in
      let te =
        { otyext_name = ext.oext_type_name;
          otyext_params = ext.oext_type_params;
          otyext_constructors = exts;
          otyext_private = ext.oext_private }
      in
        fprintf ppf "@[%a@]" print_out_type_extension te;
        if items <> [] then fprintf ppf "@ %a" print_items items
  | (tree, valopt) :: items ->
      begin match valopt with
        Some v ->
          fprintf ppf "@[<2>%a =@ %a@]" print_out_sig_item tree
            print_out_value v
      | None -> fprintf ppf "@[%a@]" print_out_sig_item tree
      end;
      if items <> [] then fprintf ppf "@ %a" print_items items

let print_out_phrase ppf =
  function
    Ophr_eval (outv, ty) ->
      fprintf ppf "@[- : %a@ =@ %a@]@." print_out_type ty print_out_value outv
  | Ophr_signature [] -> ()
  | Ophr_signature items -> fprintf ppf "@[<v>%a@]@." print_items items
  | Ophr_exception (exn, outv) -> print_out_exception ppf exn outv
