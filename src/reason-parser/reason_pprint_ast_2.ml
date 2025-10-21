open Ppxlib
open Pretty_expressive

let cf = Printer.default_cost_factory ~page_width:80 ()

module P = Printer.Make ((val cf))
open P

type settings =
  { width : int
  ; assumeExplicitArity : bool
  ; constructorLists : string list
  }

let default_settings =
  { width = 80; assumeExplicitArity = false; constructorLists = [] }

let current_settings = ref default_settings

let configure ~width ~assumeExplicitArity ~constructorLists =
  current_settings := { width; assumeExplicitArity; constructorLists }

(* Use pretty_expressive combinators at top level *)
(* We'll create printer modules with runtime width in each method *)

(* Helper function to replicate PPrint's separate *)
let separate sep docs =
  match docs with
  | [] -> empty
  | [ x ] -> x
  | x :: xs -> List.fold_left ~f:(fun acc d -> acc ^^ sep ^^ d) ~init:x xs

let rec longident = function
  | Lident s -> text s
  | Ldot (path, s) -> longident path ^^ text "." ^^ text s
  | Lapply (l1, l2) -> longident l1 ^^ text "(" ^^ longident l2 ^^ text ")"

and longident_loc li = longident li.txt

and constant = function
  | Pconst_integer (i, None) -> text i
  | Pconst_integer (i, Some m) -> text i ^^ text (String.make 1 m)
  | Pconst_string (s, _, None) -> text ("\"" ^ String.escaped s ^ "\"")
  | Pconst_string (s, _, Some delim) ->
    text ("{" ^ delim ^ "|" ^ s ^ "|" ^ delim ^ "}")
  | Pconst_char c -> text ("'" ^ Char.escaped c ^ "'")
  | Pconst_float (f, None) -> text f
  | Pconst_float (f, Some m) -> text f ^^ text (String.make 1 m)

and expression_to_text expr =
  match expr.pexp_desc with
  | Pexp_ident { txt; _ } ->
    (* Handle operators in parentheses *)
    (match txt with
    | Lident name
      when String.length name > 0
           &&
           match name.[0] with
           | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> false
           | _ -> true ->
      text ("(" ^ name ^ ")")
    | _ -> longident txt)
  | Pexp_constant c -> constant c
  | Pexp_let (_, vbs, e) ->
    let bindings =
      separate
        (text "; ")
        (List.map
           ~f:(fun vb ->
             text "let "
             ^^ pattern_to_text vb.pvb_pat
             ^^ text " = "
             ^^ expression_to_text vb.pvb_expr)
           vbs)
    in
    bindings ^^ text "; " ^^ expression_to_text e
  | Pexp_function (params, _, body) ->
    let params_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun param ->
             match param.pparam_desc with
             | Pparam_val (lbl, None, pat) ->
               (match lbl with
               | Nolabel -> pattern_to_text pat
               | Labelled s -> text ("~" ^ s)
               | Optional s -> text ("~" ^ s ^ "=?"))
             | Pparam_val (lbl, Some default, pat) ->
               let pat_doc = pattern_to_text pat in
               (match lbl with
               | Nolabel -> pat_doc ^^ text "=" ^^ expression_to_text default
               | Labelled s ->
                 text ("~" ^ s ^ "=") ^^ expression_to_text default
               | Optional s ->
                 text ("~" ^ s ^ "=?") ^^ expression_to_text default)
             | Pparam_newtype { txt; _ } -> text ("type " ^ txt))
           params)
    in
    let body_doc =
      match body with
      | Pfunction_body e -> expression_to_text e
      | Pfunction_cases (cases, _, _) ->
        separate
          (text " ")
          (List.map
             ~f:(fun case ->
               text "| "
               ^^ pattern_to_text case.pc_lhs
               ^^ text " => "
               ^^ expression_to_text case.pc_rhs)
             cases)
    in
    text "(" ^^ params_doc ^^ text ") => " ^^ body_doc
  | Pexp_apply (func, args) ->
    let func_doc = expression_to_text func in
    let arg_texts =
      List.map
        ~f:(fun (lbl, arg) ->
          let arg_doc = expression_to_text arg in
          let labeled_doc =
            match lbl with
            | Nolabel -> arg_doc
            | Labelled s -> text ("~" ^ s ^ "=") ^^ arg_doc
            | Optional s -> text ("~" ^ s ^ "=?") ^^ arg_doc
          in
          labeled_doc)
        args
    in
    let args_doc =
      match arg_texts with
      | [] -> empty
      | [ single ] -> single
      | first :: rest ->
        List.fold_left
          ~f:(fun acc arg -> acc ^^ text ", " ^^ arg)
          ~init:first
          rest
    in
    func_doc ^^ text "(" ^^ args_doc ^^ text ")"
  | Pexp_match (e, cases) ->
    text "switch ("
    ^^ expression_to_text e
    ^^ text ") { "
    ^^ separate
         (text " ")
         (List.map
            ~f:(fun case ->
              text "| "
              ^^ pattern_to_text case.pc_lhs
              ^^ text " => "
              ^^ expression_to_text case.pc_rhs)
            cases)
    ^^ text " }"
  | Pexp_try (e, cases) ->
    text "try "
    ^^ expression_to_text e
    ^^ text " { "
    ^^ separate
         (text " ")
         (List.map
            ~f:(fun case ->
              text "| "
              ^^ pattern_to_text case.pc_lhs
              ^^ text " => "
              ^^ expression_to_text case.pc_rhs)
            cases)
    ^^ text " }"
  | Pexp_tuple exprs ->
    text "("
    ^^ separate (text ", ") (List.map ~f:expression_to_text exprs)
    ^^ text ")"
  | Pexp_construct ({ txt = Lident "()"; _ }, None) -> text "()"
  | Pexp_construct ({ txt = Lident "true"; _ }, None) -> text "true"
  | Pexp_construct ({ txt = Lident "false"; _ }, None) -> text "false"
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> text "[]"
  | Pexp_construct (li, None) -> longident_loc li
  | Pexp_construct (li, Some expr) ->
    longident_loc li ^^ text "(" ^^ expression_to_text expr ^^ text ")"
  | Pexp_variant (tag, None) -> text ("`" ^ tag)
  | Pexp_variant (tag, Some e) ->
    text ("`" ^ tag ^ "(") ^^ expression_to_text e ^^ text ")"
  | Pexp_record (fields, None) ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun (li, e) ->
             longident_loc li ^^ text ": " ^^ expression_to_text e)
           fields)
    in
    text "{ " ^^ fields_doc ^^ text " }"
  | Pexp_record (fields, Some base) ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun (li, e) ->
             longident_loc li ^^ text ": " ^^ expression_to_text e)
           fields)
    in
    text "{ ..."
    ^^ expression_to_text base
    ^^ text ", "
    ^^ fields_doc
    ^^ text " }"
  | Pexp_field (e, li) -> expression_to_text e ^^ text "." ^^ longident_loc li
  | Pexp_setfield (e1, li, e2) ->
    expression_to_text e1
    ^^ text "."
    ^^ longident_loc li
    ^^ text " = "
    ^^ expression_to_text e2
  | Pexp_array exprs ->
    text "[| "
    ^^ separate (text ", ") (List.map ~f:expression_to_text exprs)
    ^^ text " |]"
  | Pexp_ifthenelse (cond, then_, None) ->
    text "if ("
    ^^ expression_to_text cond
    ^^ text ") { "
    ^^ expression_to_text then_
    ^^ text " }"
  | Pexp_ifthenelse (cond, then_, Some else_) ->
    text "if ("
    ^^ expression_to_text cond
    ^^ text ") { "
    ^^ expression_to_text then_
    ^^ text " } else { "
    ^^ expression_to_text else_
    ^^ text " }"
  | Pexp_sequence (e1, e2) ->
    expression_to_text e1 ^^ text "; " ^^ expression_to_text e2
  | Pexp_while (cond, body) ->
    text "while ("
    ^^ expression_to_text cond
    ^^ text ") { "
    ^^ expression_to_text body
    ^^ text " }"
  | Pexp_for (pat, start, end_, dir, body) ->
    let dir_str = match dir with Upto -> "to" | Downto -> "downto" in
    text "for ("
    ^^ pattern_to_text pat
    ^^ text " in "
    ^^ expression_to_text start
    ^^ text (" " ^ dir_str ^ " ")
    ^^ expression_to_text end_
    ^^ text ") { "
    ^^ expression_to_text body
    ^^ text " }"
  | Pexp_constraint (e, typ) ->
    text "("
    ^^ expression_to_text e
    ^^ text ": "
    ^^ core_type_to_text typ
    ^^ text ")"
  | Pexp_coerce (e, None, typ) ->
    text "("
    ^^ expression_to_text e
    ^^ text " :> "
    ^^ core_type_to_text typ
    ^^ text ")"
  | Pexp_coerce (e, Some typ1, typ2) ->
    text "("
    ^^ expression_to_text e
    ^^ text ": "
    ^^ core_type_to_text typ1
    ^^ text " :> "
    ^^ core_type_to_text typ2
    ^^ text ")"
  | Pexp_send (e, { txt; _ }) -> expression_to_text e ^^ text "#" ^^ text txt
  | Pexp_new li -> text "new " ^^ longident_loc li
  | Pexp_setinstvar ({ txt; _ }, e) ->
    text txt ^^ text " = " ^^ expression_to_text e
  | Pexp_override fields ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun ({ txt; _ }, e) ->
             text txt ^^ text ": " ^^ expression_to_text e)
           fields)
    in
    text "{< " ^^ fields_doc ^^ text " >}"
  | Pexp_letmodule ({ txt = Some name; _ }, me, e) ->
    text "let module "
    ^^ text name
    ^^ text " = "
    ^^ module_expr_to_text me
    ^^ text "; "
    ^^ expression_to_text e
  | Pexp_letmodule ({ txt = None; _ }, me, e) ->
    text "let module _ = "
    ^^ module_expr_to_text me
    ^^ text "; "
    ^^ expression_to_text e
  | Pexp_letexception (ec, e) ->
    text "let exception "
    ^^ extension_constructor_to_text ec
    ^^ text "; "
    ^^ expression_to_text e
  | Pexp_assert e -> text "assert(" ^^ expression_to_text e ^^ text ")"
  | Pexp_lazy e -> text "lazy(" ^^ expression_to_text e ^^ text ")"
  | Pexp_poly (e, None) -> expression_to_text e
  | Pexp_poly (e, Some typ) ->
    text "("
    ^^ expression_to_text e
    ^^ text ": "
    ^^ core_type_to_text typ
    ^^ text ")"
  | Pexp_object cs -> class_structure_to_text cs
  | Pexp_newtype ({ txt; _ }, e) ->
    text "(type " ^^ text txt ^^ text ") => " ^^ expression_to_text e
  | Pexp_pack me -> text "(module " ^^ module_expr_to_text me ^^ text ")"
  | Pexp_open (od, e) ->
    text "open "
    ^^ module_expr_to_text od.popen_expr
    ^^ text "; "
    ^^ expression_to_text e
  | Pexp_letop { let_; ands; body } ->
    let let_doc = binding_op_to_text let_ in
    let ands_doc = separate (text " ") (List.map ~f:binding_op_to_text ands) in
    let_doc ^^ text " " ^^ ands_doc ^^ text " in " ^^ expression_to_text body
  | Pexp_extension ext -> extension_to_text ext
  | Pexp_unreachable -> text "."

and pattern_to_text pat =
  match pat.ppat_desc with
  | Ppat_any -> text "_"
  | Ppat_var { txt; _ } ->
    (* Handle operators in parentheses *)
    if
      String.length txt > 0
      &&
      match txt.[0] with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> false
      | _ -> true
    then text ("(" ^ txt ^ ")")
    else text txt
  | Ppat_alias (p, { txt; _ }) -> pattern_to_text p ^^ text " as " ^^ text txt
  | Ppat_constant c -> constant c
  | Ppat_interval (c1, c2) -> constant c1 ^^ text " .. " ^^ constant c2
  | Ppat_tuple pats ->
    text "("
    ^^ separate (text ", ") (List.map ~f:pattern_to_text pats)
    ^^ text ")"
  | Ppat_construct ({ txt = Lident "()"; _ }, None) -> text "()"
  | Ppat_construct ({ txt = Lident "true"; _ }, None) -> text "true"
  | Ppat_construct ({ txt = Lident "false"; _ }, None) -> text "false"
  | Ppat_construct ({ txt = Lident "[]"; _ }, None) -> text "[]"
  | Ppat_construct (li, None) -> longident_loc li
  | Ppat_construct (li, Some (_, pat)) ->
    longident_loc li ^^ text "(" ^^ pattern_to_text pat ^^ text ")"
  | Ppat_variant (tag, None) -> text ("`" ^ tag)
  | Ppat_variant (tag, Some pat) ->
    text ("`" ^ tag ^ "(") ^^ pattern_to_text pat ^^ text ")"
  | Ppat_record (fields, Closed) ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun (li, pat) ->
             longident_loc li ^^ text ": " ^^ pattern_to_text pat)
           fields)
    in
    text "{ " ^^ fields_doc ^^ text " }"
  | Ppat_record (fields, Open) ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun (li, pat) ->
             longident_loc li ^^ text ": " ^^ pattern_to_text pat)
           fields)
    in
    text "{ " ^^ fields_doc ^^ text ", _ }"
  | Ppat_array pats ->
    text "[| "
    ^^ separate (text ", ") (List.map ~f:pattern_to_text pats)
    ^^ text " |]"
  | Ppat_or (p1, p2) -> pattern_to_text p1 ^^ text " | " ^^ pattern_to_text p2
  | Ppat_constraint (p, typ) ->
    text "("
    ^^ pattern_to_text p
    ^^ text ": "
    ^^ core_type_to_text typ
    ^^ text ")"
  | Ppat_type li -> text "#" ^^ longident_loc li
  | Ppat_lazy p -> text "lazy(" ^^ pattern_to_text p ^^ text ")"
  | Ppat_unpack { txt = Some name; _ } -> text ("(module " ^ name ^ ")")
  | Ppat_unpack { txt = None; _ } -> text "(module _)"
  | Ppat_exception p -> text "exception " ^^ pattern_to_text p
  | Ppat_extension ext -> extension_to_text ext
  | Ppat_open (li, p) ->
    longident_loc li ^^ text ".(" ^^ pattern_to_text p ^^ text ")"

and core_type_to_text typ =
  match typ.ptyp_desc with
  | Ptyp_any -> text "_"
  | Ptyp_var s -> text ("'" ^ s)
  | Ptyp_arrow (Nolabel, t1, t2) ->
    core_type_to_text t1 ^^ text " => " ^^ core_type_to_text t2
  | Ptyp_arrow (Labelled s, t1, t2) ->
    text ("~" ^ s ^ ": ")
    ^^ core_type_to_text t1
    ^^ text " => "
    ^^ core_type_to_text t2
  | Ptyp_arrow (Optional s, t1, t2) ->
    text ("~" ^ s ^ ": ")
    ^^ core_type_to_text t1
    ^^ text "=? => "
    ^^ core_type_to_text t2
  | Ptyp_tuple typs ->
    text "("
    ^^ separate (text ", ") (List.map ~f:core_type_to_text typs)
    ^^ text ")"
  | Ptyp_constr (li, []) -> longident_loc li
  | Ptyp_constr (li, args) ->
    longident_loc li
    ^^ text "("
    ^^ separate (text ", ") (List.map ~f:core_type_to_text args)
    ^^ text ")"
  | Ptyp_object (fields, Closed) ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun field ->
             match field.pof_desc with
             | Otag ({ txt; _ }, typ) ->
               text ("\"" ^ txt ^ "\": ") ^^ core_type_to_text typ
             | Oinherit typ -> text "..." ^^ core_type_to_text typ)
           fields)
    in
    text "{ . " ^^ fields_doc ^^ text " }"
  | Ptyp_object (fields, Open) ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun field ->
             match field.pof_desc with
             | Otag ({ txt; _ }, typ) ->
               text ("\"" ^ txt ^ "\": ") ^^ core_type_to_text typ
             | Oinherit typ -> text "..." ^^ core_type_to_text typ)
           fields)
    in
    text "{ .. " ^^ fields_doc ^^ text " }"
  | Ptyp_class (li, []) -> text "#" ^^ longident_loc li
  | Ptyp_class (li, args) ->
    text "#"
    ^^ longident_loc li
    ^^ text "("
    ^^ separate (text ", ") (List.map ~f:core_type_to_text args)
    ^^ text ")"
  | Ptyp_alias (typ, { txt; _ }) -> core_type_to_text typ ^^ text (" as '" ^ txt)
  | Ptyp_variant (rows, Closed, None) ->
    let rows_doc = separate (text " ") (List.map ~f:row_field_to_text rows) in
    text "[ " ^^ rows_doc ^^ text " ]"
  | Ptyp_variant (rows, Closed, Some tags) ->
    let rows_doc = separate (text " ") (List.map ~f:row_field_to_text rows) in
    let tags_doc =
      separate (text " ") (List.map ~f:(fun t -> text ("`" ^ t)) tags)
    in
    text "[ " ^^ rows_doc ^^ text " > " ^^ tags_doc ^^ text " ]"
  | Ptyp_variant (rows, Open, _) ->
    let rows_doc = separate (text " ") (List.map ~f:row_field_to_text rows) in
    text "[ > " ^^ rows_doc ^^ text " ]"
  | Ptyp_poly ([], typ) -> core_type_to_text typ
  | Ptyp_poly (vars, typ) ->
    separate (text " ") (List.map ~f:(fun { txt; _ } -> text ("'" ^ txt)) vars)
    ^^ text ". "
    ^^ core_type_to_text typ
  | Ptyp_package (li, []) -> text "module " ^^ longident_loc li
  | Ptyp_package (li, constrs) ->
    let constrs_doc =
      separate
        (text " and ")
        (List.map
           ~f:(fun (li', typ) ->
             text "type "
             ^^ longident_loc li'
             ^^ text " = "
             ^^ core_type_to_text typ)
           constrs)
    in
    text "module " ^^ longident_loc li ^^ text " with " ^^ constrs_doc
  | Ptyp_extension ext -> extension_to_text ext
  | Ptyp_open (m, typ) ->
    longident m.txt ^^ text ".(" ^^ core_type_to_text typ ^^ text ")"

and row_field_to_text field =
  match field.prf_desc with
  | Rtag ({ txt; _ }, _, []) -> text ("| `" ^ txt)
  | Rtag ({ txt; _ }, _, typs) ->
    text ("| `" ^ txt ^ "(")
    ^^ separate (text ", ") (List.map ~f:core_type_to_text typs)
    ^^ text ")"
  | Rinherit typ -> text "| " ^^ core_type_to_text typ

and structure_item_to_text item =
  match item.pstr_desc with
  | Pstr_eval (e, _) -> expression_to_text e
  | Pstr_value (Nonrecursive, vbs) ->
    separate
      (text "; ")
      (List.map
         ~f:(fun vb ->
           text "let "
           ^^ pattern_to_text vb.pvb_pat
           ^^ text " = "
           ^^ expression_to_text vb.pvb_expr)
         vbs)
  | Pstr_value (Recursive, vbs) ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i vb ->
           text (if i = 0 then "let rec " else "and ")
           ^^ pattern_to_text vb.pvb_pat
           ^^ text " = "
           ^^ expression_to_text vb.pvb_expr)
         vbs)
  | Pstr_primitive vd ->
    text "external "
    ^^ text vd.pval_name.txt
    ^^ text ": "
    ^^ core_type_to_text vd.pval_type
    ^^ text " = "
    ^^ separate
         (text " ")
         (List.map ~f:(fun s -> text ("\"" ^ s ^ "\"")) vd.pval_prim)
  | Pstr_type (_, tds) ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i td ->
           text (if i = 0 then "type " else "") ^^ type_declaration_to_text td)
         tds)
  | Pstr_typext te ->
    text "type "
    ^^ longident_loc te.ptyext_path
    ^^ text " += "
    ^^ separate
         (text " ")
         (List.map ~f:extension_constructor_to_text te.ptyext_constructors)
  | Pstr_exception ec ->
    text "exception " ^^ extension_constructor_to_text ec.ptyexn_constructor
  | Pstr_module { pmb_name; pmb_expr; _ } ->
    let name = match pmb_name.txt with Some n -> n | None -> "_" in
    text "module " ^^ text name ^^ text " = " ^^ module_expr_to_text pmb_expr
  | Pstr_recmodule mbs ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i mb ->
           let name = match mb.pmb_name.txt with Some n -> n | None -> "_" in
           text (if i = 0 then "module rec " else "and ")
           ^^ text name
           ^^ text " = "
           ^^ module_expr_to_text mb.pmb_expr)
         mbs)
  | Pstr_modtype { pmtd_name; pmtd_type = None; _ } ->
    text ("module type " ^ pmtd_name.txt)
  | Pstr_modtype { pmtd_name; pmtd_type = Some mt; _ } ->
    text ("module type " ^ pmtd_name.txt)
    ^^ text " = "
    ^^ module_type_to_text mt
  | Pstr_open od -> text "open " ^^ module_expr_to_text od.popen_expr
  | Pstr_class cds ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i cd ->
           text (if i = 0 then "class " else "and ")
           ^^ text cd.pci_name.txt
           ^^ (match cd.pci_params with
             | [] -> empty
             | params ->
               text "("
               ^^ separate
                    (text ", ")
                    (List.map ~f:(fun (typ, _) -> core_type_to_text typ) params)
               ^^ text ") ")
           ^^ text " = "
           ^^ class_expr_to_text cd.pci_expr)
         cds)
  | Pstr_class_type ctds ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i ctd ->
           text (if i = 0 then "class type " else "and ")
           ^^ text ctd.pci_name.txt
           ^^ (match ctd.pci_params with
             | [] -> empty
             | params ->
               text "("
               ^^ separate
                    (text ", ")
                    (List.map ~f:(fun (typ, _) -> core_type_to_text typ) params)
               ^^ text ") ")
           ^^ text " = "
           ^^ class_type_to_text ctd.pci_expr)
         ctds)
  | Pstr_include { pincl_mod; _ } ->
    text "include " ^^ module_expr_to_text pincl_mod
  | Pstr_attribute attr -> attribute_to_text attr
  | Pstr_extension (ext, _) -> item_extension_to_text ext

and signature_item_to_text item =
  match item.psig_desc with
  | Psig_value vd ->
    text "let "
    ^^ text vd.pval_name.txt
    ^^ text ": "
    ^^ core_type_to_text vd.pval_type
  | Psig_type (_, tds) ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i td ->
           text (if i = 0 then "type " else "") ^^ type_declaration_to_text td)
         tds)
  | Psig_typext te ->
    text "type "
    ^^ longident_loc te.ptyext_path
    ^^ text " += "
    ^^ separate
         (text " ")
         (List.map ~f:extension_constructor_to_text te.ptyext_constructors)
  | Psig_exception ec ->
    text "exception " ^^ extension_constructor_to_text ec.ptyexn_constructor
  | Psig_module { pmd_name; pmd_type; _ } ->
    let name = match pmd_name.txt with Some n -> n | None -> "_" in
    text "module " ^^ text name ^^ text ": " ^^ module_type_to_text pmd_type
  | Psig_recmodule mds ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i md ->
           let name = match md.pmd_name.txt with Some n -> n | None -> "_" in
           text (if i = 0 then "module rec " else "and ")
           ^^ text name
           ^^ text ": "
           ^^ module_type_to_text md.pmd_type)
         mds)
  | Psig_modtype { pmtd_name; pmtd_type = None; _ } ->
    text ("module type " ^ pmtd_name.txt)
  | Psig_modtype { pmtd_name; pmtd_type = Some mt; _ } ->
    text ("module type " ^ pmtd_name.txt)
    ^^ text " = "
    ^^ module_type_to_text mt
  | Psig_open od -> text "open " ^^ longident_loc od.popen_expr
  | Psig_include { pincl_mod; _ } ->
    text "include " ^^ module_type_to_text pincl_mod
  | Psig_class cds ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i cd ->
           text (if i = 0 then "class " else "and ")
           ^^ text cd.pci_name.txt
           ^^ (match cd.pci_params with
             | [] -> empty
             | params ->
               text "("
               ^^ separate
                    (text ", ")
                    (List.map ~f:(fun (typ, _) -> core_type_to_text typ) params)
               ^^ text ") ")
           ^^ text ": "
           ^^ class_type_to_text cd.pci_expr)
         cds)
  | Psig_class_type ctds ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i ctd ->
           text (if i = 0 then "class type " else "and ")
           ^^ text ctd.pci_name.txt
           ^^ (match ctd.pci_params with
             | [] -> empty
             | params ->
               text "("
               ^^ separate
                    (text ", ")
                    (List.map ~f:(fun (typ, _) -> core_type_to_text typ) params)
               ^^ text ") ")
           ^^ text " = "
           ^^ class_type_to_text ctd.pci_expr)
         ctds)
  | Psig_attribute attr -> attribute_to_text attr
  | Psig_extension (ext, _) -> item_extension_to_text ext
  | Psig_modsubst { pms_name; pms_manifest; _ } ->
    text ("module " ^ pms_name.txt) ^^ text " := " ^^ longident_loc pms_manifest
  | Psig_typesubst tds ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i td ->
           text (if i = 0 then "type " else "and ")
           ^^ type_declaration_to_text td
           ^^ text " := ...")
         tds)
  | Psig_modtypesubst { pmtd_name; pmtd_type = Some mt; _ } ->
    text ("module type " ^ pmtd_name.txt)
    ^^ text " := "
    ^^ module_type_to_text mt
  | Psig_modtypesubst { pmtd_name; pmtd_type = None; _ } ->
    text ("module type " ^ pmtd_name.txt ^ " := ...")

and type_declaration_to_text td =
  let params =
    match td.ptype_params with
    | [] -> empty
    | ps ->
      text "("
      ^^ separate
           (text ", ")
           (List.map ~f:(fun (typ, _) -> core_type_to_text typ) ps)
      ^^ text ") "
  in
  let name = text td.ptype_name.txt in
  let manifest =
    match td.ptype_manifest with
    | None -> empty
    | Some typ -> text " = " ^^ core_type_to_text typ
  in
  let kind =
    match td.ptype_kind with
    | Ptype_abstract -> empty
    | Ptype_variant constrs ->
      text " = "
      ^^ separate
           (text " ")
           (List.map
              ~f:(fun cd ->
                let constructor_name = text ("| " ^ cd.pcd_name.txt) in
                let args_doc =
                  match cd.pcd_args with
                  | Pcstr_tuple [] -> empty
                  | Pcstr_tuple args ->
                    text "("
                    ^^ separate (text ", ") (List.map ~f:core_type_to_text args)
                    ^^ text ")"
                  | Pcstr_record _ -> text "({ ... })"
                in
                let res_doc =
                  match cd.pcd_res with
                  | None -> empty
                  | Some typ -> text ": " ^^ core_type_to_text typ
                in
                constructor_name ^^ args_doc ^^ res_doc)
              constrs)
    | Ptype_record fields ->
      text " = { "
      ^^ separate
           (text ", ")
           (List.map
              ~f:(fun ld ->
                text ld.pld_name.txt
                ^^ text ": "
                ^^ core_type_to_text ld.pld_type)
              fields)
      ^^ text " }"
    | Ptype_open -> text " = .."
  in
  (* Type parameters come after the name in Reason syntax *)
  name
  ^^ (match td.ptype_params with [] -> empty | _ -> params)
  ^^ manifest
  ^^ kind

and extension_constructor_to_text ec =
  let name = text ec.pext_name.txt in
  match ec.pext_kind with
  | Pext_decl (_, Pcstr_tuple [], None) -> name
  | Pext_decl (_, Pcstr_tuple args, ret_type) ->
    (match ret_type with
    | None ->
      name
      ^^ text "("
      ^^ separate (text ", ") (List.map ~f:core_type_to_text args)
      ^^ text ")"
    | Some typ ->
      name
      ^^ text "("
      ^^ separate (text ", ") (List.map ~f:core_type_to_text args)
      ^^ text "): "
      ^^ core_type_to_text typ)
  | Pext_decl (_, Pcstr_record _, ret_type) ->
    (match ret_type with
    | None -> name ^^ text "({ ... })"
    | Some typ -> name ^^ text "({ ... }): " ^^ core_type_to_text typ)
  | Pext_rebind li -> name ^^ text " = " ^^ longident_loc li

and module_expr_to_text me =
  match me.pmod_desc with
  | Pmod_ident li -> longident_loc li
  | Pmod_structure items ->
    text "{ "
    ^^ separate (text "; ") (List.map ~f:structure_item_to_text items)
    ^^ text " }"
  | Pmod_functor (Unit, body) -> text "() => " ^^ module_expr_to_text body
  | Pmod_functor (Named ({ txt = Some name; _ }, mt), body) ->
    text ("(" ^ name ^ ": ")
    ^^ module_type_to_text mt
    ^^ text ") => "
    ^^ module_expr_to_text body
  | Pmod_functor (Named ({ txt = None; _ }, mt), body) ->
    text "(_: "
    ^^ module_type_to_text mt
    ^^ text ") => "
    ^^ module_expr_to_text body
  | Pmod_apply (m1, m2) ->
    module_expr_to_text m1 ^^ text "(" ^^ module_expr_to_text m2 ^^ text ")"
  | Pmod_apply_unit m -> module_expr_to_text m ^^ text "()"
  | Pmod_constraint (me, mt) ->
    text "("
    ^^ module_expr_to_text me
    ^^ text ": "
    ^^ module_type_to_text mt
    ^^ text ")"
  | Pmod_unpack e -> text "(val " ^^ expression_to_text e ^^ text ")"
  | Pmod_extension ext -> extension_to_text ext

and module_type_to_text mt =
  match mt.pmty_desc with
  | Pmty_ident li -> longident_loc li
  | Pmty_signature items ->
    text "{ "
    ^^ separate (text "; ") (List.map ~f:signature_item_to_text items)
    ^^ text " }"
  | Pmty_functor (Unit, body) -> text "() => " ^^ module_type_to_text body
  | Pmty_functor (Named ({ txt = Some name; _ }, mt1), mt2) ->
    text ("(" ^ name ^ ": ")
    ^^ module_type_to_text mt1
    ^^ text ") => "
    ^^ module_type_to_text mt2
  | Pmty_functor (Named ({ txt = None; _ }, mt1), mt2) ->
    text "(_: "
    ^^ module_type_to_text mt1
    ^^ text ") => "
    ^^ module_type_to_text mt2
  | Pmty_with (mt, constrs) ->
    module_type_to_text mt
    ^^ text " with "
    ^^ separate (text " and ") (List.map ~f:with_constraint_to_text constrs)
  | Pmty_typeof me -> text "module type of " ^^ module_expr_to_text me
  | Pmty_extension ext -> extension_to_text ext
  | Pmty_alias li -> text "module " ^^ longident_loc li

and with_constraint_to_text = function
  | Pwith_type (li, td) ->
    text "type "
    ^^ longident_loc li
    ^^ text " = "
    ^^ type_declaration_to_text td
  | Pwith_module (li1, li2) ->
    text "module " ^^ longident_loc li1 ^^ text " = " ^^ longident_loc li2
  | Pwith_modtype (li, mt) ->
    text "module type "
    ^^ longident li.txt
    ^^ text " = "
    ^^ module_type_to_text mt
  | Pwith_typesubst (li, td) ->
    (* For destructive substitution, we need to include the type parameters *)
    let params_and_name =
      match td.ptype_params with
      | [] -> longident_loc li
      | ps ->
        let params_doc =
          text "("
          ^^ separate
               (text ", ")
               (List.map ~f:(fun (typ, _) -> core_type_to_text typ) ps)
          ^^ text ") "
        in
        params_doc ^^ longident_loc li
    in
    text "type "
    ^^ params_and_name
    ^^ text " := "
    ^^
      (match td.ptype_manifest with
      | None -> type_declaration_to_text td
      | Some typ -> core_type_to_text typ)
  | Pwith_modsubst (li1, li2) ->
    text "module " ^^ longident_loc li1 ^^ text " := " ^^ longident_loc li2
  | Pwith_modtypesubst (li, mt) ->
    text "module type "
    ^^ longident li.txt
    ^^ text " := "
    ^^ module_type_to_text mt

and class_expr_to_text ce =
  match ce.pcl_desc with
  | Pcl_constr (li, []) -> longident_loc li
  | Pcl_constr (li, typs) ->
    longident_loc li
    ^^ text "("
    ^^ separate (text ", ") (List.map ~f:core_type_to_text typs)
    ^^ text ")"
  | Pcl_structure cs -> class_structure_to_text cs
  | Pcl_fun (lbl, None, pat, ce) ->
    let lbl_doc =
      match lbl with
      | Nolabel -> empty
      | Labelled s -> text ("~" ^ s)
      | Optional s -> text ("~" ^ s ^ "=?")
    in
    lbl_doc ^^ pattern_to_text pat ^^ text " => " ^^ class_expr_to_text ce
  | Pcl_fun (lbl, Some default, pat, ce) ->
    let lbl_doc =
      match lbl with
      | Nolabel -> empty
      | Labelled s -> text ("~" ^ s ^ "=")
      | Optional s -> text ("~" ^ s ^ "=?")
    in
    lbl_doc
    ^^ pattern_to_text pat
    ^^ text "="
    ^^ expression_to_text default
    ^^ text " => "
    ^^ class_expr_to_text ce
  | Pcl_apply (ce, args) ->
    class_expr_to_text ce
    ^^ text "("
    ^^ separate
         (text ", ")
         (List.map
            ~f:(fun (lbl, e) ->
              let lbl_doc =
                match lbl with
                | Nolabel -> empty
                | Labelled s -> text ("~" ^ s ^ "=")
                | Optional s -> text ("~" ^ s ^ "=?")
              in
              lbl_doc ^^ expression_to_text e)
            args)
    ^^ text ")"
  | Pcl_let (_, vbs, ce) ->
    separate
      (text "; ")
      (List.map
         ~f:(fun vb ->
           text "let "
           ^^ pattern_to_text vb.pvb_pat
           ^^ text " = "
           ^^ expression_to_text vb.pvb_expr)
         vbs)
    ^^ text "; "
    ^^ class_expr_to_text ce
  | Pcl_constraint (ce, ct) ->
    text "("
    ^^ class_expr_to_text ce
    ^^ text ": "
    ^^ class_type_to_text ct
    ^^ text ")"
  | Pcl_extension ext -> extension_to_text ext
  | Pcl_open (od, ce) ->
    text "open "
    ^^ longident_loc od.popen_expr
    ^^ text "; "
    ^^ class_expr_to_text ce

and class_structure_to_text cs =
  let self_pat =
    match cs.pcstr_self.ppat_desc with
    | Ppat_var { txt = "this"; _ } -> empty
    | _ -> text " as " ^^ pattern_to_text cs.pcstr_self
  in
  text "{ "
  ^^ self_pat
  ^^ text " "
  ^^ separate (text "; ") (List.map ~f:class_field_to_text cs.pcstr_fields)
  ^^ text " }"

and class_field_to_text cf =
  match cf.pcf_desc with
  | Pcf_inherit (_, ce, None) -> text "inherit " ^^ class_expr_to_text ce
  | Pcf_inherit (_, ce, Some { txt; _ }) ->
    text "inherit " ^^ class_expr_to_text ce ^^ text (" as " ^ txt)
  | Pcf_val ({ txt; _ }, Mutable, Cfk_concrete (_, e)) ->
    text ("val mutable " ^ txt ^ " = ") ^^ expression_to_text e
  | Pcf_val ({ txt; _ }, Immutable, Cfk_concrete (_, e)) ->
    text ("val " ^ txt ^ " = ") ^^ expression_to_text e
  | Pcf_val ({ txt; _ }, Mutable, Cfk_virtual typ) ->
    text ("val mutable virtual " ^ txt ^ ": ") ^^ core_type_to_text typ
  | Pcf_val ({ txt; _ }, Immutable, Cfk_virtual typ) ->
    text ("val virtual " ^ txt ^ ": ") ^^ core_type_to_text typ
  | Pcf_method ({ txt; _ }, Private, Cfk_concrete (_, e)) ->
    text ("pri " ^ txt ^ " = ") ^^ expression_to_text e
  | Pcf_method ({ txt; _ }, Public, Cfk_concrete (_, e)) ->
    text ("pub " ^ txt ^ " = ") ^^ expression_to_text e
  | Pcf_method ({ txt; _ }, Private, Cfk_virtual typ) ->
    text ("pri virtual " ^ txt ^ ": ") ^^ core_type_to_text typ
  | Pcf_method ({ txt; _ }, Public, Cfk_virtual typ) ->
    text ("pub virtual " ^ txt ^ ": ") ^^ core_type_to_text typ
  | Pcf_constraint (typ1, typ2) ->
    text "constraint "
    ^^ core_type_to_text typ1
    ^^ text " = "
    ^^ core_type_to_text typ2
  | Pcf_initializer e -> text "initializer " ^^ expression_to_text e
  | Pcf_attribute attr -> attribute_to_text attr
  | Pcf_extension ext -> extension_to_text ext

and class_type_to_text ct =
  match ct.pcty_desc with
  | Pcty_constr (li, []) -> longident_loc li
  | Pcty_constr (li, typs) ->
    longident_loc li
    ^^ text "("
    ^^ separate (text ", ") (List.map ~f:core_type_to_text typs)
    ^^ text ")"
  | Pcty_signature cs ->
    text "{ "
    ^^ separate
         (text "; ")
         (List.map ~f:class_type_field_to_text cs.pcsig_fields)
    ^^ text " }"
  | Pcty_arrow (lbl, typ, ct) ->
    let lbl_doc =
      match lbl with
      | Nolabel -> empty
      | Labelled s -> text ("~" ^ s ^ ": ")
      | Optional s -> text ("~" ^ s ^ ": ")
    in
    lbl_doc ^^ core_type_to_text typ ^^ text " => " ^^ class_type_to_text ct
  | Pcty_extension ext -> extension_to_text ext
  | Pcty_open (od, ct) ->
    text "open "
    ^^ longident_loc od.popen_expr
    ^^ text "; "
    ^^ class_type_to_text ct

and class_type_field_to_text ctf =
  match ctf.pctf_desc with
  | Pctf_inherit ct -> text "inherit " ^^ class_type_to_text ct
  | Pctf_val ({ txt; _ }, Mutable, Virtual, typ) ->
    text ("val mutable virtual " ^ txt ^ ": ") ^^ core_type_to_text typ
  | Pctf_val ({ txt; _ }, Immutable, Virtual, typ) ->
    text ("val virtual " ^ txt ^ ": ") ^^ core_type_to_text typ
  | Pctf_val ({ txt; _ }, Mutable, Concrete, typ) ->
    text ("val mutable " ^ txt ^ ": ") ^^ core_type_to_text typ
  | Pctf_val ({ txt; _ }, Immutable, Concrete, typ) ->
    text ("val " ^ txt ^ ": ") ^^ core_type_to_text typ
  | Pctf_method ({ txt; _ }, Private, Virtual, typ) ->
    text ("pri virtual " ^ txt ^ ": ") ^^ core_type_to_text typ
  | Pctf_method ({ txt; _ }, Public, Virtual, typ) ->
    text ("pub virtual " ^ txt ^ ": ") ^^ core_type_to_text typ
  | Pctf_method ({ txt; _ }, Private, Concrete, typ) ->
    text ("pri " ^ txt ^ ": ") ^^ core_type_to_text typ
  | Pctf_method ({ txt; _ }, Public, Concrete, typ) ->
    text ("pub " ^ txt ^ ": ") ^^ core_type_to_text typ
  | Pctf_constraint (typ1, typ2) ->
    text "constraint "
    ^^ core_type_to_text typ1
    ^^ text " = "
    ^^ core_type_to_text typ2
  | Pctf_attribute attr -> attribute_to_text attr
  | Pctf_extension ext -> extension_to_text ext

and attribute_to_text attr = text ("[@" ^ attr.attr_name.txt ^ " ...]")
and extension_to_text (name, _payload) = text ("[%" ^ name.txt ^ " ...]")
and item_extension_to_text (name, _payload) = text ("[%%" ^ name.txt ^ " ...]")

and binding_op_to_text bop =
  text bop.pbop_op.txt
  ^^ text " "
  ^^ pattern_to_text bop.pbop_pat
  ^^ text " = "
  ^^ expression_to_text bop.pbop_exp

let make () =
  object (self)
    method expression ppf expr =
      let _width = current_settings.contents.width in
      P.pretty_print (Format.pp_print_string ppf) (expression_to_text expr)

    method pattern ppf pat =
      let _width = current_settings.contents.width in
      P.pretty_print (Format.pp_print_string ppf) (pattern_to_text pat)

    method core_type ppf typ =
      let _width = current_settings.contents.width in
      P.pretty_print (Format.pp_print_string ppf) (core_type_to_text typ)

    method structure _comments ppf items =
      let _width = current_settings.contents.width in
      let doc = separate hard_nl (List.map ~f:structure_item_to_text items) in
      P.pretty_print (Format.pp_print_string ppf) doc

    method signature _comments ppf items =
      let _width = current_settings.contents.width in
      let doc = separate hard_nl (List.map ~f:signature_item_to_text items) in
      P.pretty_print (Format.pp_print_string ppf) doc

    method case_list ppf cases =
      let _width = current_settings.contents.width in
      let doc =
        separate
          (text " ")
          (List.map
             ~f:(fun case ->
               text "| "
               ^^ pattern_to_text case.pc_lhs
               ^^ text " => "
               ^^ expression_to_text case.pc_rhs)
             cases)
      in
      P.pretty_print (Format.pp_print_string ppf) doc

    method toplevel_phrase ppf phrase =
      match phrase with
      | Ptop_def items -> self#structure [] ppf items
      | Ptop_dir _ -> assert false
  end
