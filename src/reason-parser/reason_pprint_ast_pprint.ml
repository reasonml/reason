open Ppxlib
open PPrint

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

(* Forward declarations for mutually recursive functions *)
let rec longident = function
  | Lident s -> string s
  | Ldot (path, s) -> longident path ^^ string "." ^^ string s
  | Lapply (l1, l2) -> longident l1 ^^ string "(" ^^ longident l2 ^^ string ")"

and longident_loc li = longident li.txt

and constant = function
  | Pconst_integer (i, None) -> string i
  | Pconst_integer (i, Some m) -> string i ^^ string (String.make 1 m)
  | Pconst_string (s, _, None) -> string ("\"" ^ String.escaped s ^ "\"")
  | Pconst_string (s, _, Some delim) ->
    string ("{" ^ delim ^ "|" ^ s ^ "|" ^ delim ^ "}")
  | Pconst_char c -> string ("'" ^ Char.escaped c ^ "'")
  | Pconst_float (f, None) -> string f
  | Pconst_float (f, Some m) -> string f ^^ string (String.make 1 m)

and expression_to_string expr =
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
      string ("(" ^ name ^ ")")
    | _ -> longident txt)
  | Pexp_constant c -> constant c
  | Pexp_let (_, vbs, e) ->
    let bindings =
      separate
        (string "; ")
        (List.map
           ~f:(fun vb ->
             string "let "
             ^^ pattern_to_string vb.pvb_pat
             ^^ string " = "
             ^^ expression_to_string vb.pvb_expr)
           vbs)
    in
    bindings ^^ string "; " ^^ expression_to_string e
  | Pexp_function (params, _, body) ->
    let params_doc =
      separate
        (string ", ")
        (List.map
           ~f:(fun param ->
             match param.pparam_desc with
             | Pparam_val (lbl, None, pat) ->
               (match lbl with
               | Nolabel -> pattern_to_string pat
               | Labelled s -> string ("~" ^ s)
               | Optional s -> string ("~" ^ s ^ "=?"))
             | Pparam_val (lbl, Some default, pat) ->
               let pat_doc = pattern_to_string pat in
               (match lbl with
               | Nolabel ->
                 pat_doc ^^ string "=" ^^ expression_to_string default
               | Labelled s ->
                 string ("~" ^ s ^ "=") ^^ expression_to_string default
               | Optional s ->
                 string ("~" ^ s ^ "=?") ^^ expression_to_string default)
             | Pparam_newtype { txt; _ } -> string ("type " ^ txt))
           params)
    in
    let body_doc =
      match body with
      | Pfunction_body e -> expression_to_string e
      | Pfunction_cases (cases, _, _) ->
        separate
          (string " ")
          (List.map
             ~f:(fun case ->
               string "| "
               ^^ pattern_to_string case.pc_lhs
               ^^ string " => "
               ^^ expression_to_string case.pc_rhs)
             cases)
    in
    string "(" ^^ params_doc ^^ string ") => " ^^ body_doc
  | Pexp_apply (func, args) ->
    let func_doc = expression_to_string func in
    let arg_strings =
      List.map
        ~f:(fun (lbl, arg) ->
          let arg_doc = expression_to_string arg in
          let labeled_doc =
            match lbl with
            | Nolabel -> arg_doc
            | Labelled s -> string ("~" ^ s ^ "=") ^^ arg_doc
            | Optional s -> string ("~" ^ s ^ "=?") ^^ arg_doc
          in
          labeled_doc)
        args
    in
    let args_doc =
      match arg_strings with
      | [] -> empty
      | [ single ] -> single
      | first :: rest ->
        List.fold_left
          ~f:(fun acc arg -> acc ^^ string ", " ^^ arg)
          ~init:first
          rest
    in
    func_doc ^^ string "(" ^^ args_doc ^^ string ")"
  | Pexp_match (e, cases) ->
    string "switch ("
    ^^ expression_to_string e
    ^^ string ") { "
    ^^ separate
         (string " ")
         (List.map
            ~f:(fun case ->
              string "| "
              ^^ pattern_to_string case.pc_lhs
              ^^ string " => "
              ^^ expression_to_string case.pc_rhs)
            cases)
    ^^ string " }"
  | Pexp_try (e, cases) ->
    string "try "
    ^^ expression_to_string e
    ^^ string " { "
    ^^ separate
         (string " ")
         (List.map
            ~f:(fun case ->
              string "| "
              ^^ pattern_to_string case.pc_lhs
              ^^ string " => "
              ^^ expression_to_string case.pc_rhs)
            cases)
    ^^ string " }"
  | Pexp_tuple exprs ->
    string "("
    ^^ separate (string ", ") (List.map ~f:expression_to_string exprs)
    ^^ string ")"
  | Pexp_construct ({ txt = Lident "()"; _ }, None) -> string "()"
  | Pexp_construct ({ txt = Lident "true"; _ }, None) -> string "true"
  | Pexp_construct ({ txt = Lident "false"; _ }, None) -> string "false"
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> string "[]"
  | Pexp_construct (li, None) -> longident_loc li
  | Pexp_construct (li, Some expr) ->
    longident_loc li ^^ string "(" ^^ expression_to_string expr ^^ string ")"
  | Pexp_variant (tag, None) -> string ("`" ^ tag)
  | Pexp_variant (tag, Some e) ->
    string ("`" ^ tag ^ "(") ^^ expression_to_string e ^^ string ")"
  | Pexp_record (fields, None) ->
    let fields_doc =
      separate
        (string ", ")
        (List.map
           ~f:(fun (li, e) ->
             longident_loc li ^^ string ": " ^^ expression_to_string e)
           fields)
    in
    string "{ " ^^ fields_doc ^^ string " }"
  | Pexp_record (fields, Some base) ->
    let fields_doc =
      separate
        (string ", ")
        (List.map
           ~f:(fun (li, e) ->
             longident_loc li ^^ string ": " ^^ expression_to_string e)
           fields)
    in
    string "{ ..."
    ^^ expression_to_string base
    ^^ string ", "
    ^^ fields_doc
    ^^ string " }"
  | Pexp_field (e, li) ->
    expression_to_string e ^^ string "." ^^ longident_loc li
  | Pexp_setfield (e1, li, e2) ->
    expression_to_string e1
    ^^ string "."
    ^^ longident_loc li
    ^^ string " = "
    ^^ expression_to_string e2
  | Pexp_array exprs ->
    string "[| "
    ^^ separate (string ", ") (List.map ~f:expression_to_string exprs)
    ^^ string " |]"
  | Pexp_ifthenelse (cond, then_, None) ->
    string "if ("
    ^^ expression_to_string cond
    ^^ string ") { "
    ^^ expression_to_string then_
    ^^ string " }"
  | Pexp_ifthenelse (cond, then_, Some else_) ->
    string "if ("
    ^^ expression_to_string cond
    ^^ string ") { "
    ^^ expression_to_string then_
    ^^ string " } else { "
    ^^ expression_to_string else_
    ^^ string " }"
  | Pexp_sequence (e1, e2) ->
    expression_to_string e1 ^^ string "; " ^^ expression_to_string e2
  | Pexp_while (cond, body) ->
    string "while ("
    ^^ expression_to_string cond
    ^^ string ") { "
    ^^ expression_to_string body
    ^^ string " }"
  | Pexp_for (pat, start, end_, dir, body) ->
    let dir_str = match dir with Upto -> "to" | Downto -> "downto" in
    string "for ("
    ^^ pattern_to_string pat
    ^^ string " in "
    ^^ expression_to_string start
    ^^ string (" " ^ dir_str ^ " ")
    ^^ expression_to_string end_
    ^^ string ") { "
    ^^ expression_to_string body
    ^^ string " }"
  | Pexp_constraint (e, typ) ->
    string "("
    ^^ expression_to_string e
    ^^ string ": "
    ^^ core_type_to_string typ
    ^^ string ")"
  | Pexp_coerce (e, None, typ) ->
    string "("
    ^^ expression_to_string e
    ^^ string " :> "
    ^^ core_type_to_string typ
    ^^ string ")"
  | Pexp_coerce (e, Some typ1, typ2) ->
    string "("
    ^^ expression_to_string e
    ^^ string ": "
    ^^ core_type_to_string typ1
    ^^ string " :> "
    ^^ core_type_to_string typ2
    ^^ string ")"
  | Pexp_send (e, { txt; _ }) ->
    expression_to_string e ^^ string "#" ^^ string txt
  | Pexp_new li -> string "new " ^^ longident_loc li
  | Pexp_setinstvar ({ txt; _ }, e) ->
    string txt ^^ string " = " ^^ expression_to_string e
  | Pexp_override fields ->
    let fields_doc =
      separate
        (string ", ")
        (List.map
           ~f:(fun ({ txt; _ }, e) ->
             string txt ^^ string ": " ^^ expression_to_string e)
           fields)
    in
    string "{< " ^^ fields_doc ^^ string " >}"
  | Pexp_letmodule ({ txt = Some name; _ }, me, e) ->
    string "let module "
    ^^ string name
    ^^ string " = "
    ^^ module_expr_to_string me
    ^^ string "; "
    ^^ expression_to_string e
  | Pexp_letmodule ({ txt = None; _ }, me, e) ->
    string "let module _ = "
    ^^ module_expr_to_string me
    ^^ string "; "
    ^^ expression_to_string e
  | Pexp_letexception (ec, e) ->
    string "let exception "
    ^^ extension_constructor_to_string ec
    ^^ string "; "
    ^^ expression_to_string e
  | Pexp_assert e -> string "assert(" ^^ expression_to_string e ^^ string ")"
  | Pexp_lazy e -> string "lazy(" ^^ expression_to_string e ^^ string ")"
  | Pexp_poly (e, None) -> expression_to_string e
  | Pexp_poly (e, Some typ) ->
    string "("
    ^^ expression_to_string e
    ^^ string ": "
    ^^ core_type_to_string typ
    ^^ string ")"
  | Pexp_object cs -> class_structure_to_string cs
  | Pexp_newtype ({ txt; _ }, e) ->
    string "(type " ^^ string txt ^^ string ") => " ^^ expression_to_string e
  | Pexp_pack me -> string "(module " ^^ module_expr_to_string me ^^ string ")"
  | Pexp_open (od, e) ->
    string "open "
    ^^ module_expr_to_string od.popen_expr
    ^^ string "; "
    ^^ expression_to_string e
  | Pexp_letop { let_; ands; body } ->
    let let_doc = binding_op_to_string let_ in
    let ands_doc =
      separate (string " ") (List.map ~f:binding_op_to_string ands)
    in
    let_doc
    ^^ string " "
    ^^ ands_doc
    ^^ string " in "
    ^^ expression_to_string body
  | Pexp_extension ext -> extension_to_string ext
  | Pexp_unreachable -> string "."

and pattern_to_string pat =
  match pat.ppat_desc with
  | Ppat_any -> string "_"
  | Ppat_var { txt; _ } ->
    (* Handle operators in parentheses *)
    if
      String.length txt > 0
      &&
      match txt.[0] with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> false
      | _ -> true
    then string ("(" ^ txt ^ ")")
    else string txt
  | Ppat_alias (p, { txt; _ }) ->
    pattern_to_string p ^^ string " as " ^^ string txt
  | Ppat_constant c -> constant c
  | Ppat_interval (c1, c2) -> constant c1 ^^ string " .. " ^^ constant c2
  | Ppat_tuple pats ->
    string "("
    ^^ separate (string ", ") (List.map ~f:pattern_to_string pats)
    ^^ string ")"
  | Ppat_construct ({ txt = Lident "()"; _ }, None) -> string "()"
  | Ppat_construct ({ txt = Lident "true"; _ }, None) -> string "true"
  | Ppat_construct ({ txt = Lident "false"; _ }, None) -> string "false"
  | Ppat_construct ({ txt = Lident "[]"; _ }, None) -> string "[]"
  | Ppat_construct (li, None) -> longident_loc li
  | Ppat_construct (li, Some (_, pat)) ->
    longident_loc li ^^ string "(" ^^ pattern_to_string pat ^^ string ")"
  | Ppat_variant (tag, None) -> string ("`" ^ tag)
  | Ppat_variant (tag, Some pat) ->
    string ("`" ^ tag ^ "(") ^^ pattern_to_string pat ^^ string ")"
  | Ppat_record (fields, Closed) ->
    let fields_doc =
      separate
        (string ", ")
        (List.map
           ~f:(fun (li, pat) ->
             longident_loc li ^^ string ": " ^^ pattern_to_string pat)
           fields)
    in
    string "{ " ^^ fields_doc ^^ string " }"
  | Ppat_record (fields, Open) ->
    let fields_doc =
      separate
        (string ", ")
        (List.map
           ~f:(fun (li, pat) ->
             longident_loc li ^^ string ": " ^^ pattern_to_string pat)
           fields)
    in
    string "{ " ^^ fields_doc ^^ string ", _ }"
  | Ppat_array pats ->
    string "[| "
    ^^ separate (string ", ") (List.map ~f:pattern_to_string pats)
    ^^ string " |]"
  | Ppat_or (p1, p2) ->
    pattern_to_string p1 ^^ string " | " ^^ pattern_to_string p2
  | Ppat_constraint (p, typ) ->
    string "("
    ^^ pattern_to_string p
    ^^ string ": "
    ^^ core_type_to_string typ
    ^^ string ")"
  | Ppat_type li -> string "#" ^^ longident_loc li
  | Ppat_lazy p -> string "lazy(" ^^ pattern_to_string p ^^ string ")"
  | Ppat_unpack { txt = Some name; _ } -> string ("(module " ^ name ^ ")")
  | Ppat_unpack { txt = None; _ } -> string "(module _)"
  | Ppat_exception p -> string "exception " ^^ pattern_to_string p
  | Ppat_extension ext -> extension_to_string ext
  | Ppat_open (li, p) ->
    longident_loc li ^^ string ".(" ^^ pattern_to_string p ^^ string ")"

and core_type_to_string typ =
  match typ.ptyp_desc with
  | Ptyp_any -> string "_"
  | Ptyp_var s -> string ("'" ^ s)
  | Ptyp_arrow (Nolabel, t1, t2) ->
    core_type_to_string t1 ^^ string " => " ^^ core_type_to_string t2
  | Ptyp_arrow (Labelled s, t1, t2) ->
    string ("~" ^ s ^ ": ")
    ^^ core_type_to_string t1
    ^^ string " => "
    ^^ core_type_to_string t2
  | Ptyp_arrow (Optional s, t1, t2) ->
    string ("~" ^ s ^ ": ")
    ^^ core_type_to_string t1
    ^^ string "=? => "
    ^^ core_type_to_string t2
  | Ptyp_tuple typs ->
    string "("
    ^^ separate (string ", ") (List.map ~f:core_type_to_string typs)
    ^^ string ")"
  | Ptyp_constr (li, []) -> longident_loc li
  | Ptyp_constr (li, args) ->
    longident_loc li
    ^^ string "("
    ^^ separate (string ", ") (List.map ~f:core_type_to_string args)
    ^^ string ")"
  | Ptyp_object (fields, Closed) ->
    let fields_doc =
      separate
        (string ", ")
        (List.map
           ~f:(fun field ->
             match field.pof_desc with
             | Otag ({ txt; _ }, typ) ->
               string ("\"" ^ txt ^ "\": ") ^^ core_type_to_string typ
             | Oinherit typ -> string "..." ^^ core_type_to_string typ)
           fields)
    in
    string "{ . " ^^ fields_doc ^^ string " }"
  | Ptyp_object (fields, Open) ->
    let fields_doc =
      separate
        (string ", ")
        (List.map
           ~f:(fun field ->
             match field.pof_desc with
             | Otag ({ txt; _ }, typ) ->
               string ("\"" ^ txt ^ "\": ") ^^ core_type_to_string typ
             | Oinherit typ -> string "..." ^^ core_type_to_string typ)
           fields)
    in
    string "{ .. " ^^ fields_doc ^^ string " }"
  | Ptyp_class (li, []) -> string "#" ^^ longident_loc li
  | Ptyp_class (li, args) ->
    string "#"
    ^^ longident_loc li
    ^^ string "("
    ^^ separate (string ", ") (List.map ~f:core_type_to_string args)
    ^^ string ")"
  | Ptyp_alias (typ, { txt; _ }) ->
    core_type_to_string typ ^^ string (" as '" ^ txt)
  | Ptyp_variant (rows, Closed, None) ->
    let rows_doc =
      separate (string " ") (List.map ~f:row_field_to_string rows)
    in
    string "[ " ^^ rows_doc ^^ string " ]"
  | Ptyp_variant (rows, Closed, Some tags) ->
    let rows_doc =
      separate (string " ") (List.map ~f:row_field_to_string rows)
    in
    let tags_doc =
      separate (string " ") (List.map ~f:(fun t -> string ("`" ^ t)) tags)
    in
    string "[ " ^^ rows_doc ^^ string " > " ^^ tags_doc ^^ string " ]"
  | Ptyp_variant (rows, Open, _) ->
    let rows_doc =
      separate (string " ") (List.map ~f:row_field_to_string rows)
    in
    string "[ > " ^^ rows_doc ^^ string " ]"
  | Ptyp_poly ([], typ) -> core_type_to_string typ
  | Ptyp_poly (vars, typ) ->
    separate
      (string " ")
      (List.map ~f:(fun { txt; _ } -> string ("'" ^ txt)) vars)
    ^^ string ". "
    ^^ core_type_to_string typ
  | Ptyp_package (li, []) -> string "module " ^^ longident_loc li
  | Ptyp_package (li, constrs) ->
    let constrs_doc =
      separate
        (string " and ")
        (List.map
           ~f:(fun (li', typ) ->
             string "type "
             ^^ longident_loc li'
             ^^ string " = "
             ^^ core_type_to_string typ)
           constrs)
    in
    string "module " ^^ longident_loc li ^^ string " with " ^^ constrs_doc
  | Ptyp_extension ext -> extension_to_string ext
  | Ptyp_open (m, typ) ->
    longident m.txt ^^ string ".(" ^^ core_type_to_string typ ^^ string ")"

and row_field_to_string field =
  match field.prf_desc with
  | Rtag ({ txt; _ }, _, []) -> string ("| `" ^ txt)
  | Rtag ({ txt; _ }, _, typs) ->
    string ("| `" ^ txt ^ "(")
    ^^ separate (string ", ") (List.map ~f:core_type_to_string typs)
    ^^ string ")"
  | Rinherit typ -> string "| " ^^ core_type_to_string typ

and structure_item_to_string item =
  match item.pstr_desc with
  | Pstr_eval (e, _) -> expression_to_string e
  | Pstr_value (Nonrecursive, vbs) ->
    separate
      (string "; ")
      (List.map
         ~f:(fun vb ->
           string "let "
           ^^ pattern_to_string vb.pvb_pat
           ^^ string " = "
           ^^ expression_to_string vb.pvb_expr)
         vbs)
  | Pstr_value (Recursive, vbs) ->
    separate
      (string " and ")
      (List.mapi
         ~f:(fun i vb ->
           string (if i = 0 then "let rec " else "and ")
           ^^ pattern_to_string vb.pvb_pat
           ^^ string " = "
           ^^ expression_to_string vb.pvb_expr)
         vbs)
  | Pstr_primitive vd ->
    string "external "
    ^^ string vd.pval_name.txt
    ^^ string ": "
    ^^ core_type_to_string vd.pval_type
    ^^ string " = "
    ^^ separate
         (string " ")
         (List.map ~f:(fun s -> string ("\"" ^ s ^ "\"")) vd.pval_prim)
  | Pstr_type (_, tds) ->
    separate
      (string " and ")
      (List.mapi
         ~f:(fun i td ->
           string (if i = 0 then "type " else "")
           ^^ type_declaration_to_string td)
         tds)
  | Pstr_typext te ->
    string "type "
    ^^ longident_loc te.ptyext_path
    ^^ string " += "
    ^^ separate
         (string " ")
         (List.map ~f:extension_constructor_to_string te.ptyext_constructors)
  | Pstr_exception ec ->
    string "exception " ^^ extension_constructor_to_string ec.ptyexn_constructor
  | Pstr_module { pmb_name; pmb_expr; _ } ->
    let name = match pmb_name.txt with Some n -> n | None -> "_" in
    string "module "
    ^^ string name
    ^^ string " = "
    ^^ module_expr_to_string pmb_expr
  | Pstr_recmodule mbs ->
    separate
      (string " and ")
      (List.mapi
         ~f:(fun i mb ->
           let name = match mb.pmb_name.txt with Some n -> n | None -> "_" in
           string (if i = 0 then "module rec " else "and ")
           ^^ string name
           ^^ string " = "
           ^^ module_expr_to_string mb.pmb_expr)
         mbs)
  | Pstr_modtype { pmtd_name; pmtd_type = None; _ } ->
    string ("module type " ^ pmtd_name.txt)
  | Pstr_modtype { pmtd_name; pmtd_type = Some mt; _ } ->
    string ("module type " ^ pmtd_name.txt)
    ^^ string " = "
    ^^ module_type_to_string mt
  | Pstr_open od -> string "open " ^^ module_expr_to_string od.popen_expr
  | Pstr_class cds ->
    separate
      (string " and ")
      (List.mapi
         ~f:(fun i cd ->
           string (if i = 0 then "class " else "and ")
           ^^ string cd.pci_name.txt
           ^^ (match cd.pci_params with
             | [] -> empty
             | params ->
               string "("
               ^^ separate
                    (string ", ")
                    (List.map
                       ~f:(fun (typ, _) -> core_type_to_string typ)
                       params)
               ^^ string ") ")
           ^^ string " = "
           ^^ class_expr_to_string cd.pci_expr)
         cds)
  | Pstr_class_type ctds ->
    separate
      (string " and ")
      (List.mapi
         ~f:(fun i ctd ->
           string (if i = 0 then "class type " else "and ")
           ^^ string ctd.pci_name.txt
           ^^ (match ctd.pci_params with
             | [] -> empty
             | params ->
               string "("
               ^^ separate
                    (string ", ")
                    (List.map
                       ~f:(fun (typ, _) -> core_type_to_string typ)
                       params)
               ^^ string ") ")
           ^^ string " = "
           ^^ class_type_to_string ctd.pci_expr)
         ctds)
  | Pstr_include { pincl_mod; _ } ->
    string "include " ^^ module_expr_to_string pincl_mod
  | Pstr_attribute attr -> attribute_to_string attr
  | Pstr_extension (ext, _) -> item_extension_to_string ext

and signature_item_to_string item =
  match item.psig_desc with
  | Psig_value vd ->
    string "let "
    ^^ string vd.pval_name.txt
    ^^ string ": "
    ^^ core_type_to_string vd.pval_type
  | Psig_type (_, tds) ->
    separate
      (string " and ")
      (List.mapi
         ~f:(fun i td ->
           string (if i = 0 then "type " else "")
           ^^ type_declaration_to_string td)
         tds)
  | Psig_typext te ->
    string "type "
    ^^ longident_loc te.ptyext_path
    ^^ string " += "
    ^^ separate
         (string " ")
         (List.map ~f:extension_constructor_to_string te.ptyext_constructors)
  | Psig_exception ec ->
    string "exception " ^^ extension_constructor_to_string ec.ptyexn_constructor
  | Psig_module { pmd_name; pmd_type; _ } ->
    let name = match pmd_name.txt with Some n -> n | None -> "_" in
    string "module "
    ^^ string name
    ^^ string ": "
    ^^ module_type_to_string pmd_type
  | Psig_recmodule mds ->
    separate
      (string " and ")
      (List.mapi
         ~f:(fun i md ->
           let name = match md.pmd_name.txt with Some n -> n | None -> "_" in
           string (if i = 0 then "module rec " else "and ")
           ^^ string name
           ^^ string ": "
           ^^ module_type_to_string md.pmd_type)
         mds)
  | Psig_modtype { pmtd_name; pmtd_type = None; _ } ->
    string ("module type " ^ pmtd_name.txt)
  | Psig_modtype { pmtd_name; pmtd_type = Some mt; _ } ->
    string ("module type " ^ pmtd_name.txt)
    ^^ string " = "
    ^^ module_type_to_string mt
  | Psig_open od -> string "open " ^^ longident_loc od.popen_expr
  | Psig_include { pincl_mod; _ } ->
    string "include " ^^ module_type_to_string pincl_mod
  | Psig_class cds ->
    separate
      (string " and ")
      (List.mapi
         ~f:(fun i cd ->
           string (if i = 0 then "class " else "and ")
           ^^ string cd.pci_name.txt
           ^^ (match cd.pci_params with
             | [] -> empty
             | params ->
               string "("
               ^^ separate
                    (string ", ")
                    (List.map
                       ~f:(fun (typ, _) -> core_type_to_string typ)
                       params)
               ^^ string ") ")
           ^^ string ": "
           ^^ class_type_to_string cd.pci_expr)
         cds)
  | Psig_class_type ctds ->
    separate
      (string " and ")
      (List.mapi
         ~f:(fun i ctd ->
           string (if i = 0 then "class type " else "and ")
           ^^ string ctd.pci_name.txt
           ^^ (match ctd.pci_params with
             | [] -> empty
             | params ->
               string "("
               ^^ separate
                    (string ", ")
                    (List.map
                       ~f:(fun (typ, _) -> core_type_to_string typ)
                       params)
               ^^ string ") ")
           ^^ string " = "
           ^^ class_type_to_string ctd.pci_expr)
         ctds)
  | Psig_attribute attr -> attribute_to_string attr
  | Psig_extension (ext, _) -> item_extension_to_string ext
  | Psig_modsubst { pms_name; pms_manifest; _ } ->
    string ("module " ^ pms_name.txt)
    ^^ string " := "
    ^^ longident_loc pms_manifest
  | Psig_typesubst tds ->
    separate
      (string " and ")
      (List.mapi
         ~f:(fun i td ->
           string (if i = 0 then "type " else "and ")
           ^^ type_declaration_to_string td
           ^^ string " := ...")
         tds)
  | Psig_modtypesubst { pmtd_name; pmtd_type = Some mt; _ } ->
    string ("module type " ^ pmtd_name.txt)
    ^^ string " := "
    ^^ module_type_to_string mt
  | Psig_modtypesubst { pmtd_name; pmtd_type = None; _ } ->
    string ("module type " ^ pmtd_name.txt ^ " := ...")

and type_declaration_to_string td =
  let params =
    match td.ptype_params with
    | [] -> empty
    | ps ->
      string "("
      ^^ separate
           (string ", ")
           (List.map ~f:(fun (typ, _) -> core_type_to_string typ) ps)
      ^^ string ") "
  in
  let name = string td.ptype_name.txt in
  let manifest =
    match td.ptype_manifest with
    | None -> empty
    | Some typ -> string " = " ^^ core_type_to_string typ
  in
  let kind =
    match td.ptype_kind with
    | Ptype_abstract -> empty
    | Ptype_variant _constrs -> string " = "
    | Ptype_record fields ->
      string " = { "
      ^^ separate
           (string ", ")
           (List.map
              ~f:(fun ld ->
                string ld.pld_name.txt
                ^^ string ": "
                ^^ core_type_to_string ld.pld_type)
              fields)
      ^^ string " }"
    | Ptype_open -> string " = .."
  in
  (* Type parameters come after the name in Reason syntax *)
  name
  ^^ (match td.ptype_params with [] -> empty | _ -> params)
  ^^ manifest
  ^^ kind

and extension_constructor_to_string ec =
  let name = string ec.pext_name.txt in
  match ec.pext_kind with
  | Pext_decl (_, Pcstr_tuple [], None) -> name
  | Pext_decl (_, Pcstr_tuple args, ret_type) ->
    (match ret_type with
    | None ->
      name
      ^^ string "("
      ^^ separate (string ", ") (List.map ~f:core_type_to_string args)
      ^^ string ")"
    | Some typ ->
      name
      ^^ string "("
      ^^ separate (string ", ") (List.map ~f:core_type_to_string args)
      ^^ string "): "
      ^^ core_type_to_string typ)
  | Pext_decl (_, Pcstr_record _, ret_type) ->
    (match ret_type with
    | None -> name ^^ string "({ ... })"
    | Some typ -> name ^^ string "({ ... }): " ^^ core_type_to_string typ)
  | Pext_rebind li -> name ^^ string " = " ^^ longident_loc li

and module_expr_to_string me =
  match me.pmod_desc with
  | Pmod_ident li -> longident_loc li
  | Pmod_structure items ->
    string "{ "
    ^^ separate (string "; ") (List.map ~f:structure_item_to_string items)
    ^^ string " }"
  | Pmod_functor (Unit, body) -> string "() => " ^^ module_expr_to_string body
  | Pmod_functor (Named ({ txt = Some name; _ }, mt), body) ->
    string ("(" ^ name ^ ": ")
    ^^ module_type_to_string mt
    ^^ string ") => "
    ^^ module_expr_to_string body
  | Pmod_functor (Named ({ txt = None; _ }, mt), body) ->
    string "(_: "
    ^^ module_type_to_string mt
    ^^ string ") => "
    ^^ module_expr_to_string body
  | Pmod_apply (m1, m2) ->
    module_expr_to_string m1
    ^^ string "("
    ^^ module_expr_to_string m2
    ^^ string ")"
  | Pmod_apply_unit m -> module_expr_to_string m ^^ string "()"
  | Pmod_constraint (me, mt) ->
    string "("
    ^^ module_expr_to_string me
    ^^ string ": "
    ^^ module_type_to_string mt
    ^^ string ")"
  | Pmod_unpack e -> string "(val " ^^ expression_to_string e ^^ string ")"
  | Pmod_extension ext -> extension_to_string ext

and module_type_to_string mt =
  match mt.pmty_desc with
  | Pmty_ident li -> longident_loc li
  | Pmty_signature items ->
    string "{ "
    ^^ separate (string "; ") (List.map ~f:signature_item_to_string items)
    ^^ string " }"
  | Pmty_functor (Unit, body) -> string "() => " ^^ module_type_to_string body
  | Pmty_functor (Named ({ txt = Some name; _ }, mt1), mt2) ->
    string ("(" ^ name ^ ": ")
    ^^ module_type_to_string mt1
    ^^ string ") => "
    ^^ module_type_to_string mt2
  | Pmty_functor (Named ({ txt = None; _ }, mt1), mt2) ->
    string "(_: "
    ^^ module_type_to_string mt1
    ^^ string ") => "
    ^^ module_type_to_string mt2
  | Pmty_with (mt, constrs) ->
    module_type_to_string mt
    ^^ string " with "
    ^^ separate (string " and ") (List.map ~f:with_constraint_to_string constrs)
  | Pmty_typeof me -> string "module type of " ^^ module_expr_to_string me
  | Pmty_extension ext -> extension_to_string ext
  | Pmty_alias li -> string "module " ^^ longident_loc li

and with_constraint_to_string = function
  | Pwith_type (li, td) ->
    string "type "
    ^^ longident_loc li
    ^^ string " = "
    ^^ type_declaration_to_string td
  | Pwith_module (li1, li2) ->
    string "module " ^^ longident_loc li1 ^^ string " = " ^^ longident_loc li2
  | Pwith_modtype (li, mt) ->
    string "module type "
    ^^ longident li.txt
    ^^ string " = "
    ^^ module_type_to_string mt
  | Pwith_typesubst (li, td) ->
    (* For destructive substitution, we need to include the type parameters *)
    let params_and_name =
      match td.ptype_params with
      | [] -> longident_loc li
      | ps ->
        let params_doc =
          string "("
          ^^ separate
               (string ", ")
               (List.map ~f:(fun (typ, _) -> core_type_to_string typ) ps)
          ^^ string ") "
        in
        params_doc ^^ longident_loc li
    in
    string "type "
    ^^ params_and_name
    ^^ string " := "
    ^^
      (match td.ptype_manifest with
      | None -> type_declaration_to_string td
      | Some typ -> core_type_to_string typ)
  | Pwith_modsubst (li1, li2) ->
    string "module " ^^ longident_loc li1 ^^ string " := " ^^ longident_loc li2
  | Pwith_modtypesubst (li, mt) ->
    string "module type "
    ^^ longident li.txt
    ^^ string " := "
    ^^ module_type_to_string mt

and class_expr_to_string ce =
  match ce.pcl_desc with
  | Pcl_constr (li, []) -> longident_loc li
  | Pcl_constr (li, typs) ->
    longident_loc li
    ^^ string "("
    ^^ separate (string ", ") (List.map ~f:core_type_to_string typs)
    ^^ string ")"
  | Pcl_structure cs -> class_structure_to_string cs
  | Pcl_fun (lbl, None, pat, ce) ->
    let lbl_doc =
      match lbl with
      | Nolabel -> empty
      | Labelled s -> string ("~" ^ s)
      | Optional s -> string ("~" ^ s ^ "=?")
    in
    lbl_doc ^^ pattern_to_string pat ^^ string " => " ^^ class_expr_to_string ce
  | Pcl_fun (lbl, Some default, pat, ce) ->
    let lbl_doc =
      match lbl with
      | Nolabel -> empty
      | Labelled s -> string ("~" ^ s ^ "=")
      | Optional s -> string ("~" ^ s ^ "=?")
    in
    lbl_doc
    ^^ pattern_to_string pat
    ^^ string "="
    ^^ expression_to_string default
    ^^ string " => "
    ^^ class_expr_to_string ce
  | Pcl_apply (ce, args) ->
    class_expr_to_string ce
    ^^ string "("
    ^^ separate
         (string ", ")
         (List.map
            ~f:(fun (lbl, e) ->
              let lbl_doc =
                match lbl with
                | Nolabel -> empty
                | Labelled s -> string ("~" ^ s ^ "=")
                | Optional s -> string ("~" ^ s ^ "=?")
              in
              lbl_doc ^^ expression_to_string e)
            args)
    ^^ string ")"
  | Pcl_let (_, vbs, ce) ->
    separate
      (string "; ")
      (List.map
         ~f:(fun vb ->
           string "let "
           ^^ pattern_to_string vb.pvb_pat
           ^^ string " = "
           ^^ expression_to_string vb.pvb_expr)
         vbs)
    ^^ string "; "
    ^^ class_expr_to_string ce
  | Pcl_constraint (ce, ct) ->
    string "("
    ^^ class_expr_to_string ce
    ^^ string ": "
    ^^ class_type_to_string ct
    ^^ string ")"
  | Pcl_extension ext -> extension_to_string ext
  | Pcl_open (od, ce) ->
    string "open "
    ^^ longident_loc od.popen_expr
    ^^ string "; "
    ^^ class_expr_to_string ce

and class_structure_to_string cs =
  let self_pat =
    match cs.pcstr_self.ppat_desc with
    | Ppat_var { txt = "this"; _ } -> empty
    | _ -> string " as " ^^ pattern_to_string cs.pcstr_self
  in
  string "{ "
  ^^ self_pat
  ^^ string " "
  ^^ separate (string "; ") (List.map ~f:class_field_to_string cs.pcstr_fields)
  ^^ string " }"

and class_field_to_string cf =
  match cf.pcf_desc with
  | Pcf_inherit (_, ce, None) -> string "inherit " ^^ class_expr_to_string ce
  | Pcf_inherit (_, ce, Some { txt; _ }) ->
    string "inherit " ^^ class_expr_to_string ce ^^ string (" as " ^ txt)
  | Pcf_val ({ txt; _ }, Mutable, Cfk_concrete (_, e)) ->
    string ("val mutable " ^ txt ^ " = ") ^^ expression_to_string e
  | Pcf_val ({ txt; _ }, Immutable, Cfk_concrete (_, e)) ->
    string ("val " ^ txt ^ " = ") ^^ expression_to_string e
  | Pcf_val ({ txt; _ }, Mutable, Cfk_virtual typ) ->
    string ("val mutable virtual " ^ txt ^ ": ") ^^ core_type_to_string typ
  | Pcf_val ({ txt; _ }, Immutable, Cfk_virtual typ) ->
    string ("val virtual " ^ txt ^ ": ") ^^ core_type_to_string typ
  | Pcf_method ({ txt; _ }, Private, Cfk_concrete (_, e)) ->
    string ("pri " ^ txt ^ " = ") ^^ expression_to_string e
  | Pcf_method ({ txt; _ }, Public, Cfk_concrete (_, e)) ->
    string ("pub " ^ txt ^ " = ") ^^ expression_to_string e
  | Pcf_method ({ txt; _ }, Private, Cfk_virtual typ) ->
    string ("pri virtual " ^ txt ^ ": ") ^^ core_type_to_string typ
  | Pcf_method ({ txt; _ }, Public, Cfk_virtual typ) ->
    string ("pub virtual " ^ txt ^ ": ") ^^ core_type_to_string typ
  | Pcf_constraint (typ1, typ2) ->
    string "constraint "
    ^^ core_type_to_string typ1
    ^^ string " = "
    ^^ core_type_to_string typ2
  | Pcf_initializer e -> string "initializer " ^^ expression_to_string e
  | Pcf_attribute attr -> attribute_to_string attr
  | Pcf_extension ext -> extension_to_string ext

and class_type_to_string ct =
  match ct.pcty_desc with
  | Pcty_constr (li, []) -> longident_loc li
  | Pcty_constr (li, typs) ->
    longident_loc li
    ^^ string "("
    ^^ separate (string ", ") (List.map ~f:core_type_to_string typs)
    ^^ string ")"
  | Pcty_signature cs ->
    string "{ "
    ^^ separate
         (string "; ")
         (List.map ~f:class_type_field_to_string cs.pcsig_fields)
    ^^ string " }"
  | Pcty_arrow (lbl, typ, ct) ->
    let lbl_doc =
      match lbl with
      | Nolabel -> empty
      | Labelled s -> string ("~" ^ s ^ ": ")
      | Optional s -> string ("~" ^ s ^ ": ")
    in
    lbl_doc
    ^^ core_type_to_string typ
    ^^ string " => "
    ^^ class_type_to_string ct
  | Pcty_extension ext -> extension_to_string ext
  | Pcty_open (od, ct) ->
    string "open "
    ^^ longident_loc od.popen_expr
    ^^ string "; "
    ^^ class_type_to_string ct

and class_type_field_to_string ctf =
  match ctf.pctf_desc with
  | Pctf_inherit ct -> string "inherit " ^^ class_type_to_string ct
  | Pctf_val ({ txt; _ }, Mutable, Virtual, typ) ->
    string ("val mutable virtual " ^ txt ^ ": ") ^^ core_type_to_string typ
  | Pctf_val ({ txt; _ }, Immutable, Virtual, typ) ->
    string ("val virtual " ^ txt ^ ": ") ^^ core_type_to_string typ
  | Pctf_val ({ txt; _ }, Mutable, Concrete, typ) ->
    string ("val mutable " ^ txt ^ ": ") ^^ core_type_to_string typ
  | Pctf_val ({ txt; _ }, Immutable, Concrete, typ) ->
    string ("val " ^ txt ^ ": ") ^^ core_type_to_string typ
  | Pctf_method ({ txt; _ }, Private, Virtual, typ) ->
    string ("pri virtual " ^ txt ^ ": ") ^^ core_type_to_string typ
  | Pctf_method ({ txt; _ }, Public, Virtual, typ) ->
    string ("pub virtual " ^ txt ^ ": ") ^^ core_type_to_string typ
  | Pctf_method ({ txt; _ }, Private, Concrete, typ) ->
    string ("pri " ^ txt ^ ": ") ^^ core_type_to_string typ
  | Pctf_method ({ txt; _ }, Public, Concrete, typ) ->
    string ("pub " ^ txt ^ ": ") ^^ core_type_to_string typ
  | Pctf_constraint (typ1, typ2) ->
    string "constraint "
    ^^ core_type_to_string typ1
    ^^ string " = "
    ^^ core_type_to_string typ2
  | Pctf_attribute attr -> attribute_to_string attr
  | Pctf_extension ext -> extension_to_string ext

and attribute_to_string attr = string ("[@" ^ attr.attr_name.txt ^ " ...]")
and extension_to_string (name, _payload) = string ("[%" ^ name.txt ^ " ...]")

and item_extension_to_string (name, _payload) =
  string ("[%%" ^ name.txt ^ " ...]")

and binding_op_to_string bop =
  string bop.pbop_op.txt
  ^^ string " "
  ^^ pattern_to_string bop.pbop_pat
  ^^ string " = "
  ^^ expression_to_string bop.pbop_exp

let make () =
  object (self)
    method expression ppf expr =
      let width = current_settings.contents.width in
      PPrint.ToFormatter.pretty 1.0 width ppf (expression_to_string expr)

    method pattern ppf pat =
      let width = current_settings.contents.width in
      PPrint.ToFormatter.pretty 1.0 width ppf (pattern_to_string pat)

    method core_type ppf typ =
      let width = current_settings.contents.width in
      PPrint.ToFormatter.pretty 1.0 width ppf (core_type_to_string typ)

    method structure _comments ppf items =
      let doc =
        separate hardline (List.map ~f:structure_item_to_string items)
      in
      let width = current_settings.contents.width in
      PPrint.ToFormatter.pretty 1.0 width ppf doc

    method signature _comments ppf items =
      let doc =
        separate hardline (List.map ~f:signature_item_to_string items)
      in
      let width = current_settings.contents.width in
      PPrint.ToFormatter.pretty 1.0 width ppf doc

    method case_list ppf cases =
      let doc =
        separate
          (string " ")
          (List.map
             ~f:(fun case ->
               string "| "
               ^^ pattern_to_string case.pc_lhs
               ^^ string " => "
               ^^ expression_to_string case.pc_rhs)
             cases)
      in
      let width = current_settings.contents.width in
      PPrint.ToFormatter.pretty 1.0 width ppf doc

    method toplevel_phrase ppf phrase =
      match phrase with
      | Ptop_def items -> self#structure [] ppf items
      | Ptop_dir _ -> assert false
  end
