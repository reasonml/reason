open Ppxlib

type settings =
  { width : int
  ; assumeExplicitArity : bool
  ; constructorLists : string list
  }

let default_settings =
  { width = 80; assumeExplicitArity = false; constructorLists = [] }

let current_settings = ref default_settings
let use_new_printer = ref false

let configure ~width ~assumeExplicitArity ~constructorLists =
  current_settings := { width; assumeExplicitArity; constructorLists }

let enable_new_printer () = use_new_printer := true
let is_new_printer_enabled () = !use_new_printer

let cost_factory =
  Pretty_expressive.Printer.default_cost_factory
    ~page_width:(fun () -> !current_settings.width)
    ()

module P = Pretty_expressive.Printer.Make ((val cost_factory))
open P

exception Invalid_parsetree of string

let separate sep docs =
  match docs with
  | [] -> empty
  | [ x ] -> x
  | x :: xs -> List.fold_left ~f:(fun acc d -> acc ^^ sep ^^ d) ~init:x xs

let current_comment_table : Reason_comment_table.t option ref = ref None

let with_comments loc doc =
  match !current_comment_table with
  | None -> doc
  | Some tbl ->
    Reason_comment_table.print_comments
      ~text
      ~concat:( ^^ )
      ~space:(text " ")
      ~hard_line:hard_nl
      ~empty
      doc
      tbl
      loc

let get_jsx_prop_loc expr =
  List.find_map
    ~f:(fun attr ->
      if attr.attr_name.txt = "reason.jsx_prop_loc"
      then Some attr.attr_name.loc
      else None)
    expr.pexp_attributes

let variance_to_text = function
  | NoVariance -> empty
  | Covariant -> text "+"
  | Contravariant -> text "-"

let rec longident = function
  | Lident s -> text s
  | Ldot (path, s) -> longident path ^^ text "." ^^ text s
  | Lapply (l1, l2) -> longident l1 ^^ text "(" ^^ longident l2 ^^ text ")"

and longident_loc li = longident li.txt

and constant = function
  | Pconst_integer (i, None) -> text i
  | Pconst_integer (i, Some m) -> text i ^^ text (String.make 1 m)
  | Pconst_string (s, _loc, None) -> text ("\"" ^ String.escaped s ^ "\"")
  | Pconst_string (s, _loc, Some delim) ->
    text ("{" ^ delim ^ "|" ^ s ^ "|" ^ delim ^ "}")
  | Pconst_char c -> text ("'" ^ Char.escaped c ^ "'")
  | Pconst_float (f, None) -> text f
  | Pconst_float (f, Some m) -> text f ^^ text (String.make 1 m)

and expression_to_doc expr =
  match expr.pexp_desc with
  | Pexp_ident { txt; loc = _ } ->
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
  | Pexp_let (Nonrecursive, vbs, e) ->
    let bindings =
      separate
        (text "; ")
        (List.map
           ~f:(fun vb ->
             text "let "
             ^^ pattern_to_doc vb.pvb_pat
             ^^ text " = "
             ^^ expression_to_doc vb.pvb_expr)
           vbs)
    in
    bindings ^^ text "; " ^^ expression_to_doc e
  | Pexp_let (Recursive, vbs, e) ->
    let bindings =
      separate
        (text "; ")
        (List.map
           ~f:(fun vb ->
             text "let "
             ^^ pattern_to_doc vb.pvb_pat
             ^^ text " = "
             ^^ expression_to_doc vb.pvb_expr)
           vbs)
    in
    bindings ^^ text "; " ^^ expression_to_doc e
  | Pexp_function (params, constraint_opt, body) ->
    let params_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun param ->
             match param.pparam_desc with
             | Pparam_val (lbl, None, pat) ->
               with_comments
                 pat.ppat_loc
                 (match lbl with
                 | Nolabel -> pattern_to_doc pat
                 | Labelled s -> text ("~" ^ s)
                 | Optional s -> text ("~" ^ s ^ "=?"))
             | Pparam_val (lbl, Some default, pat) ->
               let default_doc =
                 with_comments default.pexp_loc (expression_to_doc default)
               in
               with_comments
                 pat.ppat_loc
                 (match lbl with
                 | Nolabel -> pattern_to_doc pat ^^ text "=" ^^ default_doc
                 | Labelled s -> text ("~" ^ s ^ "=") ^^ default_doc
                 | Optional s -> text ("~" ^ s ^ "=?") ^^ default_doc)
             | Pparam_newtype { txt; loc } ->
               with_comments loc (text ("type " ^ txt)))
           params)
    in
    let constraint_doc =
      match constraint_opt with
      | None -> empty
      | Some (Pconstraint typ) ->
        text ": " ^^ with_comments typ.ptyp_loc (core_type_to_doc typ)
      | Some (Pcoerce (None, typ)) ->
        text " :> " ^^ with_comments typ.ptyp_loc (core_type_to_doc typ)
      | Some (Pcoerce (Some typ1, typ2)) ->
        text ": "
        ^^ with_comments typ1.ptyp_loc (core_type_to_doc typ1)
        ^^ text " :> "
        ^^ with_comments typ2.ptyp_loc (core_type_to_doc typ2)
    in
    let body_doc =
      match body with
      | Pfunction_body e -> with_comments e.pexp_loc (expression_to_doc e)
      | Pfunction_cases (cases, _loc, attrs) ->
        attributes_to_doc attrs
        ^^ separate
             (text " ")
             (List.map
                ~f:(fun case ->
                  text "| "
                  ^^ with_comments
                       case.pc_lhs.ppat_loc
                       (pattern_to_doc case.pc_lhs)
                  ^^ text " => "
                  ^^ with_comments
                       case.pc_rhs.pexp_loc
                       (expression_to_doc case.pc_rhs))
                cases)
    in
    text "("
    ^^ params_doc
    ^^ text ")"
    ^^ constraint_doc
    ^^ text " => "
    ^^ body_doc
  | Pexp_apply (func, args) ->
    (match expr.pexp_attributes with
    | [ { attr_name = { txt = "JSX"; loc = _ }; attr_payload = PStr []; _ } ] ->
      jsx_pexp_apply_to_doc func args
    | _ -> pexp_apply_to_doc func args)
  | Pexp_match (e, cases) ->
    text "switch ("
    ^^ with_comments e.pexp_loc (expression_to_doc e)
    ^^ text ") {"
    ^^ separate
         empty
         (List.map
            ~f:(fun case ->
              let case_loc =
                { case.pc_lhs.ppat_loc with
                  Location.loc_end = case.pc_rhs.pexp_loc.Location.loc_end
                }
              in
              with_comments
                case_loc
                (text " | "
                ^^ with_comments
                     case.pc_lhs.ppat_loc
                     (pattern_to_doc case.pc_lhs)
                ^^ text " => "
                ^^ with_comments
                     case.pc_rhs.pexp_loc
                     (expression_to_doc case.pc_rhs)))
            cases)
    ^^ text " }"
  | Pexp_try (e, cases) ->
    text "try ("
    ^^ with_comments e.pexp_loc (expression_to_doc e)
    ^^ text ") {"
    ^^ separate
         empty
         (List.map
            ~f:(fun case ->
              let case_loc =
                { case.pc_lhs.ppat_loc with
                  Location.loc_end = case.pc_rhs.pexp_loc.Location.loc_end
                }
              in
              with_comments
                case_loc
                (text " | "
                ^^ with_comments
                     case.pc_lhs.ppat_loc
                     (pattern_to_doc case.pc_lhs)
                ^^ text " => "
                ^^ with_comments
                     case.pc_rhs.pexp_loc
                     (expression_to_doc case.pc_rhs)))
            cases)
    ^^ text " }"
  | Pexp_tuple exprs ->
    text "("
    ^^ separate
         (text ", ")
         (List.map
            ~f:(fun e -> with_comments e.pexp_loc (expression_to_doc e))
            exprs)
    ^^ text ")"
  | Pexp_construct ({ txt = Lident "()"; loc = _ }, None) -> text "()"
  | Pexp_construct ({ txt = Lident "true"; loc = _ }, None) -> text "true"
  | Pexp_construct ({ txt = Lident "false"; loc = _ }, None) -> text "false"
  | Pexp_construct ({ txt = Lident "[]"; loc = _ }, None) -> text "[]"
  | Pexp_construct (li, None) -> longident_loc li
  | Pexp_construct (li, Some expr) ->
    longident_loc li
    ^^ text "("
    ^^ with_comments expr.pexp_loc (expression_to_doc expr)
    ^^ text ")"
  | Pexp_variant (tag, None) -> text ("`" ^ tag)
  | Pexp_variant (tag, Some e) ->
    text ("`" ^ tag ^ "(")
    ^^ with_comments e.pexp_loc (expression_to_doc e)
    ^^ text ")"
  | Pexp_record (fields, None) ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun (li, e) ->
             let row_loc =
               { li.loc with Location.loc_end = e.pexp_loc.Location.loc_end }
             in
             with_comments
               row_loc
               (with_comments li.loc (longident_loc li)
               ^^ text ": "
               ^^ with_comments e.pexp_loc (expression_to_doc e)))
           fields)
    in
    text "{ " ^^ fields_doc ^^ text " }"
  | Pexp_record (fields, Some base) ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun (li, e) ->
             let row_loc =
               { li.loc with Location.loc_end = e.pexp_loc.Location.loc_end }
             in
             with_comments
               row_loc
               (with_comments li.loc (longident_loc li)
               ^^ text ": "
               ^^ with_comments e.pexp_loc (expression_to_doc e)))
           fields)
    in
    text "{ "
    ^^ with_comments base.pexp_loc (expression_to_doc base)
    ^^ text ", "
    ^^ fields_doc
    ^^ text " }"
  | Pexp_field (e, li) -> expression_to_doc e ^^ text "." ^^ longident_loc li
  | Pexp_setfield (e1, li, e2) ->
    expression_to_doc e1
    ^^ text "."
    ^^ longident_loc li
    ^^ text " = "
    ^^ expression_to_doc e2
  | Pexp_array exprs ->
    text "[| "
    ^^ separate
         (text ", ")
         (List.map
            ~f:(fun e -> with_comments e.pexp_loc (expression_to_doc e))
            exprs)
    ^^ text " |]"
  | Pexp_ifthenelse (cond, then_, None) ->
    text "if ("
    ^^ with_comments cond.pexp_loc (expression_to_doc cond)
    ^^ text ") { "
    ^^ with_comments then_.pexp_loc (expression_to_doc then_)
    ^^ text " }"
  | Pexp_ifthenelse (cond, then_, Some else_) ->
    text "if ("
    ^^ with_comments cond.pexp_loc (expression_to_doc cond)
    ^^ text ") { "
    ^^ with_comments then_.pexp_loc (expression_to_doc then_)
    ^^ text " } else { "
    ^^ with_comments else_.pexp_loc (expression_to_doc else_)
    ^^ text " }"
  | Pexp_sequence (e1, e2) ->
    with_comments e1.pexp_loc (expression_to_doc e1)
    ^^ text "; "
    ^^ with_comments e2.pexp_loc (expression_to_doc e2)
  | Pexp_while (cond, body) ->
    text "while ("
    ^^ with_comments cond.pexp_loc (expression_to_doc cond)
    ^^ text ") { "
    ^^ with_comments body.pexp_loc (expression_to_doc body)
    ^^ text " }"
  | Pexp_for (pat, start, end_, dir, body) ->
    let dir_str = match dir with Upto -> "to" | Downto -> "downto" in
    text "for ("
    ^^ with_comments pat.ppat_loc (pattern_to_doc pat)
    ^^ text " in "
    ^^ with_comments start.pexp_loc (expression_to_doc start)
    ^^ text (" " ^ dir_str ^ " ")
    ^^ with_comments end_.pexp_loc (expression_to_doc end_)
    ^^ text ") { "
    ^^ with_comments body.pexp_loc (expression_to_doc body)
    ^^ text " }"
  | Pexp_constraint (e, typ) ->
    text "("
    ^^ expression_to_doc e
    ^^ text ": "
    ^^ core_type_to_doc typ
    ^^ text ")"
  | Pexp_coerce (e, None, typ) ->
    text "("
    ^^ expression_to_doc e
    ^^ text " :> "
    ^^ core_type_to_doc typ
    ^^ text ")"
  | Pexp_coerce (e, Some typ1, typ2) ->
    text "("
    ^^ expression_to_doc e
    ^^ text ": "
    ^^ core_type_to_doc typ1
    ^^ text " :> "
    ^^ core_type_to_doc typ2
    ^^ text ")"
  | Pexp_send (e, { txt; loc = _ }) ->
    expression_to_doc e ^^ text "#" ^^ text txt
  | Pexp_new li -> text "new " ^^ longident_loc li
  | Pexp_setinstvar ({ txt; loc = _ }, e) ->
    text txt ^^ text " = " ^^ expression_to_doc e
  | Pexp_override fields ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun ({ txt; loc = _ }, e) ->
             text txt ^^ text ": " ^^ expression_to_doc e)
           fields)
    in
    text "{< " ^^ fields_doc ^^ text " >}"
  | Pexp_letmodule ({ txt = Some name; loc = _ }, me, e) ->
    text "let module "
    ^^ text name
    ^^ text " = "
    ^^ module_expr_to_doc me
    ^^ text "; "
    ^^ expression_to_doc e
  | Pexp_letmodule ({ txt = None; loc = _ }, me, e) ->
    text "let module _ = "
    ^^ module_expr_to_doc me
    ^^ text "; "
    ^^ expression_to_doc e
  | Pexp_letexception (ec, e) ->
    text "let exception "
    ^^ extension_constructor_to_doc ec
    ^^ text "; "
    ^^ expression_to_doc e
  | Pexp_assert e -> text "assert(" ^^ expression_to_doc e ^^ text ")"
  | Pexp_lazy e -> text "lazy(" ^^ expression_to_doc e ^^ text ")"
  | Pexp_poly (e, None) -> expression_to_doc e
  | Pexp_poly (e, Some typ) ->
    text "("
    ^^ expression_to_doc e
    ^^ text ": "
    ^^ core_type_to_doc typ
    ^^ text ")"
  | Pexp_object cs -> class_structure_to_doc cs
  | Pexp_newtype ({ txt; loc = _ }, e) ->
    text "(type " ^^ text txt ^^ text ") => " ^^ expression_to_doc e
  | Pexp_pack me -> text "(module " ^^ module_expr_to_doc me ^^ text ")"
  | Pexp_open (od, e) ->
    text "open "
    ^^ module_expr_to_doc od.popen_expr
    ^^ text "; "
    ^^ expression_to_doc e
  | Pexp_letop { let_; ands; body } ->
    let let_doc = with_comments let_.pbop_loc (binding_op_to_doc let_) in
    let ands_doc =
      separate
        (text " ")
        (List.map
           ~f:(fun bop -> with_comments bop.pbop_loc (binding_op_to_doc bop))
           ands)
    in
    let_doc ^^ text " " ^^ ands_doc ^^ text " in " ^^ expression_to_doc body
  | Pexp_extension ext -> extension_to_doc ext
  | Pexp_unreachable -> text "."

and pexp_apply_to_doc func args =
  let func_doc = expression_to_doc func in
  let arg_texts =
    List.map
      ~f:(fun (lbl, arg) ->
        let arg_doc = with_comments arg.pexp_loc (expression_to_doc arg) in
        match lbl with
        | Nolabel -> arg_doc
        | Labelled s -> text ("~" ^ s ^ "=") ^^ arg_doc
        | Optional s -> text ("~" ^ s ^ "=?") ^^ arg_doc)
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

and jsx_pexp_apply_to_doc func args =
  let rec extract_children_list expr =
    match expr.pexp_desc with
    | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> []
    | Pexp_construct
        ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple [ hd; tl ]; _ })
      ->
      hd :: extract_children_list tl
    | _ -> [ expr ]
  in
  let children = ref None in
  let props =
    List.filter_map args ~f:(fun arg ->
      match arg with
      | ( Labelled "children"
        , ({ pexp_desc = Pexp_construct (_, None); _ } as expr) ) ->
        children := Some (extract_children_list expr);
        None
      | Labelled "children", expr ->
        children := Some (extract_children_list expr);
        None
      | Nolabel, { pexp_desc = Pexp_construct ({ txt = Lident "()"; _ }, _); _ }
        ->
        None
      | prop -> Some prop)
  in

  let tag_name =
    match func.pexp_desc with
    | Pexp_ident { txt = Lident name; _ } -> name
    | Pexp_ident { txt = Ldot (id, name); _ } ->
      let rec flatten = function
        | Lident s -> [ s ]
        | Ldot (path, s) -> flatten path @ [ s ]
        | Lapply _ -> []
      in
      let path = flatten id in
      (match name with
      | "createElement" -> String.concat ~sep:"." path
      | name -> Printf.sprintf "%s.%s" (String.concat ~sep:"." path) name)
    | _ -> raise (Invalid_parsetree "JSX element tag is not Longident.t")
  in

  let fmt_prop (lbl, expr) =
    let expr_doc_with_comments =
      with_comments expr.pexp_loc (expression_to_doc expr)
    in
    let prop_doc =
      match lbl with
      | Nolabel -> text "{" ^^ expr_doc_with_comments ^^ text "}"
      | Labelled label ->
        (* punning: <Foo bar /> where bar is an identifier *)
        (match expr.pexp_desc with
        | Pexp_ident { txt = Lident id; _ } when String.equal id label ->
          text label
        | _ -> text label ^^ text "={" ^^ expr_doc_with_comments ^^ text "}")
      | Optional label ->
        (* punning with optional: <Foo ?bar /> *)
        (match expr.pexp_desc with
        | Pexp_ident { txt = Lident id; _ } when String.equal id label ->
          text "?" ^^ text label
        | _ -> text (label ^ "=?{") ^^ expr_doc_with_comments ^^ text "}")
    in
    match get_jsx_prop_loc expr with
    | Some name_loc ->
      (* Construct full prop span to match what the comment walker uses *)
      let prop_loc =
        { name_loc with Location.loc_end = expr.pexp_loc.Location.loc_end }
      in
      with_comments prop_loc prop_doc
    | None -> prop_doc
  in

  let props_doc =
    match props with
    | [] -> empty
    | _ ->
      let prop_docs = List.map ~f:fmt_prop props in
      space ^^ separate (nl ^^ space) prop_docs
  in

  let start_tag = text "<" ^^ with_comments func.pexp_loc (text tag_name) in

  match !children with
  | None | Some [] -> group (nest 2 (start_tag ^^ props_doc ^^ nl) ^^ text "/>")
  | Some children ->
    let end_tag = text "</" ^^ text tag_name ^^ text ">" in
    let children_docs =
      List.map
        ~f:(fun child -> with_comments child.pexp_loc (expression_to_doc child))
        children
    in
    let children_doc = separate nl children_docs in
    group
      (nest 2 (start_tag ^^ props_doc ^^ text ">")
      ^^ nl
      ^^ nest 2 children_doc
      ^^ nl
      ^^ end_tag)

and pattern_to_doc pat =
  match pat.ppat_desc with
  | Ppat_any -> text "_"
  | Ppat_var { txt; loc = _ } ->
    if
      String.length txt > 0
      &&
      match txt.[0] with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> false
      | _ -> true
    then text ("(" ^ txt ^ ")")
    else text txt
  | Ppat_alias (p, { txt; loc = _ }) ->
    pattern_to_doc p ^^ text " as " ^^ text txt
  | Ppat_constant c -> constant c
  | Ppat_interval (c1, c2) -> constant c1 ^^ text " .. " ^^ constant c2
  | Ppat_tuple pats ->
    text "("
    ^^ separate
         (text ", ")
         (List.map
            ~f:(fun p -> with_comments p.ppat_loc (pattern_to_doc p))
            pats)
    ^^ text ")"
  | Ppat_construct ({ txt = Lident "()"; loc = _ }, None) -> text "()"
  | Ppat_construct ({ txt = Lident "true"; loc = _ }, None) -> text "true"
  | Ppat_construct ({ txt = Lident "false"; loc = _ }, None) -> text "false"
  | Ppat_construct ({ txt = Lident "[]"; loc = _ }, None) -> text "[]"
  | Ppat_construct (li, None) -> longident_loc li
  | Ppat_construct (li, Some (_existential_vars, arg_pat)) ->
    with_comments li.loc (longident_loc li)
    ^^ text "("
    ^^ with_comments arg_pat.ppat_loc (pattern_to_doc arg_pat)
    ^^ text ")"
  | Ppat_variant (tag, None) -> text ("`" ^ tag)
  | Ppat_variant (tag, Some pat) ->
    text ("`" ^ tag ^ "(")
    ^^ with_comments pat.ppat_loc (pattern_to_doc pat)
    ^^ text ")"
  | Ppat_record (fields, Closed) ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun (li, pat) ->
             let row_loc =
               { li.loc with Location.loc_end = pat.ppat_loc.Location.loc_end }
             in
             with_comments
               row_loc
               (with_comments li.loc (longident_loc li)
               ^^ text ": "
               ^^ with_comments pat.ppat_loc (pattern_to_doc pat)))
           fields)
    in
    text "{ " ^^ fields_doc ^^ text " }"
  | Ppat_record (fields, Open) ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun (li, pat) ->
             let row_loc =
               { li.loc with Location.loc_end = pat.ppat_loc.Location.loc_end }
             in
             with_comments
               row_loc
               (with_comments li.loc (longident_loc li)
               ^^ text ": "
               ^^ with_comments pat.ppat_loc (pattern_to_doc pat)))
           fields)
    in
    text "{ " ^^ fields_doc ^^ text ", _ }"
  | Ppat_array pats ->
    text "[| "
    ^^ separate
         (text ", ")
         (List.map
            ~f:(fun p -> with_comments p.ppat_loc (pattern_to_doc p))
            pats)
    ^^ text " |]"
  | Ppat_or (p1, p2) -> pattern_to_doc p1 ^^ text " | " ^^ pattern_to_doc p2
  | Ppat_constraint (p, typ) ->
    text "("
    ^^ pattern_to_doc p
    ^^ text ": "
    ^^ core_type_to_doc typ
    ^^ text ")"
  | Ppat_type li -> text "#" ^^ longident_loc li
  | Ppat_lazy p -> text "lazy(" ^^ pattern_to_doc p ^^ text ")"
  | Ppat_unpack { txt = Some name; loc = _ } -> text ("(module " ^ name ^ ")")
  | Ppat_unpack { txt = None; loc = _ } -> text "(module _)"
  | Ppat_exception p -> text "exception " ^^ pattern_to_doc p
  | Ppat_extension ext -> extension_to_doc ext
  | Ppat_open (li, p) ->
    longident_loc li ^^ text ".(" ^^ pattern_to_doc p ^^ text ")"

and core_type_to_doc typ =
  match typ.ptyp_desc with
  | Ptyp_any -> text "_"
  | Ptyp_var s -> text ("'" ^ s)
  | Ptyp_arrow (Nolabel, t1, t2) ->
    core_type_to_doc t1 ^^ text " => " ^^ core_type_to_doc t2
  | Ptyp_arrow (Labelled s, t1, t2) ->
    text ("~" ^ s ^ ": ")
    ^^ core_type_to_doc t1
    ^^ text " => "
    ^^ core_type_to_doc t2
  | Ptyp_arrow (Optional s, t1, t2) ->
    text ("~" ^ s ^ ": ")
    ^^ core_type_to_doc t1
    ^^ text "=? => "
    ^^ core_type_to_doc t2
  | Ptyp_tuple typs ->
    text "("
    ^^ separate
         (text ", ")
         (List.map
            ~f:(fun typ -> with_comments typ.ptyp_loc (core_type_to_doc typ))
            typs)
    ^^ text ")"
  | Ptyp_constr (li, []) -> longident_loc li
  | Ptyp_constr (li, args) ->
    longident_loc li
    ^^ text "("
    ^^ separate
         (text ", ")
         (List.map
            ~f:(fun t -> with_comments t.ptyp_loc (core_type_to_doc t))
            args)
    ^^ text ")"
  | Ptyp_object (fields, Closed) ->
    let fields_doc =
      separate
        (text ", ")
        (List.map
           ~f:(fun field ->
             match field.pof_desc with
             | Otag ({ txt; loc = _ }, typ) ->
               text ("\"" ^ txt ^ "\": ") ^^ core_type_to_doc typ
             | Oinherit typ -> text "..." ^^ core_type_to_doc typ)
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
             | Otag ({ txt; loc = _ }, typ) ->
               text ("\"" ^ txt ^ "\": ") ^^ core_type_to_doc typ
             | Oinherit typ -> text "..." ^^ core_type_to_doc typ)
           fields)
    in
    text "{ .. " ^^ fields_doc ^^ text " }"
  | Ptyp_class (li, []) -> text "#" ^^ longident_loc li
  | Ptyp_class (li, args) ->
    text "#"
    ^^ longident_loc li
    ^^ text "("
    ^^ separate
         (text ", ")
         (List.map
            ~f:(fun t -> with_comments t.ptyp_loc (core_type_to_doc t))
            args)
    ^^ text ")"
  | Ptyp_alias (typ, { txt; loc = _ }) ->
    core_type_to_doc typ ^^ text (" as '" ^ txt)
  | Ptyp_variant (rows, Closed, None) ->
    let rows_doc =
      separate
        (text " ")
        (List.map
           ~f:(fun r -> with_comments r.prf_loc (row_field_to_doc r))
           rows)
    in
    text "[ " ^^ rows_doc ^^ text " ]"
  | Ptyp_variant (rows, Closed, Some tags) ->
    let rows_doc =
      separate
        (text " ")
        (List.map
           ~f:(fun r -> with_comments r.prf_loc (row_field_to_doc r))
           rows)
    in
    let tags_doc =
      separate (text " ") (List.map ~f:(fun t -> text ("`" ^ t)) tags)
    in
    text "[ " ^^ rows_doc ^^ text " > " ^^ tags_doc ^^ text " ]"
  | Ptyp_variant (rows, Open, _lower_bound_tags) ->
    let rows_doc =
      separate
        (text " ")
        (List.map
           ~f:(fun r -> with_comments r.prf_loc (row_field_to_doc r))
           rows)
    in
    text "[ > " ^^ rows_doc ^^ text " ]"
  | Ptyp_poly ([], typ) -> core_type_to_doc typ
  | Ptyp_poly (vars, typ) ->
    separate
      (text " ")
      (List.map ~f:(fun { txt; loc = _ } -> text ("'" ^ txt)) vars)
    ^^ text ". "
    ^^ core_type_to_doc typ
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
             ^^ core_type_to_doc typ)
           constrs)
    in
    text "module " ^^ longident_loc li ^^ text " with " ^^ constrs_doc
  | Ptyp_extension ext -> extension_to_doc ext
  | Ptyp_open (m, typ) ->
    longident m.txt ^^ text ".(" ^^ core_type_to_doc typ ^^ text ")"

and row_field_to_doc field =
  match field.prf_desc with
  | Rtag ({ txt; loc }, _constant_constructor, []) ->
    text "| " ^^ with_comments loc (text ("`" ^ txt))
  | Rtag ({ txt; loc }, _constant_constructor, typs) ->
    text "| "
    ^^ with_comments loc (text ("`" ^ txt))
    ^^ text "("
    ^^ separate
         (text ", ")
         (List.map
            ~f:(fun t -> with_comments t.ptyp_loc (core_type_to_doc t))
            typs)
    ^^ text ")"
  | Rinherit typ ->
    text "| " ^^ with_comments typ.ptyp_loc (core_type_to_doc typ)

and structure_item_to_doc item =
  match item.pstr_desc with
  | Pstr_eval (e, attrs) -> attributes_to_doc attrs ^^ expression_to_doc e
  | Pstr_value (Nonrecursive, vbs) ->
    separate
      (text "; ")
      (List.map
         ~f:(fun vb ->
           text "let "
           ^^ with_comments vb.pvb_pat.ppat_loc (pattern_to_doc vb.pvb_pat)
           ^^ text " = "
           ^^ with_comments vb.pvb_expr.pexp_loc (expression_to_doc vb.pvb_expr))
         vbs)
  | Pstr_value (Recursive, vbs) ->
    separate
      (text " ")
      (List.mapi
         ~f:(fun i vb ->
           text (if i = 0 then "let rec " else "and ")
           ^^ with_comments vb.pvb_pat.ppat_loc (pattern_to_doc vb.pvb_pat)
           ^^ text " = "
           ^^ with_comments vb.pvb_expr.pexp_loc (expression_to_doc vb.pvb_expr))
         vbs)
  | Pstr_primitive vd ->
    text "external "
    ^^ with_comments vd.pval_name.loc (text vd.pval_name.txt)
    ^^ text ": "
    ^^ with_comments vd.pval_type.ptyp_loc (core_type_to_doc vd.pval_type)
    ^^ text " = "
    ^^ separate
         (text " ")
         (List.map ~f:(fun s -> text ("\"" ^ s ^ "\"")) vd.pval_prim)
  | Pstr_type (_, tds) ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i td ->
           with_comments
             td.ptype_loc
             (text (if i = 0 then "type " else "") ^^ type_declaration_to_doc td))
         tds)
  | Pstr_typext te ->
    text "type "
    ^^ longident_loc te.ptyext_path
    ^^ text " += "
    ^^ separate
         (text " ")
         (List.map
            ~f:(fun ec ->
              with_comments ec.pext_loc (extension_constructor_to_doc ec))
            te.ptyext_constructors)
  | Pstr_exception ec ->
    text "exception "
    ^^ with_comments
         ec.ptyexn_constructor.pext_loc
         (extension_constructor_to_doc ec.ptyexn_constructor)
  | Pstr_module { pmb_name; pmb_expr; pmb_attributes; pmb_loc = _ } ->
    let name = match pmb_name.txt with Some n -> n | None -> "_" in
    attributes_to_doc pmb_attributes
    ^^ text "module "
    ^^ with_comments pmb_name.loc (text name)
    ^^ text " = "
    ^^ with_comments pmb_expr.pmod_loc (module_expr_to_doc pmb_expr)
  | Pstr_recmodule mbs ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i mb ->
           let name = match mb.pmb_name.txt with Some n -> n | None -> "_" in
           text (if i = 0 then "module rec " else "and ")
           ^^ with_comments mb.pmb_name.loc (text name)
           ^^ text " = "
           ^^ with_comments
                mb.pmb_expr.pmod_loc
                (module_expr_to_doc mb.pmb_expr))
         mbs)
  | Pstr_modtype { pmtd_name; pmtd_type = None; pmtd_attributes; pmtd_loc = _ }
    ->
    attributes_to_doc pmtd_attributes
    ^^ text "module type "
    ^^ with_comments pmtd_name.loc (text pmtd_name.txt)
  | Pstr_modtype
      { pmtd_name; pmtd_type = Some mt; pmtd_attributes; pmtd_loc = _ } ->
    attributes_to_doc pmtd_attributes
    ^^ text "module type "
    ^^ with_comments pmtd_name.loc (text pmtd_name.txt)
    ^^ text " = "
    ^^ with_comments mt.pmty_loc (module_type_to_doc mt)
  | Pstr_open od -> text "open " ^^ module_expr_to_doc od.popen_expr
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
                    (List.map
                       ~f:
                         (fun
                           ( typ
                           , ( variance
                             , _injectivity
                               (* The parser hardcodes NoInjectivity *) ) ) ->
                         variance_to_text variance ^^ core_type_to_doc typ)
                       params)
               ^^ text ") ")
           ^^ text " = "
           ^^ class_expr_to_doc cd.pci_expr)
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
                    (List.map
                       ~f:
                         (fun
                           ( typ
                           , ( variance
                             , _injectivity
                               (* The parser hardcodes NoInjectivity *) ) ) ->
                         variance_to_text variance ^^ core_type_to_doc typ)
                       params)
               ^^ text ") ")
           ^^ text " = "
           ^^ class_type_to_doc ctd.pci_expr)
         ctds)
  | Pstr_include { pincl_mod; pincl_attributes; pincl_loc = _ } ->
    attributes_to_doc pincl_attributes
    ^^ text "include "
    ^^ module_expr_to_doc pincl_mod
  | Pstr_attribute attr -> attribute_to_doc attr
  | Pstr_extension (ext, attrs) ->
    attributes_to_doc attrs ^^ item_extension_to_doc ext

and signature_item_to_doc item =
  match item.psig_desc with
  | Psig_value vd ->
    text "let "
    ^^ with_comments vd.pval_name.loc (text vd.pval_name.txt)
    ^^ text ": "
    ^^ with_comments vd.pval_type.ptyp_loc (core_type_to_doc vd.pval_type)
  | Psig_type (_, tds) ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i td ->
           with_comments
             td.ptype_loc
             (text (if i = 0 then "type " else "") ^^ type_declaration_to_doc td))
         tds)
  | Psig_typext te ->
    text "type "
    ^^ longident_loc te.ptyext_path
    ^^ text " += "
    ^^ separate
         (text " ")
         (List.map
            ~f:(fun ec ->
              with_comments ec.pext_loc (extension_constructor_to_doc ec))
            te.ptyext_constructors)
  | Psig_exception ec ->
    text "exception "
    ^^ with_comments
         ec.ptyexn_constructor.pext_loc
         (extension_constructor_to_doc ec.ptyexn_constructor)
  | Psig_module { pmd_name; pmd_type; pmd_attributes; pmd_loc = _ } ->
    let name = match pmd_name.txt with Some n -> n | None -> "_" in
    attributes_to_doc pmd_attributes
    ^^ text "module "
    ^^ with_comments pmd_name.loc (text name)
    ^^ text ": "
    ^^ with_comments pmd_type.pmty_loc (module_type_to_doc pmd_type)
  | Psig_recmodule mds ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i md ->
           let name = match md.pmd_name.txt with Some n -> n | None -> "_" in
           text (if i = 0 then "module rec " else "and ")
           ^^ with_comments md.pmd_name.loc (text name)
           ^^ text ": "
           ^^ with_comments
                md.pmd_type.pmty_loc
                (module_type_to_doc md.pmd_type))
         mds)
  | Psig_modtype { pmtd_name; pmtd_type = None; pmtd_attributes; pmtd_loc = _ }
    ->
    attributes_to_doc pmtd_attributes
    ^^ text "module type "
    ^^ with_comments pmtd_name.loc (text pmtd_name.txt)
  | Psig_modtype
      { pmtd_name; pmtd_type = Some mt; pmtd_attributes; pmtd_loc = _ } ->
    attributes_to_doc pmtd_attributes
    ^^ text "module type "
    ^^ with_comments pmtd_name.loc (text pmtd_name.txt)
    ^^ text " = "
    ^^ with_comments mt.pmty_loc (module_type_to_doc mt)
  | Psig_open od -> text "open " ^^ longident_loc od.popen_expr
  | Psig_include { pincl_mod; pincl_attributes; pincl_loc = _ } ->
    attributes_to_doc pincl_attributes
    ^^ text "include "
    ^^ module_type_to_doc pincl_mod
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
                    (List.map
                       ~f:(fun (typ, (variance, _injectivity)) ->
                         variance_to_text variance ^^ core_type_to_doc typ)
                       params)
               ^^ text ") ")
           ^^ text ": "
           ^^ class_type_to_doc cd.pci_expr)
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
                    (List.map
                       ~f:(fun (typ, (variance, _injectivity)) ->
                         variance_to_text variance ^^ core_type_to_doc typ)
                       params)
               ^^ text ") ")
           ^^ text " = "
           ^^ class_type_to_doc ctd.pci_expr)
         ctds)
  | Psig_attribute attr -> attribute_to_doc attr
  | Psig_extension (ext, attrs) ->
    attributes_to_doc attrs ^^ item_extension_to_doc ext
  | Psig_modsubst { pms_name; pms_manifest; pms_attributes; pms_loc = _ } ->
    attributes_to_doc pms_attributes
    ^^ text ("module " ^ pms_name.txt)
    ^^ text " := "
    ^^ longident_loc pms_manifest
  | Psig_typesubst tds ->
    separate
      (text " and ")
      (List.mapi
         ~f:(fun i td ->
           with_comments
             td.ptype_loc
             (text (if i = 0 then "type " else "and ")
             ^^ type_declaration_to_doc td
             ^^ text " := "
             ^^
             match td.ptype_manifest with
             | Some typ -> core_type_to_doc typ
             | None -> assert false))
         tds)
  | Psig_modtypesubst
      { pmtd_name; pmtd_type = Some mt; pmtd_attributes; pmtd_loc = _ } ->
    attributes_to_doc pmtd_attributes
    ^^ text ("module type " ^ pmtd_name.txt)
    ^^ text " := "
    ^^ module_type_to_doc mt
  | Psig_modtypesubst
      { pmtd_name; pmtd_type = None; pmtd_attributes; pmtd_loc = _ } ->
    attributes_to_doc pmtd_attributes ^^ text ("module type " ^ pmtd_name.txt)

and type_declaration_to_doc td =
  let params =
    match td.ptype_params with
    | [] -> empty
    | ps ->
      text "("
      ^^ separate
           (text ", ")
           (List.map
              ~f:(fun (typ, (variance, _injectivity)) ->
                with_comments
                  typ.ptyp_loc
                  (variance_to_text variance ^^ core_type_to_doc typ))
              ps)
      ^^ text ") "
  in
  let name = with_comments td.ptype_name.loc (text td.ptype_name.txt) in
  let manifest =
    match td.ptype_manifest with
    | None -> empty
    | Some typ ->
      text " = " ^^ with_comments typ.ptyp_loc (core_type_to_doc typ)
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
                with_comments
                  cd.pcd_loc
                  (let constructor_name = text ("| " ^ cd.pcd_name.txt) in
                   let args_doc =
                     match cd.pcd_args with
                     | Pcstr_tuple [] -> empty
                     | Pcstr_tuple args ->
                       text "("
                       ^^ separate
                            (text ", ")
                            (List.map
                               ~f:(fun typ ->
                                 with_comments
                                   typ.ptyp_loc
                                   (core_type_to_doc typ))
                               args)
                       ^^ text ")"
                     | Pcstr_record fields ->
                       text "({ "
                       ^^ separate
                            (text ", ")
                            (List.map
                               ~f:(fun ld ->
                                 with_comments
                                   ld.pld_loc
                                   (text ld.pld_name.txt
                                   ^^ text ": "
                                   ^^ core_type_to_doc ld.pld_type))
                               fields)
                       ^^ text " })"
                   in
                   let res_doc =
                     match cd.pcd_res with
                     | None -> empty
                     | Some typ -> text ": " ^^ core_type_to_doc typ
                   in
                   constructor_name ^^ args_doc ^^ res_doc))
              constrs)
    | Ptype_record fields ->
      text " = { "
      ^^ separate
           (text ", ")
           (List.map
              ~f:(fun ld ->
                with_comments
                  ld.pld_loc
                  (with_comments ld.pld_name.loc (text ld.pld_name.txt)
                  ^^ text ": "
                  ^^ with_comments
                       ld.pld_type.ptyp_loc
                       (core_type_to_doc ld.pld_type)))
              fields)
      ^^ text " }"
    | Ptype_open -> text " = .."
  in
  name
  ^^ (match td.ptype_params with [] -> empty | _ -> params)
  ^^ manifest
  ^^ kind

and extension_constructor_to_doc ec =
  let name = with_comments ec.pext_name.loc (text ec.pext_name.txt) in
  match ec.pext_kind with
  | Pext_decl (_type_params, Pcstr_tuple [], None) -> name
  | Pext_decl (_type_params, Pcstr_tuple args, ret_type) ->
    (match ret_type with
    | None ->
      name
      ^^ text "("
      ^^ separate
           (text ", ")
           (List.map
              ~f:(fun t -> with_comments t.ptyp_loc (core_type_to_doc t))
              args)
      ^^ text ")"
    | Some typ ->
      name
      ^^ text "("
      ^^ separate
           (text ", ")
           (List.map
              ~f:(fun t -> with_comments t.ptyp_loc (core_type_to_doc t))
              args)
      ^^ text "): "
      ^^ with_comments typ.ptyp_loc (core_type_to_doc typ))
  | Pext_decl (_type_params, Pcstr_record fields, ret_type) ->
    let fields_doc =
      text "({ "
      ^^ separate
           (text ", ")
           (List.map
              ~f:(fun ld ->
                with_comments
                  ld.pld_loc
                  (text ld.pld_name.txt
                  ^^ text ": "
                  ^^ core_type_to_doc ld.pld_type))
              fields)
      ^^ text " })"
    in
    (match ret_type with
    | None -> name ^^ fields_doc
    | Some typ ->
      name
      ^^ fields_doc
      ^^ text ": "
      ^^ with_comments typ.ptyp_loc (core_type_to_doc typ))
  | Pext_rebind li -> name ^^ text " = " ^^ longident_loc li

and module_expr_to_doc me =
  match me.pmod_desc with
  | Pmod_ident li -> longident_loc li
  | Pmod_structure items ->
    text "{ "
    ^^ separate
         (text "; ")
         (List.map
            ~f:(fun item ->
              with_comments item.pstr_loc (structure_item_to_doc item))
            items)
    ^^ text " }"
  | Pmod_functor (Unit, body) ->
    text "() => " ^^ with_comments body.pmod_loc (module_expr_to_doc body)
  | Pmod_functor (Named ({ txt = Some name; loc }, mt), body) ->
    text "("
    ^^ with_comments loc (text name)
    ^^ text ": "
    ^^ with_comments mt.pmty_loc (module_type_to_doc mt)
    ^^ text ") => "
    ^^ with_comments body.pmod_loc (module_expr_to_doc body)
  | Pmod_functor (Named ({ txt = None; loc }, mt), body) ->
    text "("
    ^^ with_comments loc (text "_")
    ^^ text ": "
    ^^ with_comments mt.pmty_loc (module_type_to_doc mt)
    ^^ text ") => "
    ^^ with_comments body.pmod_loc (module_expr_to_doc body)
  | Pmod_apply (m1, m2) ->
    with_comments m1.pmod_loc (module_expr_to_doc m1)
    ^^ text "("
    ^^ with_comments m2.pmod_loc (module_expr_to_doc m2)
    ^^ text ")"
  | Pmod_apply_unit m ->
    with_comments m.pmod_loc (module_expr_to_doc m) ^^ text "()"
  | Pmod_constraint (me, mt) ->
    text "("
    ^^ with_comments me.pmod_loc (module_expr_to_doc me)
    ^^ text ": "
    ^^ with_comments mt.pmty_loc (module_type_to_doc mt)
    ^^ text ")"
  | Pmod_unpack e ->
    text "(val " ^^ with_comments e.pexp_loc (expression_to_doc e) ^^ text ")"
  | Pmod_extension ext -> extension_to_doc ext

and module_type_to_doc mt =
  match mt.pmty_desc with
  | Pmty_ident li -> longident_loc li
  | Pmty_signature items ->
    text "{ "
    ^^ separate
         (text "; ")
         (List.map
            ~f:(fun item ->
              with_comments item.psig_loc (signature_item_to_doc item))
            items)
    ^^ text " }"
  | Pmty_functor (Unit, body) ->
    text "() => " ^^ with_comments body.pmty_loc (module_type_to_doc body)
  | Pmty_functor (Named ({ txt = Some name; loc }, mt1), mt2) ->
    text "("
    ^^ with_comments loc (text name)
    ^^ text ": "
    ^^ with_comments mt1.pmty_loc (module_type_to_doc mt1)
    ^^ text ") => "
    ^^ with_comments mt2.pmty_loc (module_type_to_doc mt2)
  | Pmty_functor (Named ({ txt = None; loc }, mt1), mt2) ->
    text "("
    ^^ with_comments loc (text "_")
    ^^ text ": "
    ^^ with_comments mt1.pmty_loc (module_type_to_doc mt1)
    ^^ text ") => "
    ^^ with_comments mt2.pmty_loc (module_type_to_doc mt2)
  | Pmty_with (mt, constrs) ->
    module_type_to_doc mt
    ^^ text " with "
    ^^ separate (text " and ") (List.map ~f:with_constraint_to_doc constrs)
  | Pmty_typeof me -> text "module type of " ^^ module_expr_to_doc me
  | Pmty_extension ext -> extension_to_doc ext
  | Pmty_alias li -> text "module " ^^ longident_loc li

and with_constraint_to_doc = function
  | Pwith_type (li, td) ->
    text "type " ^^ longident_loc li ^^ text " = " ^^ type_declaration_to_doc td
  | Pwith_module (li1, li2) ->
    text "module " ^^ longident_loc li1 ^^ text " = " ^^ longident_loc li2
  | Pwith_modtype (li, mt) ->
    text "module type "
    ^^ longident li.txt
    ^^ text " = "
    ^^ module_type_to_doc mt
  | Pwith_typesubst (li, td) ->
    let params_and_name =
      match td.ptype_params with
      | [] -> longident_loc li
      | ps ->
        let params_doc =
          text "("
          ^^ separate
               (text ", ")
               (List.map
                  ~f:(fun (typ, (variance, _injectivity)) ->
                    variance_to_text variance ^^ core_type_to_doc typ)
                  ps)
          ^^ text ") "
        in
        params_doc ^^ longident_loc li
    in
    text "type "
    ^^ params_and_name
    ^^ text " := "
    ^^
      (match td.ptype_manifest with
      | None -> type_declaration_to_doc td
      | Some typ -> core_type_to_doc typ)
  | Pwith_modsubst (li1, li2) ->
    text "module " ^^ longident_loc li1 ^^ text " := " ^^ longident_loc li2
  | Pwith_modtypesubst (li, mt) ->
    text "module type "
    ^^ longident li.txt
    ^^ text " := "
    ^^ module_type_to_doc mt

and class_expr_to_doc ce =
  match ce.pcl_desc with
  | Pcl_constr (li, []) -> longident_loc li
  | Pcl_constr (li, typs) ->
    longident_loc li
    ^^ text "("
    ^^ separate
         (text ", ")
         (List.map
            ~f:(fun t -> with_comments t.ptyp_loc (core_type_to_doc t))
            typs)
    ^^ text ")"
  | Pcl_structure cs -> class_structure_to_doc cs
  | Pcl_fun (lbl, None, pat, ce) ->
    let lbl_doc =
      match lbl with
      | Nolabel -> empty
      | Labelled s -> text ("~" ^ s)
      | Optional s -> text ("~" ^ s ^ "=?")
    in
    lbl_doc ^^ pattern_to_doc pat ^^ text " => " ^^ class_expr_to_doc ce
  | Pcl_fun (lbl, Some default, pat, ce) ->
    let lbl_doc =
      match lbl with
      | Nolabel -> empty
      | Labelled s -> text ("~" ^ s ^ "=")
      | Optional s -> text ("~" ^ s ^ "=?")
    in
    lbl_doc
    ^^ pattern_to_doc pat
    ^^ text "="
    ^^ expression_to_doc default
    ^^ text " => "
    ^^ class_expr_to_doc ce
  | Pcl_apply (ce, args) ->
    class_expr_to_doc ce
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
              lbl_doc ^^ expression_to_doc e)
            args)
    ^^ text ")"
  | Pcl_let (_, vbs, ce) ->
    separate
      (text "; ")
      (List.map
         ~f:(fun vb ->
           text "let "
           ^^ pattern_to_doc vb.pvb_pat
           ^^ text " = "
           ^^ expression_to_doc vb.pvb_expr)
         vbs)
    ^^ text "; "
    ^^ class_expr_to_doc ce
  | Pcl_constraint (ce, ct) ->
    text "("
    ^^ class_expr_to_doc ce
    ^^ text ": "
    ^^ class_type_to_doc ct
    ^^ text ")"
  | Pcl_extension ext -> extension_to_doc ext
  | Pcl_open (od, ce) ->
    text "open "
    ^^ longident_loc od.popen_expr
    ^^ text "; "
    ^^ class_expr_to_doc ce

and class_structure_to_doc cs =
  let self_pat =
    match cs.pcstr_self.ppat_desc with
    | Ppat_var { txt = "this"; loc = _ } -> empty
    | _ -> text " as " ^^ pattern_to_doc cs.pcstr_self
  in
  text "{ "
  ^^ self_pat
  ^^ text " "
  ^^ separate
       (text "; ")
       (List.map
          ~f:(fun cf -> with_comments cf.pcf_loc (class_field_to_doc cf))
          cs.pcstr_fields)
  ^^ text " }"

and class_field_to_doc cf =
  match cf.pcf_desc with
  | Pcf_inherit (_, ce, None) -> text "inherit " ^^ class_expr_to_doc ce
  | Pcf_inherit (_, ce, Some { txt; loc = _ }) ->
    text "inherit " ^^ class_expr_to_doc ce ^^ text (" as " ^ txt)
  | Pcf_val ({ txt; loc = _ }, Mutable, Cfk_concrete (_, e)) ->
    text ("val mutable " ^ txt ^ " = ") ^^ expression_to_doc e
  | Pcf_val ({ txt; loc = _ }, Immutable, Cfk_concrete (_, e)) ->
    text ("val " ^ txt ^ " = ") ^^ expression_to_doc e
  | Pcf_val ({ txt; loc = _ }, Mutable, Cfk_virtual typ) ->
    text ("val mutable virtual " ^ txt ^ ": ") ^^ core_type_to_doc typ
  | Pcf_val ({ txt; loc = _ }, Immutable, Cfk_virtual typ) ->
    text ("val virtual " ^ txt ^ ": ") ^^ core_type_to_doc typ
  | Pcf_method ({ txt; loc = _ }, Private, Cfk_concrete (_, e)) ->
    text ("pri " ^ txt ^ " = ") ^^ expression_to_doc e
  | Pcf_method ({ txt; loc = _ }, Public, Cfk_concrete (_, e)) ->
    text ("pub " ^ txt ^ " = ") ^^ expression_to_doc e
  | Pcf_method ({ txt; loc = _ }, Private, Cfk_virtual typ) ->
    text ("pri virtual " ^ txt ^ ": ") ^^ core_type_to_doc typ
  | Pcf_method ({ txt; loc = _ }, Public, Cfk_virtual typ) ->
    text ("pub virtual " ^ txt ^ ": ") ^^ core_type_to_doc typ
  | Pcf_constraint (typ1, typ2) ->
    text "constraint "
    ^^ core_type_to_doc typ1
    ^^ text " = "
    ^^ core_type_to_doc typ2
  | Pcf_initializer e -> text "initializer " ^^ expression_to_doc e
  | Pcf_attribute attr -> attribute_to_doc attr
  | Pcf_extension ext -> extension_to_doc ext

and class_type_to_doc ct =
  match ct.pcty_desc with
  | Pcty_constr (li, []) -> longident_loc li
  | Pcty_constr (li, typs) ->
    longident_loc li
    ^^ text "("
    ^^ separate
         (text ", ")
         (List.map
            ~f:(fun t -> with_comments t.ptyp_loc (core_type_to_doc t))
            typs)
    ^^ text ")"
  | Pcty_signature cs ->
    text "{ "
    ^^ separate
         (text "; ")
         (List.map
            ~f:(fun ctf ->
              with_comments ctf.pctf_loc (class_type_field_to_doc ctf))
            cs.pcsig_fields)
    ^^ text " }"
  | Pcty_arrow (lbl, typ, ct) ->
    let lbl_doc =
      match lbl with
      | Nolabel -> empty
      | Labelled s -> text ("~" ^ s ^ ": ")
      | Optional s -> text ("~" ^ s ^ ": ")
    in
    lbl_doc ^^ core_type_to_doc typ ^^ text " => " ^^ class_type_to_doc ct
  | Pcty_extension ext -> extension_to_doc ext
  | Pcty_open (od, ct) ->
    text "open "
    ^^ longident_loc od.popen_expr
    ^^ text "; "
    ^^ class_type_to_doc ct

and class_type_field_to_doc ctf =
  match ctf.pctf_desc with
  | Pctf_inherit ct -> text "inherit " ^^ class_type_to_doc ct
  | Pctf_val ({ txt; loc = _ }, Mutable, Virtual, typ) ->
    text ("val mutable virtual " ^ txt ^ ": ") ^^ core_type_to_doc typ
  | Pctf_val ({ txt; loc = _ }, Immutable, Virtual, typ) ->
    text ("val virtual " ^ txt ^ ": ") ^^ core_type_to_doc typ
  | Pctf_val ({ txt; loc = _ }, Mutable, Concrete, typ) ->
    text ("val mutable " ^ txt ^ ": ") ^^ core_type_to_doc typ
  | Pctf_val ({ txt; loc = _ }, Immutable, Concrete, typ) ->
    text ("val " ^ txt ^ ": ") ^^ core_type_to_doc typ
  | Pctf_method ({ txt; loc = _ }, Private, Virtual, typ) ->
    text ("pri virtual " ^ txt ^ ": ") ^^ core_type_to_doc typ
  | Pctf_method ({ txt; loc = _ }, Public, Virtual, typ) ->
    text ("pub virtual " ^ txt ^ ": ") ^^ core_type_to_doc typ
  | Pctf_method ({ txt; loc = _ }, Private, Concrete, typ) ->
    text ("pri " ^ txt ^ ": ") ^^ core_type_to_doc typ
  | Pctf_method ({ txt; loc = _ }, Public, Concrete, typ) ->
    text ("pub " ^ txt ^ ": ") ^^ core_type_to_doc typ
  | Pctf_constraint (typ1, typ2) ->
    text "constraint "
    ^^ core_type_to_doc typ1
    ^^ text " = "
    ^^ core_type_to_doc typ2
  | Pctf_attribute attr -> attribute_to_doc attr
  | Pctf_extension ext -> extension_to_doc ext

and attribute_to_doc attr =
  text "[@"
  ^^ text attr.attr_name.txt
  ^^ text " "
  ^^ payload_to_doc attr.attr_payload
  ^^ text "]"

and payload_to_doc payload =
  match payload with
  | PStr [] -> empty
  | PStr [ item ] -> structure_item_to_doc item
  | PStr items -> separate (text "; ") (List.map ~f:structure_item_to_doc items)
  | PTyp typ -> core_type_to_doc typ
  | PSig [] -> empty
  | PSig [ item ] -> signature_item_to_doc item
  | PSig items -> separate (text "; ") (List.map ~f:signature_item_to_doc items)
  | PPat (pat, _expr) -> pattern_to_doc pat

and attributes_to_doc attrs =
  match attrs with
  | [] -> empty
  | attrs ->
    separate (text " ") (List.map ~f:attribute_to_doc attrs) ^^ text " "

and extension_to_doc (name, payload) =
  text "[%" ^^ text name.txt ^^ text " " ^^ payload_to_doc payload ^^ text "]"

and item_extension_to_doc (name, payload) =
  text "[%%" ^^ text name.txt ^^ text " " ^^ payload_to_doc payload ^^ text "]"

and binding_op_to_doc bop =
  text bop.pbop_op.txt
  ^^ text " "
  ^^ pattern_to_doc bop.pbop_pat
  ^^ text " = "
  ^^ expression_to_doc bop.pbop_exp

let make () =
  object (self)
    method expression ppf expr =
      let _width = current_settings.contents.width in
      P.pretty_print (Format.pp_print_string ppf) (expression_to_doc expr)

    method pattern ppf pat =
      let _width = current_settings.contents.width in
      P.pretty_print (Format.pp_print_string ppf) (pattern_to_doc pat)

    method core_type ppf typ =
      let _width = current_settings.contents.width in
      P.pretty_print (Format.pp_print_string ppf) (core_type_to_doc typ)

    method structure comments ppf items =
      let _width = current_settings.contents.width in
      (* Build and populate comment table *)
      let tbl = Reason_comment_table.make () in
      Reason_comment_table.walk_structure items tbl comments;
      (* Set current table for nested lookups *)
      current_comment_table := Some tbl;
      (* Print each structure item with its comments, preserving blank lines *)
      let rec print_items prev_end_line acc = function
        | [] -> List.rev acc
        | si :: rest ->
          let loc = si.pstr_loc in
          (* Check if there are leading comments - use their start line if so *)
          let effective_start_line =
            match Hashtbl.find_opt tbl.Reason_comment_table.leading loc with
            | Some (cmt :: _) ->
              (Reason_comment.location cmt).loc_start.pos_lnum
            | _ -> loc.loc_start.pos_lnum
          in
          (* Calculate blank lines to insert *)
          let blank_lines =
            if prev_end_line = 0
            then 0
            else max 0 (effective_start_line - prev_end_line - 1)
          in
          let separator =
            if prev_end_line = 0
            then empty
            else if blank_lines > 0
            then
              (* Insert extra blank lines *)
              let rec make_blanks n acc =
                if n <= 0 then acc else make_blanks (n - 1) (hard_nl ^^ acc)
              in
              hard_nl ^^ make_blanks blank_lines empty
            else hard_nl
          in
          let item_doc =
            Reason_comment_table.print_comments
              ~text
              ~concat:( ^^ )
              ~space:(text " ")
              ~hard_line:hard_nl
              ~empty
              (structure_item_to_doc si)
              tbl
              loc
          in
          let doc = separator ^^ item_doc in
          print_items loc.loc_end.pos_lnum (doc :: acc) rest
      in
      let docs = print_items 0 [] items in
      let doc = List.fold_left ~f:( ^^ ) ~init:empty docs in
      current_comment_table := None;
      P.pretty_print (Format.pp_print_string ppf) doc

    method signature comments ppf items =
      let _width = current_settings.contents.width in
      let tbl = Reason_comment_table.make () in
      Reason_comment_table.walk_signature items tbl comments;
      current_comment_table := Some tbl;
      let rec print_items prev_end_line acc = function
        | [] -> List.rev acc
        | si :: rest ->
          let loc = si.psig_loc in
          let found_comments =
            Hashtbl.find_opt tbl.Reason_comment_table.leading loc
          in
          let effective_start_line =
            match found_comments with
            | Some (cmt :: _) ->
              (Reason_comment.location cmt).loc_start.pos_lnum
            | _ -> loc.loc_start.pos_lnum
          in
          let blank_lines =
            if prev_end_line = 0
            then 0
            else max 0 (effective_start_line - prev_end_line - 1)
          in
          let separator =
            if prev_end_line = 0
            then empty
            else if blank_lines > 0
            then
              (* Insert extra blank lines *)
              let rec make_blanks n acc =
                if n <= 0 then acc else make_blanks (n - 1) (hard_nl ^^ acc)
              in
              hard_nl ^^ make_blanks blank_lines empty
            else hard_nl
          in
          let item_doc =
            Reason_comment_table.print_comments
              ~text
              ~concat:( ^^ )
              ~space:(text " ")
              ~hard_line:hard_nl
              ~empty
              (signature_item_to_doc si)
              tbl
              loc
          in
          let doc = separator ^^ item_doc in
          print_items loc.loc_end.pos_lnum (doc :: acc) rest
      in
      let docs = print_items 0 [] items in
      let doc = List.fold_left ~f:( ^^ ) ~init:empty docs in
      current_comment_table := None;
      P.pretty_print (Format.pp_print_string ppf) doc

    method case_list ppf cases =
      let _width = current_settings.contents.width in
      let doc =
        separate
          (text " ")
          (List.map
             ~f:(fun case ->
               text "| "
               ^^ with_comments
                    case.pc_lhs.ppat_loc
                    (pattern_to_doc case.pc_lhs)
               ^^ text " => "
               ^^ with_comments
                    case.pc_rhs.pexp_loc
                    (expression_to_doc case.pc_rhs))
             cases)
      in
      P.pretty_print (Format.pp_print_string ppf) doc

    method toplevel_phrase ppf phrase =
      match phrase with
      | Ptop_def items -> self#structure [] ppf items
      | Ptop_dir _ -> assert false
  end
