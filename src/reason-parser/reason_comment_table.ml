module Comment = Reason_comment

type t =
  { leading : (Location.t, Comment.t list) Hashtbl.t
  ; inside : (Location.t, Comment.t list) Hashtbl.t
  ; trailing : (Location.t, Comment.t list) Hashtbl.t
  }

let make () =
  { leading = Hashtbl.create 100
  ; inside = Hashtbl.create 100
  ; trailing = Hashtbl.create 100
  }

let copy tbl =
  { leading = Hashtbl.copy tbl.leading
  ; inside = Hashtbl.copy tbl.inside
  ; trailing = Hashtbl.copy tbl.trailing
  }

let empty = make ()

let attach tbl loc comments =
  match comments with
  | [] -> ()
  | comments ->
    let existing = Hashtbl.find_opt tbl loc |> Option.value ~default:[] in
    Hashtbl.replace tbl loc (existing @ comments)

(* Partition comments into leading/inside/trailing relative to a location *)
let partition_by_loc comments loc =
  let rec loop (leading, inside, trailing) comments =
    let open Location in
    match comments with
    | comment :: rest ->
      let cmt_loc = Comment.location comment in
      (* Use line numbers when character positions are ambiguous/overlapping *)
      let is_before =
        cmt_loc.loc_end.pos_lnum < loc.loc_start.pos_lnum
        || (cmt_loc.loc_end.pos_lnum == loc.loc_start.pos_lnum
           && cmt_loc.loc_end.pos_cnum <= loc.loc_start.pos_cnum)
      in
      let is_after =
        cmt_loc.loc_start.pos_lnum > loc.loc_end.pos_lnum
        || (cmt_loc.loc_start.pos_lnum == loc.loc_end.pos_lnum
           && cmt_loc.loc_start.pos_cnum >= loc.loc_end.pos_cnum)
      in
      if is_before
      then loop (comment :: leading, inside, trailing) rest
      else if is_after
      then loop (leading, inside, comment :: trailing) rest
      else loop (leading, comment :: inside, trailing) rest
    | [] -> List.rev leading, List.rev inside, List.rev trailing
  in
  loop ([], [], []) comments

(* Partition into leading and trailing only *)
let partition_leading_trailing comments loc =
  let rec loop (leading, trailing) comments =
    let open Location in
    match comments with
    | comment :: rest ->
      let cmt_loc = Comment.location comment in
      if cmt_loc.loc_end.pos_cnum <= loc.loc_start.pos_cnum
      then loop (comment :: leading, trailing) rest
      else loop (leading, comment :: trailing) rest
    | [] -> List.rev leading, List.rev trailing
  in
  loop ([], []) comments

(* Partition comments by same line vs different line *)
let partition_by_on_same_line loc comments =
  let rec loop (on_same_line, on_other_line) comments =
    let open Location in
    match comments with
    | [] -> List.rev on_same_line, List.rev on_other_line
    | comment :: rest ->
      let cmt_loc = Comment.location comment in
      if cmt_loc.loc_start.pos_lnum == loc.loc_end.pos_lnum
      then loop (comment :: on_same_line, on_other_line) rest
      else loop (on_same_line, comment :: on_other_line) rest
  in
  loop ([], []) comments

(* Extract comments immediately adjacent to a location *)
let partition_adjacent_trailing loc1 comments =
  let open Location in
  let rec loop ~prev_end_pos after_loc1 comments =
    match comments with
    | [] -> List.rev after_loc1, []
    | comment :: rest ->
      let cmt_loc = Comment.location comment in
      (* Use line-based adjacency *)
      if cmt_loc.loc_start.pos_lnum == prev_end_pos.Lexing.pos_lnum
      then
        let comment_end = cmt_loc.loc_end in
        loop ~prev_end_pos:comment_end (comment :: after_loc1) rest
      else List.rev after_loc1, comments
  in
  loop ~prev_end_pos:loc1.loc_end [] comments

(* Flatten list expressions (:: chains) into a flat list of elements *)
let rec collect_list_exprs acc expr =
  let open Ppxlib.Parsetree in
  match expr.pexp_desc with
  | Pexp_construct
      ( { txt = Lident "::"; _ }
      , Some { pexp_desc = Pexp_tuple [ head; tail ]; _ } ) ->
    collect_list_exprs (head :: acc) tail
  | Pexp_construct ({ txt = Lident "[]"; _ }, _) -> List.rev acc
  | _ -> List.rev (expr :: acc)

(* Flatten list patterns (:: chains) into a flat list of patterns *)
let rec collect_list_patterns acc pattern =
  let open Ppxlib.Parsetree in
  match pattern.ppat_desc with
  | Ppat_construct
      ( { txt = Lident "::"; _ }
      , Some (_, { ppat_desc = Ppat_tuple [ head; tail ]; _ }) ) ->
    collect_list_patterns (head :: acc) tail
  | Ppat_construct ({ txt = Lident "[]"; _ }, _) -> List.rev acc
  | _ -> List.rev (pattern :: acc)

(* JSX detection and prop location extraction *)
let is_jsx_expression expr =
  List.exists
    ~f:(fun attr -> attr.Ppxlib.attr_name.txt = "JSX")
    expr.Ppxlib.pexp_attributes

let get_jsx_prop_loc expr =
  List.find_map
    ~f:(fun attr ->
      if attr.Ppxlib.attr_name.txt = "reason.jsx_prop_loc"
      then Some attr.Ppxlib.attr_name.loc
      else None)
    expr.Ppxlib.pexp_attributes

let get_comment_loc_typ typ =
  List.find_map
    ~f:(fun attr ->
      if attr.Ppxlib.attr_name.txt = "reason.comment_loc"
      then Some attr.Ppxlib.attr_name.loc
      else None)
    typ.Ppxlib.ptyp_attributes

(* JSX prop info for comment attachment *)
type jsx_prop_info =
  { jsx_prop_name_loc : Location.t
  ; jsx_prop_value_expr : Ppxlib.Parsetree.expression
  }

(* Node types for generic list walking *)
type node =
  | Case of Ppxlib.Parsetree.case
  | CoreType of Ppxlib.Parsetree.core_type
  | Expression of Ppxlib.Parsetree.expression
  | ExprRecordRow of
      Ppxlib.Longident.t Ppxlib.Asttypes.loc * Ppxlib.Parsetree.expression
  | ExtensionConstructor of Ppxlib.Parsetree.extension_constructor
  | JsxProp of jsx_prop_info
  | LabelDeclaration of Ppxlib.Parsetree.label_declaration
  | ModuleBinding of Ppxlib.Parsetree.module_binding
  | ModuleDeclaration of Ppxlib.Parsetree.module_declaration
  | ModuleExpr of Ppxlib.Parsetree.module_expr
  | Pattern of Ppxlib.Parsetree.pattern
  | PatternRecordRow of
      Ppxlib.Longident.t Ppxlib.Asttypes.loc * Ppxlib.Parsetree.pattern
  | RowField of Ppxlib.Parsetree.row_field
  | SignatureItem of Ppxlib.Parsetree.signature_item
  | StructureItem of Ppxlib.Parsetree.structure_item
  | TypeDeclaration of Ppxlib.Parsetree.type_declaration
  | ValueBinding of Ppxlib.Parsetree.value_binding

let get_loc node =
  let open Ppxlib.Parsetree in
  match node with
  | Case case ->
    { case.pc_lhs.ppat_loc with loc_end = case.pc_rhs.pexp_loc.loc_end }
  | CoreType ct -> ct.ptyp_loc
  | Expression e -> e.pexp_loc
  | ExprRecordRow (li, e) -> { li.loc with loc_end = e.pexp_loc.loc_end }
  | ExtensionConstructor ec -> ec.pext_loc
  | JsxProp { jsx_prop_name_loc; jsx_prop_value_expr } ->
    (* Location spans from prop name to end of value *)
    { jsx_prop_name_loc with
      loc_end = jsx_prop_value_expr.pexp_loc.Location.loc_end
    }
  | LabelDeclaration ld -> ld.pld_loc
  | ModuleBinding mb -> mb.pmb_loc
  | ModuleDeclaration md -> md.pmd_loc
  | ModuleExpr me -> me.pmod_loc
  | Pattern p -> p.ppat_loc
  | PatternRecordRow (li, p) -> { li.loc with loc_end = p.ppat_loc.loc_end }
  | RowField rf ->
    (match rf.prf_desc with
    | Rtag ({ loc; _ }, _, _) -> loc
    | Rinherit { ptyp_loc; _ } -> ptyp_loc)
  | SignatureItem si -> si.psig_loc
  | StructureItem si -> si.pstr_loc
  | TypeDeclaration td -> td.ptype_loc
  | ValueBinding vb -> vb.pvb_loc

(* Main walk functions *)

let rec walk_structure s t comments =
  match s with
  | _ when comments = [] -> ()
  | [] -> attach t.inside Location.none comments
  | s -> walk_list (List.map ~f:(fun si -> StructureItem si) s) t comments

and walk_signature signature t comments =
  match signature with
  | _ when comments = [] -> ()
  | [] -> attach t.inside Location.none comments
  | s -> walk_list (List.map ~f:(fun si -> SignatureItem si) s) t comments

and walk_node node tbl comments =
  match node with
  | Case c -> walk_case c tbl comments
  | CoreType ct -> walk_core_type ct tbl comments
  | Expression e -> walk_expression e tbl comments
  | ExprRecordRow (li, e) -> walk_expr_record_row (li, e) tbl comments
  | ExtensionConstructor ec -> walk_extension_constructor ec tbl comments
  | JsxProp prop -> walk_jsx_prop prop tbl comments
  | LabelDeclaration ld -> walk_label_declaration ld tbl comments
  | ModuleBinding mb -> walk_module_binding mb tbl comments
  | ModuleDeclaration md -> walk_module_declaration md tbl comments
  | ModuleExpr me -> walk_module_expr me tbl comments
  | Pattern p -> walk_pattern p tbl comments
  | PatternRecordRow (li, p) -> walk_pattern_record_row (li, p) tbl comments
  | RowField rf -> walk_row_field rf tbl comments
  | SignatureItem si -> walk_signature_item si tbl comments
  | StructureItem si -> walk_structure_item si tbl comments
  | TypeDeclaration td -> walk_type_declaration td tbl comments
  | ValueBinding vb -> walk_value_binding vb tbl comments

and walk_jsx_prop { jsx_prop_name_loc; jsx_prop_value_expr } t comments =
  let open Ppxlib.Parsetree in
  let before_name, after_name =
    partition_leading_trailing comments jsx_prop_name_loc
  in
  attach t.leading jsx_prop_name_loc before_name;
  (* Comments between name and value attach as trailing to name *)
  let between, rest =
    partition_leading_trailing after_name jsx_prop_value_expr.pexp_loc
  in
  attach t.trailing jsx_prop_name_loc between;
  let before_value, inside, after_value =
    partition_by_loc rest jsx_prop_value_expr.pexp_loc
  in
  attach t.leading jsx_prop_value_expr.pexp_loc before_value;
  walk_expression jsx_prop_value_expr t inside;
  attach t.trailing jsx_prop_value_expr.pexp_loc after_value

and walk_list ?prev_loc l t comments =
  match l with
  | _ when comments = [] -> ()
  | [] ->
    (match prev_loc with
    | Some loc -> attach t.trailing loc comments
    | None -> ())
  | node :: rest ->
    let curr_loc = get_loc node in
    let leading, inside, trailing = partition_by_loc comments curr_loc in
    (match prev_loc with
    | None ->
      (* First node - all leading comments attach here *)
      attach t.leading curr_loc leading
    | Some prev_loc ->
      (* Split leading between prev trailing and curr leading *)
      if
        prev_loc.Location.loc_end.pos_lnum
        == curr_loc.Location.loc_start.pos_lnum
      then (
        (* Same line *)
        let after_prev, before_curr =
          partition_adjacent_trailing prev_loc leading
        in
        attach t.trailing prev_loc after_prev;
        attach t.leading curr_loc before_curr)
      else
        (* Different lines *)
        let on_same_line_as_prev, after_prev =
          partition_by_on_same_line prev_loc leading
        in
        attach t.trailing prev_loc on_same_line_as_prev;
        attach t.leading curr_loc after_prev);
    walk_node node t inside;
    walk_list ~prev_loc:curr_loc rest t trailing

and walk_structure_item si t comments =
  let open Ppxlib.Parsetree in
  match si.pstr_desc with
  | _ when comments = [] -> ()
  | Pstr_primitive vd -> walk_value_description vd t comments
  | Pstr_open od -> walk_open_description od t comments
  | Pstr_value (_, vbs) -> walk_value_bindings vbs t comments
  | Pstr_type (_, tds) -> walk_type_declarations tds t comments
  | Pstr_eval (expr, _) -> walk_expression expr t comments
  | Pstr_module mb -> walk_module_binding mb t comments
  | Pstr_recmodule mbs ->
    walk_list (List.map ~f:(fun mb -> ModuleBinding mb) mbs) t comments
  | Pstr_modtype mtd -> walk_module_type_declaration mtd t comments
  | Pstr_attribute attr -> walk_attribute attr t comments
  | Pstr_extension (ext, _) -> walk_extension ext t comments
  | Pstr_include id -> walk_include_declaration id t comments
  | Pstr_exception ec ->
    walk_extension_constructor ec.ptyexn_constructor t comments
  | Pstr_typext te -> walk_type_extension te t comments
  | Pstr_class _ | Pstr_class_type _ ->
    (* Classes not fully supported, attach inside *)
    attach t.inside si.pstr_loc comments

and walk_signature_item si t comments =
  let open Ppxlib.Parsetree in
  match si.psig_desc with
  | _ when comments = [] -> ()
  | Psig_value vd -> walk_value_description vd t comments
  | Psig_type (_, tds) -> walk_type_declarations tds t comments
  | Psig_typext te -> walk_type_extension te t comments
  | Psig_exception ec ->
    walk_extension_constructor ec.ptyexn_constructor t comments
  | Psig_module md -> walk_module_declaration md t comments
  | Psig_recmodule mds ->
    walk_list (List.map ~f:(fun md -> ModuleDeclaration md) mds) t comments
  | Psig_modtype mtd -> walk_module_type_declaration mtd t comments
  | Psig_open od -> walk_open_description_sig od t comments
  | Psig_include id -> walk_include_description id t comments
  | Psig_attribute attr -> walk_attribute attr t comments
  | Psig_extension (ext, _) -> walk_extension ext t comments
  | Psig_class _ | Psig_class_type _ | Psig_modsubst _ | Psig_typesubst _
  | Psig_modtypesubst _ ->
    attach t.inside si.psig_loc comments

and walk_value_description vd t comments =
  let open Ppxlib.Parsetree in
  let leading, trailing =
    partition_leading_trailing comments vd.pval_name.loc
  in
  attach t.leading vd.pval_name.loc leading;
  let after_name, rest =
    partition_adjacent_trailing vd.pval_name.loc trailing
  in
  attach t.trailing vd.pval_name.loc after_name;
  let before, inside, after = partition_by_loc rest vd.pval_type.ptyp_loc in
  attach t.leading vd.pval_type.ptyp_loc before;
  walk_core_type vd.pval_type t inside;
  attach t.trailing vd.pval_type.ptyp_loc after

and walk_type_extension te t comments =
  let open Ppxlib.Parsetree in
  let leading, trailing =
    partition_leading_trailing comments te.ptyext_path.loc
  in
  attach t.leading te.ptyext_path.loc leading;
  let after_path, rest =
    partition_adjacent_trailing te.ptyext_path.loc trailing
  in
  attach t.trailing te.ptyext_path.loc after_path;
  let rest =
    List.fold_left te.ptyext_params ~init:rest ~f:(fun cmts (typ, _) ->
      let before, inside, after = partition_by_loc cmts typ.ptyp_loc in
      attach t.leading typ.ptyp_loc before;
      walk_core_type typ t inside;
      let after_typ, rest = partition_adjacent_trailing typ.ptyp_loc after in
      attach t.trailing typ.ptyp_loc after_typ;
      rest)
  in
  walk_list
    (List.map ~f:(fun ec -> ExtensionConstructor ec) te.ptyext_constructors)
    t
    rest

and walk_include_declaration id t comments =
  let open Ppxlib.Parsetree in
  let before, inside, after = partition_by_loc comments id.pincl_mod.pmod_loc in
  attach t.leading id.pincl_mod.pmod_loc before;
  walk_module_expr id.pincl_mod t inside;
  attach t.trailing id.pincl_mod.pmod_loc after

and walk_include_description id t comments =
  let open Ppxlib.Parsetree in
  let before, inside, after = partition_by_loc comments id.pincl_mod.pmty_loc in
  attach t.leading id.pincl_mod.pmty_loc before;
  walk_module_type id.pincl_mod t inside;
  attach t.trailing id.pincl_mod.pmty_loc after

and walk_module_type_declaration mtd t comments =
  let open Ppxlib.Parsetree in
  let leading, trailing =
    partition_leading_trailing comments mtd.pmtd_name.loc
  in
  attach t.leading mtd.pmtd_name.loc leading;
  match mtd.pmtd_type with
  | None -> attach t.trailing mtd.pmtd_name.loc trailing
  | Some mod_type ->
    let after_name, rest =
      partition_adjacent_trailing mtd.pmtd_name.loc trailing
    in
    attach t.trailing mtd.pmtd_name.loc after_name;
    let before, inside, after = partition_by_loc rest mod_type.pmty_loc in
    attach t.leading mod_type.pmty_loc before;
    walk_module_type mod_type t inside;
    attach t.trailing mod_type.pmty_loc after

and walk_module_binding mb t comments =
  let open Ppxlib.Parsetree in
  let leading, trailing = partition_leading_trailing comments mb.pmb_name.loc in
  attach t.leading mb.pmb_name.loc leading;
  let after_name, rest = partition_adjacent_trailing mb.pmb_name.loc trailing in
  attach t.trailing mb.pmb_name.loc after_name;
  let leading, inside, trailing = partition_by_loc rest mb.pmb_expr.pmod_loc in
  attach t.leading mb.pmb_expr.pmod_loc leading;
  walk_module_expr mb.pmb_expr t inside;
  attach t.trailing mb.pmb_expr.pmod_loc trailing

and walk_module_declaration md t comments =
  let open Ppxlib.Parsetree in
  let leading, trailing = partition_leading_trailing comments md.pmd_name.loc in
  attach t.leading md.pmd_name.loc leading;
  let after_name, rest = partition_adjacent_trailing md.pmd_name.loc trailing in
  attach t.trailing md.pmd_name.loc after_name;
  let leading, inside, trailing = partition_by_loc rest md.pmd_type.pmty_loc in
  attach t.leading md.pmd_type.pmty_loc leading;
  walk_module_type md.pmd_type t inside;
  attach t.trailing md.pmd_type.pmty_loc trailing

and walk_value_bindings vbs t comments =
  walk_list (List.map ~f:(fun vb -> ValueBinding vb) vbs) t comments

and walk_open_description od t comments =
  let open Ppxlib.Parsetree in
  let loc = od.popen_expr.pmod_loc in
  let leading, trailing = partition_leading_trailing comments loc in
  attach t.leading loc leading;
  attach t.trailing loc trailing

and walk_open_description_sig od t comments =
  let open Ppxlib.Parsetree in
  let loc = od.popen_expr.loc in
  let leading, trailing = partition_leading_trailing comments loc in
  attach t.leading loc leading;
  attach t.trailing loc trailing

and walk_type_declarations tds t comments =
  walk_list (List.map ~f:(fun td -> TypeDeclaration td) tds) t comments

and walk_type_declaration td t comments =
  let open Ppxlib.Parsetree in
  let before_name, rest =
    partition_leading_trailing comments td.ptype_name.loc
  in
  attach t.leading td.ptype_name.loc before_name;
  (* Don't use partition_adjacent_trailing for name - it consumes too many
     comments on same line. Instead, use partition_by_loc with the next element
     to properly separate comments. *)
  let after_name, rest =
    match td.ptype_params, td.ptype_manifest, td.ptype_kind with
    | (typ, _) :: _, _, _ ->
      (* Has type params - partition by first param location *)
      partition_by_loc rest typ.ptyp_loc |> fun (before, inside, after) ->
      before, inside @ after
    | [], Some manifest, _ ->
      (* Has manifest - partition by manifest location *)
      partition_by_loc rest manifest.ptyp_loc |> fun (before, inside, after) ->
      before, inside @ after
    | [], None, Ptype_variant (cd :: _) ->
      (* Has variant constructors - partition by first constructor *)
      partition_by_loc rest cd.pcd_loc |> fun (before, inside, after) ->
      before, inside @ after
    | [], None, Ptype_record (ld :: _) ->
      (* Has record fields - partition by first field *)
      partition_by_loc rest ld.pld_loc |> fun (before, inside, after) ->
      before, inside @ after
    | [], None, _ ->
      (* No following elements - take only same-line comments *)
      partition_by_on_same_line td.ptype_name.loc rest
  in
  attach t.trailing td.ptype_name.loc after_name;
  (* Type params - need to be careful not to consume comments belonging to
     manifest *)
  let params = td.ptype_params in
  let rest =
    List.fold_left
      (List.mapi params ~f:(fun i p -> i, p))
      ~init:rest
      ~f:(fun cmts (i, (typ, _)) ->
        let before, inside, after = partition_by_loc cmts typ.ptyp_loc in
        attach t.leading typ.ptyp_loc before;
        walk_core_type typ t inside;
        (* For trailing comments, only take truly adjacent ones if there's more
           content after *)
        let has_more_params = i < List.length params - 1 in
        let has_manifest = Option.is_some td.ptype_manifest in
        let has_kind =
          match td.ptype_kind with Ptype_abstract -> false | _ -> true
        in
        let after_typ, remaining =
          if has_more_params || has_manifest || has_kind
          then
            (* More content follows - leave all comments for the next element.
               Don't consume comments as trailing to type params. *)
            [], after
          else partition_by_on_same_line typ.ptyp_loc after
        in
        attach t.trailing typ.ptyp_loc after_typ;
        remaining)
  in
  (* Manifest *)
  let rest =
    match td.ptype_manifest with
    | Some typ ->
      let before, inside, after = partition_by_loc rest typ.ptyp_loc in
      attach t.leading typ.ptyp_loc before;
      walk_core_type typ t inside;
      let after_typ, rest = partition_by_on_same_line typ.ptyp_loc after in
      attach t.trailing typ.ptyp_loc after_typ;
      rest
    | None -> rest
  in
  (* Kind *)
  let rest =
    match td.ptype_kind with
    | Ptype_abstract | Ptype_open -> rest
    | Ptype_record lds ->
      if lds = []
      then (
        attach t.inside td.ptype_loc rest;
        [])
      else (
        walk_list (List.map ~f:(fun ld -> LabelDeclaration ld) lds) t rest;
        [])
    | Ptype_variant cds ->
      walk_constructor_declarations cds t rest;
      []
  in
  attach t.trailing td.ptype_loc rest

and walk_label_declaration ld t comments =
  let open Ppxlib.Parsetree in
  let before_name, rest = partition_leading_trailing comments ld.pld_name.loc in
  attach t.leading ld.pld_name.loc before_name;
  let after_name, rest = partition_adjacent_trailing ld.pld_name.loc rest in
  attach t.trailing ld.pld_name.loc after_name;
  let before, inside, after = partition_by_loc rest ld.pld_type.ptyp_loc in
  attach t.leading ld.pld_type.ptyp_loc before;
  walk_core_type ld.pld_type t inside;
  attach t.trailing ld.pld_type.ptyp_loc after

and walk_constructor_declarations cds t comments =
  let open Ppxlib.Parsetree in
  let _ =
    List.fold_left cds ~init:(None, comments) ~f:(fun (prev_loc, cmts) cd ->
      let curr_loc = cd.pcd_loc in
      let leading, inside, trailing = partition_by_loc cmts curr_loc in
      (match prev_loc with
      | None -> attach t.leading curr_loc leading
      | Some prev_loc ->
        if
          prev_loc.Location.loc_end.pos_lnum
          == curr_loc.Location.loc_start.pos_lnum
        then (
          let after_prev, before_curr =
            partition_adjacent_trailing prev_loc leading
          in
          attach t.trailing prev_loc after_prev;
          attach t.leading curr_loc before_curr)
        else
          let on_same_line, after_prev =
            partition_by_on_same_line prev_loc leading
          in
          attach t.trailing prev_loc on_same_line;
          attach t.leading curr_loc after_prev);
      walk_constructor_declaration cd t inside;
      Some curr_loc, trailing)
  in
  ()

and walk_constructor_declaration cd t comments =
  let open Ppxlib.Parsetree in
  let before_name, rest = partition_leading_trailing comments cd.pcd_name.loc in
  attach t.leading cd.pcd_name.loc before_name;
  (* For constructor name trailing comments, only take comments immediately
     adjacent. Comments before arguments should go to the arguments, not to the
     name. *)
  let after_name, rest =
    match cd.pcd_args with
    | Pcstr_tuple (_ :: _) | Pcstr_record (_ :: _) ->
      (* Has args - only take comments on the same line AND before the opening
         paren. Don't consume comments that should be leading to arguments. *)
      [], rest (* Leave all remaining comments for the arguments *)
    | Pcstr_tuple [] | Pcstr_record [] ->
      (* No args - take same-line comments *)
      partition_by_on_same_line cd.pcd_name.loc rest
  in
  attach t.trailing cd.pcd_name.loc after_name;
  let rest = walk_constructor_arguments cd.pcd_args t rest in
  let rest =
    match cd.pcd_res with
    | Some typ ->
      let before, inside, after = partition_by_loc rest typ.ptyp_loc in
      attach t.leading typ.ptyp_loc before;
      walk_core_type typ t inside;
      let after_typ, rest = partition_adjacent_trailing typ.ptyp_loc after in
      attach t.trailing typ.ptyp_loc after_typ;
      rest
    | None -> rest
  in
  attach t.trailing cd.pcd_loc rest

and walk_constructor_arguments args t comments =
  let open Ppxlib.Parsetree in
  match args with
  | Pcstr_tuple typs ->
    let n = List.length typs in
    List.fold_left
      (List.mapi typs ~f:(fun i typ -> i, typ))
      ~init:comments
      ~f:(fun cmts (i, typ) ->
        let effective_loc =
          match get_comment_loc_typ typ with
          | Some loc ->
            { typ.ptyp_loc with Location.loc_start = loc.Location.loc_start }
          | None -> typ.ptyp_loc
        in
        let before, inside, after = partition_by_loc cmts effective_loc in
        attach t.leading typ.ptyp_loc before;
        walk_core_type typ t inside;
        let after_typ, rest =
          if i < n - 1
          then
            let next_typ = List.nth typs (i + 1) in
            let next_loc =
              match get_comment_loc_typ next_typ with
              | Some loc ->
                { next_typ.ptyp_loc with
                  Location.loc_start = loc.Location.loc_start
                }
              | None -> next_typ.ptyp_loc
            in
            partition_by_loc after next_loc |> fun (b, i, a) -> b, i @ a
          else partition_by_on_same_line typ.ptyp_loc after
        in
        attach t.trailing typ.ptyp_loc after_typ;
        rest)
  | Pcstr_record lds ->
    walk_list (List.map ~f:(fun ld -> LabelDeclaration ld) lds) t comments;
    []

and walk_value_binding vb t comments =
  let open Ppxlib.Parsetree in
  let pattern_loc = vb.pvb_pat.ppat_loc in
  let expr_loc = vb.pvb_expr.pexp_loc in
  let leading, inside, trailing = partition_by_loc comments pattern_loc in
  attach t.leading pattern_loc leading;
  walk_pattern vb.pvb_pat t inside;
  (* Separate comments that are inside the expression from those before it. Only
     apply adjacent-trailing logic to comments BEFORE the expression. *)
  let before_expr_start, inside_or_after_expr =
    List.partition
      ~f:(fun comment ->
        let cmt_loc = Reason_comment.location comment in
        cmt_loc.loc_start.pos_cnum < expr_loc.loc_start.pos_cnum)
      trailing
  in
  (* For comments before expr, apply adjacent trailing logic *)
  let after_pat, before_expr =
    partition_adjacent_trailing pattern_loc before_expr_start
  in
  attach t.trailing pattern_loc after_pat;
  attach t.leading expr_loc before_expr;
  (* For comments inside or after expr, pass to walk_expression *)
  let _leading, inside_expr, after_expr =
    partition_by_loc inside_or_after_expr expr_loc
  in
  walk_expression vb.pvb_expr t inside_expr;
  attach t.trailing expr_loc after_expr

and walk_expression expr t comments =
  let open Ppxlib.Parsetree in
  let open Location in
  match expr.pexp_desc with
  | _ when comments = [] -> ()
  | Pexp_constant _ ->
    let leading, trailing = partition_leading_trailing comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    attach t.trailing expr.pexp_loc trailing
  | Pexp_ident longident ->
    (* Use longident.loc for partitioning but attach to expr.pexp_loc for lookup *)
    let leading, trailing = partition_leading_trailing comments longident.loc in
    attach t.leading expr.pexp_loc leading;
    attach t.trailing expr.pexp_loc trailing
  | Pexp_let
      ( _
      , vbs
      , { pexp_desc = Pexp_construct ({ txt = Lident "()"; _ }, None); _ } ) ->
    walk_value_bindings vbs t comments
  | Pexp_let (_, vbs, expr2) ->
    let comments =
      List.fold_left vbs ~init:comments ~f:(fun cmts vb ->
        let loc =
          if vb.pvb_pat.ppat_loc.loc_ghost
          then vb.pvb_expr.pexp_loc
          else vb.pvb_loc
        in
        let before, inside, after = partition_by_loc cmts loc in
        attach t.leading loc before;
        walk_value_binding vb t inside;
        let after_vb, rest = partition_by_on_same_line loc after in
        attach t.trailing loc after_vb;
        rest)
    in
    let leading, inside, trailing = partition_by_loc comments expr2.pexp_loc in
    attach t.leading expr2.pexp_loc leading;
    walk_expression expr2 t inside;
    attach t.trailing expr2.pexp_loc trailing
  | Pexp_sequence (e1, e2) ->
    let leading, inside, trailing = partition_by_loc comments e1.pexp_loc in
    let comments =
      let after_expr, rest = partition_by_on_same_line e1.pexp_loc trailing in
      attach t.leading e1.pexp_loc leading;
      walk_expression e1 t inside;
      attach t.trailing e1.pexp_loc after_expr;
      rest
    in
    let leading, inside, trailing = partition_by_loc comments e2.pexp_loc in
    attach t.leading e2.pexp_loc leading;
    walk_expression e2 t inside;
    attach t.trailing e2.pexp_loc trailing
  | Pexp_open (_, expr2) ->
    let leading, trailing = partition_leading_trailing comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    let leading, inside, trailing = partition_by_loc trailing expr2.pexp_loc in
    attach t.leading expr2.pexp_loc leading;
    walk_expression expr2 t inside;
    attach t.trailing expr2.pexp_loc trailing
  | Pexp_extension ext -> walk_extension ext t comments
  | Pexp_letexception (ec, expr2) ->
    let leading, inside, trailing = partition_by_loc comments ec.pext_loc in
    attach t.leading ec.pext_loc leading;
    walk_extension_constructor ec t inside;
    let after_ec, rest = partition_by_on_same_line ec.pext_loc trailing in
    attach t.trailing ec.pext_loc after_ec;
    let leading, inside, trailing = partition_by_loc rest expr2.pexp_loc in
    attach t.leading expr2.pexp_loc leading;
    walk_expression expr2 t inside;
    attach t.trailing expr2.pexp_loc trailing
  | Pexp_letmodule (name, mod_expr, expr2) ->
    let leading, trailing = partition_leading_trailing comments name.loc in
    attach t.leading name.loc leading;
    let after_name, rest = partition_adjacent_trailing name.loc trailing in
    attach t.trailing name.loc after_name;
    let before, inside, after = partition_by_loc rest mod_expr.pmod_loc in
    attach t.leading mod_expr.pmod_loc before;
    walk_module_expr mod_expr t inside;
    let after_mod, rest = partition_by_on_same_line mod_expr.pmod_loc after in
    attach t.trailing mod_expr.pmod_loc after_mod;
    let leading, inside, trailing = partition_by_loc rest expr2.pexp_loc in
    attach t.leading expr2.pexp_loc leading;
    walk_expression expr2 t inside;
    attach t.trailing expr2.pexp_loc trailing
  | Pexp_assert e | Pexp_lazy e ->
    let leading, inside, trailing = partition_by_loc comments e.pexp_loc in
    attach t.leading e.pexp_loc leading;
    walk_expression e t inside;
    attach t.trailing e.pexp_loc trailing
  | Pexp_coerce (e, _, typ) ->
    let leading, inside, trailing = partition_by_loc comments e.pexp_loc in
    attach t.leading e.pexp_loc leading;
    walk_expression e t inside;
    let after_expr, rest = partition_adjacent_trailing e.pexp_loc trailing in
    attach t.trailing e.pexp_loc after_expr;
    let leading, inside, trailing = partition_by_loc rest typ.ptyp_loc in
    attach t.leading typ.ptyp_loc leading;
    walk_core_type typ t inside;
    attach t.trailing typ.ptyp_loc trailing
  | Pexp_constraint (e, typ) ->
    let leading, inside, trailing = partition_by_loc comments e.pexp_loc in
    attach t.leading e.pexp_loc leading;
    walk_expression e t inside;
    let after_expr, rest = partition_adjacent_trailing e.pexp_loc trailing in
    attach t.trailing e.pexp_loc after_expr;
    let leading, inside, trailing = partition_by_loc rest typ.ptyp_loc in
    attach t.leading typ.ptyp_loc leading;
    walk_core_type typ t inside;
    attach t.trailing typ.ptyp_loc trailing
  | Pexp_tuple [] | Pexp_array [] -> attach t.inside expr.pexp_loc comments
  | Pexp_construct ({ txt = Lident "[]"; _ }, _) ->
    attach t.inside expr.pexp_loc comments
  | Pexp_construct ({ txt = Lident "::"; _ }, _) ->
    (* Flatten list expressions to properly handle comments between elements *)
    let elements = collect_list_exprs [] expr in
    walk_list (List.map ~f:(fun e -> Expression e) elements) t comments
  | Pexp_construct (longident, None) ->
    (* Use longident.loc for partitioning but attach to expr.pexp_loc for lookup *)
    let leading, trailing = partition_leading_trailing comments longident.loc in
    attach t.leading expr.pexp_loc leading;
    attach t.trailing expr.pexp_loc trailing
  | Pexp_construct (longident, Some arg) ->
    let leading, trailing = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc leading;
    let before_arg, inside_arg, after_arg =
      partition_by_loc trailing arg.pexp_loc
    in
    attach t.trailing longident.loc before_arg;
    walk_expression arg t inside_arg;
    attach t.trailing arg.pexp_loc after_arg
  | Pexp_variant (_, None) -> ()
  | Pexp_variant (_, Some e) -> walk_expression e t comments
  | Pexp_array exprs | Pexp_tuple exprs ->
    walk_list (List.map ~f:(fun e -> Expression e) exprs) t comments
  | Pexp_record (rows, spread_opt) ->
    let comments =
      match spread_opt with
      | None -> comments
      | Some spread ->
        let leading, inside, trailing =
          partition_by_loc comments spread.pexp_loc
        in
        attach t.leading spread.pexp_loc leading;
        walk_expression spread t inside;
        let after_spread, rest =
          partition_adjacent_trailing spread.pexp_loc trailing
        in
        attach t.trailing spread.pexp_loc after_spread;
        rest
    in
    walk_list
      (List.map ~f:(fun (li, e) -> ExprRecordRow (li, e)) rows)
      t
      comments
  | Pexp_field (e, longident) ->
    let leading, inside, trailing = partition_by_loc comments e.pexp_loc in
    attach t.leading e.pexp_loc leading;
    walk_expression e t inside;
    let after_expr, rest = partition_adjacent_trailing e.pexp_loc trailing in
    attach t.trailing e.pexp_loc after_expr;
    let leading, trailing = partition_leading_trailing rest longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | Pexp_setfield (e1, longident, e2) ->
    let leading, inside, trailing = partition_by_loc comments e1.pexp_loc in
    attach t.leading e1.pexp_loc leading;
    walk_expression e1 t inside;
    let after_e1, rest = partition_adjacent_trailing e1.pexp_loc trailing in
    attach t.trailing e1.pexp_loc after_e1;
    let before_longident, after_longident =
      partition_leading_trailing rest longident.loc
    in
    attach t.leading longident.loc before_longident;
    let after_longident, rest =
      partition_adjacent_trailing longident.loc after_longident
    in
    attach t.trailing longident.loc after_longident;
    let leading, inside, trailing = partition_by_loc rest e2.pexp_loc in
    attach t.leading e2.pexp_loc leading;
    walk_expression e2 t inside;
    attach t.trailing e2.pexp_loc trailing
  | Pexp_ifthenelse (cond, then_expr, else_opt) ->
    let leading, rest = partition_leading_trailing comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    let leading, inside, trailing = partition_by_loc rest cond.pexp_loc in
    let comments =
      let after_cond, rest =
        partition_adjacent_trailing cond.pexp_loc trailing
      in
      attach t.leading cond.pexp_loc leading;
      walk_expression cond t inside;
      attach t.trailing cond.pexp_loc after_cond;
      rest
    in
    let leading, inside, trailing =
      partition_by_loc comments then_expr.pexp_loc
    in
    let comments =
      let after_then, rest =
        partition_adjacent_trailing then_expr.pexp_loc trailing
      in
      attach t.leading then_expr.pexp_loc leading;
      walk_expression then_expr t inside;
      attach t.trailing then_expr.pexp_loc after_then;
      rest
    in
    (match else_opt with
    | None -> ()
    | Some else_expr ->
      let leading, inside, trailing =
        partition_by_loc comments else_expr.pexp_loc
      in
      attach t.leading else_expr.pexp_loc leading;
      walk_expression else_expr t inside;
      attach t.trailing else_expr.pexp_loc trailing)
  | Pexp_while (cond, body) ->
    let leading, inside, trailing = partition_by_loc comments cond.pexp_loc in
    let rest =
      let after_cond, rest =
        partition_adjacent_trailing cond.pexp_loc trailing
      in
      attach t.leading cond.pexp_loc leading;
      walk_expression cond t inside;
      attach t.trailing cond.pexp_loc after_cond;
      rest
    in
    let leading, inside, trailing = partition_by_loc rest body.pexp_loc in
    attach t.leading body.pexp_loc leading;
    walk_expression body t inside;
    attach t.trailing body.pexp_loc trailing
  | Pexp_for (pat, start, finish, _, body) ->
    let leading, inside, trailing = partition_by_loc comments pat.ppat_loc in
    attach t.leading pat.ppat_loc leading;
    walk_pattern pat t inside;
    let after_pat, rest = partition_adjacent_trailing pat.ppat_loc trailing in
    attach t.trailing pat.ppat_loc after_pat;
    let leading, inside, trailing = partition_by_loc rest start.pexp_loc in
    attach t.leading start.pexp_loc leading;
    walk_expression start t inside;
    let after_start, rest =
      partition_adjacent_trailing start.pexp_loc trailing
    in
    attach t.trailing start.pexp_loc after_start;
    let leading, inside, trailing = partition_by_loc rest finish.pexp_loc in
    attach t.leading finish.pexp_loc leading;
    walk_expression finish t inside;
    let after_finish, rest =
      partition_adjacent_trailing finish.pexp_loc trailing
    in
    attach t.trailing finish.pexp_loc after_finish;
    let leading, inside, trailing = partition_by_loc rest body.pexp_loc in
    attach t.leading body.pexp_loc leading;
    walk_expression body t inside;
    attach t.trailing body.pexp_loc trailing
  | Pexp_pack mod_expr ->
    let leading, inside, trailing =
      partition_by_loc comments mod_expr.pmod_loc
    in
    attach t.leading mod_expr.pmod_loc leading;
    walk_module_expr mod_expr t inside;
    attach t.trailing mod_expr.pmod_loc trailing
  | Pexp_match (e, cases) | Pexp_try (e, cases) ->
    let leading, inside, trailing = partition_by_loc comments e.pexp_loc in
    attach t.leading e.pexp_loc leading;
    walk_expression e t inside;
    let after_expr, rest = partition_adjacent_trailing e.pexp_loc trailing in
    attach t.trailing e.pexp_loc after_expr;
    walk_cases cases t rest
  | Pexp_apply (func, args) when is_jsx_expression expr ->
    (* JSX expression - use prop name locations for comment attachment *)
    let before, inside, after = partition_by_loc comments func.pexp_loc in
    attach t.leading func.pexp_loc before;
    walk_expression func t inside;
    (* Don't use partition_adjacent_trailing here - all comments after the tag
       should be available for props, not attached to the tag *)
    (* Separate children from props and handle them specially *)
    let rec extract_children expr =
      match expr.pexp_desc with
      | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> []
      | Pexp_construct
          ( { txt = Lident "::"; _ }
          , Some { pexp_desc = Pexp_tuple [ hd; tl ]; _ } ) ->
        hd :: extract_children tl
      | _ -> [ expr ]
    in
    let props, children_exprs =
      List.partition_map args ~f:(fun (lbl, e) ->
        match lbl with
        | Ppxlib.Asttypes.Labelled "children" -> Right (extract_children e)
        (* Filter out trailing unit argument *)
        | Ppxlib.Asttypes.Nolabel ->
          (match e.pexp_desc with
          | Pexp_construct ({ txt = Lident "()"; _ }, None) -> Right []
          | _ -> Left (lbl, e))
        | _ -> Left (lbl, e))
    in
    let children = List.concat children_exprs in
    (* Convert props to JsxProp nodes when they have prop location metadata *)
    let jsx_prop_nodes =
      List.filter_map props ~f:(fun (_, e) ->
        match get_jsx_prop_loc e with
        | Some name_loc ->
          Some
            (JsxProp { jsx_prop_name_loc = name_loc; jsx_prop_value_expr = e })
        | None -> Some (Expression e))
    in
    (* Combine props and children as nodes, then walk them all together *)
    let child_nodes = List.map ~f:(fun e -> Expression e) children in
    let all_nodes = jsx_prop_nodes @ child_nodes in
    walk_list all_nodes t after
  | Pexp_apply (func, args) ->
    (* Regular function application *)
    let before, inside, after = partition_by_loc comments func.pexp_loc in
    (* Attach leading to expr.pexp_loc so they're found when printer uses expr
       location *)
    attach t.leading expr.pexp_loc before;
    walk_expression func t inside;
    (* Don't use partition_adjacent_trailing - comments after func should go to
       args *)
    walk_list (List.map ~f:(fun (_, e) -> Expression e) args) t after
  | Pexp_function (params, constraint_opt, body) ->
    (* For function params, don't use partition_adjacent_trailing. Comments
       between params should go to the next param as leading, not to the
       previous param as trailing. *)
    let comments =
      List.fold_left params ~init:comments ~f:(fun cmts param ->
        match param.pparam_desc with
        | Pparam_val (_, default_opt, pat) ->
          let leading, inside, trailing = partition_by_loc cmts pat.ppat_loc in
          attach t.leading pat.ppat_loc leading;
          walk_pattern pat t inside;
          (match default_opt with
          | Some default ->
            let before_default, inside_default, after_default =
              partition_by_loc trailing default.pexp_loc
            in
            attach t.leading default.pexp_loc before_default;
            walk_expression default t inside_default;
            after_default
          | None -> trailing)
        | Pparam_newtype name ->
          let leading, trailing = partition_leading_trailing cmts name.loc in
          attach t.leading name.loc leading;
          trailing)
    in
    let comments =
      match constraint_opt with
      | None -> comments
      | Some (Pconstraint typ) ->
        let leading, inside, trailing =
          partition_by_loc comments typ.ptyp_loc
        in
        attach t.leading typ.ptyp_loc leading;
        walk_core_type typ t inside;
        let after_typ, rest =
          partition_adjacent_trailing typ.ptyp_loc trailing
        in
        attach t.trailing typ.ptyp_loc after_typ;
        rest
      | Some (Pcoerce (_, typ)) ->
        let leading, inside, trailing =
          partition_by_loc comments typ.ptyp_loc
        in
        attach t.leading typ.ptyp_loc leading;
        walk_core_type typ t inside;
        let after_typ, rest =
          partition_adjacent_trailing typ.ptyp_loc trailing
        in
        attach t.trailing typ.ptyp_loc after_typ;
        rest
    in
    (match body with
    | Pfunction_body e ->
      let leading, inside, trailing = partition_by_loc comments e.pexp_loc in
      attach t.leading e.pexp_loc leading;
      walk_expression e t inside;
      attach t.trailing e.pexp_loc trailing
    | Pfunction_cases (cases, _, _) -> walk_cases cases t comments)
  | Pexp_newtype (_, e) -> walk_expression e t comments
  | Pexp_send (e, _) ->
    let leading, inside, trailing = partition_by_loc comments e.pexp_loc in
    attach t.leading e.pexp_loc leading;
    walk_expression e t inside;
    attach t.trailing e.pexp_loc trailing
  | Pexp_new _ | Pexp_setinstvar _ | Pexp_override _ | Pexp_poly _
  | Pexp_object _ | Pexp_letop _ | Pexp_unreachable ->
    attach t.inside expr.pexp_loc comments

(* Walk a single case and return remaining comments after the case *)
and walk_case_and_return_rest case t comments =
  let open Ppxlib.Parsetree in
  let before, inside, after = partition_by_loc comments case.pc_lhs.ppat_loc in
  walk_pattern case.pc_lhs t (before @ inside);
  let after_pat, rest =
    partition_adjacent_trailing case.pc_lhs.ppat_loc after
  in
  attach t.trailing case.pc_lhs.ppat_loc after_pat;
  let comments =
    match case.pc_guard with
    | Some guard ->
      let before, inside, after = partition_by_loc rest guard.pexp_loc in
      let after_guard, rest =
        partition_adjacent_trailing guard.pexp_loc after
      in
      attach t.leading guard.pexp_loc before;
      walk_expression guard t inside;
      attach t.trailing guard.pexp_loc after_guard;
      rest
    | None -> rest
  in
  let leading, inside, trailing =
    partition_by_loc comments case.pc_rhs.pexp_loc
  in
  attach t.leading case.pc_rhs.pexp_loc leading;
  walk_expression case.pc_rhs t inside;
  let after_rhs, remaining =
    partition_adjacent_trailing case.pc_rhs.pexp_loc trailing
  in
  attach t.trailing case.pc_rhs.pexp_loc after_rhs;
  remaining

and walk_case case t comments =
  let _ = walk_case_and_return_rest case t comments in
  ()

(* Walk cases directly - simpler approach that processes cases sequentially *)
and walk_cases cases t comments =
  let rec loop cases comments =
    match cases with
    | [] -> ()
    | [ case ] ->
      (* Last case gets all remaining comments *)
      walk_case case t comments
    | case :: rest ->
      let remaining = walk_case_and_return_rest case t comments in
      loop rest remaining
  in
  loop cases comments

and walk_expr_record_row (longident, e) t comments =
  let open Ppxlib.Parsetree in
  let before, after = partition_leading_trailing comments longident.loc in
  attach t.leading longident.loc before;
  let after_longident, rest = partition_adjacent_trailing longident.loc after in
  attach t.trailing longident.loc after_longident;
  let leading, inside, trailing = partition_by_loc rest e.pexp_loc in
  attach t.leading e.pexp_loc leading;
  walk_expression e t inside;
  attach t.trailing e.pexp_loc trailing

and walk_extension_constructor ec t comments =
  let open Ppxlib.Parsetree in
  let leading, trailing =
    partition_leading_trailing comments ec.pext_name.loc
  in
  attach t.leading ec.pext_name.loc leading;
  (* Use partition_by_loc to not consume comments that belong to arguments *)
  walk_extension_constructor_kind ec.pext_kind t trailing

and walk_extension_constructor_kind kind t comments =
  let open Ppxlib.Parsetree in
  match kind with
  | Pext_rebind longident ->
    let leading, trailing = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | Pext_decl (_, args, res_opt) ->
    let rest = walk_constructor_arguments args t comments in
    (match res_opt with
    | None -> ()
    | Some typ ->
      let leading, inside, trailing = partition_by_loc rest typ.ptyp_loc in
      attach t.leading typ.ptyp_loc leading;
      walk_core_type typ t inside;
      attach t.trailing typ.ptyp_loc trailing)

and walk_module_expr me t comments =
  let open Ppxlib.Parsetree in
  match me.pmod_desc with
  | Pmod_ident longident ->
    let leading, trailing = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | Pmod_structure [] -> attach t.inside me.pmod_loc comments
  | Pmod_structure structure -> walk_structure structure t comments
  | Pmod_extension ext -> walk_extension ext t comments
  | Pmod_unpack e ->
    let leading, inside, trailing = partition_by_loc comments e.pexp_loc in
    attach t.leading e.pexp_loc leading;
    walk_expression e t inside;
    attach t.trailing e.pexp_loc trailing
  | Pmod_constraint (mod_expr, mod_type) ->
    let before, inside, after = partition_by_loc comments mod_expr.pmod_loc in
    attach t.leading mod_expr.pmod_loc before;
    walk_module_expr mod_expr t inside;
    let after_mod, rest = partition_adjacent_trailing mod_expr.pmod_loc after in
    attach t.trailing mod_expr.pmod_loc after_mod;
    let before, inside, after = partition_by_loc rest mod_type.pmty_loc in
    attach t.leading mod_type.pmty_loc before;
    walk_module_type mod_type t inside;
    attach t.trailing mod_type.pmty_loc after
  | Pmod_apply (m1, m2) -> walk_list [ ModuleExpr m1; ModuleExpr m2 ] t comments
  | Pmod_apply_unit m ->
    let leading, inside, trailing = partition_by_loc comments m.pmod_loc in
    attach t.leading m.pmod_loc leading;
    walk_module_expr m t inside;
    attach t.trailing m.pmod_loc trailing
  | Pmod_functor (Unit, body) -> walk_module_expr body t comments
  | Pmod_functor (Named (name, mod_type), body) ->
    let leading, trailing = partition_leading_trailing comments name.loc in
    attach t.leading name.loc leading;
    let after_name, rest = partition_adjacent_trailing name.loc trailing in
    attach t.trailing name.loc after_name;
    let before, inside, after = partition_by_loc rest mod_type.pmty_loc in
    attach t.leading mod_type.pmty_loc before;
    walk_module_type mod_type t inside;
    let after_type, rest =
      partition_adjacent_trailing mod_type.pmty_loc after
    in
    attach t.trailing mod_type.pmty_loc after_type;
    walk_module_expr body t rest

and walk_module_type mt t comments =
  let open Ppxlib.Parsetree in
  match mt.pmty_desc with
  | Pmty_ident longident | Pmty_alias longident ->
    let leading, trailing = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | Pmty_signature [] -> attach t.inside mt.pmty_loc comments
  | Pmty_signature signature -> walk_signature signature t comments
  | Pmty_extension ext -> walk_extension ext t comments
  | Pmty_typeof mod_expr ->
    let leading, inside, trailing =
      partition_by_loc comments mod_expr.pmod_loc
    in
    attach t.leading mod_expr.pmod_loc leading;
    walk_module_expr mod_expr t inside;
    attach t.trailing mod_expr.pmod_loc trailing
  | Pmty_with (mod_type, _) ->
    let before, inside, after = partition_by_loc comments mod_type.pmty_loc in
    attach t.leading mod_type.pmty_loc before;
    walk_module_type mod_type t inside;
    attach t.trailing mod_type.pmty_loc after
  | Pmty_functor (Unit, body) -> walk_module_type body t comments
  | Pmty_functor (Named (name, mod_type), body) ->
    let leading, trailing = partition_leading_trailing comments name.loc in
    attach t.leading name.loc leading;
    let after_name, rest = partition_adjacent_trailing name.loc trailing in
    attach t.trailing name.loc after_name;
    let before, inside, after = partition_by_loc rest mod_type.pmty_loc in
    attach t.leading mod_type.pmty_loc before;
    walk_module_type mod_type t inside;
    let after_type, rest =
      partition_adjacent_trailing mod_type.pmty_loc after
    in
    attach t.trailing mod_type.pmty_loc after_type;
    walk_module_type body t rest

and walk_pattern pat t comments =
  let open Ppxlib.Parsetree in
  match pat.ppat_desc with
  | _ when comments = [] -> ()
  | Ppat_alias (p, alias) ->
    let leading, inside, trailing = partition_by_loc comments p.ppat_loc in
    attach t.leading p.ppat_loc leading;
    walk_pattern p t inside;
    let after_pat, rest = partition_adjacent_trailing p.ppat_loc trailing in
    attach t.trailing p.ppat_loc after_pat;
    let before_alias, after_alias = partition_leading_trailing rest alias.loc in
    attach t.leading alias.loc before_alias;
    attach t.trailing alias.loc after_alias
  | Ppat_tuple []
  | Ppat_array []
  | Ppat_construct ({ txt = Lident "()"; _ }, _)
  | Ppat_construct ({ txt = Lident "[]"; _ }, _) ->
    attach t.inside pat.ppat_loc comments
  | Ppat_construct ({ txt = Lident "::"; _ }, _) ->
    (* Flatten list patterns to properly handle comments between elements *)
    let elements = collect_list_patterns [] pat in
    walk_list (List.map ~f:(fun p -> Pattern p) elements) t comments
  | Ppat_array pats | Ppat_tuple pats ->
    walk_list (List.map ~f:(fun p -> Pattern p) pats) t comments
  | Ppat_construct (li, None) ->
    (* Use constructor location for partitioning since pattern loc may include |
       bar *)
    let leading, trailing = partition_leading_trailing comments li.loc in
    attach t.leading pat.ppat_loc leading;
    attach t.trailing pat.ppat_loc trailing
  | Ppat_construct (constr, Some (_, p)) ->
    let leading, trailing = partition_leading_trailing comments constr.loc in
    attach t.leading constr.loc leading;
    let after_constr, rest = partition_adjacent_trailing constr.loc trailing in
    attach t.trailing constr.loc after_constr;
    let leading, inside, trailing = partition_by_loc rest p.ppat_loc in
    attach t.leading p.ppat_loc leading;
    walk_pattern p t inside;
    attach t.trailing p.ppat_loc trailing
  | Ppat_variant (_, None) -> ()
  | Ppat_variant (_, Some p) -> walk_pattern p t comments
  | Ppat_type _ -> ()
  | Ppat_record (rows, _) ->
    walk_list
      (List.map ~f:(fun (li, p) -> PatternRecordRow (li, p)) rows)
      t
      comments
  | Ppat_or _ ->
    let pats = collect_or_pattern_chain pat in
    walk_list (List.map ~f:(fun p -> Pattern p) pats) t comments
  | Ppat_constraint (p, typ) ->
    let before, inside, after = partition_by_loc comments p.ppat_loc in
    attach t.leading p.ppat_loc before;
    walk_pattern p t inside;
    let after_pat, rest = partition_adjacent_trailing p.ppat_loc after in
    attach t.trailing p.ppat_loc after_pat;
    let before, inside, after = partition_by_loc rest typ.ptyp_loc in
    attach t.leading typ.ptyp_loc before;
    walk_core_type typ t inside;
    attach t.trailing typ.ptyp_loc after
  | Ppat_exception p ->
    let leading, inside, trailing = partition_by_loc comments p.ppat_loc in
    attach t.leading p.ppat_loc leading;
    walk_pattern p t inside;
    attach t.trailing p.ppat_loc trailing
  | Ppat_unpack name ->
    let leading, trailing = partition_leading_trailing comments name.loc in
    attach t.leading name.loc leading;
    attach t.trailing name.loc trailing
  | Ppat_extension ext -> walk_extension ext t comments
  | Ppat_any | Ppat_var _ | Ppat_constant _ | Ppat_interval _ ->
    let leading, trailing = partition_leading_trailing comments pat.ppat_loc in
    attach t.leading pat.ppat_loc leading;
    attach t.trailing pat.ppat_loc trailing
  | Ppat_lazy p | Ppat_open (_, p) -> walk_pattern p t comments

and collect_or_pattern_chain pat =
  let open Ppxlib.Parsetree in
  match pat.ppat_desc with
  | Ppat_or (p1, p2) ->
    collect_or_pattern_chain p1 @ collect_or_pattern_chain p2
  | _ -> [ pat ]

and walk_pattern_record_row (longident, p) t comments =
  let open Ppxlib.Parsetree in
  (* Check for punning *)
  match longident.txt, p.ppat_desc with
  | Lident ident, Ppat_var { txt; _ } when String.equal ident txt ->
    let leading, trailing = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | _ ->
    let before, after = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc before;
    let after_lbl, rest = partition_adjacent_trailing longident.loc after in
    attach t.trailing longident.loc after_lbl;
    let leading, inside, trailing = partition_by_loc rest p.ppat_loc in
    attach t.leading p.ppat_loc leading;
    walk_pattern p t inside;
    attach t.trailing p.ppat_loc trailing

and walk_row_field rf t comments =
  let open Ppxlib.Parsetree in
  match rf.prf_desc with
  | Rtag ({ loc; _ }, _, _) ->
    let leading, trailing = partition_leading_trailing comments loc in
    attach t.leading loc leading;
    attach t.trailing loc trailing
  | Rinherit _ -> ()

and walk_core_type typ t comments =
  let open Ppxlib.Parsetree in
  match typ.ptyp_desc with
  | _ when comments = [] -> ()
  | Ptyp_tuple types ->
    walk_list (List.map ~f:(fun ct -> CoreType ct) types) t comments
  | Ptyp_extension ext -> walk_extension ext t comments
  | Ptyp_package (longident, constraints) ->
    let before, after = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc before;
    let after_longident, rest =
      partition_adjacent_trailing longident.loc after
    in
    attach t.trailing longident.loc after_longident;
    List.iter
      ~f:(fun (li, typ) ->
        let before, inside, after = partition_by_loc rest typ.ptyp_loc in
        attach t.leading li.Ppxlib.Asttypes.loc before;
        walk_core_type typ t inside;
        attach t.trailing typ.ptyp_loc after)
      constraints
  | Ptyp_alias (typ, _) ->
    let before, inside, after = partition_by_loc comments typ.ptyp_loc in
    attach t.leading typ.ptyp_loc before;
    walk_core_type typ t inside;
    attach t.trailing typ.ptyp_loc after
  | Ptyp_poly (vars, typ) ->
    let comments =
      List.fold_left vars ~init:comments ~f:(fun cmts v ->
        let loc = v.Ppxlib.Asttypes.loc in
        let before, after = partition_leading_trailing cmts loc in
        attach t.leading loc before;
        let after_v, rest = partition_adjacent_trailing loc after in
        attach t.trailing loc after_v;
        rest)
    in
    let before, inside, after = partition_by_loc comments typ.ptyp_loc in
    attach t.leading typ.ptyp_loc before;
    walk_core_type typ t inside;
    attach t.trailing typ.ptyp_loc after
  | Ptyp_variant (rows, _, _) ->
    walk_list (List.map ~f:(fun rf -> RowField rf) rows) t comments
  | Ptyp_constr (longident, args) ->
    let before, after = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc before;
    let after_longident, rest =
      partition_adjacent_trailing longident.loc after
    in
    attach t.trailing longident.loc after_longident;
    walk_list (List.map ~f:(fun ct -> CoreType ct) args) t rest
  | Ptyp_arrow (_, t1, t2) ->
    let before, inside, after = partition_by_loc comments t1.ptyp_loc in
    attach t.leading t1.ptyp_loc before;
    walk_core_type t1 t inside;
    let after_t1, rest = partition_adjacent_trailing t1.ptyp_loc after in
    attach t.trailing t1.ptyp_loc after_t1;
    let before, inside, after = partition_by_loc rest t2.ptyp_loc in
    attach t.leading t2.ptyp_loc before;
    walk_core_type t2 t inside;
    attach t.trailing t2.ptyp_loc after
  | Ptyp_object (fields, _) ->
    List.iter
      ~f:(fun field ->
        match field.pof_desc with
        | Otag (lbl, typ) ->
          let before, after = partition_leading_trailing comments lbl.loc in
          attach t.leading lbl.loc before;
          let after_lbl, rest = partition_adjacent_trailing lbl.loc after in
          attach t.trailing lbl.loc after_lbl;
          let before, inside, after = partition_by_loc rest typ.ptyp_loc in
          attach t.leading typ.ptyp_loc before;
          walk_core_type typ t inside;
          attach t.trailing typ.ptyp_loc after
        | Oinherit _ -> ())
      fields
  | Ptyp_class (longident, args) ->
    let before, after = partition_leading_trailing comments longident.loc in
    attach t.leading longident.loc before;
    let after_longident, rest =
      partition_adjacent_trailing longident.loc after
    in
    attach t.trailing longident.loc after_longident;
    walk_list (List.map ~f:(fun ct -> CoreType ct) args) t rest
  | Ptyp_any | Ptyp_var _ ->
    let leading, trailing = partition_leading_trailing comments typ.ptyp_loc in
    attach t.leading typ.ptyp_loc leading;
    attach t.trailing typ.ptyp_loc trailing
  | Ptyp_open (_, typ) -> walk_core_type typ t comments

and walk_extension (id, payload) t comments =
  let before, after = partition_leading_trailing comments id.loc in
  attach t.leading id.loc before;
  let after_id, rest = partition_adjacent_trailing id.loc after in
  attach t.trailing id.loc after_id;
  walk_payload payload t rest

and walk_attribute (attr : Ppxlib.Parsetree.attribute) t comments =
  let before, after = partition_leading_trailing comments attr.attr_name.loc in
  attach t.leading attr.attr_name.loc before;
  let after_id, rest = partition_adjacent_trailing attr.attr_name.loc after in
  attach t.trailing attr.attr_name.loc after_id;
  walk_payload attr.attr_payload t rest

and walk_payload payload t comments =
  let open Ppxlib.Parsetree in
  match payload with
  | PStr s -> walk_structure s t comments
  | PSig s -> walk_signature s t comments
  | PTyp typ -> walk_core_type typ t comments
  | PPat (pat, _) -> walk_pattern pat t comments

(* Comment printing functions *)

(* Strip the EOL marker character from comment text *)
let strip_eol_marker s =
  let eol_char = Reason_syntax_util.EOLMarker.char in
  let buf = Buffer.create (String.length s) in
  String.iter ~f:(fun c -> if c <> eol_char then Buffer.add_char buf c) s;
  Buffer.contents buf

let print_leading_comment ~hard_line ?next_comment comment =
  let single_line = Reason_comment.isLineComment comment in
  let content = strip_eol_marker (Reason_comment.wrap comment) in
  let separator =
    match next_comment with
    | Some next ->
      let next_loc = Comment.location next in
      let curr_loc = Comment.location comment in
      let diff =
        next_loc.Location.loc_start.pos_lnum
        - curr_loc.Location.loc_end.pos_lnum
      in
      let next_single_line = Reason_comment.isLineComment next in
      if single_line && next_single_line
      then hard_line
      else if single_line && not next_single_line
      then hard_line
      else if diff > 1
      then hard_line
      else hard_line
    | None -> hard_line
  in
  content, separator

let print_leading_comments ~text ~concat ~hard_line ~empty node tbl loc =
  let rec loop acc comments =
    match comments with
    | [] -> concat acc node
    | [ comment ] ->
      let content, _separator = print_leading_comment ~hard_line comment in
      let cmt_doc = text content in
      let diff =
        loc.Location.loc_start.pos_lnum
        - (Comment.location comment).Location.loc_end.pos_lnum
      in
      let separator =
        if Reason_comment.isLineComment comment
        then hard_line
        else if diff == 0
        then hard_line
        else if diff > 1
        then concat hard_line hard_line
        else hard_line
      in
      concat (concat (concat acc cmt_doc) separator) node
    | comment :: (next_comment :: _ as rest) ->
      let content, separator =
        print_leading_comment ~hard_line ~next_comment comment
      in
      let cmt_doc = text content in
      loop (concat (concat acc cmt_doc) separator) rest
  in
  match Hashtbl.find_opt tbl loc with
  | None -> node
  | Some comments ->
    Hashtbl.remove tbl loc;
    loop empty comments

let print_trailing_comment
      ~text
      ~concat
      ~space
      ~hard_line
      prev_loc
      node_loc
      comment
  =
  let single_line = Reason_comment.isLineComment comment in
  let content = strip_eol_marker (Reason_comment.wrap comment) in
  let diff =
    let cmt_start = (Comment.location comment).loc_start in
    cmt_start.pos_lnum - prev_loc.Location.loc_end.pos_lnum
  in
  let is_below =
    (Comment.location comment).loc_start.pos_lnum
    > node_loc.Location.loc_end.pos_lnum
  in
  if diff > 0 || is_below
  then
    concat
      (if diff > 1 then concat hard_line hard_line else hard_line)
      (text content)
  else if not single_line
  then concat space (text content)
  else concat space (text content)

let print_trailing_comments ~text ~concat ~space ~hard_line ~empty node tbl loc =
  let rec loop prev acc comments =
    match comments with
    | [] -> concat node acc
    | comment :: comments ->
      let cmt_doc =
        print_trailing_comment ~text ~concat ~space ~hard_line prev loc comment
      in
      loop (Comment.location comment) (concat acc cmt_doc) comments
  in
  match Hashtbl.find_opt tbl loc with
  | None -> node
  | Some [] -> node
  | Some comments ->
    Hashtbl.remove tbl loc;
    loop loc empty comments

let print_comments ~text ~concat ~space ~hard_line ~empty doc tbl loc =
  let with_leading =
    print_leading_comments ~text ~concat ~hard_line ~empty doc tbl.leading loc
  in
  print_trailing_comments
    ~text
    ~concat
    ~space
    ~hard_line
    ~empty
    with_leading
    tbl.trailing
    loc
