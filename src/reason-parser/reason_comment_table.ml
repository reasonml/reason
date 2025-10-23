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

let attach tbl loc comments =
  match comments with [] -> () | comments -> Hashtbl.replace tbl loc comments

(* Partition comments into leading/inside/trailing relative to a location *)
let partition_by_loc comments loc =
  let rec loop (leading, inside, trailing) comments =
    let open Location in
    match comments with
    | comment :: rest ->
      let cmt_loc = Comment.location comment in
      if cmt_loc.loc_end.pos_cnum <= loc.loc_start.pos_cnum
      then loop (comment :: leading, inside, trailing) rest
      else if cmt_loc.loc_start.pos_cnum >= loc.loc_end.pos_cnum
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
  let open Lexing in
  let rec loop ~prev_end_pos after_loc1 comments =
    match comments with
    | [] -> List.rev after_loc1, []
    | comment :: rest as all_comments ->
      (* Check if comment is adjacent (no whitespace) *)
      let cmt_loc = Comment.location comment in
      (* For now, use line-based adjacency since we don't have prev_tok_end_pos *)
      if cmt_loc.loc_start.pos_lnum == prev_end_pos.pos_lnum
      then
        let comment_end = cmt_loc.loc_end in
        loop ~prev_end_pos:comment_end (comment :: after_loc1) rest
      else List.rev after_loc1, all_comments
  in
  loop ~prev_end_pos:loc1.loc_end [] comments

(* Walk functions for AST nodes - simplified versions *)

let rec walk_structure s t comments =
  match s with
  | _ when comments = [] -> ()
  | [] -> attach t.inside Location.none comments
  | items ->
    let nodes = List.map ~f:(fun si -> `StructureItem si) items in
    walk_list nodes t comments

and walk_signature signature t comments =
  match signature with
  | _ when comments = [] -> ()
  | [] -> attach t.inside Location.none comments
  | items ->
    let nodes = List.map ~f:(fun si -> `SignatureItem si) items in
    walk_list nodes t comments

and get_loc node =
  match node with
  | `StructureItem si -> si.Ppxlib.Parsetree.pstr_loc
  | `SignatureItem si -> si.Ppxlib.Parsetree.psig_loc
  | `Expression e -> e.Ppxlib.Parsetree.pexp_loc
  | `Pattern p -> p.Ppxlib.Parsetree.ppat_loc
  | `CoreType ct -> ct.Ppxlib.Parsetree.ptyp_loc
  | `ValueBinding vb -> vb.Ppxlib.Parsetree.pvb_loc

and walk_node node t comments =
  match node with
  | `StructureItem si -> walk_structure_item si t comments
  | `SignatureItem si -> walk_signature_item si t comments
  | `Expression e -> walk_expression e t comments
  | `Pattern p -> walk_pattern p t comments
  | `CoreType ct -> walk_core_type ct t comments
  | `ValueBinding vb -> walk_value_binding vb t comments

and walk_list ?(prev_loc = None) l t comments =
  match l with
  | _ when comments = [] -> ()
  | [] ->
    (match prev_loc with
    | Some loc -> attach t.trailing loc comments
    | None -> ())
  | node :: rest ->
    let curr_loc = get_loc node in
    let leading, inside, trailing = partition_by_loc comments curr_loc in
    let updated_inside, updated_trailing =
      match prev_loc with
      | None ->
        (* First node - all leading comments attach here *)
        attach t.leading curr_loc leading;
        inside, trailing
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
          attach t.leading curr_loc before_curr;
          inside, trailing)
        else
          (* Different lines *)
          let on_same_line_as_prev, after_prev =
            partition_by_on_same_line prev_loc leading
          in
          attach t.trailing prev_loc on_same_line_as_prev;
          (* DON'T lose inside/trailing comments from after_prev! *)
          attach t.leading curr_loc after_prev;
          inside, trailing
    in
    walk_node node t updated_inside;
    walk_list ~prev_loc:(Some curr_loc) rest t updated_trailing

and walk_structure_item si t comments =
  match si.Ppxlib.Parsetree.pstr_desc with
  | _ when comments = [] -> ()
  | Pstr_value (_, value_bindings) ->
    let nodes = List.map ~f:(fun vb -> `ValueBinding vb) value_bindings in
    walk_list nodes t comments
  | Pstr_eval (expr, _) -> walk_expression expr t comments
  | _ ->
    (* For unhandled structure items, attach all comments as "inside" *)
    attach t.inside si.Ppxlib.Parsetree.pstr_loc comments

(* Simplified for now *)

and walk_signature_item si t comments =
  match comments with
  | [] -> ()
  | _ ->
    (* For now, attach all comments as "inside" the signature item *)
    attach t.inside si.Ppxlib.Parsetree.psig_loc comments

(* Simplified for now *)

and walk_value_binding vb t comments =
  let open Ppxlib.Parsetree in
  let pattern_loc = vb.pvb_pat.ppat_loc in
  let expr_loc = vb.pvb_expr.pexp_loc in
  (* Partition for pattern *)
  let leading, inside, trailing = partition_by_loc comments pattern_loc in
  attach t.leading pattern_loc leading;
  walk_pattern vb.pvb_pat t inside;
  let after_pat, surrounding_expr =
    partition_adjacent_trailing pattern_loc trailing
  in
  attach t.trailing pattern_loc after_pat;
  (* Partition for expression *)
  let before_expr, inside_expr, after_expr =
    partition_by_loc surrounding_expr expr_loc
  in
  attach t.leading expr_loc before_expr;
  walk_expression vb.pvb_expr t inside_expr;
  attach t.trailing expr_loc after_expr

and walk_expression expr t comments =
  let open Ppxlib.Parsetree in
  match expr.pexp_desc with
  | _ when comments = [] -> ()
  | Pexp_constant _ | Pexp_ident _ ->
    let leading, trailing = partition_leading_trailing comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    attach t.trailing expr.pexp_loc trailing
  | Pexp_let (_, value_bindings, expr2) ->
    let remaining =
      List.fold_left value_bindings ~init:comments ~f:(fun comments vb ->
        let leading, inside, trailing = partition_by_loc comments vb.pvb_loc in
        attach t.leading vb.pvb_loc leading;
        walk_value_binding vb t inside;
        trailing)
    in
    let leading, inside, trailing = partition_by_loc remaining expr2.pexp_loc in
    attach t.leading expr2.pexp_loc leading;
    walk_expression expr2 t inside;
    attach t.trailing expr2.pexp_loc trailing
  | Pexp_apply (func, args) ->
    let before, inside, after = partition_by_loc comments func.pexp_loc in
    attach t.leading func.pexp_loc before;
    walk_expression func t inside;
    let after_func, rest = partition_adjacent_trailing func.pexp_loc after in
    attach t.trailing func.pexp_loc after_func;
    let nodes = List.map ~f:(fun (_lbl, e) -> `Expression e) args in
    walk_list nodes t rest
  | Pexp_tuple exprs | Pexp_array exprs ->
    let nodes = List.map ~f:(fun e -> `Expression e) exprs in
    walk_list nodes t comments
  | Pexp_construct (_, Some expr) -> walk_expression expr t comments
  | Pexp_construct (_, None) ->
    let leading, trailing = partition_leading_trailing comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    attach t.trailing expr.pexp_loc trailing
  | Pexp_record (rows, spread_opt) ->
    let comments =
      match spread_opt with
      | None -> comments
      | Some spread_expr ->
        let leading, inside, trailing =
          partition_by_loc comments spread_expr.pexp_loc
        in
        attach t.leading spread_expr.pexp_loc leading;
        walk_expression spread_expr t inside;
        let after_spread, rest =
          partition_adjacent_trailing spread_expr.pexp_loc trailing
        in
        attach t.trailing spread_expr.pexp_loc after_spread;
        rest
    in
    let nodes = List.map ~f:(fun (_, e) -> `Expression e) rows in
    walk_list nodes t comments
  | Pexp_match (e, cases) | Pexp_try (e, cases) ->
    let before, inside, after = partition_by_loc comments e.pexp_loc in
    attach t.leading e.pexp_loc before;
    walk_expression e t inside;
    let after_expr, rest = partition_adjacent_trailing e.pexp_loc after in
    attach t.trailing e.pexp_loc after_expr;
    List.iter
      ~f:(fun case ->
        let leading, inside, trailing =
          partition_by_loc rest case.pc_lhs.ppat_loc
        in
        attach t.leading case.pc_lhs.ppat_loc leading;
        walk_pattern case.pc_lhs t inside;
        let after_pat, rest2 =
          partition_adjacent_trailing case.pc_lhs.ppat_loc trailing
        in
        attach t.trailing case.pc_lhs.ppat_loc after_pat;
        let leading, inside, trailing =
          partition_by_loc rest2 case.pc_rhs.pexp_loc
        in
        attach t.leading case.pc_rhs.pexp_loc leading;
        walk_expression case.pc_rhs t inside;
        attach t.trailing case.pc_rhs.pexp_loc trailing)
      cases
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
  | Pexp_field (e, _) | Pexp_send (e, _) ->
    let leading, inside, trailing = partition_by_loc comments e.pexp_loc in
    attach t.leading e.pexp_loc leading;
    walk_expression e t inside;
    attach t.trailing e.pexp_loc trailing
  | Pexp_setfield (e1, _, e2) ->
    let leading, inside, trailing = partition_by_loc comments e1.pexp_loc in
    attach t.leading e1.pexp_loc leading;
    walk_expression e1 t inside;
    let after_e1, rest = partition_adjacent_trailing e1.pexp_loc trailing in
    attach t.trailing e1.pexp_loc after_e1;
    let leading, inside, trailing = partition_by_loc rest e2.pexp_loc in
    attach t.leading e2.pexp_loc leading;
    walk_expression e2 t inside;
    attach t.trailing e2.pexp_loc trailing
  | Pexp_sequence (e1, e2) ->
    let leading, inside, trailing = partition_by_loc comments e1.pexp_loc in
    attach t.leading e1.pexp_loc leading;
    walk_expression e1 t inside;
    let after_e1, rest = partition_by_on_same_line e1.pexp_loc trailing in
    attach t.trailing e1.pexp_loc after_e1;
    let leading, inside, trailing = partition_by_loc rest e2.pexp_loc in
    attach t.leading e2.pexp_loc leading;
    walk_expression e2 t inside;
    attach t.trailing e2.pexp_loc trailing
  | Pexp_ifthenelse (cond, then_expr, else_opt) ->
    let leading, inside, trailing = partition_by_loc comments cond.pexp_loc in
    attach t.leading cond.pexp_loc leading;
    walk_expression cond t inside;
    let after_cond, rest = partition_adjacent_trailing cond.pexp_loc trailing in
    attach t.trailing cond.pexp_loc after_cond;
    let leading, inside, trailing = partition_by_loc rest then_expr.pexp_loc in
    attach t.leading then_expr.pexp_loc leading;
    walk_expression then_expr t inside;
    let after_then, rest =
      partition_adjacent_trailing then_expr.pexp_loc trailing
    in
    attach t.trailing then_expr.pexp_loc after_then;
    (match else_opt with
    | None -> ()
    | Some else_expr ->
      let leading, inside, trailing =
        partition_by_loc rest else_expr.pexp_loc
      in
      attach t.leading else_expr.pexp_loc leading;
      walk_expression else_expr t inside;
      attach t.trailing else_expr.pexp_loc trailing)
  | Pexp_function (params, _constraint, body) ->
    (* Walk parameters *)
    let rest =
      List.fold_left params ~init:comments ~f:(fun comments param ->
        match param.Ppxlib.Parsetree.pparam_desc with
        | Pparam_val (_, default_opt, pat) ->
          let leading, inside, trailing =
            partition_by_loc comments pat.ppat_loc
          in
          attach t.leading pat.ppat_loc leading;
          walk_pattern pat t inside;
          let after_pat, rest =
            partition_adjacent_trailing pat.ppat_loc trailing
          in
          attach t.trailing pat.ppat_loc after_pat;
          (match default_opt with
          | Some default_expr ->
            let leading, inside, trailing =
              partition_by_loc rest default_expr.pexp_loc
            in
            attach t.leading default_expr.pexp_loc leading;
            walk_expression default_expr t inside;
            let after, rest =
              partition_adjacent_trailing default_expr.pexp_loc trailing
            in
            attach t.trailing default_expr.pexp_loc after;
            rest
          | None -> rest)
        | Pparam_newtype _ -> comments)
    in
    (match body with
    | Pfunction_body e ->
      let leading, inside, trailing = partition_by_loc rest e.pexp_loc in
      attach t.leading e.pexp_loc leading;
      walk_expression e t inside;
      attach t.trailing e.pexp_loc trailing
    | Pfunction_cases (cases, _, _) ->
      List.iter
        ~f:(fun case ->
          let leading, inside, trailing =
            partition_by_loc rest case.pc_lhs.ppat_loc
          in
          attach t.leading case.pc_lhs.ppat_loc leading;
          walk_pattern case.pc_lhs t inside;
          let after_pat, rest2 =
            partition_adjacent_trailing case.pc_lhs.ppat_loc trailing
          in
          attach t.trailing case.pc_lhs.ppat_loc after_pat;
          let leading, inside, trailing =
            partition_by_loc rest2 case.pc_rhs.pexp_loc
          in
          attach t.leading case.pc_rhs.pexp_loc leading;
          walk_expression case.pc_rhs t inside;
          attach t.trailing case.pc_rhs.pexp_loc trailing)
        cases)
  | _ ->
    (* Default: attach all comments as inside *)
    attach t.inside expr.pexp_loc comments

and walk_pattern pat t comments =
  let open Ppxlib.Parsetree in
  match pat.ppat_desc with
  | _ when comments = [] -> ()
  | Ppat_any | Ppat_var _ | Ppat_constant _ ->
    let leading, trailing = partition_leading_trailing comments pat.ppat_loc in
    attach t.leading pat.ppat_loc leading;
    attach t.trailing pat.ppat_loc trailing
  | Ppat_tuple pats | Ppat_array pats ->
    let nodes = List.map ~f:(fun p -> `Pattern p) pats in
    walk_list nodes t comments
  | Ppat_construct (_, Some (_, p)) -> walk_pattern p t comments
  | Ppat_construct (_, None) ->
    let leading, trailing = partition_leading_trailing comments pat.ppat_loc in
    attach t.leading pat.ppat_loc leading;
    attach t.trailing pat.ppat_loc trailing
  | Ppat_record (rows, _) ->
    let nodes = List.map ~f:(fun (_, p) -> `Pattern p) rows in
    walk_list nodes t comments
  | Ppat_or (p1, p2) -> walk_list [ `Pattern p1; `Pattern p2 ] t comments
  | Ppat_constraint (p, typ) ->
    let leading, inside, trailing = partition_by_loc comments p.ppat_loc in
    attach t.leading p.ppat_loc leading;
    walk_pattern p t inside;
    let after_pat, rest = partition_adjacent_trailing p.ppat_loc trailing in
    attach t.trailing p.ppat_loc after_pat;
    let leading, inside, trailing = partition_by_loc rest typ.ptyp_loc in
    attach t.leading typ.ptyp_loc leading;
    walk_core_type typ t inside;
    attach t.trailing typ.ptyp_loc trailing
  | Ppat_alias (p, _) | Ppat_lazy p | Ppat_exception p ->
    walk_pattern p t comments
  | _ -> ()

and walk_core_type typ t comments =
  let open Ppxlib.Parsetree in
  match typ.ptyp_desc with
  | _ when comments = [] -> ()
  | Ptyp_any | Ptyp_var _ ->
    let leading, trailing = partition_leading_trailing comments typ.ptyp_loc in
    attach t.leading typ.ptyp_loc leading;
    attach t.trailing typ.ptyp_loc trailing
  | Ptyp_tuple types ->
    let nodes = List.map ~f:(fun ct -> `CoreType ct) types in
    walk_list nodes t comments
  | Ptyp_constr (_, args) ->
    let nodes = List.map ~f:(fun ct -> `CoreType ct) args in
    walk_list nodes t comments
  | Ptyp_arrow (_, t1, t2) ->
    let leading, inside, trailing = partition_by_loc comments t1.ptyp_loc in
    attach t.leading t1.ptyp_loc leading;
    walk_core_type t1 t inside;
    let after_t1, rest = partition_adjacent_trailing t1.ptyp_loc trailing in
    attach t.trailing t1.ptyp_loc after_t1;
    let leading, inside, trailing = partition_by_loc rest t2.ptyp_loc in
    attach t.leading t2.ptyp_loc leading;
    walk_core_type t2 t inside;
    attach t.trailing t2.ptyp_loc trailing
  | Ptyp_alias (typ, _) -> walk_core_type typ t comments
  | Ptyp_poly (_, typ) -> walk_core_type typ t comments
  | _ -> ()

(* Comment printing functions *)

let print_leading_comment ~hard_line ?next_comment comment =
  let single_line = Reason_comment.isLineComment comment in
  let content = Reason_comment.wrap comment in
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
      then if diff > 1 then hard_line else hard_line
      else if single_line && not next_single_line
      then if diff > 1 then hard_line else hard_line
      else if diff > 1
      then hard_line
      else if diff == 1
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
        then if diff > 1 then hard_line else hard_line
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
  let content = Reason_comment.wrap comment in
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
    (* Comment on different line *)
    concat
      (if diff > 1 then concat hard_line hard_line else hard_line)
      (text content)
  else if not single_line
  then
    (* Multi-line comment on same line *)
    concat space (text content)
  else
    (* Single-line comment on same line *)
    concat space (text content)

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
