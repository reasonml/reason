module Range = Reason_location.Range
module Comment = Reason_comment

type whitespace_region =
  { range : Range.t
  ; comments : Comment.t list
  ; newlines : int
  }

let make_whitespace_region ~range ~newlines () =
  { range; comments = []; newlines }

let add_comment region comment =
  { region with comments = comment :: region.comments }

let modify_newlines region newlines = { region with newlines }

type 'doc t =
  | Doc of 'doc
  | SourceMap of Location.t * 'doc t
  | Whitespace of whitespace_region * 'doc t
  | Sequence of 'doc t list

(* Constructor helpers *)
let doc d = Doc d
let source_map ~loc t = if loc = Location.none then t else SourceMap (loc, t)
let with_whitespace ~region t = Whitespace (region, t)
let sequence items = Sequence items

(* Get location from a doc tree *)
let rec get_location = function
  | Doc _ -> None
  | SourceMap (loc, _) -> Some loc
  | Whitespace (_, sub) -> get_location sub
  | Sequence items ->
    (match items with
    | [] -> None
    | first :: _ ->
      (match List.rev items with
      | [] -> None
      | last :: _ ->
        (match get_location first, get_location last with
        | Some loc1, Some loc2 ->
          Some { loc1 with Location.loc_end = loc2.Location.loc_end }
        | Some loc, _ | _, Some loc -> Some loc
        | None, None -> None)))

(* Check if location is before the doc *)
let is_before ~location doc =
  match get_location doc with
  | None -> true
  | Some loc -> Reason_syntax_util.location_is_before loc location

(* Check if doc contains location *)
let contains_location doc ~location =
  match get_location doc with
  | None -> false
  | Some doc_loc -> Reason_syntax_util.location_contains doc_loc location

(* Convert to pure doc after comment insertion *)
let rec to_doc ~concat ~hard_line ~format_comment = function
  | Doc d -> d
  | SourceMap (_, t) -> to_doc ~concat ~hard_line ~format_comment t
  | Whitespace ({ newlines; comments; _ }, t) ->
    let comment_docs = List.map ~f:format_comment comments in
    let blank_lines = List.init ~len:newlines ~f:(fun _ -> hard_line) in
    let parts =
      blank_lines
      @ comment_docs
      @ [ to_doc ~concat ~hard_line ~format_comment t ]
    in
    List.fold_left ~f:concat ~init:hard_line parts
  | Sequence items ->
    (match items with
    | [] -> hard_line
    | first :: rest ->
      List.fold_left
        ~f:(fun acc item ->
          concat acc (to_doc ~concat ~hard_line ~format_comment item))
        ~init:(to_doc ~concat ~hard_line ~format_comment first)
        rest)

(* Comment insertion functions - ported from reason_pprint_ast.ml *)

let rec prepend_single_line_comment ~concat ~format_comment comment doc =
  let formatted_comment = Doc (format_comment comment) in
  match doc with
  | SourceMap (loc, sub) ->
    SourceMap
      (loc, prepend_single_line_comment ~concat ~format_comment comment sub)
  | Whitespace (info, sub) ->
    insert_comment_into_whitespace_region
      ~concat
      ~format_comment
      comment
      info
      sub
  | Doc d -> Doc (concat (format_comment comment) d)
  | Sequence items -> Sequence (formatted_comment :: items)

and insert_comment_into_whitespace_region
      ~concat
      ~format_comment
      comment
      region
      sub_doc
  =
  let cl = Comment.location comment in
  let range = region.range in
  let next_region = add_comment region comment in

  (* Helper to prepend comment doc to sub_doc *)
  let prepend_to_sub sub =
    match sub with
    | Doc d -> Doc (concat (format_comment comment) d)
    | other -> Sequence [ Doc (format_comment comment); other ]
  in

  match region.comments with
  | [] ->
    (* First comment in region *)
    if range.lnum_end = cl.loc_end.pos_lnum
    then
      (* Comment at end of whitespace region *)
      Whitespace (next_region, prepend_to_sub sub_doc)
    else if range.lnum_start = cl.loc_start.pos_lnum
    then
      (* Comment at start of whitespace region *)
      let next_region = modify_newlines next_region 0 in
      Whitespace (next_region, prepend_to_sub sub_doc)
    else
      (* Comment floats in whitespace region *)
      Whitespace (next_region, prepend_to_sub sub_doc)
  | prev_comment :: _cs ->
    let pcl = Comment.location prev_comment in
    let attached_to_start_region = cl.loc_start.pos_lnum = range.lnum_start in
    let next_region =
      if attached_to_start_region
      then modify_newlines next_region 0
      else next_region
    in
    if Reason_location.hasSpaceBetween pcl cl
    then
      (* Whitespace between comments *)
      Whitespace (next_region, prepend_to_sub sub_doc)
    else
      (* No whitespace between comments *)
      Whitespace (next_region, prepend_to_sub sub_doc)

let rec insert_single_line_comment ~concat ~format_comment doc comment =
  let location = Comment.location comment in
  match doc with
  | SourceMap (loc, sub) ->
    SourceMap
      (loc, insert_single_line_comment ~concat ~format_comment sub comment)
  | Whitespace (info, sub) ->
    let range = info.range in
    if Range.containsLoc range location
    then
      insert_comment_into_whitespace_region
        ~concat
        ~format_comment
        comment
        info
        sub
    else
      Whitespace
        (info, insert_single_line_comment ~concat ~format_comment sub comment)
  | Doc _ -> prepend_single_line_comment ~concat ~format_comment comment doc
  | Sequence sub_docs ->
    let before_comment, after_comment =
      Reason_syntax_util.pick_while (is_before ~location) sub_docs
    in
    (match after_comment with
    | [] ->
      (* Comment is after all items *)
      let append_comment = function
        | [] -> []
        | items ->
          Reason_syntax_util.map_last
            (fun item ->
               match item with
               | Doc d -> Doc (concat d (format_comment comment))
               | other -> Sequence [ other; Doc (format_comment comment) ])
            items
      in
      Sequence (append_comment before_comment)
    | hd :: tl ->
      let after_comment =
        match get_location hd with
        | Some loc when Reason_syntax_util.location_contains loc location ->
          insert_single_line_comment ~concat ~format_comment hd comment :: tl
        | Some loc ->
          SourceMap
            (loc, prepend_single_line_comment ~concat ~format_comment comment hd)
          :: tl
        | None ->
          prepend_single_line_comment ~concat ~format_comment comment hd :: tl
      in
      Sequence (before_comment @ after_comment))

let rec partition_comments acc = function
  | [] -> acc
  | com :: tl ->
    let single_lines, end_of_lines, regulars = acc in
    (match Comment.category com with
    | Comment.EndOfLine ->
      partition_comments (single_lines, com :: end_of_lines, regulars) tl
    | Comment.SingleLine ->
      partition_comments (com :: single_lines, end_of_lines, regulars) tl
    | Comment.Regular ->
      partition_comments (single_lines, end_of_lines, com :: regulars) tl)

let partition_all_comments comments =
  let single_lines, end_of_lines, regulars =
    partition_comments ([], [], []) comments
  in
  single_lines, List.rev end_of_lines, regulars

let partition_single_line_comments loc single_line_comments =
  let before, after =
    List.fold_left
      single_line_comments
      ~init:([], [])
      ~f:(fun (before, after) comment ->
        let cl = Comment.location comment in
        let is_after = loc.Location.loc_end.pos_lnum < cl.loc_start.pos_lnum in
        if is_after then before, comment :: after else comment :: before, after)
  in
  List.rev before, after

let append_single_line_comments_to_end
      ~concat
      ~format_comment
      loc
      doc
      single_line_comments
  =
  let rec aux _prev_loc doc _i = function
    | comment :: cs ->
      let loc = Comment.location comment in
      let comment_doc = Doc (format_comment comment) in
      let new_doc =
        match doc with
        | Doc d -> Doc (concat d (format_comment comment))
        | other -> Sequence [ other; comment_doc ]
      in
      aux loc new_doc (_i + 1) cs
    | [] -> doc
  in
  aux loc doc 0 single_line_comments

let attach_single_line_comments ~concat ~format_comment single_line_comments doc
  =
  match doc with
  | SourceMap (loc, sub_doc) ->
    let before, after =
      partition_single_line_comments loc single_line_comments
    in
    let doc =
      List.fold_left before ~init:sub_doc ~f:(fun acc comment ->
        insert_single_line_comment ~concat ~format_comment acc comment)
    in
    append_single_line_comments_to_end ~concat ~format_comment loc doc after
  | doc ->
    List.fold_left single_line_comments ~init:doc ~f:(fun acc comment ->
      insert_single_line_comment ~concat ~format_comment acc comment)

(* Append comment to end of doc - for end-of-line comments *)
(* concat_inline is used to attach without line breaks *)
let rec append_comment
          ~break_ancestors:_
          ~concat:_
          ~concat_inline
          ~format_comment
          doc
          comment
  =
  let formatted_comment = format_comment comment in
  (* Use concat_inline for same-line attachment *)
  let attach_inline d = concat_inline d formatted_comment in
  match doc with
  | Whitespace (info, sublayout) ->
    (* Attach comment inline after the sublayout *)
    let with_comment =
      match sublayout with
      | Doc d -> Doc (attach_inline d)
      | other -> Sequence [ other; Doc formatted_comment ]
    in
    Whitespace (info, with_comment)
  | Doc d ->
    (* Attach inline *)
    Doc (attach_inline d)
  | SourceMap (loc, sub) ->
    SourceMap
      ( loc
      , append_comment
          ~break_ancestors:false
          ~concat:concat_inline
          ~concat_inline
          ~format_comment
          sub
          comment )
  | other -> Sequence [ other; Doc formatted_comment ]

(* Loosely attach comment - used for end-of-line and regular comments *)
let rec loosely_attach_comment
          ~break_ancestors
          ~concat
          ~concat_inline
          ~format_comment
          doc
          comment
  =
  let location = Comment.location comment in
  match doc with
  | SourceMap (loc, sub) ->
    SourceMap
      ( loc
      , loosely_attach_comment
          ~break_ancestors
          ~concat
          ~concat_inline
          ~format_comment
          sub
          comment )
  | Whitespace (info, sub) ->
    Whitespace
      ( info
      , loosely_attach_comment
          ~break_ancestors
          ~concat
          ~concat_inline
          ~format_comment
          sub
          comment )
  | Doc _ -> Sequence [ doc; Doc (format_comment comment) ]
  | Sequence subLayouts
    when List.exists ~f:(contains_location ~location) subLayouts ->
    (* Comment is contained within one of the sublayouts - recurse into it *)
    let recurse_sublayout layout =
      if contains_location layout ~location
      then
        loosely_attach_comment
          ~break_ancestors
          ~concat
          ~concat_inline
          ~format_comment
          layout
          comment
      else layout
    in
    Sequence (List.map ~f:recurse_sublayout subLayouts)
  | Sequence [] ->
    (* Empty sequence - just the comment *)
    Sequence [ Doc (format_comment comment) ]
  | Sequence subLayouts ->
    let before_comment, after_comment =
      Reason_syntax_util.pick_while (is_before ~location) subLayouts
    in
    let new_sublayouts =
      match List.rev before_comment with
      | [] ->
        (* Comment is before all items - prepend to first *)
        Reason_syntax_util.map_first
          (prepend_single_line_comment ~concat ~format_comment comment)
          after_comment
      | hd :: tl ->
        (* Comment is after some items - append to last before item *)
        List.rev_append
          (append_comment
             ~break_ancestors
             ~concat
             ~concat_inline
             ~format_comment
             hd
             comment
          :: tl)
          after_comment
    in
    Sequence new_sublayouts

(* Insert end-of-line comment *)
let insert_end_of_line_comment
      ~concat
      ~concat_inline
      ~format_comment
      doc
      comment
  =
  loosely_attach_comment
    ~break_ancestors:true
    ~concat
    ~concat_inline
    ~format_comment
    doc
    comment

(* Insert regular comment *)
let insert_regular_comment ~concat ~concat_inline ~format_comment doc comment =
  (* For now, use loose attachment without perfect attachment *)
  (* TODO: Port perfectlyAttachComment for inline comments *)
  loosely_attach_comment
    ~break_ancestors:false
    ~concat
    ~concat_inline
    ~format_comment
    doc
    comment

(* Insert all comments into doc tree *)
let insert_comments
      ~concat
      ~concat_inline
      ~hard_line:_
      ~format_comment
      comments
      doc
  =
  let single_lines, end_of_lines, regulars = partition_all_comments comments in
  (* Insert in order: regular, then end-of-line, then single-line *)
  let doc =
    List.fold_left regulars ~init:doc ~f:(fun acc comment ->
      insert_regular_comment ~concat ~concat_inline ~format_comment acc comment)
  in
  let doc =
    List.fold_left end_of_lines ~init:doc ~f:(fun acc comment ->
      insert_end_of_line_comment
        ~concat
        ~concat_inline
        ~format_comment
        acc
        comment)
  in
  attach_single_line_comments ~concat ~format_comment single_lines doc

(* Group items and create Whitespace nodes - ported from reason_pprint_ast.ml *)
let group_and_print ~concat:_ ~xf ~get_loc ~comments items =
  let rec group prev_loc curr acc = function
    | x :: xs ->
      let item = xf x in
      let loc = get_loc x in
      let range = Range.makeRangeBetween prev_loc loc in
      if Range.containsWhitespace ~range ~comments ()
      then group loc [ range, item ] (List.rev curr :: acc) xs
      else group loc ((range, item) :: curr) acc xs
    | [] ->
      let groups = List.rev (List.rev curr :: acc) in
      List.mapi
        ~f:(fun i group ->
          match group with
          | (range, x) :: xs ->
            let newlines = if i > 0 then 1 else 0 in
            let region = make_whitespace_region ~range ~newlines () in
            let first_doc = Whitespace (region, x) in
            first_doc :: List.map ~f:snd xs
          | [] -> [])
        groups
  in
  match items with
  | first :: rest -> List.concat (group (get_loc first) [] [] (first :: rest))
  | [] -> []
