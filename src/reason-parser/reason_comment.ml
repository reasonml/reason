open Location

type category =
  | EndOfLine
  | SingleLine
  | Regular

let string_of_category = function
  | Regular -> "Regular"
  | EndOfLine -> "End of Line"
  | SingleLine -> "SingleLine"

type t = {
  location: Location.t;
  category: category;
  text: string;
}

let tracked_comments = ref []

let track comment =
  tracked_comments := comment::(!tracked_comments)

let getComments () = !tracked_comments

let heightCommentsForRange range comments =
  List.fold_left (fun acc curr ->
    let startLnum = curr.location.loc_start.pos_lnum in
    let endLnum = curr.location.loc_end.pos_lnum in
    let open Reason_location in
    if range.lnum_start <= startLnum && endLnum <= range.lnum_end then
      acc + (endLnum - startLnum + 1)
    else
      acc
  ) 0 !tracked_comments

let commentsBefore range comment =
  let open Reason_location in
  List.filter (fun comment ->
    let beginLine = comment.location.loc_start.pos_lnum in
    let endLine = comment.location.loc_end.pos_lnum in
    beginLine >= range.lnum_start
    && endLine <= range.lnum_end
    && endLine < comment.location.loc_start.pos_lnum
  ) !tracked_comments

let category t = t.category

let location t = t.location

let dump ppf t =
  let open Lexing in
  Format.fprintf ppf "%d (%d:%d)-%d (%d:%d) -- %s:||%s||"
    t.location.loc_start.pos_cnum
    t.location.loc_start.pos_lnum
    (t.location.loc_start.pos_cnum - t.location.loc_start.pos_bol)
    t.location.loc_end.pos_cnum
    t.location.loc_end.pos_lnum
    (t.location.loc_end.pos_cnum - t.location.loc_end.pos_bol)
    (string_of_category t.category)
    t.text

let dump_list ppf list =
  List.iter (Format.fprintf ppf "%a\n" dump) list

let wrap t =
  match t.text with
  | "" | "*" -> "/***/"
  | txt when txt.[0] = '*' && txt.[1] <> '*' -> "/**" ^ txt ^ "*/"
  | txt -> "/*" ^ txt ^ "*/"

let is_doc t =
  String.length t.text > 0 && t.text.[0] == '*'

let make ~location category text =
  { text; category; location }
