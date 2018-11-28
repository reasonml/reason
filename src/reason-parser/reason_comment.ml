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

let category t = t.category

let location t = t.location

let dump ppf t =
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
  | txt when Reason_syntax_util.isLineComment txt ->
    "//"
    (* single line comments of the form `// comment` have a `\n` at the end *)
    ^ (String.sub txt 0 (String.length txt - 1))
    ^ Reason_syntax_util.EOLMarker.string
  | txt when txt.[0] = '*' && txt.[1] <> '*' -> "/**" ^ txt ^ "*/"
  | txt -> "/*" ^ txt ^ "*/"

let is_doc t =
  String.length t.text > 0 && t.text.[0] == '*'

let make ~location category text =
  { text; category; location }

let isLineComment {category; text} = match category with
  | SingleLine -> Reason_syntax_util.isLineComment text
  | EndOfLine | Regular -> false
