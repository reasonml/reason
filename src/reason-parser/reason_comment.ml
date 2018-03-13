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

let is_doc t =
  String.length t.text > 0 && t.text.[0] == '*'

let make ~location category text =
  { text; category; location }

let align_lines text =
  match Syntax_util.split_by ~keep_empty:true (fun x -> x = '\n') text with
  | [] -> ""
  | [text] -> text
  | first :: (second :: _ as rest) ->
    let leading_spaces str =
      let len = String.length str and i = ref 0 in
      while !i < len && str.[!i] = ' ' do incr i done;
      !i
    in
    let smallest_leading_spaces lines =
      let rec loop count = function
        | [] -> count
        | "" :: rest -> loop count rest
        | str :: rest -> loop (min (leading_spaces str) count) rest
      in
      loop max_int lines
    in
    let begins_with_star str =
      let len = String.length str and i = ref 0 in
      while !i < len && (str.[!i] = ' ' || str.[!i] = '\t') do incr i done;
      (!i < len && str.[!i] = '*')
    in
    let first_indent =
      let len = String.length first and i = ref 1 in
      let pred = function ' ' | '\t' | '*' -> true | _ -> false in
      while !i < len && pred first.[!i] do incr i done;
      if !i = len then 1 else !i
    in
    let attempt_remove = smallest_leading_spaces rest in
    let left_pad =
      String.make (if begins_with_star second then 1 else first_indent) ' '
    in
    let pad_line = function
      | "" -> ""
      | s ->
        let offset = min attempt_remove (leading_spaces s) in
        left_pad ^ String.sub s offset (String.length s - offset)
    in
    String.concat "\n" (first :: List.map pad_line rest)

let wrap t =
  match t.text with
  | "" | "*" -> "/***/"
  | txt when Syntax_util.is_line_comment txt ->
    "//" ^ txt
  | txt when txt.[0] = '*' && txt.[1] <> '*' ->
    align_lines ("/**" ^ txt ^ "*/")
  | txt -> align_lines ("/*" ^ txt ^ "*/")
