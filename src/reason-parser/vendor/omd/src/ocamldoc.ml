open Ast

type t =
  | TripleSurround of string * t * string * t * string
  | Surround of string * t * string
  | BlockSurround of string * t * string
  | GeneralBlock of t
  | Text of string
  | Raw of string
  | Null
  | Concat of t * t

let nl = Raw "\n"

let escape_uri s =
  let b = Buffer.create (String.length s) in
  String.iter (function
      | '!' | '*' | '\'' | '(' | ')' | ';' | ':'
      | '@' | '=' | '+' | '$' | ',' | '/' | '?' | '%'
      | '#' | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' as c ->
          Buffer.add_char b c
      | '&' ->
          Buffer.add_string b "&amp;"
      | _ as c ->
          Printf.bprintf b "%%%2X" (Char.code c)
    ) s;
  Buffer.contents b

let to_plain_text t =
  let buf = Buffer.create 1024 in
  let rec go = function
    | Text t -> Buffer.add_string buf t
    | Concat (t1, t2) -> go t1; go t2
    | _ -> ()
  in
  go t;
  Buffer.contents buf

let rec add_to_buffer buf = function
  | TripleSurround (s1, t1, s2, t2, s3) ->
    Printf.bprintf buf "%s%a%s%a%s" s1 add_to_buffer t1 s2 add_to_buffer t2 s3
  | Surround (s1, t1, s2) -> Printf.bprintf buf "%s%a%s" s1 add_to_buffer t1 s2
  | BlockSurround (s1, t1, s2) -> Printf.bprintf buf "\n\b%s%a%s\n" s1 add_to_buffer t1 s2
  | GeneralBlock (t) -> Printf.bprintf buf "\n%a\n" add_to_buffer t
  | Text s -> Buffer.add_string buf s
  | Raw s -> Buffer.add_string buf s
  | Null -> ()
  | Concat (t1, t2) ->
    add_to_buffer buf t1;
    add_to_buffer buf t2

let text s = Text s
let raw s = Raw s
let concat s1 s2 = match (s1, s2) with
  | Null, s
  | s, Null -> s
  | s1, s2 -> Concat(s1, s2)

let concat_map f l =
  List.fold_left (fun accu x -> concat accu (f x)) Null l

let cross_reference_words = [
  "module";
  "modtype";
  "class";
  "classtype";
  "val";
  "type";
  "exception";
  "attribute";
  "method";
  "section";
  "const";
  "recfield";
]

let is_cross_reference_regexps = cross_reference_words |> List.map(fun w -> (Str.regexp_string (w ^ ":")))
let inferred_cross_reference = Str.regexp_string("ref:")

let rec inline {il_desc; il_attributes = _} =
  match il_desc with
  | Concat l -> concat_map inline l
  | Text s -> text s
  | Emph il -> Surround ("{e ", inline il, "}")
  | Strong il -> Surround ("{b ", inline il, "}")
  | Code s -> Surround ("[", text s, "]")
  | Hard_break -> text "\n\n"
  | Soft_break -> text "\n"
  | Html body -> Surround ("{%html: ", (text body), "%}")
  | Link {label; destination; title = _} ->
    let cross_reference = match label with
      | {il_desc = Text s; _} when s == destination ->
        if (Str.string_match inferred_cross_reference destination 0) then
          Some(Str.string_after destination 4)
        else
          if List.exists (fun r -> Str.string_match r destination 0) is_cross_reference_regexps then Some(destination) else None
      | _ ->
        None in
    (
      match cross_reference with
      | Some(cross_reference) ->
        Surround("{!", (text cross_reference), "}")
      | None ->
        TripleSurround("{{: ", (text destination), "} ", inline label, "}")
    )
  | Image {label; destination; title} ->
      let img = "<img src=\"" ^ escape_uri(destination) ^ "\" alt=\"" ^ (to_plain_text (inline label)) ^ "\"" ^ (match title with
      | None -> ""
      | Some title -> " title=\"" ^ title ^ "\"") ^ "/>" in
      Surround ("{%html: ", text img, "%}")


let rec block {bl_desc; bl_attributes = _attr} =
  match bl_desc with
  | Blockquote q -> BlockSurround("{v ", (concat_map block q), "v}")
  | Paragraph md -> GeneralBlock(inline md)
  | List (ty, sp, bl) ->
      let sign = match ty with Ordered _ -> "+ " | Bullet _ -> "- " in
      let li t =
        let block' t =
          match t.bl_desc, sp with
          | Paragraph t, Tight -> concat (inline t) nl
          | _ -> block t
        in
        let nl = if sp = Tight then Null else nl in
        Surround (sign, concat nl (concat_map block' t), "") in
      concat nl (concat_map li bl)
  | Code_block (_label, code) ->
    BlockSurround("{[\n", text code, "]}")
  | Thematic_break -> GeneralBlock(text "***")
  | Html_block body -> raw body
  | Heading(level, text) ->
    BlockSurround("{" ^ (
      match level with
        | 1 -> "0"
        | 2 -> "1"
        | 3 -> "2"
        | 4 -> "3"
        | 5 -> "4"
        | _ -> "5"
      ) ^ " ", inline text, "}")
  | Definition_list _ -> Null
  | Link_def _ -> Null

let of_doc doc =
  concat_map block doc

let to_string t =
  let buf = Buffer.create 1024 in
  add_to_buffer buf t;
  Buffer.contents buf;
