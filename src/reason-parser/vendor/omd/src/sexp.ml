open Ast

type t =
  | Atom of string
  | List of t list

let atom s = Atom s

let rec link_def : 'a. ('a -> t) -> 'a link_def -> t =
  fun f {label; destination; title; _} ->
    let title = match title with Some title -> [Atom title] | None -> [] in
    List (Atom "link-def" :: f label :: Atom destination :: title)

and inline {il_desc; _} =
  match il_desc with
  | Concat xs ->
      List (Atom "concat" :: List.map inline xs)
  | Text s ->
      Atom s
  | Emph il ->
      List [Atom "emph"; inline il]
  | Strong il ->
      List [Atom "strong"; inline il]
  | Code _ ->
      Atom "code"
  | Hard_break ->
      Atom "hard-break"
  | Soft_break ->
      Atom "soft-break"
  | Link def ->
      List [Atom "url"; link_def inline def]
  | Html s ->
      List [Atom "html"; Atom s]
  | Image _ ->
      Atom "img"

let rec block {bl_desc; bl_attributes = _} =
  match bl_desc with
  | Paragraph x ->
      List [Atom "paragraph"; inline x]
  | List (_, _, bls) ->
      List (Atom "list" :: List.map (fun xs -> List (Atom "list-item" :: List.map block xs)) bls)
  | Blockquote xs ->
      List (Atom "blockquote" :: List.map block xs)
  | Thematic_break ->
      Atom "thematic-break"
  | Heading (level, text) ->
      List [Atom "heading"; Atom (string_of_int level); inline text]
  | Code_block (info, _) ->
      List [Atom "code-block"; Atom info]
  | Html_block s ->
      List [Atom "html"; Atom s]
  | Link_def {label; destination; _} ->
      List [Atom "link-def"; Atom label; Atom destination]
  | Definition_list l ->
      List [Atom "def-list";
            List (List.map (fun elt ->
                List [inline elt.term;
                      List (List.map inline elt.defs)]) l)]

let create ast =
  List (List.map block ast)

let needs_quotes s =
  let rec loop i =
    if i >= String.length s then
      false
    else begin
      match s.[i] with
      | ' ' | '\t' | '\x00'..'\x1F' | '\x7F'..'\x9F' ->
          true
      | _ ->
          loop (succ i)
    end
  in
  loop 0

let rec print ppf = function
  | Atom s when needs_quotes s ->
      Format.fprintf ppf "%S" s
  | Atom s ->
      Format.pp_print_string ppf s
  | List l ->
      Format.fprintf ppf "@[<1>(%a)@]" (Format.pp_print_list ~pp_sep:Format.pp_print_space print) l
