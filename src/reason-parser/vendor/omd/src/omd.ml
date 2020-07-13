module Pre = Block.Pre

include Ast

type doc = block list

let parse_inline defs s =
  Parser.inline defs (Parser.P.of_string s)

let parse_inlines md =
  let defs =
    let f (def, attr) = {def with label = Parser.normalize def.label}, attr in
    List.map f (Raw.defs md)
  in
  List.map (Mapper.map (parse_inline defs)) md

let of_channel ic =
  let md = Pre.of_channel ic in
  parse_inlines md

let of_string s =
  let md = Pre.of_string s in
  parse_inlines md

let to_html doc =
  Html.to_string (Html.of_doc doc)

let to_sexp ast =
  Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)

let to_ocamldoc doc =
  Ocamldoc.to_string (Ocamldoc.of_doc doc)
