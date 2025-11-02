type category =
  | EndOfLine
  | SingleLine
  | Regular

type t =
  { location : Location.t
  ; category : category
  ; text : string
  }

let category t = t.category
let location t = t.location

let wrap t =
  match t.text with
  | "" | "*" -> "/***/"
  | txt when Reason_syntax_util.isLineComment txt ->
    "//"
    (* single line comments of the form `// comment` have a `\n` at the end *)
    ^ String.sub txt ~pos:0 ~len:(String.length txt - 1)
    ^ Reason_syntax_util.EOLMarker.string
  | txt when txt.[0] = '*' && txt.[1] <> '*' ->
    (* CHECK: this comment printing seems fishy.
     *  It apply to invalid docstrings.
     *  In this case, it will add a spurious '*'.
     *  E.g. /**
     *        * bla */
     *  In an invalid context is turned into
     *       /***
     *        * bla */
     *  I think this case should be removed.
     *)
    "/**" ^ txt ^ "*/"
  | txt -> "/*" ^ txt ^ "*/"

let make ~location category text = { text; category; location }

let isLineComment { category; text; _ } =
  match category with
  | SingleLine -> Reason_syntax_util.isLineComment text
  | EndOfLine | Regular -> false
