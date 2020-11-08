{
open Printf

let print_ocaml_version version =
  let patt_len = String.length "OCAML_VERSION" in
  (* Note: the spaces in the replacements are to preserve locations *)
  printf "%-*s" patt_len version
}

rule rewrite is_current ocaml_version = parse
  | "OCAML_VERSION"
    { print_ocaml_version ocaml_version;
      rewrite is_current ocaml_version lexbuf
    }
  |          "(*IF_CURRENT " ([^'*']* as s) "*)"
    { let chunk = if is_current
        then "             " ^ s ^          "  "
        else Lexing.lexeme lexbuf
      in
      print_string chunk;
      rewrite is_current ocaml_version lexbuf
    }
  |          "(*IF_AT_LEAST " ([^'*' ' ']* as v) " " ([^'*']* as s) "*)"
    { let chunk = if (v <= ocaml_version)
        then "              " ^ String.make (String.length v + 1) ' ' ^ s ^          "  "
        else Lexing.lexeme lexbuf
      in
      print_string chunk;
      rewrite is_current ocaml_version lexbuf
    }
  |          "(*IF_NOT_AT_LEAST " ([^'*' ' ']* as v) " " ([^'*']* as s) "*)"
    { let chunk = if not (v <= ocaml_version)
        then "                  " ^ String.make (String.length v + 1) ' ' ^ s ^          "  "
        else Lexing.lexeme lexbuf
      in
      print_string chunk;
      rewrite is_current ocaml_version lexbuf
    }
  | _ as c
    { print_char c;
      rewrite is_current ocaml_version lexbuf
    }
  | eof { () }


