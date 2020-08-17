open Migrate_parsetree

(* Define the rewriter on OCaml 4.04 AST *)
open Ast_404
let ocaml_version = Versions.ocaml_404

let from_current = Versions.migrate Versions.ocaml_current ocaml_version

let prepare_lexbuf pos source =
  let lexbuf = Lexing.from_string source in
  lexbuf.Lexing.lex_curr_p <- pos;
  lexbuf

let prepare_for_parsing pexp =
  let open Parsetree in
  match pexp.pexp_desc with
  | Pexp_constant (Pconst_string (source, Some "quote")) ->
    let pos =
      let pos = pexp.pexp_loc.Location.loc_start in
      let pos_cnum = pos.Lexing.pos_cnum + String.length "{quote|" in
      {pos with Lexing.pos_cnum}
    in
    Some (prepare_lexbuf pos source)
  | _ -> None

let mapper _config _cookies =
  let open Ast_mapper in
  let open Ast_helper in
  let expr mapper pexp =
    let pexp = default_mapper.expr mapper pexp in
    match prepare_for_parsing pexp with
    | Some lexbuf ->
      from_current.Versions.copy_expression (Parse.expression lexbuf)
    | None -> pexp
  in
  {default_mapper with expr}

(* Register the rewriter in the driver *)
let () =
  Driver.register ~name:"ppx_parse" ocaml_version mapper
