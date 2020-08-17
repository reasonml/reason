open Migrate_parsetree

(* Define the rewriter on OCaml 4.05 AST *)
open Ast_405
let ocaml_version = Versions.ocaml_405

(* Action of the rewriter: replace __HERE__ expression by a tuple ("filename",
   line, col) *)
let mapper _config _cookies =
  let open Ast_mapper in
  let open Ast_helper in
  let expr mapper pexp =
    match pexp.Parsetree.pexp_desc with
    | Parsetree.Pexp_ident {Location.txt = Longident.Lident "__HERE__"; loc} ->
      let {Lexing. pos_fname; pos_lnum; pos_cnum; pos_bol} =
        loc.Location.loc_start in
      let loc = {loc with Location.loc_ghost = true} in
      let fname = Exp.constant ~loc (Const.string pos_fname) in
      let line  = Exp.constant ~loc (Const.int pos_lnum) in
      let col   = Exp.constant ~loc (Const.int (pos_cnum - pos_bol)) in
      {pexp with Parsetree.pexp_desc =
                   Parsetree.Pexp_tuple [fname; line; col]}
    | _ -> default_mapper.expr mapper pexp
  in
  {default_mapper with expr}

(* Register the rewriter in the driver *)
let () =
  Driver.register ~name:"ppx_here" ocaml_version mapper
