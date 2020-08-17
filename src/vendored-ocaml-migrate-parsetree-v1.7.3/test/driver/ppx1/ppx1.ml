(* Rewrite [%fourty_two] as 42 *)

open Migrate_parsetree
open OCaml_403.Ast
open Parsetree

let rewriter _config _cookies =
  let super = Ast_mapper.default_mapper in
  let expr self e =
    match e.pexp_desc with
    | Pexp_extension ({ txt = "forty_two"; _ }, PStr []) ->
      { e with pexp_desc = Pexp_constant (Pconst_integer ("42", None)) }
    | _ -> super.expr self e
  in
  { super with expr }

let () =
  Driver.register ~name:"ppx1"
    (module OCaml_403)
    rewriter
