(* Rewrite [%fourty_two] as 42 *)

open Migrate_parsetree
open OCaml_403.Ast
open Parsetree

let cmd_line_arg = ref "unset"

let get_plop cookies ~loc =
  match Driver.get_cookie cookies "plop" (module OCaml_403) with
  | Some e -> e
  | None ->
    let open Ast_helper in
    Exp.constant ~loc (Const.string "unset")

let rewriter _config cookies =
  let super = Ast_mapper.default_mapper in
  let expr self e =
    match e.pexp_desc with
    | Pexp_extension ({ txt = "cmd_line_arg"; _ }, PStr []) ->
      { e with pexp_desc = Pexp_constant (Pconst_string (!cmd_line_arg, None)) }
    | Pexp_extension ({ txt = "plop"; _ }, PStr []) ->
      get_plop cookies ~loc:e.pexp_loc
    | _ -> super.expr self e
  in
  { super with expr }

let () =
  Driver.register ~name:"ppx2"
    ~args:[("-message", Arg.Set_string cmd_line_arg, "MSG Set [%cmd_line_arg] to MSG")]
    (module OCaml_403)
    rewriter
