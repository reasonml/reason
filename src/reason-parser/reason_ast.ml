open Ast_404
open Parsetree

let processFastPipe e =
  match e.pexp_desc with
  | Pexp_apply(
      {pexp_desc = Pexp_ident({txt = Longident.Lident("|."); loc})} as identExp,
      args
    ) ->
    let pipe = {identExp with pexp_desc =
      Pexp_ident {txt = Longident.Lident("->"); loc}
    } in
    {e with pexp_desc = Pexp_apply(pipe, args) }
  | _ -> e
