open Ast_404
open Parsetree
open Asttypes
open Ast_helper

let parseFastPipe e1 e2 =
  let attr = (Location.mknoloc "reason.fast_pipe", PStr []) in
  let markFastPipe e = { e with pexp_attributes =
    attr::e.pexp_attributes
  } in
  let e = match e2.pexp_desc with
  | Pexp_ident _ ->
    Ast_helper.Exp.apply e2 [Nolabel, e1]
  | Pexp_apply(f, args) ->
    Ast_helper.Exp.apply f ((Nolabel, e1)::args)
  | Pexp_construct(lident, None) ->
    Ast_helper.Exp.construct lident (Some e1)
  | Pexp_construct(lident, Some({pexp_desc=Pexp_tuple l})) ->
    Ast_helper.Exp.construct
      ~attrs:[Location.mknoloc "explicit_arity", PStr []]
      lident
      (Some (Ast_helper.Exp.tuple (e1::l)))
  | _ ->
    let msg = "Unsupported fast pipe expression" in
    raise Reason_syntax_util.(Error(e2.pexp_loc, (Syntax_error msg)))
  in
  markFastPipe e

let unparseFastPipe e =
  let minusGreater = Ast_helper.Exp.ident (Location.mknoloc (Longident.parse "->")) in
  match e.pexp_desc with
  | Pexp_apply(f, (Nolabel, e1)::args) ->
    let f = match args with
    | [] -> f
    | args -> Ast_helper.Exp.apply f args
    in
    Ast_helper.Exp.apply
      minusGreater
      [(Nolabel, e1); (Nolabel, f)]
  | Pexp_construct(lident, Some tupArg) ->
      let (tupArg, ctorChild) = match tupArg.pexp_desc with
        | Pexp_tuple(x::xs) ->
            begin match xs with
            | y::_ys -> (x, Some(Ast_helper.Exp.tuple xs))
            | [] -> (x, None)
            end
        | _ -> (tupArg, None)
      in
      let ctor =
        Ast_helper.Exp.construct
          ~attrs:[Location.mknoloc "explicit_arity", PStr []]
          lident ctorChild
      in
      Ast_helper.Exp.apply
        minusGreater
        [(Nolabel, tupArg); (Nolabel, ctor)]
  | _ -> e
