open Ast_404
open Parsetree
open Asttypes
open Ast_helper

let parseFastPipe e1 e2 =
  let attr = (Location.mknoloc "reason.fast_pipe", PStr []) in
  let markFastPipe e = { e with pexp_attributes =
    attr::e.pexp_attributes
  } in
  let rec rewrite e = match e.pexp_desc with
  | Pexp_ident _ ->
    Ast_helper.Exp.apply e [Nolabel, e1]
  | Pexp_apply(f, args) ->
    Ast_helper.Exp.apply f ((Nolabel, e1)::args)
  | Pexp_construct(lident, None) ->
    Ast_helper.Exp.construct lident (Some e1)
  | Pexp_construct(lident, Some({pexp_desc=Pexp_tuple l})) ->
    Ast_helper.Exp.construct
      ~attrs:[Location.mknoloc "explicit_arity", PStr []]
      lident
      (Some (Ast_helper.Exp.tuple (e1::l)))
  | Pexp_fun(
      Nolabel,
      None,
      _,
      {pexp_desc=Pexp_apply(f, args)})
    ->
    Ast_helper.Exp.apply e [Nolabel, e1]
  | Pexp_open(Fresh, lident, subExp) ->
      { e with pexp_desc = Pexp_open(Fresh, lident, rewrite subExp) }
  | Pexp_tuple(expList) ->
      Ast_helper.Exp.tuple (List.map (fun e -> rewrite e) expList)
  | _ ->
    let msg = "Unsupported fast pipe expression" in
    raise Reason_syntax_util.(Error(e.pexp_loc, (Syntax_error msg)))
  in
  markFastPipe (rewrite e2)

let unparseFastPipe e =
  let minusGreater = Ast_helper.Exp.ident (Location.mknoloc (Longident.parse "->")) in
  let rec rewrite e = match e.pexp_desc with
    | Pexp_apply(f, (Nolabel, e1)::args) ->
      let f = match args with
      | [] -> f
      | args -> Ast_helper.Exp.apply f args
      in
      Ast_helper.Exp.apply ~attrs:e.pexp_attributes
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
    | Pexp_open(Fresh, lident, subExpr) ->
      {e with pexp_desc = Pexp_open(Fresh, lident, rewrite subExpr)}
    | Pexp_tuple(expList) ->
      let firstE = ref None in
      let children = List.map (fun e ->
        match e.pexp_desc with
        | Pexp_ident i -> e
        | Pexp_apply(f, (Nolabel, e1)::args) ->
          firstE := Some e1;
          begin match args with
          | [] -> f
          | _ -> Ast_helper.Exp.apply f args
          end
        | Pexp_construct(lident, Some subExp) ->
            begin match subExp.pexp_desc with
            | Pexp_tuple(hd::tail) ->
                firstE := Some hd;
                begin match tail with
                | [] ->
                  Ast_helper.Exp.ident lident
                | _ ->
                  Ast_helper.Exp.construct
                    ~attrs:[Location.mknoloc "explicit_arity", PStr []]
                    lident
                    (Some (Ast_helper.Exp.tuple tail))
                end
            | _ ->
              firstE := Some subExp;
              Ast_helper.Exp.ident lident
            end
        | _ -> e
      ) expList in
      begin match !firstE with
      | Some e1 ->
          Ast_helper.Exp.apply
            minusGreater
            [(Nolabel, e1); (Nolabel, Ast_helper.Exp.tuple children)]
      | None ->
        Ast_helper.Exp.tuple children
      end
    | _ -> e
  in
  rewrite e
