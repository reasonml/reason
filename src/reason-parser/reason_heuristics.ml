open Ppxlib

let is_punned_labelled_expression e lbl =
  match e.pexp_desc with
  | Pexp_ident { txt; _ }
  | Pexp_constraint ({ pexp_desc = Pexp_ident { txt; _ }; _ }, _)
  | Pexp_coerce ({ pexp_desc = Pexp_ident { txt; _ }; _ }, _, _) ->
    Reason_syntax_util.parse_lid lbl = txt
  | _ -> false

(* We manually check the length of `Thing.map(foo, bar, baz`, * in
   `Thing.map(foo, bar, baz, (a) => doStuff(a))` * because Easyformat doesn't
   have a hook to change printing when a list breaks * * we check if all
   arguments aside from the final one are either strings or identifiers, * where
   the sum of the string contents and identifier names are less than the print
   width *)
let funAppCallbackExceedsWidth ~printWidth ~args ~funExpr () =
  let funLen =
    match funExpr.pexp_desc with
    | Pexp_ident ident ->
      let identList = Longident.flatten_exn ident.txt in
      let lengthOfDots = List.length identList - 1 in
      let len =
        List.fold_left
          (fun acc curr -> acc + String.length curr)
          lengthOfDots
          identList
      in
      len
    | _ -> -1
  in
  (* eats an argument & substract its length from the printWidth * as soon as
     the print width reaches a sub-zero value, * we know the print width is
     exceeded & returns *)
  let rec aux len = function
    | _ when len < 0 -> true
    | [] -> false
    | arg :: args ->
      (match arg with
      | label, ({ pexp_desc = Pexp_ident ident; _ } as e) ->
        let identLen =
          List.fold_left
            (fun acc curr -> acc + String.length curr)
            len
            (Longident.flatten_exn ident.txt)
        in
        (match label with
        | Nolabel -> aux (len - identLen) args
        | Labelled s when is_punned_labelled_expression e s ->
          aux (len - (identLen + 1)) args
        | Labelled s -> aux (len - (identLen + 2 + String.length s)) args
        | Optional s -> aux (len - (identLen + 3 + String.length s)) args)
      | label, { pexp_desc = Pexp_constant (Pconst_string (str, _, _)); _ } ->
        let strLen = String.length str in
        (match label with
        | Nolabel -> aux (len - strLen) args
        | Labelled s -> aux (len - (strLen + 2 + String.length s)) args
        | Optional s -> aux (len - (strLen + 3 + String.length s)) args)
      | _ ->
        (* if we encounter a non-string or non-identifier argument exit *)
        true)
  in
  aux (printWidth - funLen) args

(* * Whether or not an identiier is small enough to justify omitting the *
   trailing comma for single identifier patterns. For single identifier *
   patterns, usually the identifier is not "far right" in the document, and * is
   one of the last things to require breaking. We can omit the trailing comma *
   in these cases because it likely will never render anyways and therefore the
   * space taken up by the trailing comma doesn't disrupt wrapping length
   calculations. * * For example, the `X` hardly ever benefits from a trailing
   comma. * | X(y) => *)
let singleTokenPatternOmmitTrail txt = String.length txt < 4

(* Indicates whether an expression can be printed with the uncurried * dot
   notation. At the moment uncurried function application & definition * only
   makes sense in the context of a Pexp_apply or Pexp_fun * * Examples: * [@bs]
   add(2, 3); -> add(. 2, 3); (* Pexp_apply *) * setTimeout([@bs] () =>
   Js.log("hola"), 1000); (* Pexp_fun *) * -> setTimeout((.) => Js.log("hola"),
   1000); *)
let bsExprCanBeUncurried expr =
  match Parsetree.(expr.pexp_desc) with
  | Pexp_function _ | Pexp_apply _ -> true
  | _ -> false

let isUnderscoreIdent expr =
  match Parsetree.(expr.pexp_desc) with
  | Pexp_ident { txt = Lident "_"; _ } -> true
  | _ -> false

let isPipeFirst e =
  match Parsetree.(e.pexp_desc) with
  | Pexp_ident { txt = Longident.Lident "|."; _ } -> true
  | Pexp_apply
      ({ pexp_desc = Pexp_ident { txt = Longident.Lident "|."; _ }; _ }, _) ->
    true
  | _ -> false

let isUnderscoreApplication expr =
  match expr with
  | { pexp_attributes = []
    ; pexp_desc =
        Pexp_function
          ( { pparam_desc =
                Pparam_val
                  ( Nolabel
                  , None
                  , { ppat_desc = Ppat_var { txt = "__x"; _ }
                    ; ppat_attributes = []
                    ; _
                    } )
            ; _
            }
            :: _
          , _
          , _ )
    ; _
    } ->
    true
  | _ -> false

(* <div> {items->Belt.Array.map(ReasonReact.string)->ReasonReact.array} </div>;
 * An application with pipe first inside jsx children requires special treatment.
 * Jsx children don't allow expression application, hence we need the braces
 * preserved in this case. *)
let isPipeFirstWithNonSimpleJSXChild e =
  match Parsetree.(e.pexp_desc) with
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = Longident.Lident "|."; _ }; _ }
      , [ (Nolabel, { pexp_desc = Pexp_apply _; _ }); _ ] ) ->
    true
  (* Handle <div> {url->a(b, _)} </div>;
   * underscore sugar needs protection *)
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = Longident.Lident "|."; _ }; _ }
      , [ _; (Nolabel, fe) ] )
    when isUnderscoreApplication fe ->
    true
  | _ -> false
