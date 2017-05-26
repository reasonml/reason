(* transform `div props1::a props2::b children::[foo, bar] () [@JSX]` into
  `ReactDOMRe.createElement "div" props::[%bs.obj {props1: 1, props2: b}] [|foo, bar|]`.
  Don't transform the upper-cased case: `Foo.createElement foo::bar children::[] () [@JSX]`.
*)

(* Why do we need a transform, instead of just using the original format?
Because that one currently doesn't work well for the existing React.js *)
open Migrate_parsetree
open Ast_404

open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident

let rec listToArray' lst accum =
  (* not in the sense of converting a list to an array; convert the AST
    reprensentation of a list to the AST reprensentation of an array *)
  match lst with
  | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} -> accum
  | {
      pexp_desc = Pexp_construct (
        {txt = Lident "::"},
        Some {pexp_desc = Pexp_tuple (v::acc::[])}
      )
  } -> listToArray' acc (v::accum)
  | _ -> raise (
    Invalid_argument "JSX: the `children` prop must be a literal list (of react elements)."
  )

let listToArray lst = listToArray' lst [] |> List.rev

let extractChildrenForDOMElements ?(removeLastPositionUnit=false) ~loc propsAndChildren =
  let rec allButLast_ lst acc = match lst with
    | [] -> []
    | (Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})::[] -> acc
    | (Nolabel, _)::rest -> raise (Invalid_argument "JSX: found non-labelled argument before the last position")
    | arg::rest -> allButLast_ rest (arg::acc)
  in
  let allButLast lst = allButLast_ lst [] |> List.rev in
  match (List.partition (fun (label, expr) -> label = Labelled "children") propsAndChildren) with
  | ((label, childrenExpr)::[], props) ->
    (childrenExpr, if removeLastPositionUnit then allButLast props else props)
  | ([], props) ->
    (* no children provided? Place a placeholder list (don't forgot we're talking about DOM element conversion here only) *)
    (Exp.construct ~loc {loc; txt = Lident "[]"} None, if removeLastPositionUnit then allButLast props else props)
  | (moreThanOneChild, props) -> raise (Invalid_argument "JSX: somehow there's more than one `children` label")

(* TODO: some line number might still be wrong *)
let jsxMapper = Reason_toolchain.To_current.copy_mapper {
  default_mapper with
  expr = (fun mapper expression -> match expression with
    (* spotted a function application! Does it have the @JSX attribute? *)
    | {
        pexp_desc = Pexp_apply ({pexp_desc = createElementWrap}, propsAndChildren);
        pexp_attributes
      } when Syntax_util.attribute_exists "JSX" pexp_attributes ->
        (match createElementWrap with
        | Pexp_ident caller ->
          (match caller with

          | {txt = Lident "createElement"} ->
            raise (Invalid_argument "JSX: `createElement` should be preceeded by a module name.")

          (* Foo.createElement key::k ref::r prop1::foo prop2:bar children::[bla] ()
             --->
             ReasonReact.element key::k ref::r (Foo.make prop1::foo prop2::bar [|bla|])  *)
          | {loc; txt = Ldot (Lident moduleIdentifier, "createElement")} ->
            let (children, argsWithLabels) =
              extractChildrenForDOMElements ~loc ~removeLastPositionUnit:true propsAndChildren in
            let argIsKeyRef = function
              | (Labelled ("key" | "ref") , _) -> true
              | _ -> false in
            let (argsKeyRef, argsForMake) = List.partition argIsKeyRef argsWithLabels in

            let childrenExpr =
              Exp.array (
                listToArray children |> List.map (fun a -> mapper.expr mapper a)
              ) in
            let args = argsForMake @ [ (Nolabel, childrenExpr) ] in
            let wrapWithReasonReactElement e = (* ReasonReact.element ::key ::ref (...) *)
              Exp.apply
                ~loc
                (Exp.ident ~loc {loc; txt = Ldot (Lident "ReasonReact", "element")})
                (argsKeyRef @ [(Nolabel, e)]) in
            Exp.apply
              ~loc
              (* throw away the [@JSX] attribute and keep the others, if any *)
              ~attrs:(pexp_attributes |> List.filter (fun (attribute, _) -> attribute.txt <> "JSX"))
              (* Foo.make *)
              (Exp.ident ~loc {loc; txt = Ldot (Lident moduleIdentifier, "make")})
              args
            |> wrapWithReasonReactElement

          (* div prop1::foo prop2:bar children::[bla] ()
             ----->
             ReactDOMRe.createElement props::(ReactDOMRe.props props1::foo props2::bar ()) [|bla|] *)
          | {loc; txt = Lident lowercaseIdentifier} ->
            let (children, propsWithLabelsPlusUnitAtEnd (* labelled args and () at the end *)) =
              extractChildrenForDOMElements ~loc propsAndChildren
            in
            let componentNameExpr =
              Exp.constant ~loc (Pconst_string (lowercaseIdentifier, None))
            in
            let childrenExpr =
              Exp.array (
                listToArray children |> List.map (fun a -> mapper.expr mapper a)
              )
            in
            let args = match propsWithLabelsPlusUnitAtEnd with
            | [theUnitArgumentAtEnd] ->
              [
                (* "div" *)
                (Nolabel, componentNameExpr);
                (* [|moreCreateElementCallsHere|] *)
                (Nolabel, childrenExpr)
              ]
            | nonEmptyProps ->
              let propsCall =
                Exp.apply
                  ~loc
                  (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "props")})
                  (nonEmptyProps |> List.map (fun (label, expression) -> (label, mapper.expr mapper expression)))
              in
              [
                (* "div" *)
                (Nolabel, componentNameExpr);
                (* ReactDOMRe.props className:blabla foo::bar () *)
                (Labelled "props", propsCall);
                (* [|moreCreateElementCallsHere|] *)
                (Nolabel, childrenExpr)
              ]
            in
            Exp.apply
              ~loc
              (* throw away the [@JSX] attribute and keep the others, if any *)
              ~attrs:(pexp_attributes |> List.filter (fun (attribute, _) -> attribute.txt <> "JSX"))
              (* ReactDOMRe.createDOMElement *)
              (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "createElement")})
              args

          | {txt = Ldot (_, anythingNotCreateElement)} ->
            raise (
              Invalid_argument
                ("JSX: the JSX attribute should be attached to a `YourModuleName.createElement` call. We saw `"
                  ^ anythingNotCreateElement
                  ^ "` instead"
                )
            )

          | {txt = Lapply _} ->
            (* don't think there's ever a case where this is reached *)
            raise (
              Invalid_argument "JSX: encountered a weird case while processing the code. Please report this!"
            )
          )
        | anythingElseThanIdent ->
          raise (
            Invalid_argument "JSX: `createElement` should be preceeded by a simple, direct module name."
          )
        )
    (* Delegate to the default mapper, a deep identity traversal *)
    | x -> default_mapper.expr mapper x)
}

let () = Compiler_libs.Ast_mapper.register "JSX" (fun _argv -> jsxMapper)
