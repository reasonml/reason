(* transform `div props1::a props2::b children::[foo, bar] () [@JSX]` into
   `ReactDOMRe.createElement "div" props::[%bs.obj {props1: 1, props2: b}] [|foo, bar|]`.

   For the old ppx: Don't transform the upper-cased case: `Foo.createElement foo::bar children::[] () [@JSX]`.
   For the new ppx: transform the upper-cased case `Foo.createElement key::a ref::b foo::bar children::[] () [@JSX]` into
   `ReasonReact.element key::a ref::b (Foo.make foo::bar [||] [@JSX])`
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

let listToArray lst =
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
      ) in
  listToArray' lst [] |> List.rev

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
let jsxMapper () =

  let oldJSX mapper loc attrs callExpression callArguments =
    Exp.apply
      ~loc
      ~attrs
      callExpression
      (
        callArguments |> List.map (fun (label, expr) -> (label, mapper.expr mapper expr))
      ) in

  let newJSX modulePath mapper loc attrs callExpression callArguments =
    let (children, argsWithLabels) =
      extractChildrenForDOMElements ~loc ~removeLastPositionUnit:true callArguments in
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
      ~attrs
      (* Foo.make *)
      (Exp.ident ~loc {loc; txt = Ldot (modulePath, "make")})
      args
    |> wrapWithReasonReactElement in

  let lowercaseCaller mapper loc attrs callArguments id  =
    let (children, propsWithLabels) =
      extractChildrenForDOMElements ~loc callArguments in
    let componentNameExpr =
      Exp.constant ~loc (Pconst_string (id, None)) in
    let childrenExpr =
      Exp.array (
        listToArray children |> List.map (fun a -> mapper.expr mapper a)
      ) in
    let args = match propsWithLabels with
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
        ] in
    Exp.apply
      ~loc
      (* throw away the [@JSX] attribute and keep the others, if any *)
      ~attrs
      (* ReactDOMRe.createDOMElement *)
      (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "createElement")})
      args in

  let useOldJSX = ref false in
  let structure_item =
    (fun mapper structure_item -> match structure_item with
       | {pstr_desc = Pstr_attribute ({loc; txt = "oldJSX" | "oldJsx"}, b); pstr_loc} -> begin
           useOldJSX := true;
           default_mapper.structure_item mapper structure_item
         end
       | _ -> default_mapper.structure_item mapper structure_item
    ) in

  let handleJsxCall mapper callExpression callArguments attrs =
    (match callExpression.pexp_desc with
     | Pexp_ident caller ->
       (match caller with
        | {txt = Lident "createElement"} ->
          raise (Invalid_argument "JSX: `createElement` should be preceeded by a module name.")
        (* Foo.createElement prop1::foo prop2:bar children::[] () *)
        (* no change *)
        | {loc; txt = Ldot (modulePath, "createElement")} ->
          (if !useOldJSX then oldJSX  else newJSX modulePath)
            mapper loc attrs callExpression callArguments
        (* div prop1::foo prop2:bar children::[bla] () *)
        (* turn that into ReactDOMRe.createElement props::(ReactDOMRe.props props1::foo props2::bar ()) [|bla|] *)
        | {loc; txt = Lident id} ->
          lowercaseCaller mapper loc attrs callArguments id
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
    ) in
  let expr = 
    (fun mapper expression -> match expression with
       (* Function applicationwith the @JSX attribute? *)
       |
         {
           pexp_desc = Pexp_apply (callExpression, callArguments);
           pexp_attributes
         } when Syntax_util.attribute_exists "JSX" pexp_attributes ->
         let attributesNoJsx =
           List.filter (fun (attribute, _) -> attribute.txt <> "JSX") pexp_attributes in
         handleJsxCall mapper callExpression callArguments attributesNoJsx 
       (* Delegate to the default mapper, a deep identity traversal *)
       | e ->
         default_mapper.expr mapper e) in
  Reason_toolchain.To_current.copy_mapper { default_mapper with structure_item; expr }

let () = Compiler_libs.Ast_mapper.register "JSX" (fun _argv -> jsxMapper ())
