(* transform `div props1::a props2::b [foo, bar][@JSX]` into
  `ReactRe.createElement div [%bs.obj {props1: 1, props2: b}] [|foo, bar|]`
  We don't transform the upper-cased case: `Foo.createElement foo::bar [][@JSX]`
  because our current API works as-is without macro modification. The lower-case
  case is really just as an escape hatch, e.g. for creating DOM components
*)
(* Why do we need a transform, instead of just using the previous
  Foo.createElement format? Because that one currently doesn't type check well for
  the existing React.js, and doesn't produce great output from BuckleScript. *)
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(* actual ppx logic *)
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
    Invalid_argument "JSX: the last argument to `Foo.createElement` must be a list (of children)."
  )

let listToArray lst = listToArray' lst [] |> List.rev

(* turn bla::1 blaa::2 into [%bs.obj {bla: 1, blaa: 2}] *)
let functionCallWithLabelsToBSObj ~loc callWithLabels =
  (* structure *)
  let record = callWithLabels
    |> List.map (fun (label, expression) ->
      ({loc; txt = Lident label}, expression)
    )
  in
  Exp.extension ~loc (
    {loc; txt = "bs.obj"},
    PStr [Str.eval ~loc (Exp.record ~loc record None)]
  )

(* props might contain `createElement` calls too; parse them recursively *)
let recursivelyMapPropsCall ~props ~mapper =
  props
  |> List.map (fun (label, expression) -> (label, mapper.expr mapper expression))

(* given that we're gathered all we have, construct the AST for `ReactRe.createElement ?? ?? [|childrenHere|]` *)
let constructReactReCall ~loc ~attributes ~callNode ~props ~children ~mapper =
  Exp.apply
    ~loc
    (* throw away the [@JSX] attribute and keep the others, if any *)
    ~attrs:(attributes |> List.filter (fun (attribute, _) -> attribute.txt <> "JSX"))
    (* ReactRe.createElement *)
    (Exp.ident ~loc {
      loc;
      txt = Ldot (Lident "ReactRe", "createElement");
    })
    (* "div" or fooComponentEscapeHatch *)
    [
      ("", callNode);
      (* [%bs.obj {props1: bla, props2: blabla}] *)
      ("", props);
      (* [|moreCreateElementCallsHere|] *)
      ("", Exp.array (
          listToArray children |> List.map (fun a -> mapper.expr mapper a)
        )
      )
    ]

let splitPropsCallLabelsFromChildren propsAndChildren =
  match propsAndChildren with
  | [] ->
    (* should never happen; nevertheless, provide a traceable error just in case *)
    raise (
      Invalid_argument "JSX: somehow props and children are nonexistent"
    )
  (* no props, only children. We get a pair of (label, value). Don't care about
    label *)
  | children::[] -> (None, snd children)
  | propsAndChildren ->
    (*     V---actualProps--V   *)
    (* div prop1::a props2::b [...] *)
    (*                        ^ reactChildren  *)
    let last lst = List.length lst - 1 |> List.nth lst in
    let allButLast lst = List.rev lst |> List.tl |> List.rev in
    let actualProps = allButLast propsAndChildren in
    let reactChildren = snd (last propsAndChildren) in
    (Some actualProps, reactChildren)

(* TODO: some line number might still be wrong *)
let jsxMapper argv = {
  default_mapper with
  expr = (fun mapper expression -> match expression with
    (* spotted a function application! Does it have the @JSX attribute *)
    | {
        pexp_desc = Pexp_apply ({pexp_desc = createElementWrap} as wrap, propsAndChildren);
        pexp_attributes
      } when Syntax_util.attribute_exists "JSX" pexp_attributes ->
        (match createElementWrap with
        | Pexp_ident fooCreateElement ->
          (match fooCreateElement with
          | {txt = Lident "createElement"} ->
            raise (Invalid_argument "JSX: `createElement` should be preceeded by a module name.")
          (* Foo.createElement prop1::foo prop2:bar [] *)
          | {loc; txt = Ldot (moduleNames, "createElement")} ->
            let attrs = pexp_attributes |> List.filter (fun (attribute, _) -> attribute.txt <> "JSX") in
            Exp.apply
              ~loc
              ~attrs
              wrap
              (propsAndChildren |> List.map (fun (label, expr) -> (label, mapper.expr mapper expr)))
          (* div prop1::foo prop2:bar [] *)
          (* the div is Pexp_ident "div" *)
          (* similar code to the above case, with a few exceptions *)
          | {loc; txt = Lident lowercaseIdentifier} ->
            let (propsWithLabels, children) =
              splitPropsCallLabelsFromChildren propsAndChildren
            in
            let propsOrNull = match propsWithLabels with
              | None ->
                (* if there's no prop, transform to `Js.null` *)
                Exp.ident ~loc {
                  loc;
                  txt = Ldot (Lident "Js", "null")
                }
              | Some props ->
                (* Js.Null.return [bs.obj {foo: bar, baz: qux}] *)
                Exp.apply
                  ~loc
                  (Exp.ident ~loc {loc; txt = Ldot (Ldot (Lident "Js", "Null"), "return")})
                  [("", functionCallWithLabelsToBSObj ~loc (recursivelyMapPropsCall ~props ~mapper))]
              in
              constructReactReCall
                ~loc
                ~attributes:pexp_attributes
                ~callNode:(Exp.constant ~loc (Const_string (lowercaseIdentifier, None)))
                ~props:propsOrNull ~children ~mapper
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

let () = register "JSX" jsxMapper
