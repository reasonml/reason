(* transform `Foo.createElement props1::a props2::b [foo, bar]`
  into
  `ReactRe.createElement Foo.comp (Foo.props props1::a, props2::b ()) [|foo, bar|]`
  and `div props1::a props2::b [foo, bar]`
  into
  `ReactRe.createElement div [%bs.obj {props1: 1, props2: b}] [|foo, bar|]`
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

(* construct Obj.magic, for children array usage below *)
let objMagicNode ~loc a =
  Exp.apply
    ~loc
    (Exp.ident ~loc {
      loc;
      txt = Ldot (Lident "Obj", "magic");
    })
    [("", a)]

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
    (* Foo.comp or "div" *)
    [
      ("", callNode);
      (* prop1::bla, prop2::blabla or [%bs.obj {props1: bla, props2: blabla}] *)
      ("", props);
      (* [|moreCreateElementCallsHere|] *)
      ("", Exp.array (
        (* children array needs to be homogenous. Reactjs' children clearly are
          not (supports string, number, react elements, recursive react children
          array, etc.). There's no good way of typing them while preserving
          interop right now, so we'll cast them to Obj.magic for now. *)
          listToArray children |> List.map (fun a -> mapper.expr mapper a |> objMagicNode ~loc)
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
    (* composite components: *)
    (*                   V---actualProps--V   *)
    (* Foo.createElement prop1::a props2::b [...] *)
    (*                                      ^ reactChildren  *)
    (* dom components: *)
    (*     V---actualProps--V   *)
    (* div prop1::a props2::b [...] *)
    (*                        ^ reactChildren  *)
    let last lst = List.length lst - 1 |> List.nth lst in
    let allButLast lst = List.rev lst |> List.tl |> List.rev in
    let actualProps = allButLast propsAndChildren in
    let reactChildren = snd (last propsAndChildren) in
    (Some actualProps, reactChildren)

(* TODO: line number are all wrong *)
let jsxMapper argv = {
  default_mapper with
  expr = (fun mapper expression -> match expression with
    (* spotted a function application! Does it have the @JSX attribute *)
    | {
        pexp_desc = Pexp_apply ({pexp_desc = createElementWrap}, propsAndChildren);
        pexp_attributes
      } when Syntax_util.attribute_exists "JSX" pexp_attributes ->
      (* usually, when a pattern also has a `when` clause, we lose the ability
        for the compiler to warn that we didn't cover all the cases; it's fine
        here since we have a catch-all below that maps over the rest using
        `identity` *)
        (match createElementWrap with
        | Pexp_ident fooCreateElement ->
          (match fooCreateElement with
          | {txt = Lident "createElement"} ->
            raise (Invalid_argument "JSX: `createElement` should be preceeded by a module name.")
          (* Foo.createElement prop1::foo prop2:bar [] *)
          | {loc; txt = Ldot (moduleNames, "createElement")} ->
            let (propsWithLabels, children) =
              splitPropsCallLabelsFromChildren propsAndChildren
            in
            let propsOrNull = match propsWithLabels with
            | Some props ->
              let unit = Exp.construct ~loc
                {loc; txt = Lident "()"} None
              in
              Exp.apply ~loc
                (* Foo.props *)
                (* This is the loc that'll point to the right location in case the component's not found. *)
                (Exp.ident ~loc {
                  loc;
                  txt = Ldot (moduleNames, "props")
                })
                (* see comment at the top of file. We need a () at the end to
                  signify "finished applying the function!". The ideal API
                  Foo.createElement circumvents this because the non-labelled
                  children _is_ the last argument *)
                ((recursivelyMapPropsCall ~props ~mapper) @ [("", unit)])
            | None ->
              (* if there's no prop, transform to `Js.null` *)
              Exp.ident ~loc {
                loc;
                txt = Ldot (Lident "Js", "null")
              }
            in
              constructReactReCall
                ~loc
                ~attributes:pexp_attributes
                ~callNode:(Exp.ident ~loc {
                    loc;
                    txt = Ldot (moduleNames, "comp")
                  })
                ~props:propsOrNull
                ~children
                ~mapper
          (* div prop1::foo prop2:bar [] *)
          (* the div is Pexp_ident "div" *)
          (* similar code to the above case, with a few exceptions *)
          | {loc; txt = Lident lowercaseIdentifier} ->
            let (propsWithLabels, children) =
              splitPropsCallLabelsFromChildren propsAndChildren
            in
            let propsOrNull = match propsWithLabels with
              | Some props ->
                (* [bs.obj {foo: bar, baz: qux}] *)
                functionCallWithLabelsToBSObj ~loc (recursivelyMapPropsCall ~props ~mapper)
              | None ->
                (* if there's no prop, transform to `Js.null` *)
                Exp.ident ~loc {
                  loc;
                  txt = Ldot (Lident "Js", "null")
                }
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
