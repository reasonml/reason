
open Ast_404
open Parsetree
open Location

module T = struct
  (** Kinds of attributes *)
  type attributesPartition = {
    arityAttrs : attributes;
    docAttrs : attributes;
    stdAttrs : attributes;
    jsxAttrs : attributes;
    refmtAttrs : attributes;
    literalAttrs : attributes;
    uncurried : bool
  }
end
open T

let letBangTag = "let_bang"

let isRefmtTag tag attr =
  match attr with
  | (
      {txt="refmt"; loc},
      PStr [{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant(Pconst_string(foundTag, None))}, _)}]
    ) -> foundTag = tag
  | _ -> false

let hasRefmtTag tag = List.exists (isRefmtTag tag)

let isRefmt ~filter attr =
  match attr with
  | (
      {txt="refmt"; loc},
      PStr [{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant(Pconst_string(tag, None))}, _)}]
    ) -> (
      match filter with
      | None -> true
      | Some style -> String.compare tag style == 0
    )
  | _ -> false
let isRefmtExplicitBraces = isRefmt ~filter:(Some "explicitBraces")
let isRefmtInlineOpen = isRefmt ~filter:(Some "inlineOpen")

(** Partition attributes into kinds *)
let rec partitionAttributes ?(partDoc=false) ?(allowUncurry=true) attrs : attributesPartition =
  match attrs with
    | [] ->
      {arityAttrs=[]; docAttrs=[]; stdAttrs=[]; jsxAttrs=[]; refmtAttrs=[]; literalAttrs=[]; uncurried = false}
    | (({txt = "bs"}, PStr []) as attr)::atTl ->
        let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
        if allowUncurry then
          {partition with uncurried = true}
        else {partition with stdAttrs=attr::partition.stdAttrs}
    | attr::atTl when isRefmt ~filter:None attr ->
        let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
        {partition with refmtAttrs=attr::partition.refmtAttrs}
    | (({txt="JSX"; loc}, _) as jsx)::atTl ->
        let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
        {partition with jsxAttrs=jsx::partition.jsxAttrs}
    | (({txt="explicit_arity"; loc}, _) as arity_attr)::atTl
    | (({txt="implicit_arity"; loc}, _) as arity_attr)::atTl ->
        let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
        {partition with arityAttrs=arity_attr::partition.arityAttrs}
    | (({txt="ocaml.text"; loc}, _) as doc)::atTl when partDoc = true ->
        let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
        {partition with docAttrs=doc::partition.docAttrs}
    | (({txt="ocaml.doc"; loc}, _) as doc)::atTl when partDoc = true ->
        let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
        {partition with docAttrs=doc::partition.docAttrs}
    | (({txt="reason.raw_literal"; _}, _) as attr) :: atTl ->
        let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
        {partition with literalAttrs=attr::partition.literalAttrs}
    | atHd :: atTl ->
        let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
        {partition with stdAttrs=atHd::partition.stdAttrs}

let extractStdAttrs attrs =
  (partitionAttributes attrs).stdAttrs

let extract_raw_literal attrs =
  let rec loop acc = function
    | ({txt="reason.raw_literal"; loc},
       PStr [{pstr_desc = Pstr_eval({pexp_desc = Pexp_constant(Pconst_string(text, None)); _}, _); _}])
      :: rest ->
      (Some text, List.rev_append acc rest)
    | [] -> (None, List.rev acc)
    | attr :: rest -> loop (attr :: acc) rest
  in
  loop [] attrs

(* Returns (selected, remaining) *)
let rec partition fn attrs : attribute list * attribute list =
  match attrs with
  | [] -> ([], [])
  | attr::atTl when fn attr ->
    let (selectedRec, remainingRec) = partition fn atTl in
    (attr::selectedRec, remainingRec)
  | attr::atTl ->
    let (selectedRec, remainingRec) = partition fn atTl in
    (selectedRec, attr::remainingRec)