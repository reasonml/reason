(**
  * Kinds of attributes.
  * TODO: Deprecate this in favor of `partition`.
  *)
open Ast_404.Parsetree
open Ast_404.Asttypes
type attributesPartition = {
  arityAttrs : attributes;
  docAttrs : attributes;
  stdAttrs : attributes;
  jsxAttrs : attributes;
  refmtAttrs : attributes;
  (* Sometimes these are printed, and sometimes not! *)
  uncurriedAttrs : attributes;
}


let isArity = function
  | ({txt="explicit_arity"; loc}, _)
  | ({txt="implicit_arity"; loc}, _) -> true
  | _ -> false

let isJsx = function
  | ({txt="JSX"; loc}, _) -> true
  | _ -> false

let isUncurried ?(bsIsUncurried=true) = function
  | ({txt = "bs"}, PStr []) -> bsIsUncurried
  | _ -> false

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

let isStandard ?(bsIsUncurried=true) attr = not (
  isArity attr ||
  isJsx attr ||
  isRefmt ~filter:None attr ||
  isUncurried ~bsIsUncurried attr
)

(** Partition attributes into kinds:
    NOTE: Use the better `partition` version which helps preserve
    ordering by only pulling off the attributes you need.
    bsIsUncurried: Whether or not uncurried attribute should be treated as a
    special non "standard" attribute. Another valid name would have been
    "considerUncurriedArgumentsSpecial" *)
let rec partitionAttributes ?(bsIsUncurried=true) attrs : attributesPartition =
  match attrs with
  | [] ->
    {arityAttrs=[]; docAttrs=[]; stdAttrs=[]; jsxAttrs=[]; refmtAttrs=[]; uncurriedAttrs=[]}
  | attr::atTl when isUncurried ~bsIsUncurried attr ->
    let partition = partitionAttributes atTl in
    {partition with uncurriedAttrs=attr::partition.uncurriedAttrs}
  | attr::atTl when isJsx attr ->
    let partition = partitionAttributes atTl in
    {partition with jsxAttrs=attr::partition.jsxAttrs}
  | attr::atTl when (isRefmt ~filter:None) attr ->
    let partition = partitionAttributes atTl in
    {partition with refmtAttrs=attr::partition.refmtAttrs}
  | attr::atTl when isArity attr ->
    let partition = partitionAttributes atTl in
    {partition with arityAttrs=attr::partition.arityAttrs}
  (*| (({txt="ocaml.text"; loc}, _) as doc)::atTl
  | (({txt="ocaml.doc"; loc}, _) as doc)::atTl ->
      let partition = partitionAttributes atTl in
      {partition with docAttrs=doc::partition.docAttrs}*)
  | atHd::atTl ->
      let partition = partitionAttributes atTl in
      {partition with stdAttrs=atHd::partition.stdAttrs}

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
