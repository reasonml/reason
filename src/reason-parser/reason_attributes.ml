open Migrate_parsetree
open Ast_404
open Location
open Parsetree

(** Kinds of attributes *)
type attributesPartition = {
  arityAttrs : attributes;
  docAttrs : attributes;
  stdAttrs : attributes;
  jsxAttrs : attributes;
  stylisticAttrs : attributes;
  uncurried : bool
}

(** Partition attributes into kinds *)
let rec partitionAttributes ?(partDoc=false) ?(allowUncurry=true) attrs : attributesPartition =
  match attrs with
  | [] ->
    {arityAttrs=[]; docAttrs=[]; stdAttrs=[]; jsxAttrs=[]; stylisticAttrs=[]; uncurried = false}
  | (({txt = "bs"}, PStr []) as attr)::atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    if allowUncurry then
      {partition with uncurried = true}
    else {partition with stdAttrs=attr::partition.stdAttrs}
  | (({txt="JSX"}, _) as jsx)::atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with jsxAttrs=jsx::partition.jsxAttrs}
  | (({txt="explicit_arity"}, _) as arity_attr)::atTl
  | (({txt="implicit_arity"}, _) as arity_attr)::atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with arityAttrs=arity_attr::partition.arityAttrs}
  | (({txt="ocaml.text"}, _) as doc)::atTl when partDoc = true ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with docAttrs=doc::partition.docAttrs}
  | (({txt="ocaml.doc"}, _) as doc)::atTl when partDoc = true ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with docAttrs=doc::partition.docAttrs}
  | (({txt="reason.raw_literal"}, _) as attr) :: atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with stylisticAttrs=attr::partition.stylisticAttrs}
  | (({txt="reason.preserve_braces"}, _) as attr) :: atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with stylisticAttrs=attr::partition.stylisticAttrs}
  | atHd :: atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    {partition with stdAttrs=atHd::partition.stdAttrs}

let extractStdAttrs attrs =
  (partitionAttributes attrs).stdAttrs

let extract_raw_literal attrs =
  let rec loop acc = function
    | ({txt="reason.raw_literal"},
       PStr [{pstr_desc = Pstr_eval({pexp_desc = Pexp_constant(Pconst_string(text, None))}, _)}])
      :: rest ->
      (Some text, List.rev_append acc rest)
    | [] -> (None, List.rev acc)
    | attr :: rest -> loop (attr :: acc) rest
  in
  loop [] attrs

let without_stylistic_attrs attrs =
  let rec loop acc = function
    | attr :: rest when (partitionAttributes [attr]).stylisticAttrs != [] ->
        loop acc rest
    | [] -> List.rev acc
    | attr :: rest -> loop (attr :: acc) rest
  in
  loop [] attrs

let is_preserve_braces_attr ({txt}, _) =
  txt = "reason.preserve_braces"

let has_preserve_braces_attrs stylisticAttrs =
  (List.filter is_preserve_braces_attr stylisticAttrs) != []

let maybe_remove_stylistic_attrs attrs should_preserve =
  if should_preserve then
    attrs
  else
    List.filter (function
      | ({txt="reason.raw_literal"}, _) -> true
      | _ -> false)
      attrs
