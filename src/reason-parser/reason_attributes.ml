open Ppxlib

type attributesPartition =
  { arityAttrs : attributes
  ; docAttrs : attributes
  ; stdAttrs : attributes
  ; jsxAttrs : attributes
  ; stylisticAttrs : attributes
  ; uncurried : bool
  }
(** Kinds of attributes *)

(** Partition attributes into kinds *)
let rec partitionAttributes ?(partDoc = false) ?(allowUncurry = true) attrs :
  attributesPartition
  =
  match attrs with
  | [] ->
    { arityAttrs = []
    ; docAttrs = []
    ; stdAttrs = []
    ; jsxAttrs = []
    ; stylisticAttrs = []
    ; uncurried = false
    }
  | ({ attr_name = { txt = "u" | "bs"; _ }; attr_payload = PStr []; _ } as attr)
    :: atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    if allowUncurry
    then { partition with uncurried = true }
    else { partition with stdAttrs = attr :: partition.stdAttrs }
  | ({ attr_name = { txt = "JSX"; _ }; _ } as jsx) :: atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    { partition with jsxAttrs = jsx :: partition.jsxAttrs }
  | ({ attr_name = { txt = "explicit_arity"; _ }; _ } as arity_attr) :: atTl
  | ({ attr_name = { txt = "implicit_arity"; _ }; _ } as arity_attr) :: atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    { partition with arityAttrs = arity_attr :: partition.arityAttrs }
  | ({ attr_name = { txt = "ocaml.text"; _ }; _ } as doc) :: atTl
    when partDoc = true ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    { partition with docAttrs = doc :: partition.docAttrs }
  | ({ attr_name = { txt = "ocaml.doc" | "ocaml.text"; _ }; _ } as doc) :: atTl
    when partDoc = true ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    { partition with docAttrs = doc :: partition.docAttrs }
  | ({ attr_name = { txt = "reason.raw_literal"; _ }; _ } as attr) :: atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    { partition with stylisticAttrs = attr :: partition.stylisticAttrs }
  | ({ attr_name = { txt = "reason.preserve_braces"; _ }; _ } as attr) :: atTl
    ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    { partition with stylisticAttrs = attr :: partition.stylisticAttrs }
  | ({ attr_name = { txt = "reason.openSyntaxNotation"; _ }; _ } as attr)
    :: atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    { partition with stylisticAttrs = attr :: partition.stylisticAttrs }
  | ({ attr_name = { txt = "reason.quoted_extension"; _ }; _ } as attr) :: atTl
    ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    { partition with stylisticAttrs = attr :: partition.stylisticAttrs }
  | atHd :: atTl ->
    let partition = partitionAttributes ~partDoc ~allowUncurry atTl in
    { partition with stdAttrs = atHd :: partition.stdAttrs }

let extractStdAttrs attrs = (partitionAttributes attrs).stdAttrs

let extract_raw_literal attrs =
  let rec loop acc = function
    | { attr_name = { txt = "reason.raw_literal"; _ }
      ; attr_payload =
          PStr
            [ { pstr_desc =
                  Pstr_eval
                    ( { pexp_desc = Pexp_constant (Pconst_string (text, _, None))
                      ; _
                      }
                    , _ )
              ; _
              }
            ]
      ; _
      }
      :: rest ->
      Some text, List.rev_append acc rest
    | [] -> None, List.rev acc
    | attr :: rest -> loop (attr :: acc) rest
  in
  loop [] attrs

let without_stylistic_attrs attrs =
  let rec loop acc = function
    | attr :: rest when (partitionAttributes [ attr ]).stylisticAttrs != [] ->
      loop acc rest
    | [] -> List.rev acc
    | attr :: rest -> loop (attr :: acc) rest
  in
  loop [] attrs

(* TODO: Make this fast and not filter *)
let has_jsx_attributes =
  let is_jsx_attribute { attr_name = { txt; _ }; _ } = txt = "JSX" in
  fun attrs -> List.exists is_jsx_attribute attrs

let has_preserve_braces_attrs =
  let is_preserve_braces_attr { attr_name = { txt; _ }; _ } =
    txt = "reason.preserve_braces"
  in
  fun stylisticAttrs -> List.exists is_preserve_braces_attr stylisticAttrs

let has_quoted_extension_attrs =
  let is_quoted_extension_attr { attr_name = { txt; _ }; _ } =
    txt = "reason.quoted_extension"
  in
  fun stylisticAttrs -> List.exists is_quoted_extension_attr stylisticAttrs

let maybe_remove_stylistic_attrs attrs ~should_preserve =
  if should_preserve
  then attrs
  else
    List.filter
      (function
        | { attr_name = { txt = "reason.raw_literal"; _ }; _ } -> true
        | _ -> false)
      attrs

let has_open_notation_attr =
  let is_open_notation_attr { attr_name = { txt; _ }; _ } =
    txt = "reason.openSyntaxNotation"
  in
  fun stylisticAttrs -> List.exists is_open_notation_attr stylisticAttrs
