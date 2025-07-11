open Ppxlib

(* Rename labels in function definition/application and records *)
let rename_labels = ref false

(** Check to see if the string `s` is made up of `keyword` and zero or more
    trailing `_` characters. *)
let potentially_conflicts_with ~keyword s =
  let s_length = String.length s in
  let keyword_length = String.length keyword in
  (* It can't be a match if s is shorter than keyword *)
  s_length >= keyword_length
  &&
  try
    (* Ensure s starts with keyword... *)
    for i = 0 to keyword_length - 1 do
      if keyword.[i] <> s.[i] then raise Exit
    done;
    (* ...and contains nothing else except trailing _ characters *)
    for i = keyword_length to s_length - 1 do
      if s.[i] <> '_' then raise Exit
    done;
    (* If we've made it this far there's a potential conflict *)
    true
  with
  | Exit -> false

(** Add/remove an appropriate suffix when mangling potential keywords *)
let string_add_suffix x = x ^ "_"

let string_drop_suffix x = String.sub x 0 (String.length x - 1)

(** What do these *_swap functions do? Here's an example: Reason code uses `!`
    for logical not, while ocaml uses `not`. So, for converting between reason
    and ocaml syntax, ocaml `not` converts to `!`, reason `!` converts to
    `not`.

    In more complicated cases where a reserved keyword exists in one syntax but
    not the other, these functions translate any potentially conflicting
    identifier into the same identifier with a suffix attached, or remove the
    suffix when converting back. Two examples:

    reason to ocaml:

    pub: invalid in reason to begin with
    pub_: pub
    pub__: pub_

    ocaml to reason:

    pub: pub_
    pub_: pub__
    pub__: pub___

    =====

    reason to ocaml:

    match: match_
    match_: match__
    match__: match___

    ocaml to reason:

    match: invalid in ocaml to begin with
    match_: match
    match__: match_
*)

let reason_to_ml_swap = function
  | "!" -> "not"
  | "^" -> "!"
  | "++" -> "^"
  | "===" -> "=="
  | "==" -> "="
  (* ===\/ and !==\/ are not representable in OCaml but
   * representable in Reason
   *)
  | "\\!==" -> "!=="
  | "\\===" -> "==="
  | "!=" -> "<>"
  | "!==" -> "!="
  | x
    when potentially_conflicts_with ~keyword:"match" x
         || potentially_conflicts_with ~keyword:"method" x
         || potentially_conflicts_with ~keyword:"private" x
         || potentially_conflicts_with ~keyword:"not" x ->
    string_add_suffix x
  | x
    when potentially_conflicts_with ~keyword:"switch_" x
         || potentially_conflicts_with ~keyword:"pub_" x
         || potentially_conflicts_with ~keyword:"pri_" x ->
    string_drop_suffix x
  | everything_else -> everything_else

let ml_to_reason_swap = function
  | "not" -> "!"
  | "!" -> "^"
  | "^" -> "++"
  | "==" -> "==="
  | "=" -> "=="
  (* ===\/ and !==\/ are not representable in OCaml but
   * representable in Reason
   *)
  | "!==" -> "\\!=="
  | "===" -> "\\==="
  | "<>" -> "!="
  | "!=" -> "!=="
  | x
    when potentially_conflicts_with ~keyword:"match_" x
         || potentially_conflicts_with ~keyword:"method_" x
         || potentially_conflicts_with ~keyword:"private_" x
         || potentially_conflicts_with ~keyword:"not_" x ->
    string_drop_suffix x
  | x
    when potentially_conflicts_with ~keyword:"switch" x
         || potentially_conflicts_with ~keyword:"pub" x
         || potentially_conflicts_with ~keyword:"pri" x ->
    string_add_suffix x
  | everything_else -> everything_else

let escape_string str =
  let buf = Buffer.create (String.length str) in
  String.iter
    (fun c ->
       match c with
       | '\t' -> Buffer.add_string buf "\\t"
       | '\r' -> Buffer.add_string buf "\\r"
       | '\n' -> Buffer.add_string buf "\\n"
       | '\\' -> Buffer.add_string buf "\\\\"
       | '"' -> Buffer.add_string buf "\\\""
       | c when c < ' ' -> Buffer.add_string buf (Char.escaped c)
       | c -> Buffer.add_char buf c)
    str;
  Buffer.contents buf

(*
    UTF-8 characters are encoded like this (most editors are UTF-8)
    0xxxxxxx (length 1)
    110xxxxx 10xxxxxx (length 2)
    1110xxxx 10xxxxxx 10xxxxxx (length 3)
    11110xxx 10xxxxxx 10xxxxxx 10xxxxxx (length 4)
   Numbers over 127 cannot be encoded in UTF in a single byte, so they use two
  bytes. That means we can use any characters between 128-255 to encode special
  characters that would never be written by the user and thus never be confused
  for our special formatting characters.
*)
(* Logic for handling special behavior that only happens if things break. We
  use characters that will never appear in the printed output if actually
  written in source code. The OCaml formatter will replace them with the escaped
  versions When moving to a new formatter, the formatter may *not* escape these
  an in that case we need the formatter to accept blacklists of characters to
  escape, but more likely is that the new formatter allows us to do these kinds
  of if-break logic without writing out special characters for post-processing.
*)
module TrailingCommaMarker = struct
  (* TODO: You can detect failed parsings by *NOT* omitting the final comma *ever*. *)
  (* A trailing comma will only be rendered if it is not immediately
   * followed by a closing paren, bracket, or brace *)
  let char = Char.chr 249 (* ˘ *)
  let string = String.make 1 char
end

(* Special character marking the end of a line. Nothing should be printed
 * after this marker. Example usage: // comments shouldn't have content printed
 * at the end of the comment. By attaching an EOLMarker.string at the end of the
 * comment our postprocessing step will ensure a linebreak at the position
 * of the marker. *)
module EOLMarker = struct
  let char = Char.chr 248
  let string = String.make 1 char
end

(** [is_prefixed prefix i str] checks if prefix is the prefix of str
  * starting from position i
  *)
let is_prefixed prefix str i =
  let len = String.length prefix in
  let j = ref 0 in
  while
    !j < len && String.unsafe_get prefix !j = String.unsafe_get str (i + !j)
  do
    incr j
  done;
  !j = len

(**
 * pick_while returns a tuple where first element is longest prefix (possibly empty) of the list of elements that satisfy p
 * and second element is the remainder of the list
 *)
let rec pick_while p = function
  | [] -> [], []
  | hd :: tl when p hd ->
    let satisfied, not_satisfied = pick_while p tl in
    hd :: satisfied, not_satisfied
  | l -> [], l

(** [find_substring sub str i]
    returns the smallest [j >= i] such that [sub = str.[j..length sub - 1]]
    raises [Not_found] if there is no such j
    behavior is not defined if [sub] is the empty string
*)
let find_substring sub str i =
  let len = String.length str - String.length sub in
  let found = ref false
  and i = ref i in
  while (not !found) && !i <= len do
    if is_prefixed sub str !i then found := true else incr i
  done;
  if not !found then raise Not_found;
  !i

(** [replace_string old_str new_str str] replaces old_str to new_str in str *)
let replace_string old_str new_str str =
  match find_substring old_str str 0 with
  | exception Not_found -> str
  | occurrence ->
    let buffer = Buffer.create (String.length str + 15) in
    let rec loop i j =
      Buffer.add_substring buffer str i (j - i);
      Buffer.add_string buffer new_str;
      let i = j + String.length old_str in
      match find_substring old_str str i with
      | j -> loop i j
      | exception Not_found ->
        Buffer.add_substring buffer str i (String.length str - i)
    in
    loop 0 occurrence;
    Buffer.contents buffer

(* This is lifted from
   https://github.com/bloomberg/bucklescript/blob/14d94bb9c7536b4c5f1208c8e8cc715ca002853d/jscomp/ext/ext_string.ml#L32
   Thanks @bobzhang and @hhugo! *)
let split_by ?(keep_empty = false) is_delim str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1
    then
      if last_pos = 0 && not keep_empty
      then
        (* {[ split " test_unsafe_obj_ffi_ppx.cmi" ~keep_empty:false ' ']} *)
        acc
      else String.sub str 0 last_pos :: acc
    else if is_delim str.[pos]
    then
      let new_len = last_pos - pos - 1 in
      if new_len <> 0 || keep_empty
      then
        let v = String.sub str (pos + 1) new_len in
        loop (v :: acc) pos (pos - 1)
      else loop acc pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let rec trim_right_idx str idx =
  if idx = -1
  then 0
  else
    match String.get str idx with
    | '\t' | ' ' | '\n' | '\r' -> trim_right_idx str (idx - 1)
    | _ -> idx + 1

let trim_right str =
  let length = String.length str in
  if length = 0
  then ""
  else
    let index = trim_right_idx str (length - 1) in
    if index = 0
    then ""
    else if index = length
    then str
    else String.sub str 0 index

let processLine line =
  let rightTrimmed = trim_right line in
  let trimmedLen = String.length rightTrimmed in
  if trimmedLen = 0
  then rightTrimmed
  else
    let segments =
      split_by
        ~keep_empty:false
        (fun c -> c = TrailingCommaMarker.char)
        rightTrimmed
    in
    (* Now we concat the portions back together without any trailing comma
       markers - except we detect if there was a final trailing comma marker
       which we know must be before a newline so we insert a regular comma. This
       achieves "intelligent" trailing commas. *)
    let hadTrailingCommaMarkerBeforeNewline =
      String.get rightTrimmed (trimmedLen - 1) = TrailingCommaMarker.char
    in
    let almostEverything = String.concat "" segments in
    let lineBuilder =
      if hadTrailingCommaMarkerBeforeNewline
      then almostEverything ^ ","
      else almostEverything
    in
    (* Ensure EOLMarker.char is replaced by a newline *)
    split_by ~keep_empty:false (fun c -> c = EOLMarker.char) lineBuilder
    |> List.map trim_right
    |> String.concat "\n"

let processLineEndingsAndStarts str =
  split_by ~keep_empty:true (fun x -> x = '\n') str
  |> List.map processLine
  |> String.concat "\n"
  |> String.trim

let isLineComment str =
  (* true iff the first \n is the last character *)
  match String.index str '\n' with
  | exception Not_found -> false
  | n -> n = String.length str - 1

let map_lident f lid =
  let swapped =
    match lid.txt with
    | Lident s -> Lident (f s)
    | Ldot (longPrefix, s) -> Ldot (longPrefix, f s)
    | Lapply (y, s) -> Lapply (y, s)
  in
  { lid with txt = swapped }

let map_arg_label f = function
  | Nolabel -> Nolabel
  | Labelled lbl -> Labelled (f lbl)
  | Optional lbl -> Optional (f lbl)

let map_class_expr f class_expr =
  { class_expr with
    pcl_desc =
      (match class_expr.pcl_desc with
      | Pcl_constr (lid, ts) -> Pcl_constr (map_lident f lid, ts)
      | e -> e)
  }

let map_class_type f class_type =
  { class_type with
    pcty_desc =
      (match class_type.pcty_desc with
      | Pcty_constr (lid, ct) -> Pcty_constr (map_lident f lid, ct)
      | Pcty_arrow (arg_lbl, ct, cls_type) ->
        Pcty_arrow (map_arg_label f arg_lbl, ct, cls_type)
      | x -> x)
  }

let map_core_type f typ =
  { typ with
    ptyp_desc =
      (match typ.ptyp_desc with
      | Ptyp_var var -> Ptyp_var (f var)
      | Ptyp_arrow (lbl, t1, t2) ->
        let lbl' =
          match lbl with
          | Labelled s when !rename_labels -> Labelled (f s)
          | Optional s when !rename_labels -> Optional (f s)
          | lbl -> lbl
        in
        Ptyp_arrow (lbl', t1, t2)
      | Ptyp_constr (lid, typs) -> Ptyp_constr (map_lident f lid, typs)
      | Ptyp_object (fields, closed_flag) when !rename_labels ->
        Ptyp_object
          ( List.map
              (function
                | { pof_desc = Otag (s, typ); _ } as pof ->
                  { pof with pof_desc = Otag ({ s with txt = f s.txt }, typ) }
                | other -> other)
              fields
          , closed_flag )
      | Ptyp_class (lid, typs) -> Ptyp_class (map_lident f lid, typs)
      | Ptyp_alias (typ, s) -> Ptyp_alias (typ, { s with txt = f s.txt })
      | Ptyp_variant (rfs, closed, lbls) ->
        Ptyp_variant
          ( List.map
              (function
                | { prf_desc = Rtag (lbl, b, cts); _ } as prf ->
                  { prf with
                    prf_desc = Rtag ({ lbl with txt = f lbl.txt }, b, cts)
                  }
                | t -> t)
              rfs
          , closed
          , lbls )
      | Ptyp_poly (vars, typ) ->
        Ptyp_poly (List.map (fun li -> { li with txt = f li.txt }) vars, typ)
      | Ptyp_package (lid, typs) ->
        Ptyp_package
          ( map_lident f lid
          , List.map (fun (lid, typ) -> map_lident f lid, typ) typs )
      | other -> other)
  }

(* class supery= Ppxlib.Ast_traverse.map *)

(** identifier_mapper maps all identifiers in an AST with a mapping function f
  this is used by swap_operator_mapper right below, to traverse the whole AST
  and swapping the symbols listed above.
  *)

class identifier_mapper f =
  let map_fields fields =
    List.map (fun (lid, x) -> map_lident f lid, x) fields
  in
  let map_name ({ txt; _ } as name) = { name with txt = f txt } in
  let map_lid lid = map_lident f lid in
  let map_label label = map_arg_label f label in

  object
    inherit Ast_traverse.map as super

    method! expression (expr : Parsetree.expression) =
      let expr =
        match expr with
        | { pexp_desc = Pexp_ident lid; _ } ->
          { expr with pexp_desc = Pexp_ident (map_lid lid) }
        | { pexp_desc = Pexp_function (params, constraint_, body); _ } ->
          let new_params =
            List.map
              (fun param ->
                 match param with
                 | { pparam_desc = Pparam_val (lbl, eo, pat); _ }
                   when !rename_labels ->
                   { param with
                     pparam_desc = Pparam_val (map_label lbl, eo, pat)
                   }
                 | { pparam_desc = Pparam_newtype s; _ } ->
                   { param with
                     pparam_desc = Pparam_newtype { s with txt = f s.txt }
                   }
                 | _ -> param)
              params
          in
          { expr with
            pexp_desc = Pexp_function (new_params, constraint_, body)
          }
        | { pexp_desc = Pexp_apply (e, args); _ } when !rename_labels ->
          { expr with
            pexp_desc =
              Pexp_apply
                (e, List.map (fun (label, e) -> map_label label, e) args)
          }
        | { pexp_desc = Pexp_variant (s, e); _ } ->
          { expr with pexp_desc = Pexp_variant (f s, e) }
        | { pexp_desc = Pexp_record (fields, closed); _ } when !rename_labels ->
          { expr with pexp_desc = Pexp_record (map_fields fields, closed) }
        | { pexp_desc = Pexp_field (e, lid); _ } when !rename_labels ->
          { expr with pexp_desc = Pexp_field (e, map_lid lid) }
        | { pexp_desc = Pexp_setfield (e1, lid, e2); _ } when !rename_labels ->
          { expr with pexp_desc = Pexp_setfield (e1, map_lid lid, e2) }
        | { pexp_desc = Pexp_send (e, s); _ } ->
          { expr with pexp_desc = Pexp_send (e, { s with txt = f s.txt }) }
        | { pexp_desc = Pexp_new lid; _ } ->
          { expr with pexp_desc = Pexp_new (map_lid lid) }
        | { pexp_desc = Pexp_setinstvar (name, e); _ } ->
          { expr with pexp_desc = Pexp_setinstvar (map_name name, e) }
        | { pexp_desc = Pexp_override name_exp_list; _ } ->
          let name_exp_list =
            List.map (fun (name, e) -> map_name name, e) name_exp_list
          in
          { expr with pexp_desc = Pexp_override name_exp_list }
        | { pexp_desc = Pexp_newtype (s, e); _ } ->
          { expr with pexp_desc = Pexp_newtype ({ s with txt = f s.txt }, e) }
        | _ -> expr
      in
      super#expression expr

    method! pattern pat =
      let pat =
        match pat with
        | { ppat_desc = Ppat_var name; _ } ->
          { pat with ppat_desc = Ppat_var (map_name name) }
        | { ppat_desc = Ppat_alias (p, name); _ } ->
          { pat with ppat_desc = Ppat_alias (p, map_name name) }
        | { ppat_desc = Ppat_variant (s, po); _ } ->
          { pat with ppat_desc = Ppat_variant (f s, po) }
        | { ppat_desc = Ppat_record (fields, closed); _ } when !rename_labels ->
          { pat with ppat_desc = Ppat_record (map_fields fields, closed) }
        | { ppat_desc = Ppat_type lid; _ } ->
          { pat with ppat_desc = Ppat_type (map_lid lid) }
        | _ -> pat
      in
      super#pattern pat

    method! value_description desc =
      let desc' = { desc with pval_name = map_name desc.pval_name } in
      super#value_description desc'

    method! type_declaration type_decl =
      let type_decl' =
        { type_decl with ptype_name = map_name type_decl.ptype_name }
      in
      let type_decl'' =
        match type_decl'.ptype_kind with
        | Ptype_record lst when !rename_labels ->
          { type_decl' with
            ptype_kind =
              Ptype_record
                (List.map
                   (fun lbl -> { lbl with pld_name = map_name lbl.pld_name })
                   lst)
          }
        | _ -> type_decl'
      in
      super#type_declaration type_decl''

    method! core_type typ = super#core_type (map_core_type f typ)

    method! class_declaration class_decl =
      let class_decl' =
        { class_decl with
          pci_name = map_name class_decl.pci_name
        ; pci_expr = map_class_expr f class_decl.pci_expr
        }
      in
      super#class_declaration class_decl'

    method! class_field class_field =
      let class_field_desc' =
        match class_field.pcf_desc with
        | Pcf_inherit (ovf, e, lo) -> Pcf_inherit (ovf, map_class_expr f e, lo)
        | Pcf_val (lbl, mut, kind) ->
          Pcf_val ({ lbl with txt = f lbl.txt }, mut, kind)
        | Pcf_method (lbl, priv, kind) ->
          Pcf_method ({ lbl with txt = f lbl.txt }, priv, kind)
        | x -> x
      in
      super#class_field { class_field with pcf_desc = class_field_desc' }

    method! class_type_field class_type_field =
      let class_type_field_desc' =
        match class_type_field.pctf_desc with
        | Pctf_inherit class_type -> Pctf_inherit (map_class_type f class_type)
        | Pctf_val (lbl, mut, vf, ct) ->
          Pctf_val ({ lbl with txt = f lbl.txt }, mut, vf, ct)
        | Pctf_method (lbl, pf, vf, ct) ->
          Pctf_method ({ lbl with txt = f lbl.txt }, pf, vf, ct)
        | x -> x
      in
      super#class_type_field
        { class_type_field with pctf_desc = class_type_field_desc' }

    method! class_type_declaration class_type_decl =
      let class_type_decl' =
        { class_type_decl with pci_name = map_name class_type_decl.pci_name }
      in
      super#class_type_declaration class_type_decl'

    method! module_type_declaration module_type_decl =
      let module_type_decl' =
        { module_type_decl with
          pmtd_name = map_name module_type_decl.pmtd_name
        }
      in
      super#module_type_declaration module_type_decl'
  end

let remove_stylistic_attrs_mapper_maker =
  object
    inherit Ast_traverse.map as super

    method! expression expr =
      let { Reason_attributes.stylisticAttrs
          ; arityAttrs
          ; docAttrs
          ; stdAttrs
          ; jsxAttrs
          ; _
          }
        =
        Reason_attributes.partitionAttributes
          ~allowUncurry:false
          expr.pexp_attributes
      in
      let expr =
        if stylisticAttrs != []
        then
          { expr with
            pexp_attributes = arityAttrs @ docAttrs @ stdAttrs @ jsxAttrs
          }
        else expr
      in
      super#expression expr

    method! pattern pat =
      let { Reason_attributes.stylisticAttrs
          ; arityAttrs
          ; docAttrs
          ; stdAttrs
          ; jsxAttrs
          ; _
          }
        =
        Reason_attributes.partitionAttributes
          ~allowUncurry:false
          pat.ppat_attributes
      in
      let pat =
        if stylisticAttrs != []
        then
          { pat with
            ppat_attributes = arityAttrs @ docAttrs @ stdAttrs @ jsxAttrs
          }
        else pat
      in
      super#pattern pat
  end

let escape_stars_slashes str =
  if String.contains str '/'
  then
    replace_string "/*" "/\\*"
    @@ replace_string "*/" "*\\/"
    @@ replace_string "//" "/\\/"
    @@ str
  else str

let remove_stylistic_attrs_mapper = remove_stylistic_attrs_mapper_maker

let let_monad_symbols =
  [ '$'; '&'; '*'; '+'; '-'; '/'; '<'; '='; '>'; '@'; '^'; '|'; '.'; '!' ]

let is_letop s =
  String.length s > 3
  && s.[0] = 'l'
  && s.[1] = 'e'
  && s.[2] = 't'
  && List.mem s.[3] let_monad_symbols

let is_andop s =
  String.length s > 3
  && s.[0] = 'a'
  && s.[1] = 'n'
  && s.[2] = 'd'
  && List.mem s.[3] let_monad_symbols

(* Don't need to backport past 4.08 *)
let backport_letopt_mapper = new Ast_traverse.map
let expand_letop_identifier s = s
let compress_letop_identifier s = s

(** escape_stars_slashes_mapper escapes all stars and slashes in an AST *)
class escape_stars_slashes_mapper =
  object
    inherit identifier_mapper escape_stars_slashes
  end

(* To be used in parser, transform a token into an ast node with different
   identifier *)
class reason_to_ml_swap_operator_mapper =
  object
    inherit identifier_mapper reason_to_ml_swap
  end

(* To be used in printer, transform an ast node into a token with different
   identifier *)
class ml_to_reason_swap_operator_mapper =
  object
    inherit identifier_mapper ml_to_reason_swap
  end

(* attribute_equals tests an attribute is txt *)
let attribute_equals to_compare = function
  | { attr_name = { txt; _ }; _ } -> txt = to_compare

(* attribute_exists tests if an attribute exists in a list *)
let attribute_exists txt attributes =
  List.exists (attribute_equals txt) attributes

(* conflicted_attributes tests if both attribute1 and attribute2
 * exist
 *)
let attributes_conflicted attribute1 attribute2 attributes =
  attribute_exists attribute1 attributes
  && attribute_exists attribute2 attributes

(* normalized_attributes removes attribute from a list of attributes *)
let normalized_attributes attribute attributes =
  List.filter (fun x -> not (attribute_equals attribute x)) attributes

(* apply_mapper family applies an ast_mapper to an ast *)
let apply_mapper_to_structure mapper s = mapper#structure s
let apply_mapper_to_signature mapper s = mapper#signature s
let apply_mapper_to_type mapper s = mapper#core_type s
let apply_mapper_to_expr mapper s = mapper#expression s
let apply_mapper_to_pattern mapper s = mapper#pattern s

let apply_mapper_to_toplevel_phrase mapper toplevel_phrase =
  match toplevel_phrase with
  | Ptop_def x -> Ptop_def (apply_mapper_to_structure mapper x)
  | x -> x

let apply_mapper_to_use_file mapper use_file =
  List.map (fun x -> apply_mapper_to_toplevel_phrase mapper x) use_file

let map_first f = function
  | [] -> invalid_arg "Syntax_util.map_first: empty list"
  | x :: xs -> f x :: xs

let map_last f l =
  match List.rev l with
  | [] -> invalid_arg "Syntax_util.map_last: empty list"
  | x :: xs -> List.rev (f x :: xs)

let location_is_before loc1 loc2 =
  let open Location in
  loc1.loc_end.Lexing.pos_cnum <= loc2.loc_start.Lexing.pos_cnum

let location_contains loc1 loc2 =
  let open Location in
  loc1.loc_start.Lexing.pos_cnum <= loc2.loc_start.Lexing.pos_cnum
  && loc1.loc_end.Lexing.pos_cnum >= loc2.loc_end.Lexing.pos_cnum

let split_compiler_error (err : Location.Error.t) =
  ( Location.Error.get_location err
  , Format.asprintf "%s" (Location.Error.message err) )

let explode_str str =
  let rec loop acc i = if i < 0 then acc else loop (str.[i] :: acc) (i - 1) in
  loop [] (String.length str - 1)

module Clflags = Ocaml_common.Clflags

let parse_lid s =
  let unflatten l =
    match l with
    | [] -> None
    | hd :: tl -> Some (List.fold_left (fun p s -> Ldot (p, s)) (Lident hd) tl)
  in
  match unflatten (String.split_on_char '.' s) with
  | Some lid -> lid
  | None ->
    failwith (Format.asprintf "parse_lid: unable to parse '%s' to longident" s)
