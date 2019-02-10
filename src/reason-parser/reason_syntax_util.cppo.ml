(* Hello! Welcome to the Reason syntax util logic.

  This file's shared between the Reason repo and the BuckleScript repo. In
  Reason, it's in src/reason-parser. In BuckleScript, it's in
  jscomp/outcome_printer. We periodically copy this file from Reason (the source
  of truth) to BuckleScript, then uncomment the #if #else #end cppo macros you
  see in the file. That's because BuckleScript's on OCaml 4.02 while Reason's on
  4.04; so the #if macros surround the pieces of code that are different between
  the two compilers.

  When you modify this file, please make sure you're not dragging in too many
  things. You don't necessarily have to test the file on both Reason and
  BuckleScript; ping @chenglou and a few others and we'll keep them synced up by
  patching the right parts, through the power of types(tm)
*)

#ifdef BS_NO_COMPILER_PATCH
open Ast_404
#endif

open Asttypes
open Ast_mapper
open Parsetree
open Longident

(** Check to see if the string `s` is made up of `keyword` and zero or more
    trailing `_` characters. *)
let potentially_conflicts_with ~keyword s =
  let s_length = String.length s in
  let keyword_length = String.length keyword in
  (* It can't be a match if s is shorter than keyword *)
  s_length >= keyword_length && (
    try
      (* Ensure s starts with keyword... *)
      for i = 0 to keyword_length - 1 do
        if keyword.[i] <> s.[i] then raise Exit;
      done;
      (* ...and contains nothing else except trailing _ characters *)
      for i = keyword_length to s_length - 1 do
        if s.[i] <> '_' then raise Exit;
      done;
      (* If we've made it this far there's a potential conflict *)
      true
    with
    | Exit -> false
  )

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
  |  "\\===" -> "==="
  | "!=" -> "<>"
  | "!==" -> "!="
  | x when (
    potentially_conflicts_with ~keyword:"match" x
    || potentially_conflicts_with ~keyword:"method" x
    || potentially_conflicts_with ~keyword:"private" x
    || potentially_conflicts_with ~keyword:"not" x) -> string_add_suffix x
  | x when (
    potentially_conflicts_with ~keyword:"switch_" x
    || potentially_conflicts_with ~keyword:"pub_" x
    || potentially_conflicts_with ~keyword:"pri_" x) -> string_drop_suffix x
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
  |  "===" -> "\\==="
  | "<>" -> "!="
  | "!=" -> "!=="
  | x when (
    potentially_conflicts_with ~keyword:"match_" x
    || potentially_conflicts_with ~keyword:"method_" x
    || potentially_conflicts_with ~keyword:"private_" x
    || potentially_conflicts_with ~keyword:"not_" x) -> string_drop_suffix x
  | x when (
    potentially_conflicts_with ~keyword:"switch" x
    || potentially_conflicts_with ~keyword:"pub" x
    || potentially_conflicts_with ~keyword:"pri" x) -> string_add_suffix x
  | everything_else -> everything_else

let escape_string str =
  let buf = Buffer.create (String.length str) in
  String.iter (fun c ->
      match c with
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"'  -> Buffer.add_string buf "\\\""
      | c when c < ' ' -> Buffer.add_string buf (Char.escaped c)
      | c -> Buffer.add_char buf c
    ) str;
  Buffer.contents buf

(* the stuff below contains side-effects and are not used by BuckleScript's
  vendored version of reason_syntax_util.ml. So we can neglect it *)

#ifdef BS_NO_COMPILER_PATCH

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
  while !j < len && String.unsafe_get prefix !j =
                    String.unsafe_get str (i + !j) do
    incr j
  done;
  (!j = len)

(**
 * pick_while returns a tuple where first element is longest prefix (possibly empty) of the list of elements that satisfy p
 * and second element is the remainder of the list
 *)
let rec pick_while p = function
  | [] -> [], []
  | hd::tl when p hd ->
                  let (satisfied, not_satisfied) = pick_while p tl in
                  hd :: satisfied, not_satisfied
  | l -> ([], l)


(** [find_substring sub str i]
    returns the smallest [j >= i] such that [sub = str.[j..length sub - 1]]
    raises [Not_found] if there is no such j
    behavior is not defined if [sub] is the empty string
*)
let find_substring sub str i =
  let len = String.length str - String.length sub in
  let found = ref false and i = ref i in
  while not !found && !i <= len do
    if is_prefixed sub str !i then
      found := true
    else
      incr i;
  done;
  if not !found then
    raise Not_found;
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

(* This is lifted from https://github.com/bloomberg/bucklescript/blob/14d94bb9c7536b4c5f1208c8e8cc715ca002853d/jscomp/ext/ext_string.ml#L32
  Thanks @bobzhang and @hhugo! *)
let split_by ?(keep_empty=false) is_delim str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      if last_pos = 0 && not keep_empty then
        (*
           {[ split " test_unsafe_obj_ffi_ppx.cmi" ~keep_empty:false ' ']}
        *)
        acc
      else
        String.sub str 0 last_pos :: acc
    else
      if is_delim str.[pos] then
        let new_len = (last_pos - pos - 1) in
        if new_len <> 0 || keep_empty then
          let v = String.sub str (pos + 1) new_len in
          loop ( v :: acc)
            pos (pos - 1)
        else loop acc pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let rec trim_right_idx str idx =
  if idx = -1 then 0
  else
    match String.get str idx with
    | '\t' | ' ' | '\n' | '\r' -> trim_right_idx str (idx - 1)
    | _ -> idx + 1

let trim_right str =
  let length = String.length str in
  if length = 0 then ""
  else
    let index = trim_right_idx str (length - 1) in
    if index = 0 then ""
    else if index = length then
      str
    else String.sub str 0 index


let processLine line =
  let rightTrimmed = trim_right line in
  let trimmedLen = String.length rightTrimmed in
  if trimmedLen = 0 then
    rightTrimmed
  else
    let segments =
      split_by
        ~keep_empty:false
        (fun c -> c = TrailingCommaMarker.char)
        rightTrimmed in
    (* Now we concat the portions back together without any trailing comma markers
      - except we detect if there was a final trailing comma marker which we know
      must be before a newline so we insert a regular comma. This achieves
      "intelligent" trailing commas. *)
    let hadTrailingCommaMarkerBeforeNewline =
      String.get rightTrimmed (trimmedLen - 1) = TrailingCommaMarker.char
    in
    let almostEverything = String.concat "" segments in
    let lineBuilder = if hadTrailingCommaMarkerBeforeNewline then
      almostEverything ^ ","
    else
      almostEverything
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

(** Generate a suitable extension node for Merlin's consumption,
    for the purposes of reporting a syntax error - only used
    in recovery mode.
 *)
let syntax_error_extension_node loc message =
  let str = Location.mkloc "merlin.syntax-error" loc in
  let payload = PStr [{
    pstr_loc = Location.none;
    pstr_desc =
      Pstr_eval (
        {
          pexp_loc = Location.none;
          pexp_desc = Pexp_constant (Parsetree.Pconst_string (message, None));
          pexp_attributes = [];
        },
        []
      );
  }]
 in
 (str, payload)

(** identifier_mapper maps all identifiers in an AST with a mapping function f
  this is used by swap_operator_mapper right below, to traverse the whole AST
  and swapping the symbols listed above.
  *)
let identifier_mapper f super =
{ super with
  expr = begin fun mapper expr ->
    let expr =
      match expr with
        | {pexp_desc=Pexp_ident ({txt} as id)} ->
             let swapped = match txt with
               | Lident s -> Lident (f s)
               | Ldot(longPrefix, s) -> Ldot(longPrefix, f s)
               | Lapply (y,s) -> Lapply (y, s)
             in
             {expr with pexp_desc=Pexp_ident ({id with txt=swapped})}
        | _ -> expr
    in
    super.expr mapper expr
  end;
  pat = begin fun mapper pat ->
    let pat =
      match pat with
        | {ppat_desc=Ppat_var ({txt} as id)} ->
             {pat with ppat_desc=Ppat_var ({id with txt=(f txt)})}
        | _ -> pat
    in
    super.pat mapper pat
  end;
  signature_item = begin fun mapper signatureItem ->
    let signatureItem =
      match signatureItem with
        | {psig_desc=Psig_value ({pval_name} as name)} ->
            {signatureItem with psig_desc=Psig_value ({name with pval_name=({pval_name with txt=(f name.pval_name.txt)})})}
        | _ -> signatureItem
    in
    super.signature_item mapper signatureItem
  end;
  value_description = begin fun mapper desc ->
    let desc =
      match desc with
        | {pval_name = ({txt} as id); pval_prim } when pval_prim != [] ->
          {desc with pval_name = { id with txt = f txt }}
        | _ -> desc
    in
    super.value_description mapper desc
  end;
}

let remove_stylistic_attrs_mapper_maker super =
  let open Ast_404 in
  let open Ast_mapper in
{ super with
  expr = begin fun mapper expr ->
    let {Reason_attributes.stylisticAttrs; arityAttrs; docAttrs; stdAttrs; jsxAttrs} =
      Reason_attributes.partitionAttributes ~allowUncurry:false expr.pexp_attributes
    in
    let expr = if stylisticAttrs != [] then
      { expr with pexp_attributes = arityAttrs @ docAttrs @ stdAttrs @ jsxAttrs }
    else expr
    in
    super.expr mapper expr
  end;
  pat = begin fun mapper pat ->
    let {Reason_attributes.stylisticAttrs; arityAttrs; docAttrs; stdAttrs; jsxAttrs} =
      Reason_attributes.partitionAttributes ~allowUncurry:false pat.ppat_attributes
    in
    let pat = if stylisticAttrs != [] then
      { pat with ppat_attributes = arityAttrs @ docAttrs @ stdAttrs @ jsxAttrs }
    else pat
    in
    super.pat mapper pat
  end;
}

let remove_stylistic_attrs_mapper =
  remove_stylistic_attrs_mapper_maker Ast_mapper.default_mapper

(** escape_stars_slashes_mapper escapes all stars and slashes in an AST *)
let escape_stars_slashes_mapper =
  let escape_stars_slashes str =
    if String.contains str '/' then
      replace_string "/*" "/\\*" @@
      replace_string "*/" "*\\/" @@
      replace_string "//" "/\\/" @@
      str
    else
      str
  in
  identifier_mapper escape_stars_slashes

(* To be used in parser, transform a token into an ast node with different identifier
 *)
let reason_to_ml_swap_operator_mapper = identifier_mapper reason_to_ml_swap

(* To be used in printer, transform an ast node into a token with different identifier
 *)
let ml_to_reason_swap_operator_mapper = identifier_mapper ml_to_reason_swap

(* attribute_equals tests an attribute is txt
 *)
let attribute_equals to_compare = function
  | ({txt}, _) -> txt = to_compare

(* attribute_exists tests if an attribute exists in a list
 *)
let attribute_exists txt attributes = List.exists (attribute_equals txt) attributes

(* conflicted_attributes tests if both attribute1 and attribute2
 * exist
 *)
let attributes_conflicted attribute1 attribute2 attributes =
  attribute_exists attribute1 attributes &&
  attribute_exists attribute2 attributes

(* normalized_attributes removes attribute from a list of attributes
 *)
let normalized_attributes attribute attributes =
  List.filter (fun x -> not (attribute_equals attribute x)) attributes

(* apply_mapper family applies an ast_mapper to an ast *)
let apply_mapper_to_structure s mapper = mapper.structure mapper s
let apply_mapper_to_signature s mapper = mapper.signature mapper s
let apply_mapper_to_type      s mapper = mapper.typ       mapper s
let apply_mapper_to_expr      s mapper = mapper.expr      mapper s
let apply_mapper_to_pattern   s mapper = mapper.pat       mapper s

let apply_mapper_to_toplevel_phrase toplevel_phrase mapper =
  match toplevel_phrase with
  | Ptop_def x -> Ptop_def (apply_mapper_to_structure x mapper)
  | x -> x

let apply_mapper_to_use_file use_file mapper =
  List.map (fun x -> apply_mapper_to_toplevel_phrase x mapper) use_file

(* The following logic defines our own Error object
 * and register it with ocaml so it knows how to print it
 *)

type error = Syntax_error of string

exception Error of Location.t * error

let report_error ppf (Syntax_error err) =
  Format.(fprintf ppf "%s" err)

let () =
  Location.register_error_of_exn
    (function
     | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
     | _ ->
        None
     )

let map_first f = function
  | [] -> invalid_arg "Syntax_util.map_first: empty list"
  | x :: xs -> f x :: xs

let map_last f l =
  match List.rev l with
  | [] -> invalid_arg "Syntax_util.map_last: empty list"
  | x :: xs -> List.rev (f x :: xs)

type menhirMessagesError = {
  msg: string;
  loc: Location.t;
}

type menhirError =
  | NoMenhirMessagesError
  | MenhirMessagesError of menhirMessagesError

let menhirMessagesError = ref [NoMenhirMessagesError]

let findMenhirErrorMessage loc =
    let rec find messages =
      match messages with
      | MenhirMessagesError err :: _ when err.loc = loc -> MenhirMessagesError err
      | _ :: tail -> find tail
      | [] -> NoMenhirMessagesError
    in find !menhirMessagesError

let default_error_message = "<syntax error>"

let add_error_message err =
  let msg = try
    ignore (find_substring default_error_message err.msg 0);
    [MenhirMessagesError {err with msg = "A syntax error occurred. Help us improve this message: https://github.com/facebook/reason/blob/master/src/README.md#add-a-menhir-error-message"}]
  with
  | Not_found -> [MenhirMessagesError err]
  in
  menhirMessagesError := !menhirMessagesError @ msg

let location_is_before loc1 loc2 =
  let open Location in
  loc1.loc_end.Lexing.pos_cnum <= loc2.loc_start.Lexing.pos_cnum

let location_contains loc1 loc2 =
  let open Location in
  loc1.loc_start.Lexing.pos_cnum <= loc2.loc_start.Lexing.pos_cnum &&
  loc1.loc_end.Lexing.pos_cnum >= loc2.loc_end.Lexing.pos_cnum

let explode_str str =
  let rec loop acc i =
    if i < 0 then acc else loop (str.[i] :: acc) (i - 1)
  in
    loop [] (String.length str - 1)
#endif
