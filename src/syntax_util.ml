open Ast_mapper_plus
open Asttypes_plus
open Parsetree_plus
open Longident

(** [is_prefixed prefix i str] checks if prefix is the prefix of str
  * starting from position i
  *)
let is_prefixed prefix str i =
  let len = String.length prefix in
  if i + len > String.length str then false else
  let rec loop j =
    if j >= len then true else
      if String.unsafe_get prefix j <> String.unsafe_get str (i + j) then false else loop (j + 1)
    in
  loop 0

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

let rec replace_string_ old_str new_str i str buffer =
  if i >= String.length str then
    ()
  else
    (* found match *)
    if is_prefixed old_str str i then
      (* split string *)
      let old_str_len = String.length old_str in
      Buffer.add_string buffer new_str;
      replace_string_ old_str new_str (i + old_str_len) str buffer
    else
      let start = String.sub str i 1 in
      Buffer.add_string buffer start;
      replace_string_ old_str new_str (i + 1) str buffer


(** [replace_string old_str new_str str] replaces old_str to new_str in str *)
let replace_string old_str new_str str =
  let buffer = Buffer.create (String.length old_str * 2) in
  replace_string_ old_str new_str 0 str buffer;
  Buffer.contents buffer


module StringMap = Map.Make (String)


(** Generate a suitable extension node for Merlin's consumption,
    for the purposes of reporting a syntax error - only used
    in recovery mode.
 *)
let syntax_error_extension_node loc message =
  let str = Location.mkloc "merlin.syntax-error" loc in
  let open Parsetree_plus in
  let payload = PStr [{
    pstr_loc = Location.none;
    pstr_desc = Pstr_eval (
      {
        pexp_loc = Location.none;
        pexp_desc = Pexp_constant (Pconst_string (message, None));
        pexp_attributes = [];
      },
      []
    );
  }]
 in
 (str, payload)

let reason_to_ml_swapping_alist = [
  "===",  "==";
  "==",  "=";
  (* ===\/ and !==\/ are not representable in OCaml but
   * representable in Reason
   *)
  "\\!==", "!==";
  "\\===", "===";
  "!=", "<>";
  "!==", "!=";
]

let swap_txt map txt =
  if StringMap.mem txt map then
    StringMap.find txt map
  else
    txt

(** identifier_mapper maps all identifiers in an AST with a mapping function f
  *)
let identifier_mapper f =
{ default_mapper with
  expr = begin fun mapper expr ->
    let expr =
      match expr with
        | {pexp_desc=Pexp_ident ({txt} as id);
           pexp_loc;
           pexp_attributes} ->
             let swapped = match txt with
               | Lident s -> Lident (f s)
               | Ldot(longPrefix, s) -> Ldot(longPrefix, f s)
               | Lapply (y,s) -> Lapply (y, s)
             in
             {expr with pexp_desc=Pexp_ident ({id with txt=swapped})}
        | _ -> expr
    in
    default_mapper.expr mapper expr
  end;
  pat = begin fun mapper pat ->
    let pat =
      match pat with
        | {ppat_desc=Ppat_var ({txt} as id);
           ppat_loc;
           ppat_attributes} ->
             {pat with ppat_desc=Ppat_var ({id with txt=(f txt)})}
        | _ -> pat
    in
    default_mapper.pat mapper pat
  end;
}

(** unescape_stars_slashes_mapper unescapes all stars and slases in an AST
  *)
let unescape_stars_slashes_mapper =
  let unescape_stars_slashes str =
    let len = String.length str in
    if len < 2 then
      str
    else
      let ending = String.sub str 1 (len - 1) in
    String.sub str 0 1 ^
      replace_string "\\*" "*"
        (replace_string ("\\/") "/" ending)
  in
  identifier_mapper unescape_stars_slashes

(** escape_stars_slashes_mapper escapes all stars and slases in an AST
  *)
let escape_stars_slashes_mapper =
  let escape_stars_slashes str =
    let len = String.length str in
    if len < 2 then
      str
    else
      let ending = String.sub str 1 (len -1) in
      String.sub str 0 1 ^
        replace_string "*" "\\*"
          (replace_string "/" "\\/" ending)
  in
  identifier_mapper escape_stars_slashes

(**
 * swap_operator_mapper is a mapper that swaps two operators at parse/print time.
 * We need this since we want to transform operator such as "=" in Ocaml to "==" in Reason.
 * In this case, in the parser, everytime we see a token "==" in Reason, we transform it into "=";
 * Similarly, in the printer, everytime we see a token "=", we transform it into "==";
 *)
let swap_operator_mapper map = identifier_mapper (swap_txt map)

let reason_to_ml_swap_map = List.fold_left
                              (fun map (op1, op2) -> (StringMap.add op1 op2 map))
                              StringMap.empty
                              reason_to_ml_swapping_alist

let ml_to_reason_swap_map = List.fold_left
                              (fun map (op1, op2) -> (StringMap.add op2 op1 map))
                              StringMap.empty
                              reason_to_ml_swapping_alist

(* To be used in parser, transform a token into an ast node with different identifier
 *)
let reason_to_ml_swap_operator_mapper = swap_operator_mapper reason_to_ml_swap_map

(* To be used in printer, transform an ast node into a token with different identifier
 *)
let ml_to_reason_swap_operator_mapper = swap_operator_mapper ml_to_reason_swap_map

(* attribute_equals tests an attribute is txt
 *)
let attribute_equals to_compare = function
  | ({txt; _}, _) -> txt = to_compare

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

(*
 * apply_mapper_chain family applies an ast_mapper_chain to an ast,
 * ordering from left to right.
 *)
let apply_mapper_chain_to_structure =
  List.fold_left (fun s mapper -> mapper.structure mapper s )
let apply_mapper_chain_to_signature =
  List.fold_left (fun s mapper -> mapper.signature mapper s )
let apply_mapper_chain_to_type =
  List.fold_left (fun s mapper -> mapper.typ mapper s )
let apply_mapper_chain_to_expr =
  List.fold_left (fun s mapper -> mapper.expr mapper s )
let apply_mapper_chain_to_pattern =
  List.fold_left (fun s mapper -> mapper.pat mapper s )

let apply_mapper_chain_to_toplevel_phrase toplevel_phrase chain =
  match toplevel_phrase with
  | Ptop_def x -> Ptop_def (apply_mapper_chain_to_structure x chain)
  | x -> x

let apply_mapper_chain_to_use_file use_file chain =
  List.map (fun x -> apply_mapper_chain_to_toplevel_phrase x chain) use_file

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
