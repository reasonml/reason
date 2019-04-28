(* See the comments in menhir_error_processor.ml *)

open Reason_string

module Parser = Reason_parser
module Interp = Parser.MenhirInterpreter
module Raw = Reason_parser_explain_raw

let identlike_keywords =
  let reverse_table = lazy (
    let table = Hashtbl.create 7 in
    Hashtbl.iter (fun k v -> Hashtbl.add table v k) Reason_lexer.keyword_table;
    table
  ) in
  function
  | Parser.SIG    -> Some "sig"
  | Parser.MODULE -> Some "module"
  | Parser.BEGIN  -> Some "begin"
  | Parser.END    -> Some "end"
  | Parser.OBJECT -> Some "object"
  | Parser.SWITCH -> Some "switch"
  | Parser.TO     -> Some "to"
  | Parser.THEN   -> Some "then"
  | Parser.TYPE   -> Some "type"
  | token ->
    match Hashtbl.find (Lazy.force reverse_table) token with
    | name -> Some name
    | exception Not_found -> None

let keyword_confused_with_ident state token =
  match identlike_keywords token with
  | Some name when Raw.transitions_on_lident state
                || Raw.transitions_on_uident state ->
    (name ^ " is a reserved keyword, it cannot be used as an identifier. Try `" ^ name ^ "_` or `_" ^ name ^ "` instead")
  | _ -> raise Not_found

let uppercased_instead_of_lowercased state token =
  match token with
  | Parser.UIDENT name when Raw.transitions_on_lident state ->
    let name = String.uncapitalize_ascii name in
    if Hashtbl.mem Reason_lexer.keyword_table name then
      "variables and labels should be lowercased"
    else
      Printf.sprintf "variables and labels should be lowercased. Try `%s'" name
  | _ -> raise Not_found

let semicolon_might_be_missing state _token =
  (*let state = Interp.current_state_number env in*)
  if Raw.transitions_on_semi state then
    "syntax error, consider adding a `;' before"
  else
    raise Not_found

let token_specific_message = function
  | Parser.UNDERSCORE ->
    "underscore is not a valid identifier. Use _ only in pattern matching and partial function application"
  | _ ->
    raise Not_found

let message env (token, _, _) =
  let state = Interp.current_state_number env in
  (* Identify a keyword used as an identifier *)
  try keyword_confused_with_ident state token
  with Not_found ->
  (* Identify an uppercased identifier in a lowercase place *)
  try uppercased_instead_of_lowercased state token
  with Not_found ->
  try semicolon_might_be_missing state token
  with Not_found ->
  try token_specific_message token
  with Not_found ->
  (* Is there a message for this specific state ? *)
    (* TODO: we don't know what to say *)
    "Syntax error"
