module Parser = Reason_parser
module Interp = Parser.MenhirInterpreter
module Raw = Reason_parser_explain_raw

let identlike_keywords = function
  | Parser.SIG -> Some "sig"
  | Parser.MODULE -> Some "module"
  | _ -> None

let confused_with_ident state token =
  match identlike_keywords token with
  | Some name when Raw.transitions_on_ident state ->
    (name ^ " is a reserved keyword, it cannot be used as an identifier")
  | _ -> raise Not_found

let message env (token, startp, endp) =
  let state = Interp.current_state_number env in
  (* Is there a message for this specific state ? *)
  try Reason_parser_message.message state
  with Not_found ->
    (* Identify a keyword used as an identifier *)
    try confused_with_ident state token
    with Not_found ->
      (* TODO: we don't know what to say *)
      "<UNKNOWN SYNTAX ERROR>"
