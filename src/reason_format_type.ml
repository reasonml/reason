(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

(* No String.split in stdlib... *)
let split str ~by =
  let rec split' str ~by idx accum =
    try
      let foundIdx = String.index_from str idx by in
      split' str ~by (foundIdx + 1) ((String.sub str idx foundIdx) :: accum)
    with Not_found ->
      let lastStrPiece = String.sub str idx (String.length str - idx) in
      List.rev (lastStrPiece :: accum)
  in
  split' str ~by 0 []

let () =
  if Array.length Sys.argv <> 2 then
    print_endline @@ "Please provide a single, quoted string of all the " ^
      "types you want transformed, separated by the escaped double quote \\\""
  else
    try
      Sys.argv.(1)
      |> split ~by:'"'
      |> List.map Reason_type_of_ocaml_type.convert
      |> List.iter print_endline
    with Syntaxerr.Error a ->
      prerr_endline "Failed to parse the input type(s)."
