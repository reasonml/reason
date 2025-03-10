type eol =
  | LF
  | CRLF

let show eol = match eol with LF -> "lf" | CRLF -> "crlf"
let default_eol = match Sys.win32 with true -> CRLF | _ -> LF

let get_eol_for_file filename =
  let ic = open_in_bin filename in
  let rec loop prev =
    match input_char ic with
    | '\n' -> if prev = '\r' then CRLF else LF
    | c -> loop c
  in
  let eol = try loop ' ' with End_of_file -> default_eol in
  close_in ic;
  eol
