type eol =
  | LF 
  | CRLF 
let show eol = match eol with | LF -> "lf" | CRLF -> "crlf"
let default_eol = match Sys.win32 with | true -> CRLF | _ -> LF
let crlf_regex = Str.regexp ".*\r\n$"
let get_eol_from_string s =
  try ignore (Str.search_forward crlf_regex s 0); CRLF with | Not_found -> LF
let get_eol_for_file filename =
  let ic = open_in_bin filename in
  let eol_char = ref default_eol in
  let line = ref "" in
  let c = ref ' ' in
  try
    while (!c) <> '\n' do
      (c := (input_char ic); line := ((!line) ^ (String.make 1 (!c)))) done;
    eol_char := (get_eol_from_string (!line));
    !eol_char
  with | End_of_file -> (close_in ic; !eol_char)