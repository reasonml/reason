type eol =
  | LF 
  | CRLF 
let show eol = match eol with | LF -> "lf" | CRLF -> "crlf"
let default_eol = match Sys.win32 with | true -> CRLF | _ -> LF
let get_eol_for_file filename =
  let ic = open_in_bin filename in
  let line = ref "" in
  let c = ref ' ' in
  let prev = ref None in
  try
    while (!c) <> '\n' do
      (prev := (Some (!c));
       c := (input_char ic);
       line := ((!line) ^ (String.make 1 (!c))))
      done;
    (match !prev with
     | None -> default_eol
     | Some '\r' -> CRLF
     | Some _ -> LF)
  with | End_of_file -> (close_in ic; default_eol)