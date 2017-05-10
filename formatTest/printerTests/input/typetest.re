type mytype = Int of string | Float of (list(int)) | String;

let () =
  print_endline(show_mytype(Int("five")));
