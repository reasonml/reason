type mytype = Int of alpha | Float of (list int) | String
and alpha = string;

let () =
  print_endline (show_mytype (Int "five"));
