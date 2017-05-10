type mytype = Int(string) | Float(list(int)) | String;

let () =
  print_endline(show_mytype(Int("five")));
