type mytype = Int(hello) | Float(list(int)) | String
and hello = string;

let () =
  print_endline(show_mytype(Int("five")));
  print_endline(show_hello("aloha"));
