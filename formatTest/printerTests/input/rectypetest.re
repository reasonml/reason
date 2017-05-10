type mytype = Int of hello | Float of list(int) | String
and hello = string;

let () =
  print_endline(show_mytype(Int("five")));
  print_endline(show_hello("aloha"));
