type foo = Foo | Bar

let () = let a = Foo in
         match a with
         | Foo -> print_endline "Hello world"
