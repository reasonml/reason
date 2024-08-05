let () = UTop.require [ "reason.ocaml-migrate-parsetree"; "menhirLib" ]

let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH") with
  | Not_found -> ()

let () = UTop.require [ "reason.easy_format"; "reason" ]
let () = Reason_toploop.main ()
let () = Reason_utop.init_reason ()

let () =
  print_string
    "\n\
    \                   ___  _______   ________  _  __\n\
    \                  / _ \\/ __/ _ | / __/ __ \\/ |/ /\n\
    \                 / , _/ _// __ |_\\ \\/ /_/ /    /\n\
    \                /_/|_/___/_/ |_/___/\\____/_/|_/\n\n\
    \  Execute statements/let bindings. Hit <enter> after the semicolon. \
     Ctrl-d to quit.\n\n\
    \        >   let myVar = \"Hello Reason!\";\n\
    \        >   let myList: list(string) = [\"first\", \"second\"];\n\
    \        >   #use \"./src/myFile.re\"; /* loads the file into here */\n"

let () = UTop_main.main ()
