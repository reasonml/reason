let print_init_message () =
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

let start_utop () =
  (match !Clflags.init_file with
  | Some _ -> ()
  | None ->
    let xdg_fn =
      LTerm_resources.xdgbd_file ~loc:LTerm_resources.Config "rtop/init.re"
    in
    Clflags.init_file :=
      (match Sys.file_exists xdg_fn with
        | true -> Some xdg_fn
        | false ->
          (* If `~/.config/rtop/init.re` isn't found, we can't be loading a
             user's `init.ml` because it'll be full of syntax errors for
             `rtop`. Create an empty temp file instead. *)
          let tmp_f = Filename.temp_file "rtop" ".re" in
          Some tmp_f));
  UTop_main.main ()

let main () =
  UTop.require [ "reason.ocaml-migrate-parsetree"; "menhirLib" ];

  (try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH") with
  | Not_found -> ());

  UTop.require [ "reason.easy_format"; "reason" ];
  Reason_toploop.main ();
  Reason_utop.init_reason ();
  print_init_message ();
  start_utop ()

let () = main ()
