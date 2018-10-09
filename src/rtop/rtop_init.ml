#!/usr/bin/env ocaml
#load "compiler-libs/ocamlcommon.cma"
#require "ocaml-migrate-parsetree";;
#require "menhirLib";;
let () = try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH") with | Not_found -> ();;
#require "easy-format";;
#require "reason";;
#require "rtop";;

print_string
"
                   ___  _______   ________  _  __
                  / _ \\/ __/ _ | / __/ __ \\/ |/ /
                 / , _/ _// __ |_\\ \\/ /_/ /    /
                /_/|_/___/_/ |_/___/\\____/_/|_/

  Execute statements/let bindings. Hit <enter> after the semicolon. Ctrl-d to quit.

        >   let myVar = \"Hello Reason!\";
        >   let myList: list(string) = [\"first\", \"second\"];
        >   #use \"./src/myFile.re\"; /* loads the file into here */
"
