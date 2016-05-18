#!/usr/bin/env ocaml
               #use ".ocamlinit";;
#require "menhirLib";;
#require "reason";;

let interactive = try let _ = Sys.getenv "stdin" in false with | Not_found -> true in
if interactive then
  print_string
    "
                   ___  _______   ________  _  __
                  / _ \\/ __/ _ | / __/ __ \\/ |/ /
                 / , _/ _// __ |_\\ \\/ /_/ /    /
                /_/|_/___/_/ |_/___/\\____/_/|_/

  Execute statements/let bindings. Hit <enter> after the semicolon.

        >   let myVar = \"Hello Reason!\";
        >   let myList: list string = [\"first\", \"second\"];


"
