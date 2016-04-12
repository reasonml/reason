#!/usr/bin/env ocaml
#use ".ocamlinit";;
#require "menhirLib";;
#require "reason";;

print_string
"
                   ___  _______   ________  _  __
                  / _ \\/ __/ _ | / __/ __ \\/ |/ /
                 / , _/ _// __ |_\\ \\/ /_/ /    /
                /_/|_/___/_/ |_/___/\\____/_/|_/

  - Execute statements/let bindings. Hit <enter> after the semicolon.
  - Note: Responses values are not yet formatted in the Reason syntax.

        >   let myVar = \"Hello Reason!\";
        >   let myList: list string = [\"first\", \"second\"];


"
