#!/usr/bin/env ocaml
#use ".ocamlinit";;
#require "reasonsyntax";;
#require "reason";;

print_string
"
                   ___  _______   ________  _  __
                  / _ \\/ __/ _ | / __/ __ \\/ |/ /
                 / , _/ _// __ |_\\ \\/ /_/ /    /
                /_/|_/___/_/ |_/___/\\____/_/|_/

  - Execute statements/let bindings. Hit <enter> after the semicolon.

        >   let myVar = \"Hello Reason!\";
        <   val myVar: bytes = \"Hello Reason!\"

        >   let myList: list string = [\"first\", \"second\"];
        <   val myList: bytes list = [\"first\"; \"second\"]

  Troubleshooting:
  - Nothing happens after pressing <enter>:
    - Remember to include a semicolon after every entry (just like a .re file)?
    - Is a parse error preventing parsing? Hit <enter> once more for more info.
  - Printed values look strange: Printing works but rtop (currently) prints
    values *without* the Reason syntax applied. (FIXME).

"
