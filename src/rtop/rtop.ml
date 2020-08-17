let () = UTop.require ["reason.ocaml-migrate-parsetree"; "menhirLib";]

let () = try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH") with | Not_found -> ();;

let () = UTop.require ["reason.easy_format"; "reason";]

let print_version = Reason_version.latest_version_for_package
let () = Reason_version.cli_arg_parse_version.major <- print_version.major
let () = Reason_version.cli_arg_parse_version.minor <- print_version.minor
let () = Reason_version.print_version.major <- print_version.major
let () = Reason_version.print_version.minor <- print_version.minor

let () = Reason_toploop.main ()

let () = Reason_utop.init_reason ()

let () = print_string
{|
             _ __ ___  __ _ ___  ___  _ __
            | '__/ _ \/ _` / __|/ _ \| '_ \
            | | |  __/ (_| \__ \ (_) | | | |
            |_|  \___|\__,_|___/\___/|_| |_|

                  (syntax version 3.8)

        Semicolon submits statements. Ctrl-d to quit.

        >   let myVar = "Hello Reason!";
        >   let myList: list<string> = ["first", "second"];
        >   #use "./src/myFile.re"; /* loads the file into here */
|}

let () = UTop_main.main ()
