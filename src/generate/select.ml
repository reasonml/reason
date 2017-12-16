(*
 * Originally from https://github.com/janestreet/ppx_ast
 * Modified to be compatible with pre-4.04 OCaml
 *)

let dump_file file =
  let buf = Bytes.create 1024 in
  let ic = open_in file in
  let rec loop () =
    let len = input ic buf 0 (Bytes.length buf) in
    if len = 0
    then ()
    else
      begin
        output stdout buf 0 len;
        loop ()
      end
  in
  loop ()

let version_match = function
  | "default" -> true
  | version ->
    let len = min (String.length Sys.ocaml_version) (String.length version) in
    String.sub Sys.ocaml_version 0 len = version

let () =
  let rec select_first i =
    if i >= Array.length Sys.argv
    then failwith "select.exe failed to select a file."
    else
      let file = Sys.argv.(i) in
      match Str.split (Str.regexp "-") file with
      | [_; version] ->
        if version_match version
        then dump_file file
        else select_first (succ i)
      | _ -> invalid_arg "select.exe"
  in
  select_first 1
