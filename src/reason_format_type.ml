(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

(* No String.split in stdlib... *)
let split str ~by =
  let rec split' str ~by accum =
    try
      let foundIdx = String.index_from str 0 by in
      let subStr = String.sub str 0 foundIdx in
      split'
        (String.sub str (foundIdx + 1) (String.length str - foundIdx - 1))
        ~by
        (subStr :: accum)
    with Not_found ->
      List.rev (str :: accum)
  in
  split' str ~by []

let () =
  if Array.length Sys.argv <> 2 then
    print_endline @@ "Please provide a single, quoted string of all the " ^
      "types you want transformed, separated by the escaped double quote \\\""
  else
    try
      Sys.argv.(1)
      |> split ~by:'"'
      |> List.map (fun input ->
        try Reason_type_of_ocaml_type.convert input |> String.trim |> String.escaped
        with Syntaxerr.Error a ->
          (* Can't parse the input for some reason? Log to disk and return the input (don't crash). *)
          let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 "./unprintable_ocaml_type_error_please_report.txt" in
          output_string oc input;
          close_out oc;
          input
      )
      |> String.concat "\n"
      (* We omit printing one last line break in order to conserve the invariant
      that 1 type maps to 1 line. E.g. ["a"] maps to "a" and ["a", ""] maps to
      "a\n" *)
      |> print_string
    with Syntaxerr.Error a ->
      prerr_endline "Failed to parse the input type(s)."
