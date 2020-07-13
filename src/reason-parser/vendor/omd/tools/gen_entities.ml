let f s =
  Scanf.sscanf s " \"&%s@;\": { %_S: [%s@], %_S: \"%_s@\" }"
    (fun s nums ->
       s, List.map (fun s -> int_of_string (String.trim s)) (String.split_on_char ',' nums))

let main ic =
  let rec loop () =
    match input_line ic with
    | s ->
        begin match f s with
        | name, codepoints ->
            Printf.printf "  | %S -> [" name;
            List.iteri (fun i cp -> if i > 0 then print_string "; "; Printf.printf "Uchar.of_int %d" cp) codepoints;
            Printf.printf "]\n"
        | exception _ ->
            ()
        end;
        loop ()
    | exception End_of_file ->
        Printf.printf "  | _ -> []\n%!"
  in
  Printf.printf "let f = function\n";
  loop ()

let () =
  let ic = open_in Sys.argv.(1) in
  main ic;
  close_in ic
