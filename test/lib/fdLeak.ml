open Refmt_lib

let () =
  let files = List.init 25 (fun i -> i) in
  let input_file = "./input.re" in
  List.iter
    (fun _file -> Eol_detect.get_eol_for_file input_file |> ignore)
    files;
  Format.eprintf "EOL: done@."
