open Refmt_lib

let () =
  let files = List.init ~len:25 ~f:(fun i -> i) in
  let input_file = "./input.re" in
  List.iter
    ~f:(fun _file -> End_of_line.Detect.get_eol_for_file input_file |> ignore)
    files;
  Format.eprintf "EOL: done@."
