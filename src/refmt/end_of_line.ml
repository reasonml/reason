type t =
  | LF
  | CRLF

let pp fmt eol =
  Format.pp_print_string fmt (match eol with LF -> "lf" | CRLF -> "crlf")

module Detect = struct
  let default = match Sys.win32 with true -> CRLF | _ -> LF

  let get_eol_for_file =
    let rec loop ic prev =
      match input_char ic with
      | '\n' -> (match prev with '\r' -> CRLF | _ -> LF)
      | c -> loop ic c
    in
    fun filename ->
      let ic = open_in_bin filename in
      let eol = try loop ic ' ' with End_of_file -> default in
      close_in ic;
      eol
end

module Convert = struct
  let lf_to_crlf =
    let rec loop sz =
      match String.index sz '\n' with
      | exception Not_found -> sz
      | idx ->
        let l = String.sub sz 0 idx ^ "\r\n" in
        let length = String.length sz in
        l ^ loop (String.sub sz (idx + 1) (length - idx - 1))
    in
    fun s -> loop s

  let get_formatter =
    let out_string (out_functions : Format.formatter_out_functions) eol =
     fun s p n ->
      match eol with
      | LF -> out_functions.out_string s p n
      | CRLF ->
        let str = String.sub s p n in
        let str = lf_to_crlf str in
        out_functions.out_string str 0 (String.length str)
    in
    fun output_channel eol ->
      let f = Format.formatter_of_out_channel output_channel in
      let new_functions =
        let out_functions = Format.pp_get_formatter_out_functions f () in
        { out_functions with out_string = out_string out_functions eol }
      in
      Format.pp_set_formatter_out_functions f new_functions;
      f
end
