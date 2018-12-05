open Eol_detect

let lf_to_crlf s =
    let rec loop sz =
        match String.index sz '\n' with
        | exception Not_found -> sz
        | idx ->
            let l = (String.sub sz 0 idx) ^ "\r\n" in
            let length = String.length sz in
            l ^ loop (String.sub sz (idx + 1) (length - idx - 1))
    in
    loop s

let get_formatter output_channel eol =
    let f = Format.formatter_of_out_channel output_channel in
    let out_functions = Format.pp_get_formatter_out_functions f () in
    let out_string s p n = 
        match eol with
        | LF -> out_functions.out_string s p n
        | CRLF ->
            let str = String.sub s p n in
            let str = lf_to_crlf str in
            out_functions.out_string str 0 (String.length str)
    in
    let new_functions = { out_functions with out_string } in
    Format.pp_set_formatter_out_functions f new_functions;
    f
