type t =
  | LF
  | CRLF

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
    let rec loop ~src i j ~dst ~len =
      if i >= len
      then ()
      else
        match String.unsafe_get src i with
        | '\n' ->
          Bytes.unsafe_set dst j '\r';
          Bytes.unsafe_set dst (j + 1) '\n';
          loop (i + 1) (j + 2) ~src ~dst ~len
        | c ->
          Bytes.unsafe_set dst j c;
          loop (i + 1) (j + 1) ~src ~dst ~len
    in
    let rec count_newlines ~src ~src_len i acc =
      if i >= src_len
      then acc
      else
        match String.index_from src i '\n' with
        | exception Not_found -> acc
        | j -> count_newlines ~src ~src_len (j + 1) (acc + 1)
    in
    fun s ->
      let len = String.length s in
      match count_newlines ~src:s ~src_len:len 0 0 with
      | 0 -> s
      | nl_count ->
        let dst = Bytes.create (len + nl_count) in
        loop 0 0 ~src:s ~dst ~len;
        Bytes.unsafe_to_string dst

  let get_formatter =
    let out_string (out_functions : Format.formatter_out_functions) eol =
     fun s p n ->
      match eol with
      | LF -> out_functions.out_string s p n
      | CRLF ->
        let str = String.sub s ~pos:p ~len:n in
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
