open Reason_omp

type parse_itype = [ `ML | `Reason | `Binary | `BinaryReason | `Auto ]
type print_itype = [ `ML | `Reason | `Binary | `BinaryReason | `AST | `None ]

exception Invalid_config of string

module type PRINTER =
    sig
        type t

        val parse : use_stdin:bool ->
                    parse_itype ->
                    string ->
                    ((t * Reason_comment.t list) * bool)


        val print : print_itype ->
                    string ->
                    bool ->
                    out_channel ->
                    Format.formatter ->
                    ((t * Reason_comment.t list) -> unit)
    end

let err s = raise (Invalid_config s)

let prepare_output_file name =
    match name with
    | Some name -> open_out_bin name
    | None -> set_binary_mode_out stdout true; stdout

let close_output_file output_file output_chan =
    match output_file with
    | Some _ -> close_out output_chan
    | None -> ()

let ocamlBinaryParser use_stdin filename =
  let chan =
    match use_stdin with
      | true -> stdin
      | false ->
          let file_chan = open_in_bin filename in
          seek_in file_chan 0;
          file_chan
  in
  match Ast_io.from_channel chan with
  | Error _ -> assert false
  | Ok (_, Ast_io.Impl ((module Version), ast)) ->
    let module Convert = Convert(Version)(OCaml_411) in
    ((Obj.magic (Convert.copy_structure ast), []), true, false)
  | Ok (_, Ast_io.Intf ((module Version), ast)) ->
    let module Convert = Convert(Version)(OCaml_411) in
    ((Obj.magic (Convert.copy_signature ast), []), true, true)

let reasonBinaryParser use_stdin filename =
  let chan =
    match use_stdin with
      | true -> stdin
      | false ->
          let file_chan = open_in_bin filename in
          seek_in file_chan 0;
          file_chan
  in
  let (_, _, ast, comments, parsedAsML, parsedAsInterface) = input_value chan in
  ((ast, comments), parsedAsML, parsedAsInterface)
