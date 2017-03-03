open Migrate_parsetree
open Ast_404

type parse_itype = [ `ML | `Reason | `Binary | `BinaryReason | `Auto ]
type print_itype = [ `ML | `Reason | `Binary | `BinaryReason | `AST | `None ]

exception Invalid_config of string

module type PRINTER =
    sig
        type t

        val parse : parse_itype ->
                    bool ->
                    string ->
                    ((t * Reason_pprint_ast.commentWithCategory) * bool)

        val print : print_itype ->
                    string ->
                    bool ->
                    out_channel ->
                    Format.formatter ->
                    ((t * Reason_pprint_ast.commentWithCategory) -> unit)
    end

let err s = raise (Invalid_config s)

let prepare_output_file = function
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
          let file_chan = open_in filename in
          seek_in file_chan 0;
          file_chan
  in
  match Ast_io.from_channel chan with
  | Result.Error err -> assert false
  | Result.Ok (_, Ast_io.Impl ((module Version), ast)) ->
    let module Convert = Convert(Version)(OCaml_404) in
    ((Obj.magic (Convert.copy_structure ast), []), true, false)
  | Result.Ok (_, Ast_io.Intf ((module Version), ast)) ->
    let module Convert = Convert(Version)(OCaml_404) in
    ((Obj.magic (Convert.copy_signature ast), []), true, true)

let reasonBinaryParser use_stdin filename =
  let chan =
    match use_stdin with
      | true -> stdin
      | false ->
          let file_chan = open_in filename in
          seek_in file_chan 0;
          file_chan
  in
  let (magic_number, filename, ast, comments, parsedAsML, parsedAsInterface) = input_value chan in
  ((ast, comments), parsedAsML, parsedAsInterface)

let str_ppx_show_runtime =
  let open Asttypes in
  let open Parsetree in
  let open Longident in
  let open Ast_helper in
  let open Location in
  let mktypealias (name, params, types) =
    (* Unsure if this is correct. *)
    let manifest = Typ.constr (mknoloc (Lident name)) types in
    Str.type_ Nonrecursive [Type.mk ~params ~kind:Ptype_abstract ~manifest (mknoloc name)]
  in
  let mkmodulealias name =
    Str.module_ (Mb.mk (mknoloc name) (Mod.ident (mknoloc (Lident name))))
  in
  let mkmoduleinclude name =
    Str.include_ (Incl.mk (Mod.ident (mknoloc (Lident name))))
  in
  let type_aliases =
    let n s = (s, [], []) in
    let a s = (s, [(Typ.var "a"), Invariant], [Typ.var "a"]) in
    List.map mktypealias [n "int"; n "char"; n "string"; n "float"; n "bool";
                          n "unit"; n "exn"; a "array"; a "list"; a "option";
                          n "nativeint"; n "int32"; n "int64"; a "lazy_t";
                          n "bytes"]
  in
  let module_aliases = List.map mkmodulealias ["Pervasives"; "Char"; "String";
    "Printexc"; "Array"; "List"; "Nativeint"; "Int32"; "Int64"; "Lazy";
    "Bytes"; "Hashtbl"; "Queue"; "Stack"; "Set"; "Weak"; "Printf"; "Format";
    "Buffer"]
  in
  let module_includes = List.map mkmoduleinclude ["Pervasives"]
  in
  let structure_items = type_aliases @ module_aliases @ module_includes in
  Str.module_ (Mb.mk (mknoloc "Ppx_deriving_runtime")
                     (Mod.structure structure_items))

let sig_ppx_show_runtime =
  let open Asttypes in
  let open Parsetree in
  let open Longident in
  let open Ast_helper in
  let open Location in
  let mktypealias (name, params, types) =
    (* Unsure if this is correct. *)
    let manifest = Typ.constr (mknoloc (Lident name)) types in
    Sig.type_ Nonrecursive [Type.mk ~params ~kind:Ptype_abstract ~manifest (mknoloc name)]
  in
  let type_aliases =
    let n s = (s, [], []) in
    let a s = (s, [(Typ.var "a"), Invariant], [Typ.var "a"]) in
    List.map mktypealias [n "int"; n "char"; n "string"; n "float"; n "bool";
                          n "unit"; n "exn"; a "array"; a "list"; a "option";
                          n "nativeint"; n "int32"; n "int64"; a "lazy_t";
                          n "bytes"]
  in
  let structure_items = type_aliases in
  Sig.module_ (Md.mk (mknoloc "Ppx_deriving_runtime")
                     (Mty.signature structure_items))
