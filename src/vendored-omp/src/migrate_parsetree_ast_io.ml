(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ast =
  | Impl : (module Migrate_parsetree_versions.OCaml_version with
             type Ast.Parsetree.structure = 'concrete) * 'concrete -> ast
  | Intf : (module Migrate_parsetree_versions.OCaml_version with
             type Ast.Parsetree.signature = 'concrete) * 'concrete -> ast

type filename = string

let magic_length = String.length Ast_402.Config.ast_impl_magic_number

let read_magic ic =
  let buf = Bytes.create magic_length in
  let len = input ic buf 0 magic_length in
  let s = Bytes.sub_string buf 0 len in
  if len = magic_length then
    Ok s
  else
    Error s

type read_error =
  | Not_a_binary_ast of string
  | Unknown_version of string

let find_magic magic =
  let rec loop = function
    | [] ->
        let prefix = String.sub magic 0 9 in
        if prefix = String.sub Ast_402.Config.ast_impl_magic_number 0 9 ||
           prefix = String.sub Ast_402.Config.ast_intf_magic_number 0 9 then
          Error (Unknown_version magic)
        else
          Error (Not_a_binary_ast magic)
    | (module Frontend : Migrate_parsetree_versions.OCaml_version) :: tail ->
        if Frontend.Ast.Config.ast_impl_magic_number = magic then
          Ok (fun x -> Impl ((module Frontend), Obj.obj x))
        else if Frontend.Ast.Config.ast_intf_magic_number = magic then
          Ok (fun x -> Intf ((module Frontend), Obj.obj x))
        else
          loop tail
  in
  loop Migrate_parsetree_versions.all_versions

let from_channel ic =
  match read_magic ic with
  | Error s -> Error (Not_a_binary_ast s)
  | Ok s ->
    match find_magic s with
    | Ok inj ->
      let filename : filename = input_value ic in
      let payload = inj (input_value ic) in
      Ok (filename, payload)
    | Error _ as e  -> e

let from_bytes bytes pos =
  if Bytes.length bytes - pos < magic_length then
    Error (Not_a_binary_ast "")
  else
    let magic = Bytes.to_string (Bytes.sub bytes pos magic_length) in
    match find_magic magic with
    | Ok inj ->
      let filename_pos = pos + magic_length in
      let filename : filename = Marshal.from_bytes bytes filename_pos in
      let payload_pos = filename_pos + Marshal.total_size bytes filename_pos in
      let payload = inj (Marshal.from_bytes bytes payload_pos) in
      Ok (filename, payload)
    | Error _ as e -> e

let decompose_ast = function
  | Impl ((module Frontend), tree) ->
      (Frontend.Ast.Config.ast_impl_magic_number, Obj.repr tree)
  | Intf ((module Frontend), tree) ->
      (Frontend.Ast.Config.ast_intf_magic_number, Obj.repr tree)

let to_channel oc (filename : filename) x =
  let magic_number, payload = decompose_ast x in
  output_string oc magic_number;
  output_value oc filename;
  output_value oc payload

let to_bytes (filename : filename) x =
  let magic_number, payload = decompose_ast x in
  Bytes.cat (
    Bytes.cat
      (Bytes.of_string magic_number)
      (Marshal.to_bytes filename [])
  ) (Marshal.to_bytes payload [])
