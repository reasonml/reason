(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type term_escape =
  [ `Error of bool * string
  | `Help of Cmdliner_manpage.format * string option ]

type 'a parser =
  Cmdliner_info.Eval.t -> Cmdliner_cline.t ->
  ('a, [ `Parse of string | term_escape ]) result

type 'a t = Cmdliner_info.Arg.Set.t * 'a parser

let const v = Cmdliner_info.Arg.Set.empty, (fun _ _ -> Ok v)
let app (args_f, f) (args_v, v) =
  Cmdliner_info.Arg.Set.union args_f args_v,
  fun ei cl -> match (f ei cl) with
  | Error _ as e -> e
  | Ok f ->
      match v ei cl with
      | Error _ as e -> e
      | Ok v -> Ok (f v)

let map f v = app (const f) v
let product v0 v1 = app (app (const (fun x y -> (x, y))) v0) v1

module Syntax = struct
  let ( let+ ) v f = map f v
  let ( and+ ) = product
end

(* Terms *)

let ( $ ) = app

type 'a ret = [ `Ok of 'a | term_escape ]

let ret (al, v) =
  al, fun ei cl -> match v ei cl with
  | Ok (`Ok v) -> Ok v
  | Ok (`Error _ as err) -> Error err
  | Ok (`Help _ as help) -> Error help
  | Error _ as e -> e

let term_result ?(usage = false) (al, v) =
  al, fun ei cl -> match v ei cl with
  | Ok (Ok _ as ok) -> ok
  | Ok (Error (`Msg e)) -> Error (`Error (usage, e))
  | Error _ as e -> e

let term_result' ?usage t =
  let wrap = app (const (Result.map_error (fun e -> `Msg e))) t in
  term_result ?usage wrap

let cli_parse_result (al, v) =
  al, fun ei cl -> match v ei cl with
  | Ok (Ok _ as ok) -> ok
  | Ok (Error (`Msg e)) -> Error (`Parse e)
  | Error _ as e -> e

let cli_parse_result' t =
  let wrap = app (const (Result.map_error (fun e -> `Msg e))) t in
  cli_parse_result wrap

let main_name =
  Cmdliner_info.Arg.Set.empty,
  (fun ei _ -> Ok (Cmdliner_info.Cmd.name @@ Cmdliner_info.Eval.main ei))

let choice_names =
  Cmdliner_info.Arg.Set.empty,
  (fun ei _ ->
     (* N.B. this keeps everything backward compatible. We return the command
        names of main's children *)
     let name t = Cmdliner_info.Cmd.name t in
     let choices = Cmdliner_info.Cmd.children (Cmdliner_info.Eval.main ei) in
     Ok (List.rev_map name choices))

let with_used_args (al, v) : (_ * string list) t =
  al, fun ei cl ->
    match v ei cl with
    | Ok x ->
        let actual_args arg_info acc =
          let args = Cmdliner_cline.actual_args cl arg_info in
          List.rev_append args acc
        in
        let used = List.rev (Cmdliner_info.Arg.Set.fold actual_args al []) in
        Ok (x, used)
    | Error _ as e -> e
