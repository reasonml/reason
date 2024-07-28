(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let rev_compare n0 n1 = compare n1 n0

(* Invalid_argument strings **)

let err_not_opt = "Option argument without name"
let err_not_pos = "Positional argument with a name"

(* Documentation formatting helpers *)

let strf = Printf.sprintf
let doc_quote = Cmdliner_base.quote
let doc_alts = Cmdliner_base.alts_str
let doc_alts_enum ?quoted enum = doc_alts ?quoted (List.map fst enum)

let str_of_pp pp v = pp Format.str_formatter v; Format.flush_str_formatter ()

(* Argument converters *)

type 'a parser = string -> [ `Ok of 'a | `Error of string ]
type 'a printer = Format.formatter -> 'a -> unit

type 'a conv = 'a parser * 'a printer
type 'a converter = 'a conv

let default_docv = "VALUE"
let conv ?docv (parse, print) =
  let parse s = match parse s with Ok v -> `Ok v | Error (`Msg e) -> `Error e in
  parse, print

let conv' ?docv (parse, print) =
  let parse s = match parse s with Ok v -> `Ok v | Error e -> `Error e in
  parse, print

let pconv ?docv conv = conv

let conv_parser (parse, _) =
  fun s -> match parse s with `Ok v -> Ok v | `Error e -> Error (`Msg e)

let conv_printer (_, print) = print
let conv_docv _ = default_docv

let err_invalid s kind = `Msg (strf "invalid value '%s', expected %s" s kind)
let parser_of_kind_of_string ~kind k_of_string =
  fun s -> match k_of_string s with
  | None -> Error (err_invalid s kind)
  | Some v -> Ok v

let some = Cmdliner_base.some
let some' = Cmdliner_base.some'

(* Argument information *)

type env = Cmdliner_info.Env.info
let env_var = Cmdliner_info.Env.info

type 'a t = 'a Cmdliner_term.t
type info = Cmdliner_info.Arg.t
let info = Cmdliner_info.Arg.v

(* Arguments *)

let ( & ) f x = f x

let err e = Error (`Parse e)

let parse_to_list parser s = match parser s with
| `Ok v -> `Ok [v]
| `Error _ as e -> e

let report_deprecated_env ei e = match Cmdliner_info.Env.info_deprecated e with
| None -> ()
| Some msg ->
    let var = Cmdliner_info.Env.info_var e in
    let msg = String.concat "" ["environment variable "; var; ": "; msg ] in
    let err_fmt = Cmdliner_info.Eval.err_ppf ei in
    Cmdliner_msg.pp_err err_fmt ei ~err:msg

let try_env ei a parse ~absent = match Cmdliner_info.Arg.env a with
| None -> Ok absent
| Some env ->
    let var = Cmdliner_info.Env.info_var env in
    match Cmdliner_info.Eval.env_var ei var with
    | None -> Ok absent
    | Some v ->
        match parse v with
        | `Error e -> err (Cmdliner_msg.err_env_parse env ~err:e)
        | `Ok v -> report_deprecated_env ei env; Ok v

let arg_to_args = Cmdliner_info.Arg.Set.singleton
let list_to_args f l =
  let add acc v = Cmdliner_info.Arg.Set.add (f v) acc in
  List.fold_left add Cmdliner_info.Arg.Set.empty l

let flag a =
  if Cmdliner_info.Arg.is_pos a then invalid_arg err_not_opt else
  let convert ei cl = match Cmdliner_cline.opt_arg cl a with
  | [] -> try_env ei a Cmdliner_base.env_bool_parse ~absent:false
  | [_, _, None] -> Ok true
  | [_, f, Some v] -> err (Cmdliner_msg.err_flag_value f v)
  | (_, f, _) :: (_ ,g, _) :: _  -> err (Cmdliner_msg.err_opt_repeated f g)
  in
  arg_to_args a, convert

let flag_all a =
  if Cmdliner_info.Arg.is_pos a then invalid_arg err_not_opt else
  let a = Cmdliner_info.Arg.make_all_opts a in
  let convert ei cl = match Cmdliner_cline.opt_arg cl a with
  | [] ->
      try_env ei a (parse_to_list Cmdliner_base.env_bool_parse) ~absent:[]
  | l ->
      try
        let truth (_, f, v) = match v with
        | None -> true
        | Some v -> failwith (Cmdliner_msg.err_flag_value f v)
        in
        Ok (List.rev_map truth l)
      with Failure e -> err e
  in
  arg_to_args a, convert

let vflag v l =
  let convert _ cl =
    let rec aux fv = function
    | (v, a) :: rest ->
        begin match Cmdliner_cline.opt_arg cl a with
        | [] -> aux fv rest
        | [_, f, None] ->
            begin match fv with
            | None -> aux (Some (f, v)) rest
            | Some (g, _) -> failwith (Cmdliner_msg.err_opt_repeated g f)
            end
        | [_, f, Some v] -> failwith (Cmdliner_msg.err_flag_value f v)
        | (_, f, _) :: (_, g, _) :: _ ->
            failwith (Cmdliner_msg.err_opt_repeated g f)
        end
    | [] -> match fv with None -> v | Some (_, v) -> v
    in
    try Ok (aux None l) with Failure e -> err e
  in
  let flag (_, a) =
    if Cmdliner_info.Arg.is_pos a then invalid_arg err_not_opt else a
  in
  list_to_args flag l, convert

let vflag_all v l =
  let convert _ cl =
    let rec aux acc = function
    | (fv, a) :: rest ->
        begin match Cmdliner_cline.opt_arg cl a with
        | [] -> aux acc rest
        | l ->
            let fval (k, f, v) = match v with
            | None -> (k, fv)
            | Some v -> failwith (Cmdliner_msg.err_flag_value f v)
            in
            aux (List.rev_append (List.rev_map fval l) acc) rest
        end
    | [] ->
        if acc = [] then v else List.rev_map snd (List.sort rev_compare acc)
    in
    try Ok (aux [] l) with Failure e -> err e
  in
  let flag (_, a) =
    if Cmdliner_info.Arg.is_pos a then invalid_arg err_not_opt else
    Cmdliner_info.Arg.make_all_opts a
  in
  list_to_args flag l, convert

let parse_opt_value parse f v = match parse v with
| `Ok v -> v
| `Error err -> failwith (Cmdliner_msg.err_opt_parse f ~err)

let opt ?vopt (parse, print) v a =
  if Cmdliner_info.Arg.is_pos a then invalid_arg err_not_opt else
  let absent = match Cmdliner_info.Arg.absent a with
  | Cmdliner_info.Arg.Doc d as a when d <> "" -> a
  | _ -> Cmdliner_info.Arg.Val (lazy (str_of_pp print v))
  in
  let kind = match vopt with
  | None -> Cmdliner_info.Arg.Opt
  | Some dv -> Cmdliner_info.Arg.Opt_vopt (str_of_pp print dv)
  in
  let a = Cmdliner_info.Arg.make_opt ~absent ~kind a in
  let convert ei cl = match Cmdliner_cline.opt_arg cl a with
  | [] -> try_env ei a parse ~absent:v
  | [_, f, Some v] ->
      (try Ok (parse_opt_value parse f v) with Failure e -> err e)
  | [_, f, None] ->
      begin match vopt with
      | None -> err (Cmdliner_msg.err_opt_value_missing f)
      | Some optv -> Ok optv
      end
  | (_, f, _) :: (_, g, _) :: _ -> err (Cmdliner_msg.err_opt_repeated g f)
  in
  arg_to_args a, convert

let opt_all ?vopt (parse, print) v a =
  if Cmdliner_info.Arg.is_pos a then invalid_arg err_not_opt else
  let absent = match Cmdliner_info.Arg.absent a with
  | Cmdliner_info.Arg.Doc d as a when d <> "" -> a
  | _ -> Cmdliner_info.Arg.Val (lazy "")
  in
  let kind = match vopt with
  | None -> Cmdliner_info.Arg.Opt
  | Some dv -> Cmdliner_info.Arg.Opt_vopt (str_of_pp print dv)
  in
  let a = Cmdliner_info.Arg.make_opt_all ~absent ~kind a in
  let convert ei cl = match Cmdliner_cline.opt_arg cl a with
  | [] -> try_env ei a (parse_to_list parse) ~absent:v
  | l ->
      let parse (k, f, v) = match v with
      | Some v -> (k, parse_opt_value parse f v)
      | None -> match vopt with
      | None -> failwith (Cmdliner_msg.err_opt_value_missing f)
      | Some dv -> (k, dv)
      in
      try Ok (List.rev_map snd
                (List.sort rev_compare (List.rev_map parse l))) with
      | Failure e -> err e
  in
  arg_to_args a, convert

(* Positional arguments *)

let parse_pos_value parse a v = match parse v with
| `Ok v -> v
| `Error err -> failwith (Cmdliner_msg.err_pos_parse a ~err)

let pos ?(rev = false) k (parse, print) v a =
  if Cmdliner_info.Arg.is_opt a then invalid_arg err_not_pos else
  let absent = match Cmdliner_info.Arg.absent a with
  | Cmdliner_info.Arg.Doc d as a when d <> "" -> a
  | _ -> Cmdliner_info.Arg.Val (lazy (str_of_pp print v))
  in
  let pos = Cmdliner_info.Arg.pos ~rev ~start:k ~len:(Some 1) in
  let a = Cmdliner_info.Arg.make_pos_abs ~absent ~pos a in
  let convert ei cl = match Cmdliner_cline.pos_arg cl a with
  | [] -> try_env ei a parse ~absent:v
  | [v] ->
      (try Ok (parse_pos_value parse a v) with Failure e -> err e)
  | _ -> assert false
  in
  arg_to_args a, convert

let pos_list pos (parse, _) v a =
  if Cmdliner_info.Arg.is_opt a then invalid_arg err_not_pos else
  let a = Cmdliner_info.Arg.make_pos ~pos a in
  let convert ei cl = match Cmdliner_cline.pos_arg cl a with
  | [] -> try_env ei a (parse_to_list parse) ~absent:v
  | l ->
      try Ok (List.rev (List.rev_map (parse_pos_value parse a) l)) with
      | Failure e -> err e
  in
  arg_to_args a, convert

let all = Cmdliner_info.Arg.pos ~rev:false ~start:0 ~len:None
let pos_all c v a = pos_list all c v a

let pos_left ?(rev = false) k =
  let start = if rev then k + 1 else 0 in
  let len = if rev then None else Some k in
  pos_list (Cmdliner_info.Arg.pos ~rev ~start ~len)

let pos_right ?(rev = false) k =
  let start = if rev then 0 else k + 1 in
  let len = if rev then Some k else None in
  pos_list (Cmdliner_info.Arg.pos ~rev ~start ~len)

(* Arguments as terms *)

let absent_error args =
  let make_req a acc =
    let req_a = Cmdliner_info.Arg.make_req a in
    Cmdliner_info.Arg.Set.add req_a acc
  in
  Cmdliner_info.Arg.Set.fold make_req args Cmdliner_info.Arg.Set.empty

let value a = a

let err_arg_missing args =
  err @@ Cmdliner_msg.err_arg_missing (Cmdliner_info.Arg.Set.choose args)

let required (args, convert) =
  let args = absent_error args in
  let convert ei cl = match convert ei cl with
  | Ok (Some v) -> Ok v
  | Ok None -> err_arg_missing args
  | Error _ as e -> e
  in
  args, convert

let non_empty (al, convert) =
  let args = absent_error al in
  let convert ei cl = match convert ei cl with
  | Ok [] -> err_arg_missing args
  | Ok l -> Ok l
  | Error _ as e -> e
  in
  args, convert

let last (args, convert) =
  let convert ei cl = match convert ei cl with
  | Ok [] -> err_arg_missing args
  | Ok l -> Ok (List.hd (List.rev l))
  | Error _ as e -> e
  in
  args, convert

(* Predefined arguments *)

let man_fmts =
  ["auto", `Auto; "pager", `Pager; "groff", `Groff; "plain", `Plain]

let man_fmt_docv = "FMT"
let man_fmts_enum = Cmdliner_base.enum man_fmts
let man_fmts_alts = doc_alts_enum man_fmts
let man_fmts_doc kind =
  strf "Show %s in format $(docv). The value $(docv) must be %s. \
        With $(b,auto), the format is $(b,pager) or $(b,plain) whenever \
        the $(b,TERM) env var is $(b,dumb) or undefined."
    kind man_fmts_alts

let man_format =
  let doc = man_fmts_doc "output" in
  let docv = man_fmt_docv in
  value & opt man_fmts_enum `Pager & info ["man-format"] ~docv ~doc

let stdopt_version ~docs =
  value & flag & info ["version"] ~docs ~doc:"Show version information."

let stdopt_help ~docs =
  let doc = man_fmts_doc "this help" in
  let docv = man_fmt_docv in
  value & opt ~vopt:(Some `Auto) (some man_fmts_enum) None &
  info ["help"] ~docv ~docs ~doc

(* Predefined converters. *)

let bool = Cmdliner_base.bool
let char = Cmdliner_base.char
let int = Cmdliner_base.int
let nativeint = Cmdliner_base.nativeint
let int32 = Cmdliner_base.int32
let int64 = Cmdliner_base.int64
let float = Cmdliner_base.float
let string = Cmdliner_base.string
let enum = Cmdliner_base.enum
let file = Cmdliner_base.file
let dir = Cmdliner_base.dir
let non_dir_file = Cmdliner_base.non_dir_file
let list = Cmdliner_base.list
let array = Cmdliner_base.array
let pair = Cmdliner_base.pair
let t2 = Cmdliner_base.t2
let t3 = Cmdliner_base.t3
let t4 = Cmdliner_base.t4
