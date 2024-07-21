(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf
let quote = Cmdliner_base.quote

let pp = Format.fprintf
let pp_text = Cmdliner_base.pp_text
let pp_lines = Cmdliner_base.pp_lines

(* Environment variable errors *)

let err_env_parse env ~err =
  let var = Cmdliner_info.Env.info_var env in
  strf "environment variable %s: %s" (quote var) err

(* Positional argument errors *)

let err_pos_excess excess =
  strf "too many arguments, don't know what to do with %s"
    (String.concat ", " (List.map quote excess))

let err_pos_miss a = match Cmdliner_info.Arg.docv a with
| "" -> "a required argument is missing"
| v -> strf "required argument %s is missing" v

let err_pos_misses = function
| [] -> assert false
| [a] -> err_pos_miss a
| args ->
    let add_arg acc a = match Cmdliner_info.Arg.docv a with
    | "" -> "ARG" :: acc
    | argv -> argv :: acc
    in
    let rev_args = List.sort Cmdliner_info.Arg.rev_pos_cli_order args in
    let args = List.fold_left add_arg [] rev_args in
    let args = String.concat ", " args in
    strf "required arguments %s are missing" args

let err_pos_parse a ~err = match Cmdliner_info.Arg.docv a with
| "" -> err
| argv ->
    match Cmdliner_info.Arg.(pos_len @@ pos_kind a) with
    | Some 1 -> strf "%s argument: %s" argv err
    | None | Some _ -> strf "%s… arguments: %s" argv err

(* Optional argument errors *)

let err_flag_value flag v =
  strf "option %s is a flag, it cannot take the argument %s"
    (quote flag) (quote v)

let err_opt_value_missing f = strf "option %s needs an argument" (quote f)
let err_opt_parse f ~err = strf "option %s: %s" (quote f) err
let err_opt_repeated f f' =
  if f = f' then strf "option %s cannot be repeated" (quote f) else
  strf "options %s and %s cannot be present at the same time"
    (quote f) (quote f')

(* Argument errors *)

let err_arg_missing a =
  if Cmdliner_info.Arg.is_pos a then err_pos_miss a else
  strf "required option %s is missing" (Cmdliner_info.Arg.opt_name_sample a)

let err_cmd_missing ~dom =
  strf "required COMMAND name is missing, must be %s."
    (Cmdliner_base.alts_str ~quoted:true dom)

(* Other messages *)

let exec_name ei = Cmdliner_info.Cmd.name @@ Cmdliner_info.Eval.main ei

let pp_version ppf ei =
  match Cmdliner_info.Cmd.version @@ Cmdliner_info.Eval.main ei with
  | None -> assert false
  | Some v -> pp ppf "@[%a@]@." Cmdliner_base.pp_text v

let pp_try_help ppf ei =
  let rcmds = Cmdliner_info.Eval.(cmd ei :: parents ei) in
  match List.rev_map Cmdliner_info.Cmd.name rcmds with
  | [] -> assert false
  | [n] -> pp ppf "@[<2>Try '%s --help' for more information.@]" n
  | n :: _ as cmds ->
      let cmds = String.concat " " cmds in
      pp ppf "@[<2>Try '%s --help' or '%s --help' for more information.@]"
        cmds n

let pp_err ppf ei ~err = pp ppf "%s: @[%a@]@." (exec_name ei) pp_lines err

let pp_err_usage ppf ei ~err_lines ~err =
  let pp_err = if err_lines then pp_lines else pp_text in
  pp ppf "@[<v>%s: @[%a@]@,@[Usage: @[%a@]@]@,%a@]@."
    (exec_name ei) pp_err err (Cmdliner_docgen.pp_plain_synopsis ~errs:ppf) ei
    pp_try_help ei

let pp_backtrace ppf ei e bt =
  let bt = Printexc.raw_backtrace_to_string bt in
  let bt =
    let len = String.length bt in
    if len > 0 then String.sub bt 0 (len - 1) (* remove final '\n' *) else bt
  in
  pp ppf "%s: @[internal error, uncaught exception:@\n%a@]@."
    (exec_name ei) pp_lines (strf "%s\n%s" (Printexc.to_string e) bt)
