(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let rev_compare n0 n1 = compare n1 n0
let strf = Printf.sprintf

let order_args a0 a1 =
  match Cmdliner_info.Arg.is_opt a0, Cmdliner_info.Arg.is_opt a1 with
  | true, true -> (* optional by name *)
      let key names =
        let k = List.hd (List.sort rev_compare names) in
        let k = String.lowercase_ascii k in
        if k.[1] = '-' then String.sub k 1 (String.length k - 1) else k
      in
      compare
        (key @@ Cmdliner_info.Arg.opt_names a0)
        (key @@ Cmdliner_info.Arg.opt_names a1)
  | false, false -> (* positional by variable *)
      compare
        (String.lowercase_ascii @@ Cmdliner_info.Arg.docv a0)
        (String.lowercase_ascii @@ Cmdliner_info.Arg.docv a1)
  | true, false -> -1 (* positional first *)
  | false, true -> 1  (* optional after *)

let esc = Cmdliner_manpage.escape
let cmd_name t = esc @@ Cmdliner_info.Cmd.name t

let sorted_items_to_blocks ~boilerplate:b items =
  (* Items are sorted by section and then rev. sorted by appearance.
     We gather them by section in correct order in a `Block and prefix
     them with optional boilerplate *)
  let boilerplate = match b with None -> (fun _ -> None) | Some b -> b in
  let mk_block sec acc = match boilerplate sec with
  | None -> (sec, `Blocks acc)
  | Some b -> (sec, `Blocks (b :: acc))
  in
  let rec loop secs sec acc = function
  | (sec', it) :: its when sec' = sec -> loop secs sec (it :: acc) its
  | (sec', it) :: its -> loop (mk_block sec acc :: secs) sec' [it] its
  | [] -> (mk_block sec acc) :: secs
  in
  match items with
  | [] -> []
  | (sec, it) :: its -> loop [] sec [it] its

(* Doc string variables substitutions. *)

let env_info_subst ~subst e = function
| "env" -> Some (strf "$(b,%s)" @@ esc (Cmdliner_info.Env.info_var e))
| id -> subst id

let exit_info_subst ~subst e = function
| "status" -> Some (strf "%d" (fst @@ Cmdliner_info.Exit.info_codes e))
| "status_max" -> Some (strf "%d" (snd @@ Cmdliner_info.Exit.info_codes e))
| id -> subst id

let arg_info_subst ~subst a = function
| "docv" ->
    Some (strf "$(i,%s)" @@ esc (Cmdliner_info.Arg.docv a))
| "opt" when Cmdliner_info.Arg.is_opt a ->
    Some (strf "$(b,%s)" @@ esc (Cmdliner_info.Arg.opt_name_sample a))
| "env" as id ->
    begin match Cmdliner_info.Arg.env a with
    | Some e -> env_info_subst ~subst e id
    | None -> subst id
    end
| id -> subst id

let cmd_info_subst ei = function
| "tname" -> Some (strf "$(b,%s)" @@ cmd_name (Cmdliner_info.Eval.cmd ei))
| "mname" -> Some (strf "$(b,%s)" @@ cmd_name (Cmdliner_info.Eval.main ei))
| "iname" ->
    let cmd = Cmdliner_info.Eval.cmd ei :: Cmdliner_info.Eval.parents ei in
    let cmd = String.concat " " (List.rev_map Cmdliner_info.Cmd.name cmd) in
    Some (strf "$(b,%s)" cmd)
| _ -> None

(* Command docs *)

let invocation ?(sep = " ") ?(parents = []) cmd =
  let names = List.rev_map Cmdliner_info.Cmd.name (cmd :: parents) in
  esc @@ String.concat sep names

let synopsis_pos_arg a =
  let v = match Cmdliner_info.Arg.docv a with "" -> "ARG" | v -> v in
  let v = strf "$(i,%s)" (esc v) in
  let v = (if Cmdliner_info.Arg.is_req a then strf "%s" else strf "[%s]") v in
  match Cmdliner_info.Arg.(pos_len @@ pos_kind a) with
  | None -> v ^ "…"
  | Some 1 -> v
  | Some n ->
      let rec loop n acc = if n <= 0 then acc else loop (n - 1) (v :: acc) in
      String.concat " " (loop n [])

let synopsis_opt_arg a n =
  let var = match Cmdliner_info.Arg.docv a with "" -> "VAL" | v -> v in
  match Cmdliner_info.Arg.opt_kind a with
  | Cmdliner_info.Arg.Flag -> strf "$(b,%s)" (esc n)
  | Cmdliner_info.Arg.Opt ->
        if String.length n > 2
        then strf "$(b,%s)=$(i,%s)" (esc n) (esc var)
        else strf "$(b,%s) $(i,%s)" (esc n) (esc var)
  | Cmdliner_info.Arg.Opt_vopt _ ->
      if String.length n > 2
      then strf "$(b,%s)[=$(i,%s)]" (esc n) (esc var)
      else strf "$(b,%s) [$(i,%s)]" (esc n) (esc var)

let deprecated cmd = match Cmdliner_info.Cmd.deprecated cmd with
| None -> "" | Some _ -> "(Deprecated) "

let synopsis ?parents cmd = match Cmdliner_info.Cmd.children cmd with
| [] ->
    let rev_cli_order (a0, _) (a1, _) =
      Cmdliner_info.Arg.rev_pos_cli_order a0 a1
    in
    let args = Cmdliner_info.Cmd.args cmd in
    let oargs, pargs = Cmdliner_info.Arg.(Set.partition is_opt args) in
    let oargs =
      (* Keep only those that are listed in the s_options section and
         that are not [--version] or [--help]. * *)
      let keep a =
          let drop_names n = n = "--help" || n = "--version" in
          Cmdliner_info.Arg.docs a = Cmdliner_manpage.s_options &&
          not (List.exists drop_names (Cmdliner_info.Arg.opt_names a))
      in
      let oargs = Cmdliner_info.Arg.Set.(elements (filter keep oargs)) in
      let count = List.length oargs in
      let any_option = "[$(i,OPTION)]…" in
      if count = 0 || count > 3 then any_option else
      let syn a =
        strf "[%s]" (synopsis_opt_arg a (Cmdliner_info.Arg.opt_name_sample a))
      in
      let oargs = List.sort order_args oargs in
      let oargs = String.concat " " (List.map syn oargs) in
      String.concat " " [oargs; any_option]
    in
    let pargs =
      let pargs = Cmdliner_info.Arg.Set.elements pargs in
      if pargs = [] then "" else
      let pargs = List.map (fun a -> a, synopsis_pos_arg a) pargs in
      let pargs = List.sort rev_cli_order pargs in
      String.concat " " ("" (* add a space *) :: List.rev_map snd pargs)
    in
    strf "%s$(b,%s) %s%s"
      (deprecated cmd) (invocation ?parents cmd) oargs pargs
| _cmds ->
    let subcmd = match Cmdliner_info.Cmd.has_args cmd with
    | false -> "$(i,COMMAND)" | true -> "[$(i,COMMAND)]"
    in
    strf "%s$(b,%s) %s …" (deprecated cmd) (invocation ?parents cmd) subcmd

let cmd_docs ei = match Cmdliner_info.(Cmd.children (Eval.cmd ei)) with
| [] -> []
| cmds ->
    let add_cmd acc cmd =
      let syn = synopsis cmd in
      (Cmdliner_info.Cmd.docs cmd, `I (syn, Cmdliner_info.Cmd.doc cmd)) :: acc
    in
    let by_sec_by_rev_name (s0, `I (c0, _)) (s1, `I (c1, _)) =
      let c = compare s0 s1 in
      if c <> 0 then c else compare c1 c0 (* N.B. reverse *)
    in
    let cmds = List.fold_left add_cmd [] cmds in
    let cmds = List.sort by_sec_by_rev_name cmds in
    let cmds = (cmds :> (string * Cmdliner_manpage.block) list) in
    sorted_items_to_blocks ~boilerplate:None cmds

(* Argument docs *)

let arg_man_item_label a =
  let s = match Cmdliner_info.Arg.is_pos a with
  | true -> strf "$(i,%s)" (esc @@ Cmdliner_info.Arg.docv a)
  | false ->
      let names = List.sort compare (Cmdliner_info.Arg.opt_names a) in
      String.concat ", " (List.rev_map (synopsis_opt_arg a) names)
  in
  match Cmdliner_info.Arg.deprecated a with
  | None -> s | Some _ -> "(Deprecated) " ^ s

let arg_to_man_item ~errs ~subst ~buf a =
  let subst = arg_info_subst ~subst a in
  let or_env ~value a = match Cmdliner_info.Arg.env a with
  | None -> ""
  | Some e ->
      let value = if value then " or" else "absent " in
      strf "%s $(b,%s) env" value (esc @@ Cmdliner_info.Env.info_var e)
  in
  let absent = match Cmdliner_info.Arg.absent a with
  | Cmdliner_info.Arg.Err -> "required"
  | Cmdliner_info.Arg.Doc "" -> strf "%s" (or_env ~value:false a)
  | Cmdliner_info.Arg.Doc s ->
      let s = Cmdliner_manpage.subst_vars ~errs ~subst buf s in
      strf "absent=%s%s" s (or_env ~value:true a)
  | Cmdliner_info.Arg.Val v ->
      match Lazy.force v with
      | "" -> strf "%s" (or_env ~value:false a)
      | v -> strf "absent=$(b,%s)%s" (esc v) (or_env ~value:true a)
  in
  let optvopt = match Cmdliner_info.Arg.opt_kind a with
  | Cmdliner_info.Arg.Opt_vopt v -> strf "default=$(b,%s)" (esc v)
  | _ -> ""
  in
  let argvdoc = match optvopt, absent with
  | "", "" -> ""
  | s, "" | "", s -> strf " (%s)" s
  | s, s' -> strf " (%s) (%s)" s s'
  in
  let doc = Cmdliner_info.Arg.doc a in
  let doc = Cmdliner_manpage.subst_vars ~errs ~subst buf doc in
  (Cmdliner_info.Arg.docs a, `I (arg_man_item_label a ^ argvdoc, doc))

let arg_docs ~errs ~subst ~buf ei =
  let by_sec_by_arg a0 a1 =
    let c = compare (Cmdliner_info.Arg.docs a0) (Cmdliner_info.Arg.docs a1) in
    if c <> 0 then c else
    let c =
      match Cmdliner_info.Arg.deprecated a0, Cmdliner_info.Arg.deprecated a1
      with
      | None, None | Some _, Some _ -> 0
      | None, Some _ -> -1 | Some _, None -> 1
    in
    if c <> 0 then c else order_args a0 a1
  in
  let keep_arg a acc =
    if not Cmdliner_info.Arg.(is_pos a && (docv a = "" || doc a = ""))
    then (a :: acc) else acc
  in
  let args = Cmdliner_info.Cmd.args @@ Cmdliner_info.Eval.cmd ei in
  let args = Cmdliner_info.Arg.Set.fold keep_arg args [] in
  let args = List.sort by_sec_by_arg args in
  let args = List.rev_map (arg_to_man_item ~errs ~subst ~buf) args in
  sorted_items_to_blocks ~boilerplate:None args

(* Exit statuses doc *)

let exit_boilerplate sec = match sec = Cmdliner_manpage.s_exit_status with
| false -> None
| true -> Some (Cmdliner_manpage.s_exit_status_intro)

let exit_docs ~errs ~subst ~buf ~has_sexit ei =
  let by_sec (s0, _) (s1, _) = compare s0 s1 in
  let add_exit_item acc e =
    let subst = exit_info_subst ~subst e in
    let min, max = Cmdliner_info.Exit.info_codes e in
    let doc = Cmdliner_info.Exit.info_doc e in
    let label = if min = max then strf "%d" min else strf "%d-%d" min max in
    let item = `I (label, Cmdliner_manpage.subst_vars ~errs ~subst buf doc) in
    (Cmdliner_info.Exit.info_docs e, item) :: acc
  in
  let exits = Cmdliner_info.Cmd.exits @@ Cmdliner_info.Eval.cmd ei in
  let exits = List.sort Cmdliner_info.Exit.info_order exits in
  let exits = List.fold_left add_exit_item [] exits in
  let exits = List.stable_sort by_sec (* sort by section *) exits in
  let boilerplate = if has_sexit then None else Some exit_boilerplate in
  sorted_items_to_blocks ~boilerplate exits

(* Environment doc *)

let env_boilerplate sec = match sec = Cmdliner_manpage.s_environment with
| false -> None
| true -> Some (Cmdliner_manpage.s_environment_intro)

let env_docs ~errs ~subst ~buf ~has_senv ei =
  let add_env_item ~subst (seen, envs as acc) e =
    if Cmdliner_info.Env.Set.mem e seen then acc else
    let seen = Cmdliner_info.Env.Set.add e seen in
    let var = strf "$(b,%s)" @@ esc (Cmdliner_info.Env.info_var e) in
    let var = match Cmdliner_info.Env.info_deprecated e with
    | None -> var | Some _ -> "(Deprecated) " ^ var in
    let doc = Cmdliner_info.Env.info_doc e in
    let doc = Cmdliner_manpage.subst_vars ~errs ~subst buf doc in
    let envs = (Cmdliner_info.Env.info_docs e, `I (var, doc)) :: envs in
    seen, envs
  in
  let add_arg_env a acc = match Cmdliner_info.Arg.env a with
  | None -> acc
  | Some e -> add_env_item ~subst:(arg_info_subst ~subst a) acc e
  in
  let add_env acc e = add_env_item ~subst:(env_info_subst ~subst e) acc e in
  let by_sec_by_rev_name (s0, `I (v0, _)) (s1, `I (v1, _)) =
    let c = compare s0 s1 in
    if c <> 0 then c else compare v1 v0 (* N.B. reverse *)
  in
  (* Arg envs before term envs is important here: if the same is mentioned
     both in an arg and in a term the substs of the arg are allowed. *)
  let args = Cmdliner_info.Cmd.args @@ Cmdliner_info.Eval.cmd ei in
  let tenvs = Cmdliner_info.Cmd.envs @@ Cmdliner_info.Eval.cmd ei in
  let init = Cmdliner_info.Env.Set.empty, [] in
  let acc = Cmdliner_info.Arg.Set.fold add_arg_env args init in
  let _, envs = List.fold_left add_env acc tenvs in
  let envs = List.sort by_sec_by_rev_name envs in
  let envs = (envs :> (string * Cmdliner_manpage.block) list) in
  let boilerplate = if has_senv then None else Some env_boilerplate in
  sorted_items_to_blocks ~boilerplate envs

(* xref doc *)

let xref_docs ~errs ei =
  let main = Cmdliner_info.Eval.main ei in
  let to_xref = function
  | `Main -> Cmdliner_info.Cmd.name main, 1
  | `Tool tool -> tool, 1
  | `Page (name, sec) -> name, sec
  | `Cmd c ->
      (* N.B. we are handling only the first subcommand level here *)
      let cmds = Cmdliner_info.Cmd.children main in
      let mname = Cmdliner_info.Cmd.name main in
      let is_cmd cmd = Cmdliner_info.Cmd.name cmd = c in
      if List.exists is_cmd cmds then strf "%s-%s" mname c, 1 else
      (Format.fprintf errs "xref %s: no such command name@." c; "doc-err", 0)
  in
  let xref_str (name, sec) = strf "%s(%d)" (esc name) sec in
  let xrefs = Cmdliner_info.Cmd.man_xrefs @@ Cmdliner_info.Eval.cmd ei in
  let xrefs = match main == Cmdliner_info.Eval.cmd ei with
  | true -> List.filter (fun x -> x <> `Main) xrefs  (* filter out default *)
  | false -> xrefs
  in
  let xrefs = List.fold_left (fun acc x -> to_xref x :: acc) [] xrefs in
  let xrefs = List.(rev_map xref_str (sort rev_compare xrefs)) in
  if xrefs = [] then [] else
  [Cmdliner_manpage.s_see_also, `P (String.concat ", " xrefs)]

(* Man page construction *)

let ensure_s_name ei sm =
  if Cmdliner_manpage.(smap_has_section sm ~sec:s_name) then sm else
  let cmd = Cmdliner_info.Eval.cmd ei in
  let parents = Cmdliner_info.Eval.parents ei in
  let tname = (deprecated cmd) ^ invocation ~sep:"-" ~parents cmd in
  let tdoc = Cmdliner_info.Cmd.doc cmd in
  let tagline = if tdoc = "" then "" else strf " - %s" tdoc in
  let tagline = `P (strf "%s%s" tname tagline) in
  Cmdliner_manpage.(smap_append_block sm ~sec:s_name tagline)

let ensure_s_synopsis ei sm =
  if Cmdliner_manpage.(smap_has_section sm ~sec:s_synopsis) then sm else
  let cmd = Cmdliner_info.Eval.cmd ei in
  let parents = Cmdliner_info.Eval.parents ei in
  let synopsis = `P (synopsis ~parents cmd) in
  Cmdliner_manpage.(smap_append_block sm ~sec:s_synopsis synopsis)

let insert_cmd_man_docs ~errs ei sm =
  let buf = Buffer.create 200 in
  let subst = cmd_info_subst ei in
  let ins sm (sec, b) = Cmdliner_manpage.smap_append_block sm ~sec b in
  let has_senv = Cmdliner_manpage.(smap_has_section sm ~sec:s_environment) in
  let has_sexit = Cmdliner_manpage.(smap_has_section sm ~sec:s_exit_status) in
  let sm = List.fold_left ins sm (cmd_docs ei) in
  let sm = List.fold_left ins sm (arg_docs ~errs ~subst ~buf ei) in
  let sm = List.fold_left ins sm (exit_docs ~errs ~subst ~buf ~has_sexit ei)in
  let sm = List.fold_left ins sm (env_docs ~errs ~subst ~buf ~has_senv ei) in
  let sm = List.fold_left ins sm (xref_docs ~errs ei) in
  sm

let text ~errs ei =
  let man = Cmdliner_info.Cmd.man @@ Cmdliner_info.Eval.cmd ei in
  let sm = Cmdliner_manpage.smap_of_blocks man in
  let sm = ensure_s_name ei sm in
  let sm = ensure_s_synopsis ei sm in
  let sm = insert_cmd_man_docs ei ~errs sm in
  Cmdliner_manpage.smap_to_blocks sm

let title ei =
  let main = Cmdliner_info.Eval.main ei in
  let exec = String.capitalize_ascii (Cmdliner_info.Cmd.name main) in
  let cmd = Cmdliner_info.Eval.cmd ei in
  let parents = Cmdliner_info.Eval.parents ei in
  let name = String.uppercase_ascii (invocation ~sep:"-" ~parents cmd) in
  let center_header = esc @@ strf "%s Manual" exec in
  let left_footer =
    let version = match Cmdliner_info.Cmd.version main with
    | None -> "" | Some v -> " " ^ v
    in
    esc @@ strf "%s%s" exec version
  in
  name, 1, "", left_footer, center_header

let man ~errs ei = title ei, text ~errs ei

let pp_man ~errs fmt ppf ei =
  Cmdliner_manpage.print
    ~errs ~subst:(cmd_info_subst ei) fmt ppf (man ~errs ei)

(* Plain synopsis for usage *)

let pp_plain_synopsis ~errs ppf ei =
  let buf = Buffer.create 100 in
  let subst = cmd_info_subst ei in
  let cmd = Cmdliner_info.Eval.cmd ei in
  let parents = Cmdliner_info.Eval.parents ei in
  let synopsis = synopsis ~parents cmd in
  let syn = Cmdliner_manpage.doc_to_plain ~errs ~subst buf synopsis in
  Format.fprintf ppf "@[%s@]" syn
