(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Exit codes *)

module Exit = struct
  type code = int

  let ok = 0
  let some_error = 123
  let cli_error = 124
  let internal_error = 125

  type info =
    { codes : code * code; (* min, max *)
      doc : string; (* help. *)
      docs : string; } (* title of help section where listed. *)

  let info
      ?(docs = Cmdliner_manpage.s_exit_status) ?(doc = "undocumented") ?max min
    =
    let max = match max with None -> min | Some max -> max in
    { codes = (min, max); doc; docs }

  let info_codes i = i.codes
  let info_code i = fst i.codes
  let info_doc i = i.doc
  let info_docs i = i.docs
  let info_order i0 i1 = compare i0.codes i1.codes
  let defaults =
    [ info ok ~doc:"on success.";
      info some_error
        ~doc:"on indiscriminate errors reported on standard error.";
      info cli_error ~doc:"on command line parsing errors.";
      info internal_error ~doc:"on unexpected internal errors (bugs)."; ]
end

(* Environment variables *)

module Env = struct
  type var = string
  type info = (* information about an environment variable. *)
    { id : int; (* unique id for the env var. *)
      deprecated : string option;
      var : string; (* the variable. *)
      doc : string; (* help. *)
      docs : string; } (* title of help section where listed. *)

  let info
      ?deprecated
      ?(docs = Cmdliner_manpage.s_environment) ?(doc = "See option $(opt).") var
    =
    { id = Cmdliner_base.uid (); deprecated; var; doc; docs }

  let info_deprecated i = i.deprecated
  let info_var i = i.var
  let info_doc i = i.doc
  let info_docs i = i.docs
  let info_compare i0 i1 = Int.compare i0.id i1.id

  module Set = Set.Make (struct type t = info let compare = info_compare end)
end

(* Arguments *)

module Arg = struct
  type absence = Err | Val of string Lazy.t | Doc of string
  type opt_kind = Flag | Opt | Opt_vopt of string

  type pos_kind = (* information about a positional argument. *)
    { pos_rev : bool; (* if [true] positions are counted from the end. *)
      pos_start : int; (* start positional argument. *)
      pos_len : int option } (* number of arguments or [None] if unbounded. *)

  let pos ~rev:pos_rev ~start:pos_start ~len:pos_len =
    { pos_rev; pos_start; pos_len}

  let pos_rev p = p.pos_rev
  let pos_start p = p.pos_start
  let pos_len p = p.pos_len

  type t = (* information about a command line argument. *)
    { id : int; (* unique id for the argument. *)
      deprecated : string option; (* deprecation message *)
      absent : absence; (* behaviour if absent. *)
      env : Env.info option; (* environment variable for default value. *)
      doc : string; (* help. *)
      docv : string; (* variable name for the argument in help. *)
      docs : string; (* title of help section where listed. *)
      pos : pos_kind; (* positional arg kind. *)
      opt_kind : opt_kind; (* optional arg kind. *)
      opt_names : string list; (* names (for opt args). *)
      opt_all : bool; } (* repeatable (for opt args). *)

  let dumb_pos = pos ~rev:false ~start:(-1) ~len:None

  let v ?deprecated ?(absent = "") ?docs ?(docv = "") ?(doc = "") ?env names =
    let dash n = if String.length n = 1 then "-" ^ n else "--" ^ n in
    let opt_names = List.map dash names in
    let docs = match docs with
    | Some s -> s
    | None ->
        match names with
        | [] -> Cmdliner_manpage.s_arguments
        | _ -> Cmdliner_manpage.s_options
    in
    { id = Cmdliner_base.uid (); deprecated; absent = Doc absent;
      env; doc; docv; docs; pos = dumb_pos; opt_kind = Flag; opt_names;
      opt_all = false; }

  let id a = a.id
  let deprecated a = a.deprecated
  let absent a = a.absent
  let env a = a.env
  let doc a = a.doc
  let docv a = a.docv
  let docs a = a.docs
  let pos_kind a = a.pos
  let opt_kind a = a.opt_kind
  let opt_names a = a.opt_names
  let opt_all a = a.opt_all
  let opt_name_sample a =
    (* First long or short name (in that order) in the list; this
       allows the client to control which name is shown *)
    let rec find = function
    | [] -> List.hd a.opt_names
    | n :: ns -> if (String.length n) > 2 then n else find ns
    in
    find a.opt_names

  let make_req a = { a with absent = Err }
  let make_all_opts a = { a with opt_all = true }
  let make_opt ~absent ~kind:opt_kind a = { a with absent; opt_kind }
  let make_opt_all ~absent ~kind:opt_kind a =
    { a with absent; opt_kind; opt_all = true  }

  let make_pos ~pos a = { a with pos }
  let make_pos_abs ~absent ~pos a = { a with absent; pos }

  let is_opt a = a.opt_names <> []
  let is_pos a = a.opt_names = []
  let is_req a = a.absent = Err

  let pos_cli_order a0 a1 = (* best-effort order on the cli. *)
    let c = compare (a0.pos.pos_rev) (a1.pos.pos_rev) in
    if c <> 0 then c else
    if a0.pos.pos_rev
    then compare a1.pos.pos_start a0.pos.pos_start
    else compare a0.pos.pos_start a1.pos.pos_start

  let rev_pos_cli_order a0 a1 = pos_cli_order a1 a0

  let compare a0 a1 = Int.compare a0.id a1.id
  module Set = Set.Make (struct type nonrec t = t let compare = compare end)
end

(* Commands *)

module Cmd = struct
  type t =
    { name : string; (* name of the cmd. *)
      version : string option; (* version (for --version). *)
      deprecated : string option; (* deprecation message *)
      doc : string; (* one line description of cmd. *)
      docs : string; (* title of man section where listed (commands). *)
      sdocs : string; (* standard options, title of section where listed. *)
      exits : Exit.info list; (* exit codes for the cmd. *)
      envs : Env.info list; (* env vars that influence the cmd. *)
      man : Cmdliner_manpage.block list; (* man page text. *)
      man_xrefs : Cmdliner_manpage.xref list; (* man cross-refs. *)
      args : Arg.Set.t; (* Command arguments. *)
      has_args : bool; (* [true] if has own parsing term. *)
      children : t list; } (* Children, if any. *)

  let v
      ?deprecated ?(man_xrefs = [`Main]) ?(man = []) ?(envs = [])
      ?(exits = Exit.defaults) ?(sdocs = Cmdliner_manpage.s_common_options)
      ?(docs = Cmdliner_manpage.s_commands) ?(doc = "") ?version name
    =
    { name; version; deprecated; doc; docs; sdocs; exits;
      envs; man; man_xrefs; args = Arg.Set.empty;
      has_args = true; children = [] }

  let name t = t.name
  let version t = t.version
  let deprecated t = t.deprecated
  let doc t = t.doc
  let docs t = t.docs
  let stdopts_docs t = t.sdocs
  let exits t = t.exits
  let envs t = t.envs
  let man t = t.man
  let man_xrefs t = t.man_xrefs
  let args t = t.args
  let has_args t = t.has_args
  let children t = t.children
  let add_args t args = { t with args = Arg.Set.union args t.args }
  let with_children cmd ~args ~children =
    let has_args, args = match args with
    | None -> false, cmd.args
    | Some args -> true, Arg.Set.union args cmd.args
    in
    { cmd with has_args; args; children }
end

(* Evaluation *)

module Eval = struct
  type t = (* information about the evaluation context. *)
    { cmd : Cmd.t; (* cmd being evaluated. *)
      parents : Cmd.t list; (* parents of cmd, root is last. *)
      env : string -> string option; (* environment variable lookup. *)
      err_ppf : Format.formatter (* error formatter *) }

  let v ~cmd ~parents ~env ~err_ppf = { cmd; parents; env; err_ppf }

  let cmd e = e.cmd
  let parents e = e.parents
  let env_var e v = e.env v
  let err_ppf e = e.err_ppf
  let main e = match List.rev e.parents with [] -> e.cmd | m :: _ -> m
  let with_cmd ei cmd = { ei with cmd }
end
