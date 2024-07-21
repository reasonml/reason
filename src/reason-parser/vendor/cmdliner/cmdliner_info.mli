(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Exit codes, environment variables, arguments, commands and eval information.

    These information types gathers untyped data used to parse command
    lines report errors and format man pages. *)

(** Exit codes. *)
module Exit : sig
  type code = int
  val ok : code
  val some_error : code
  val cli_error : code
  val internal_error : code

  type info
  val info : ?docs:string -> ?doc:string -> ?max:code -> code -> info
  val info_code : info -> code
  val info_codes : info -> code * code
  val info_doc : info -> string
  val info_docs : info  -> string
  val info_order : info -> info -> int
  val defaults : info list
end

(** Environment variables. *)
module Env : sig
  type var = string
  type info
  val info : ?deprecated:string -> ?docs:string -> ?doc:string -> var -> info
  val info_var : info -> string
  val info_doc : info -> string
  val info_docs : info -> string
  val info_deprecated : info -> string option

  module Set : Set.S with type elt = info
end

(** Arguments *)
module Arg : sig

  type absence =
  | Err  (** an error is reported. *)
  | Val of string Lazy.t (** if <> "", takes the given default value. *)
  | Doc of string
    (** if <> "", a doc string interpreted in the doc markup language. *)
  (** The type for what happens if the argument is absent from the cli. *)

  type opt_kind =
  | Flag (** without value, just a flag. *)
  | Opt  (** with required value. *)
  | Opt_vopt of string (** with optional value, takes given default. *)
  (** The type for optional argument kinds. *)

  type pos_kind
  val pos : rev:bool -> start:int -> len:int option -> pos_kind
  val pos_rev : pos_kind -> bool
  val pos_start : pos_kind -> int
  val pos_len : pos_kind -> int option

  type t
  val v :
    ?deprecated:string -> ?absent:string -> ?docs:string -> ?docv:string ->
    ?doc:string -> ?env:Env.info -> string list -> t

  val id : t -> int
  val deprecated : t -> string option
  val absent : t -> absence
  val env : t -> Env.info option
  val doc : t -> string
  val docv : t -> string
  val docs : t -> string
  val opt_names : t -> string list (* has dashes *)
  val opt_name_sample : t -> string (* warning must be an opt arg *)
  val opt_kind : t -> opt_kind
  val pos_kind : t -> pos_kind

  val make_req : t -> t
  val make_all_opts : t -> t
  val make_opt : absent:absence -> kind:opt_kind -> t -> t
  val make_opt_all : absent:absence -> kind:opt_kind -> t -> t
  val make_pos : pos:pos_kind -> t -> t
  val make_pos_abs : absent:absence -> pos:pos_kind -> t -> t

  val is_opt : t -> bool
  val is_pos : t -> bool
  val is_req : t -> bool

  val pos_cli_order : t -> t -> int
  val rev_pos_cli_order : t -> t -> int

  val compare : t -> t -> int
  module Set : Set.S with type elt = t
end

(** Commands. *)
module Cmd : sig
  type t
  val v :
    ?deprecated:string ->
    ?man_xrefs:Cmdliner_manpage.xref list -> ?man:Cmdliner_manpage.block list ->
    ?envs:Env.info list -> ?exits:Exit.info list ->
    ?sdocs:string -> ?docs:string -> ?doc:string -> ?version:string ->
    string -> t

  val name : t -> string
  val version : t -> string option
  val deprecated : t -> string option
  val doc : t -> string
  val docs : t -> string
  val stdopts_docs : t -> string
  val exits : t -> Exit.info list
  val envs : t -> Env.info list
  val man : t -> Cmdliner_manpage.block list
  val man_xrefs : t -> Cmdliner_manpage.xref list
  val args : t -> Arg.Set.t
  val has_args : t -> bool
  val children : t -> t list
  val add_args : t -> Arg.Set.t -> t
  val with_children : t -> args:Arg.Set.t option -> children:t list -> t
end

(** Evaluation. *)
module Eval : sig
  type t
  val v :
    cmd:Cmd.t -> parents:Cmd.t list -> env:(string -> string option) ->
    err_ppf:Format.formatter -> t

  val cmd : t -> Cmd.t
  val main : t -> Cmd.t
  val parents : t -> Cmd.t list
  val env_var : t -> string -> string option
  val err_ppf : t -> Format.formatter
  val with_cmd : t -> Cmd.t -> t
end
