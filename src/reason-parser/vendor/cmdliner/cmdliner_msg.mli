(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Messages for the end-user. *)

(** {1:env_err Environment variable errors} *)

val err_env_parse : Cmdliner_info.Env.info -> err:string -> string

(** {1:pos_err Positional argument errors} *)

val err_pos_excess : string list -> string
val err_pos_misses : Cmdliner_info.Arg.t list -> string
val err_pos_parse : Cmdliner_info.Arg.t -> err:string -> string

(** {1:opt_err Optional argument errors} *)

val err_flag_value : string -> string -> string
val err_opt_value_missing : string -> string
val err_opt_parse : string -> err:string -> string
val err_opt_repeated : string -> string -> string

(** {1:arg_err Argument errors} *)

val err_arg_missing : Cmdliner_info.Arg.t -> string
val err_cmd_missing : dom:string list -> string

(** {1:msgs Other messages} *)

val pp_version : Format.formatter -> Cmdliner_info.Eval.t -> unit
val pp_try_help : Format.formatter -> Cmdliner_info.Eval.t -> unit
val pp_err : Format.formatter -> Cmdliner_info.Eval.t -> err:string -> unit
val pp_err_usage :
  Format.formatter -> Cmdliner_info.Eval.t -> err_lines:bool -> err:string -> unit

val pp_backtrace :
  Format.formatter ->
  Cmdliner_info.Eval.t -> exn -> Printexc.raw_backtrace -> unit
