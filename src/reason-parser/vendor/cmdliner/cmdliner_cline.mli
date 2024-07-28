(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command lines. *)

type t

val create :
  ?peek_opts:bool -> Cmdliner_info.Arg.Set.t -> string list ->
  (t, string * t) result

val opt_arg : t -> Cmdliner_info.Arg.t -> (int * string * (string option)) list
val pos_arg : t -> Cmdliner_info.Arg.t -> string list
val actual_args : t -> Cmdliner_info.Arg.t -> string list
(** Actual command line arguments from the command line *)

val is_opt : string -> bool
val deprecated_msgs : t -> string list
