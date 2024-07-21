(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command evaluation *)

(** {1:eval Evaluating commands} *)

type 'a eval_ok = [ `Ok of 'a | `Version | `Help ]
type eval_error = [ `Parse | `Term | `Exn ]
type 'a eval_exit = [ `Ok of 'a  | `Exit of Cmdliner_info.Exit.code ]

val eval_value :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array -> 'a Cmdliner_cmd.t ->
  ('a eval_ok, eval_error) result

val eval_value' :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:int -> 'a Cmdliner_cmd.t -> 'a eval_exit

val eval_peek_opts :
  ?version_opt:bool -> ?env:(string -> string option) ->
  ?argv:string array -> 'a Cmdliner_term.t ->
  'a option * ('a eval_ok, eval_error) result

val eval :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:int -> unit Cmdliner_cmd.t -> Cmdliner_info.Exit.code

val eval' :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:int -> int Cmdliner_cmd.t -> Cmdliner_info.Exit.code

val eval_result :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:Cmdliner_info.Exit.code -> (unit, string) result Cmdliner_cmd.t ->
  Cmdliner_info.Exit.code

val eval_result' :
  ?help:Format.formatter -> ?err:Format.formatter -> ?catch:bool ->
  ?env:(string -> string option) -> ?argv:string array ->
  ?term_err:Cmdliner_info.Exit.code ->
  (Cmdliner_info.Exit.code, string) result Cmdliner_cmd.t ->
  Cmdliner_info.Exit.code
