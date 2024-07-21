(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Commands and their information. *)

type info = Cmdliner_info.Cmd.t

val info :
  ?deprecated:string ->
  ?man_xrefs:Cmdliner_manpage.xref list -> ?man:Cmdliner_manpage.block list ->
  ?envs:Cmdliner_info.Env.info list -> ?exits:Cmdliner_info.Exit.info list ->
  ?sdocs:string -> ?docs:string -> ?doc:string -> ?version:string ->
  string -> info

type 'a t =
| Cmd of info * 'a Cmdliner_term.parser
| Group of info * ('a Cmdliner_term.parser option * 'a t list)

val v : info -> 'a Cmdliner_term.t -> 'a t
val group : ?default:'a Cmdliner_term.t -> info -> 'a t list -> 'a t
val name : 'a t -> string
val get_info : 'a t -> info
