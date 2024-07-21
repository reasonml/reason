(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terms *)

type term_escape =
  [ `Error of bool * string
  | `Help of Cmdliner_manpage.format * string option ]

type 'a parser =
  Cmdliner_info.Eval.t -> Cmdliner_cline.t ->
  ('a, [ `Parse of string | term_escape ]) result
(** Type type for command line parser. given static information about
    the command line and a command line to parse returns an OCaml value. *)

type 'a t = Cmdliner_info.Arg.Set.t * 'a parser
(** The type for terms. The list of arguments it can parse and the parsing
    function that does so. *)

val const : 'a -> 'a t
val app : ('a -> 'b) t -> 'a t -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
val product : 'a t -> 'b t  -> ('a * 'b) t

module Syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t

type 'a ret = [ `Ok of 'a | term_escape ]

val ret : 'a ret t -> 'a t
val term_result : ?usage:bool -> ('a, [`Msg of string]) result t -> 'a t
val term_result' : ?usage:bool -> ('a, string) result t -> 'a t
val cli_parse_result : ('a, [`Msg of string]) result t -> 'a t
val cli_parse_result' : ('a, string) result t -> 'a t
val main_name : string t
val choice_names : string list t
val with_used_args : 'a t -> ('a * string list) t
