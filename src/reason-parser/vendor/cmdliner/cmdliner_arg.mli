(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command line arguments as terms. *)

type 'a parser = string -> [ `Ok of 'a | `Error of string ]
type 'a printer = Format.formatter -> 'a -> unit
type 'a conv = 'a parser * 'a printer
type 'a converter = 'a conv

val conv :
  ?docv:string -> (string -> ('a, [`Msg of string]) result) * 'a printer ->
  'a conv

val conv' :
  ?docv:string -> (string -> ('a, string) result) * 'a printer -> 'a conv

val pconv : ?docv:string -> 'a parser * 'a printer -> 'a conv
val conv_parser : 'a conv -> (string -> ('a, [`Msg of string]) result)
val conv_printer : 'a conv -> 'a printer
val conv_docv : 'a conv -> string

val parser_of_kind_of_string :
  kind:string -> (string -> 'a option) ->
  (string -> ('a, [`Msg of string]) result)

val some : ?none:string -> 'a converter -> 'a option converter
val some' : ?none:'a -> 'a converter -> 'a option converter

type env = Cmdliner_info.Env.info
val env_var : ?deprecated:string -> ?docs:string -> ?doc:string -> string -> env

type 'a t = 'a Cmdliner_term.t

type info
val info :
  ?deprecated:string -> ?absent:string -> ?docs:string -> ?docv:string ->
  ?doc:string -> ?env:env -> string list -> info

val ( & ) : ('a -> 'b) -> 'a -> 'b

val flag : info -> bool t
val flag_all : info -> bool list t
val vflag : 'a -> ('a * info) list -> 'a t
val vflag_all : 'a list -> ('a * info) list -> 'a list t
val opt : ?vopt:'a -> 'a converter -> 'a -> info -> 'a t
val opt_all : ?vopt:'a -> 'a converter -> 'a list -> info -> 'a list t

val pos : ?rev:bool -> int -> 'a converter -> 'a -> info -> 'a t
val pos_all : 'a converter -> 'a list -> info -> 'a list t
val pos_left : ?rev:bool -> int -> 'a converter -> 'a list -> info -> 'a list t
val pos_right : ?rev:bool -> int -> 'a converter -> 'a list -> info -> 'a list t

(** {1 As terms} *)

val value : 'a t -> 'a Cmdliner_term.t
val required : 'a option t -> 'a Cmdliner_term.t
val non_empty : 'a list t -> 'a list Cmdliner_term.t
val last : 'a list t -> 'a Cmdliner_term.t

(** {1 Predefined arguments} *)

val man_format : Cmdliner_manpage.format Cmdliner_term.t
val stdopt_version : docs:string -> bool Cmdliner_term.t
val stdopt_help : docs:string -> Cmdliner_manpage.format option Cmdliner_term.t

(** {1 Converters} *)

val bool : bool converter
val char : char converter
val int : int converter
val nativeint : nativeint converter
val int32 : int32 converter
val int64 : int64 converter
val float : float converter
val string : string converter
val enum : (string * 'a) list -> 'a converter
val file : string converter
val dir : string converter
val non_dir_file : string converter
val list : ?sep:char -> 'a converter -> 'a list converter
val array : ?sep:char -> 'a converter -> 'a array converter
val pair : ?sep:char -> 'a converter -> 'b converter -> ('a * 'b) converter
val t2 : ?sep:char -> 'a converter -> 'b converter -> ('a * 'b) converter

val t3 :
  ?sep:char -> 'a converter ->'b converter -> 'c converter ->
  ('a * 'b * 'c) converter

val t4 :
  ?sep:char -> 'a converter ->'b converter -> 'c converter -> 'd converter ->
  ('a * 'b * 'c * 'd) converter

val doc_quote : string -> string
val doc_alts : ?quoted:bool -> string list -> string
val doc_alts_enum : ?quoted:bool -> (string * 'a) list -> string
