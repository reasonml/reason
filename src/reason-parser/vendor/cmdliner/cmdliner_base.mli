(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A few helpful base definitions. *)

val uid : unit -> int
(** [uid ()] is new unique for the program run. *)

val suggest : string -> string list -> string list
(** [suggest near candidates]  suggest values from [candidates]
    not too far from [near]. *)

(** {1:fmt Formatting helpers} *)

val pp_text : Format.formatter -> string -> unit
val pp_lines : Format.formatter -> string -> unit
val pp_tokens : spaces:bool -> Format.formatter -> string -> unit

(** {1:err Error message helpers} *)

val quote : string -> string
val alts_str : ?quoted:bool -> string list -> string
val err_ambiguous : kind:string -> string -> ambs:string list -> string
val err_unknown :
  ?dom:string list -> ?hints:string list -> kind:string -> string -> string
val err_multi_def :
  kind:string -> string -> ('b -> string) -> 'b -> 'b -> string

(** {1:conv Textual OCaml value converters} *)

type 'a parser = string -> [ `Ok of 'a | `Error of string ]
type 'a printer = Format.formatter -> 'a -> unit
type 'a conv = 'a parser * 'a printer

val some : ?none:string -> 'a conv -> 'a option conv
val some' : ?none:'a -> 'a conv -> 'a option conv
val bool : bool conv
val char : char conv
val int : int conv
val nativeint : nativeint conv
val int32 : int32 conv
val int64 : int64 conv
val float : float conv
val string : string conv
val enum : (string * 'a) list -> 'a conv
val file : string conv
val dir : string conv
val non_dir_file : string conv
val list : ?sep:char -> 'a conv -> 'a list conv
val array : ?sep:char -> 'a conv -> 'a array conv
val pair : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv
val t2 : ?sep:char -> 'a conv -> 'b conv -> ('a * 'b) conv
val t3 : ?sep:char -> 'a conv ->'b conv -> 'c conv -> ('a * 'b * 'c) conv
val t4 :
  ?sep:char -> 'a conv -> 'b conv -> 'c conv -> 'd conv ->
  ('a * 'b * 'c * 'd) conv

val env_bool_parse : bool parser
