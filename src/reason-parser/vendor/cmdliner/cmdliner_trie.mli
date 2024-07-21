(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tries.

    This implementation also maps any non ambiguous prefix of a
    key to its value. *)

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val add : 'a t -> string -> 'a -> [ `New of 'a t | `Replaced of 'a * 'a t ]
val find : 'a t -> string -> [ `Ok of 'a | `Ambiguous | `Not_found ]
val ambiguities : 'a t -> string -> string list
val of_list : (string * 'a) list -> 'a t
