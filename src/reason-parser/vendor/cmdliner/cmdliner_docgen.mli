(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

val pp_man :
  errs:Format.formatter -> Cmdliner_manpage.format -> Format.formatter ->
  Cmdliner_info.Eval.t -> unit

val pp_plain_synopsis :
  errs:Format.formatter -> Format.formatter -> Cmdliner_info.Eval.t -> unit
