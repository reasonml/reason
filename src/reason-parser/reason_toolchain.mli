(***********************************************************************)
(*                                                                     *)
(*                                Reason                               *)
(*                                                                     *)
(***********************************************************************)
(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* open Ppxlib *)

module ML : Reason_toolchain_conf.Toolchain
module RE : Reason_toolchain_conf.Toolchain
module From_current = Reason_toolchain_conf.From_current
module To_current = Reason_toolchain_conf.To_current

val setup_lexbuf : use_stdin:bool -> string -> Lexing.lexbuf
