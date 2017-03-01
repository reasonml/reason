(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(***********************************************************************)
(**                                                                   **)
(**               WARNING WARNING WARNING                             **)
(**                                                                   **)
(** When you change this file, you must make the parallel change      **)
(** in config.mlbuild                                                 **)
(**                                                                   **)
(***********************************************************************)


(* The main OCaml version string has moved to ../VERSION *)
let version = "4.02.3+BS"

let standard_library_default = "/usr/local/lib/ocaml/lib/ocaml" (* does not matter *)

let standard_library =
    standard_library_default

let standard_runtime = "/usr/local/bin/ocaml/bin/ocamlrun"
let ccomp_type = "cc"
let bytecomp_c_compiler = "gcc -O  -Wall -D_FILE_OFFSET_BITS=64 -D_REENTRANT -O "
let bytecomp_c_libraries = "-lcurses -lpthread"
let native_c_compiler = "gcc -O  -D_FILE_OFFSET_BITS=64 -D_REENTRANT"
let native_c_libraries = ""
let native_pack_linker = "ld -r -arch x86_64  -o "
let ranlib = "ranlib"
let ar = "ar"
let cc_profile = "-pg"
let mkdll = "gcc -bundle -flat_namespace -undefined suppress -Wl,-no_compact_unwind"
let mkexe = "gcc -Wl,-no_compact_unwind"
let mkmaindll = "gcc -bundle -flat_namespace -undefined suppress -Wl,-no_compact_unwind"

let exec_magic_number = "Caml1999X011"
and cmi_magic_number = "Caml1999I017"
and cmo_magic_number = "Caml1999O010"
and cma_magic_number = "Caml1999A011"
and cmx_magic_number = "Caml1999Y014"
and cmxa_magic_number = "Caml1999Z013"
and ast_impl_magic_number = "Caml1999M016"
and ast_intf_magic_number = "Caml1999N015"
and cmxs_magic_number = "Caml2007D002"
and cmt_magic_number = "Caml2012T004"

let load_path = ref ([] : string list)

let interface_suffix = ref ".mli"

let max_tag = 245
(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)
let lazy_tag = 246

let max_young_wosize = 256
let stack_threshold = 256 (* see byterun/config.h *)

let architecture = "amd64"
let model = "default"
let system = "macosx"

let asm = "clang -arch x86_64 -c"
let asm_cfi_supported = true
let with_frame_pointers = false

let ext_obj = ".o"
let ext_asm = ".s"
let ext_lib = ".a"
let ext_dll = ".so"

let host = "x86_64-apple-darwin15.6.0"
let target = "x86_64-apple-darwin15.6.0"

let default_executable_name =
  match Sys.os_type with
    "Unix" -> "a.out"
  | "Win32" | "Cygwin" -> "camlprog.exe"
  | _ -> "camlprog"

let systhread_supported = true;;

let print_config oc = ()
;;
