(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Public api *)

(** Build environment access *)
module type Env = sig
  val bool : string -> bool
  (** [bool key] declares [key] as being a boolean key in the environment.
      Specifing key=(true|false) on the command line becomes mandatory. *)

  val native : bool
  (** [native] is [bool "native"]. *)

  val native_dynlink : bool
  (** [native_dylink] is [bool "native-dynlink"] *)
end

(** Exts defines sets of file extensions. *)
module type Exts = sig

  type ext = [`Ext of string | `Obj | `Lib | `Dll | `Exe]
  (** The type for extensions. *)

  val interface : ext list
  (** [interface] is [[".mli"; ".cmi"; ".cmti"]] *)

  val interface_opt : ext list
  (** [interface_opt] is [".cmx" :: interface] *)

  val c_library : ext list
  (** [c_library] is the extension for C libraries. This is determined
      from [ocamlc -config]. *)

  val c_dll_library : ext list
  (** [c_dll_library] is the extension for C dynamic libraries. This
      is determined from [ocamlc -config]. *)

  val library : ext list
  (** [library] is [[".cma"; ".cmxa"; ".cmxs"] @ c_library] *)

  val module_library : ext list
  (** [module_library] is [(interface_opt @ library)]. *)

  val exe : ext list
  (** [exe] is the extension for executables. This is determined from
      [ocamlc -config]. *)

  val exts : string list -> ext list
  (** [exts sl] is [sl] as a list of extensions. *)

  val ext : string -> ext list
  (** [ext s] is [s] as a list of extensions. *)
end

(** Package description. *)
module type Pkg = sig
  type builder =
  [ `OCamlbuild of string list
  | `OCamlbuild_no_ocamlfind of string list
  | `Other of string * string ]
  (** The type for build tools.
      {ul
      {- [`OCamlbuild args], [ocamlbuild] is invoked with `args` and
         `-use-ocamlfind`.}
      {- [`OCamlbuild_no_ocamlfind args], [ocamlbuild] is invoked with
         [args]}
      {- [`Other (tool, bdir)], tool [tool] is invoked that generates
         its build artefacts in [bdir].}} *)

  type moves
  (** The type for install moves. *)

  type field =
    ?built:bool -> ?cond:bool ->
    ?exts:[`Ext of string | `Obj | `Lib | `Dll | `Exe] list -> ?dst:string ->
    string -> moves
  (** The type for field install functions. A call
      [field cond exts dst path] generates install moves as follows:
      {ul
      {- If [built] is [true] (defaults), [path] is looked up relative
         to the build directory rather than the root directory of the
         distribution.}
      {- If [cond] is [false] (defaults to [true]), no move is generated.}
      {- If [exts] is present, generates a move for each path in
         the list [List.map (fun e -> path ^ e) exts].}
      {- If [dst] is present this path is used as the move destination
         (allows to install in subdirectories). If absent [dst] is
         [Filename.basename path].} *)

  val lib : field
  val bin : ?auto:bool -> field
  (** If [auto] is true (defaults to false) generates
      [path ^ ".native"] if {!Env.native} is [true] and
      [path ^ ".byte"] if {!Env.native} is [false]. If
      [auto] is true it also adds {!Ext.exe} to the destination. *)

  val sbin : ?auto:bool -> field (** See {!bin}. *)
  val libexec : ?auto:bool -> field (** See {!bin}. *)
  val toplevel : field
  val share : field
  val share_root : field
  val etc : field
  val doc : field
  val misc : field
  val stublibs : field
  val man : field
  val describe : string -> builder:builder -> moves list -> unit
  (** [describe name builder moves] describes a package named [name] with
      builder [builder] and install moves [moves]. *)
end

(* Implementation *)

let str = Printf.sprintf

module String = struct
  include String

  (* String.trim is 4.00.0 only. *)
  let trim s =
    let max = String.length s - 1 in
    let drop = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in
    let rec left i = if i < max && drop s.[i] then left (i + 1) else i in
    let rec right i = if i >= 0 && drop s.[i] then right (i - 1) else i in
    let left, right = left 0, right max in
    let len = right - left + 1 in
    if len < 0 then "" else String.sub s left len

  let cut ?(rev = false) ~at s =
    let find_index = if rev then String.rindex else String.index in
    match try Some (find_index s at) with Not_found -> None with
    | None -> None
    | Some i ->
        Some (String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1))
end

module OCaml_config : sig
  val read : ocamlc:string -> (string * string) list
  val ccomp : (string * string) list -> [ `Win_msvc | `Win_cc | `Other ]
end = struct

  let tmp_file () =
    let f = Filename.temp_file (Filename.basename Sys.argv.(0)) "topkg" in
    let delete () = try Sys.remove f with Sys_error _ -> () in
    at_exit delete; f

  let read ~ocamlc = try
    let tmpf = tmp_file () in
    let cmd = str "%s -config > %s" ocamlc (Filename.quote tmpf) in
    let ret = Sys.command cmd in
    if ret <> 0 then failwith (str "exec `%s' exited with %d" cmd ret) else
    let ic = open_in tmpf in
    try
      let rec loop acc = try match String.cut ~at:':' (input_line ic) with
      | None -> loop acc
      | Some (k, v) -> loop ((k, String.trim v) :: acc)
      with
      | End_of_file -> close_in ic; acc
      in
      loop []
    with exn -> (try close_in ic with _ -> ()); raise exn
  with
  | Failure s | Sys_error s ->
      Printf.eprintf "Warning: could not read OCaml configuration`: %s\n" s;
      []

  let ccomp config =
    try
      let ccomp_type = List.assoc "ccomp_type" config in
      let os_type = List.assoc "os_type" config in
      match ccomp_type, os_type with
      | "msvc", _  -> `Win_msvc
      | "cc", "Win32" -> `Win_cc
      | _, _  -> `Other
    with Not_found ->
      Printf.eprintf "Warning: could not determine the C toolchain\n";
      `Other
end

module Topkg : sig
  val cmd : [`Build | `Explain | `Help ]
  val env : (string * bool) list
  val err_parse : string -> unit
  val err_mdef : string -> unit
  val err_miss : string -> unit
  val err_file : string -> string -> unit
  val warn_unused : string -> unit
end = struct

  (* Parses the command line. The actual cmd execution occurs in the call
     to Pkg.describe. *)

  let err ?(stop = true) fmt =
    let k _ = if stop then exit 1 else () in
    Format.kfprintf k Format.err_formatter ("%s: " ^^ fmt ^^ "@.") Sys.argv.(0)

  let err_parse a = err "argument `%s' is not of the form key=(true|false)" a
  let err_mdef a = err "bool `%s' is defined more than once" a
  let err_file f e = err "%s: %s" f e
  let err_miss a = err ~stop:false "argument `%s=(true|false)' is missing" a
  let warn_unused k = err ~stop:false "warning: environment key `%s` unused" k

  let cmd, env =
    let rec parse_env acc = function                            (* not t.r. *)
    | arg :: args ->
        begin try
          (* String.cut ... *)
          let len = String.length arg in
          let eq = String.index arg '=' in
          let bool = bool_of_string (String.sub arg (eq + 1) (len - eq - 1)) in
          let key = String.sub arg 0 eq in
          if key = "" then raise Exit else
          try ignore (List.assoc key acc); err_mdef key; [] with
          | Not_found -> parse_env ((key, bool) :: acc) args
        with
        | Invalid_argument _ | Not_found | Exit -> err_parse arg; []
        end
    | [] -> acc
    in
    match List.tl (Array.to_list Sys.argv) with
    | "explain" :: args -> `Explain, parse_env [] args
    | ("help" | "-h" | "--help" | "-help") :: args -> `Help, parse_env [] args
    | args -> `Build, parse_env [] args
end

module Env : sig
  include Env
  val get : unit -> (string * bool) list
  val error : unit -> bool
end = struct
  let error = ref false
  let env = ref []
  let get () = !env
  let add_bool key b = env := (key, b) :: !env
  let bool key =
    let b = try List.assoc key Topkg.env with
    | Not_found ->
        if Topkg.cmd = `Build then (error := true; Topkg.err_miss key; true)
        else true
    in
    add_bool key b; b

  let native = bool "native"
  let native_dynlink = bool "native-dynlink"
  let error () = !error
end

module Exts (* : Exts *) = struct
  type ext = [`Ext of string | `Obj | `Lib | `Dll | `Exe]
  let interface = [`Ext ".mli"; `Ext ".cmi"; `Ext ".cmti"]
  let interface_opt = `Ext ".cmx" :: interface
  let c_library = [`Lib]
  let c_dll_library = [`Dll]
  let library = [`Ext ".cma"; `Ext ".cmxa"; `Ext ".cmxs"] @ c_library
  let module_library = (interface_opt @ library)
  let exe = [`Exe]
  let ext e = [`Ext e]
  let exts es = List.map (fun e -> `Ext e) es
  let ext_to_string =
    let r map = function
    | `Ext s -> s
    | e -> try List.assoc e map with Not_found -> assert false
    in
    function
    | `Win_msvc -> r [`Obj, ".obj"; `Lib, ".lib"; `Dll, ".dll"; `Exe, ".exe"]
    | `Win_cc ->   r [`Obj, ".o";   `Lib, ".a";   `Dll, ".dll"; `Exe, ".exe"]
    | `Other ->    r [`Obj, ".o";   `Lib, ".a";   `Dll, ".so";  `Exe, ""    ]
end

module Pkg : Pkg = struct
  type builder =
  [ `OCamlbuild of string list
  | `OCamlbuild_no_ocamlfind of string list
  | `Other of string * string ]

  type file = string * Exts.ext
  type moves = (string * (bool * file * file)) list
  type field =
    ?built:bool -> ?cond:bool -> ?exts:Exts.ext list ->
    ?dst:string -> string -> moves

  let to_file s = match String.cut ~rev:true s ~at:'.' with
  | None -> s, `Ext ""
  | Some (name, ext) -> name, `Ext (str ".%s" ext)

  let warn_unused () =
    let keys = List.map fst Topkg.env in
    let keys_used = List.map fst (Env.get ()) in
    let unused = List.find_all (fun k -> not (List.mem k keys_used)) keys in
    List.iter Topkg.warn_unused unused

  let build_strings ?(exec_sep = " ") btool bdir ext_to_string mvs =
    let no_build = [ ".cmti"; ".cmt" ] in
    let install = Buffer.create 1871 in
    let exec = Buffer.create 1871 in
    let file_to_str ?(target = false) (n, ext) =
      let ext = match ext with
      (* Work around https://github.com/ocaml/ocamlbuild/issues/6 *)
      | `Exe when target -> `Ext ""
      | _ -> ext
      in
      str "%s%s" n (ext_to_string ext)
    in
    let rec add_mvs current = function
    | (field, (built, src, dst)) :: mvs when field = current ->
        let src = file_to_str ~target:true src in
        let dst = file_to_str dst in
        let bdir = if built then str "%s/" bdir else "" in
        if List.exists (Filename.check_suffix src) no_build then
          Buffer.add_string install (str "\n  \"?%s%s\" {\"%s\"}" bdir src dst)
        else begin
          if built then Buffer.add_string exec (str "%s%s" exec_sep src);
          Buffer.add_string install (str "\n  \"%s%s\" {\"%s\"}" bdir src dst);
        end;
        add_mvs current mvs
    | (((field, _) :: _) as mvs) ->
        if current <> "" (* first *) then Buffer.add_string install " ]\n";
        Buffer.add_string install (str "%s: [" field);
        add_mvs field mvs
    | [] -> ()
    in
    Buffer.add_string exec btool;
    add_mvs "" mvs;
    Buffer.add_string install " ]\n";
    Buffer.contents install, Buffer.contents exec

  let pr = Format.printf
  let pr_explanation ccomp btool bdir pkg mvs  =
    let env = Env.get () in
    let ext_to_string = Exts.ext_to_string ccomp in
    let exec_sep = " \\\n  " in
    let install, exec = build_strings ~exec_sep btool bdir ext_to_string mvs in
    pr "@[<v>";
    pr "Package name: %s@," pkg;
    pr "Build tool: %s@," btool;
    pr "Build directory: %s@," bdir;
    pr "Environment:@, ";
    List.iter (fun (k,v) -> pr "%s=%b@, " k v) (List.sort compare env);
    pr "@,Build invocation:@,";
    pr " %s@,@," exec;
    pr "Install file:@,";
    pr "%s@," install;
    pr "@]";
    ()

  let pr_help () =
    pr "Usage example:@\n %s" Sys.argv.(0);
    List.iter (fun (k,v) -> pr " %s=%b" k v) (List.sort compare (Env.get ()));
    pr "@."

  let build ccomp btool bdir pkg mvs =
    let ext_to_string = Exts.ext_to_string ccomp in
    let install, exec = build_strings btool bdir ext_to_string mvs in
    let e = Sys.command exec in
    if e <> 0 then exit e else
    let install_file = pkg ^ ".install" in
    try
      let oc = open_out install_file in
      output_string oc install; flush oc; close_out oc
    with Sys_error e -> Topkg.err_file install_file e

  let mvs
      ?(drop_exts = []) field ?(built = true) ?(cond = true) ?(exts = [])
      ?dst src
    =
    if not cond then [] else
    let mv src dst = (field, (built, src, dst)) in
    let expand exts s d = List.map (fun e -> mv (s, e) (d, e)) exts in
    let dst = match dst with None -> Filename.basename src | Some dst -> dst in
    let files =
      if exts = [] then [mv (to_file src) (to_file dst)] else
      expand exts src dst
    in
    let has_ext (_, ext) ext' = ext = ext' in
    let keep (_, (_, src, _)) = not (List.exists (has_ext src) drop_exts) in
    List.find_all keep files

  let lib =
    let drop_exts =
      if Env.native && not Env.native_dynlink then Exts.ext ".cmxs" else
      if Env.native then [] else
      Exts.c_library @ Exts.exts [".cmx"; ".cmxa"; ".cmxs"]
    in
    mvs ~drop_exts "lib"

  let share = mvs "share"
  let share_root = mvs "share_root"
  let etc = mvs "etc"
  let toplevel = mvs "toplevel"
  let doc = mvs "doc"
  let misc = mvs "misc"
  let stublibs = mvs "stublibs"
  let man = mvs "man"

  let bin_drops = if not Env.native then Exts.ext ".native" else []
  let bin_mvs field ?(auto = false) ?built ?cond ?(exts = Exts.exe) ?dst src =
    let src, dst =
      if not auto then src, dst else
      let dst = match dst with
      | None -> Some (Filename.basename src)
      | Some _ as dst -> dst
      in
      let src = if Env.native then src ^ ".native" else src ^ ".byte" in
      src, dst
    in
    mvs ~drop_exts:bin_drops field ?built ?cond ~exts ?dst  src

  let bin = bin_mvs "bin"
  let sbin = bin_mvs "sbin"
  let libexec = bin_mvs "libexec"

  let find_ocamlc = function
  | `OCamlbuild _ -> "ocamlfind ocamlc"
  | `OCamlbuild_no_ocamlfind _ | `Other _ ->
      match try Some (Sys.getenv "HOST_XBIN") with Not_found -> None with
      | Some path -> Filename.quote (Filename.concat path "ocamlc")
      | None -> "ocamlc"

  let get_ccomp builder =
    let config = OCaml_config.read ~ocamlc:(find_ocamlc builder) in
    OCaml_config.ccomp config

  let describe pkg ~builder mvs =
    if Env.error () then (pr_help (); exit 1) else
    let mvs = List.sort compare (List.flatten mvs) in
    let btool, bdir = match builder with
    | `OCamlbuild args ->
        let args = "-use-ocamlfind" :: "-classic-display" :: args in
        str "ocamlbuild %s" (String.concat " " args), "_build"
    | `OCamlbuild_no_ocamlfind args ->
        str "ocamlbuild %s" (String.concat " " args), "_build"
    | `Other (btool, bdir) -> btool, bdir
    in
    let ccomp = get_ccomp builder in
    match Topkg.cmd with
    | `Explain -> pr_explanation ccomp btool bdir pkg mvs
    | `Help -> pr_help ()
    | `Build -> warn_unused (); build ccomp btool bdir pkg mvs
end

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
