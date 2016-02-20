(*---------------------------------------------------------------------------
   Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
   
   Forked from topkg, which is provided under the license below:
   
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license below

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
  val interface : string list
  (** [interface] is [[".mli"; ".cmi"; ".cmti"]] *) 

  val interface_opt : string list
  (** [interface_opt] is [".cmx" :: interface] *) 

  val library : string list 
  (** [library] is [[".cma"; ".cmxa"; ".cmxs"; ".a"]] *) 

  val module_library : string list
  (** [module_library] is [(interface_opt @ library)]. *)
end

(** Package description. *) 
module type Pkg = sig
  type builder = [ `OCamlbuild | `Other of string * string ]
  (** The type for build tools. Either [`OCamlbuild] or an 
      [`Other (tool, bdir)] tool [tool] that generates its build artefacts
      in [bdir]. *)

  type moves 
  (** The type for install moves. *) 

  type field = ?cond:bool -> ?exts:string list -> ?dst:string -> string -> moves
  (** The type for field install functions. A call
      [field cond exts dst path] generates install moves as follows:
      {ul 
      {- If [cond] is [false] (defaults to [true]), no move is generated.}
      {- If [exts] is present, generates a move for each path in 
         the list [List.map (fun e -> path ^ e) exts].}
      {- If [dst] is present this path is used as the move destination 
         (allows to install in subdirectories). If absent [dst] is 
         [Filename.basename path].} *)

  val lib : field
  val lib_exec : ?auto:bool -> field
  val bin : ?auto:bool -> field
  (** If [auto] is true (defaults to false) generates
      [path ^ ".native"] if {!Env.native} is [true] and
      [path ^ ".byte"] if {!Env.native} is [false]. *)
  val sbin : ?auto:bool -> field (** See {!bin}. *) 
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

module Topkg : sig
  val cmd : [`Build | `Explain | `Help ] 
  val env : (string * bool) list 
  val err_parse : string -> 'a
  val err_mdef : string -> 'a 
  val err_miss : string -> 'a
  val err_file : string -> string -> 'a
  val warn_unused : string -> unit
end = struct 

  (* Parses the command line. The actual cmd execution occurs in the call
     to Pkg.describe. *)

  let err fmt = 
    let k _ = exit 1 in
    Format.kfprintf k Format.err_formatter ("%s: " ^^ fmt ^^ "@.") Sys.argv.(0)

  let err_parse a = err "argument `%s' is not of the form key=(true|false)" a
  let err_mdef a = err "bool `%s' is defined more than once" a
  let err_miss a = err "argument `%s=(true|false)' is missing" a
  let err_file f e = err "%s: %s" f e
  let warn_unused k = 
    Format.eprintf "%s: warning: environment key `%s` unused.@." Sys.argv.(0) k

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
          try ignore (List.assoc key acc); err_mdef key with 
          | Not_found -> parse_env ((key, bool) :: acc) args
        with 
        | Invalid_argument _ | Not_found | Exit -> err_parse arg 
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
end = struct
  let env = ref [] 
  let get () = !env 
  let add_bool key b = env := (key, b) :: !env
  let bool key = 
    let b = try List.assoc key Topkg.env with
    | Not_found -> if Topkg.cmd = `Build then Topkg.err_miss key else true
    in
    add_bool key b; b
      
  let native = bool "native"
  let native_dynlink = bool "native-dynlink"
end

module Exts : Exts = struct
  let interface = [".mli"; ".cmi"; ".cmti"]
  let interface_opt = ".cmx" :: interface 
  let library = [".cma"; ".cmxa"; ".cmxs"; ".a"]
  let module_library = (interface_opt @ library)
end

module Pkg : Pkg = struct
  type builder = [ `OCamlbuild | `Other of string * string ]
  type moves = (string * (string * string)) list
  type field = ?cond:bool -> ?exts:string list -> ?dst:string -> string -> moves
  
  let str = Printf.sprintf
  let warn_unused () = 
    let keys = List.map fst Topkg.env in 
    let keys_used = List.map fst (Env.get ()) in 
    let unused = List.find_all (fun k -> not (List.mem k keys_used)) keys in 
    List.iter Topkg.warn_unused unused

  let has_suffix = Filename.check_suffix
  let build_strings ?(exec_sep = " ") btool bdir mvs = 
    let no_build = [ ".cmti"; ".cmt" ] in
    let install = Buffer.create 1871 in
    let exec = Buffer.create 1871 in 
    let rec add_mvs current = function 
    | (field, (src, dst)) :: mvs when field = current -> 
        if List.exists (has_suffix src) no_build then 
          Buffer.add_string install (str "\n  \"?%s/%s\" {\"%s\"}" bdir src dst)
        else begin 
          Buffer.add_string exec (str "%s%s" exec_sep src); 
          Buffer.add_string install (str "\n  \"%s/%s\" {\"%s\"}" bdir src dst);
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
  let pr_explanation btool bdir pkg mvs  =
    let env = Env.get () in 
    let install, exec = build_strings ~exec_sep:" \\\n  " btool bdir mvs in
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
    
  let build btool bdir pkg mvs =
    let install, exec = build_strings btool bdir mvs in 
    let e = Sys.command exec in 
    if e <> 0 then exit e else 
    let install_file = pkg ^ ".install" in
    try 
      let oc = open_out install_file in 
      output_string oc install; flush oc; close_out oc
    with Sys_error e -> Topkg.err_file install_file e
    
  let mvs ?(drop_exts = []) field ?(cond = true) ?(exts = []) ?dst src = 
    if not cond then [] else
    let mv src dst = (field, (src, dst)) in 
    let expand exts s d = List.map (fun e -> mv (s ^ e) (d ^ e)) exts in
    let dst = match dst with None -> Filename.basename src | Some dst -> dst in
    let files = if exts = [] then [mv src dst] else expand exts src dst in
    let keep (_, (src, _)) = not (List.exists (has_suffix src) drop_exts) in
    List.find_all keep files
    
  let lib = 
    let drop_exts = 
      if Env.native && not Env.native_dynlink then [ ".cmxs" ] else 
      if not Env.native then [ ".a"; ".cmx"; ".cmxa"; ".cmxs" ] else []
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
      
  let bin_drops = if not Env.native then [ ".native" ] else []
  let bin_mvs field ?(auto = false) ?cond ?exts ?dst src = 
    let src, dst = 
      if not auto then src, dst else 
      let dst = match dst with 
      | None -> Some (Filename.basename src)
      | Some _ as dst -> dst 
      in
      let src = if Env.native then src ^ ".native" else src ^ ".byte" in
      src, dst
    in
    mvs ~drop_exts:bin_drops field ?cond ?dst src 

  let bin = bin_mvs "bin"
  let sbin = bin_mvs "sbin"
  let lib_exec = bin_mvs "lib"

  let describe pkg ~builder mvs =
    let mvs = List.sort compare (List.flatten mvs) in
    let btool, bdir = match builder with
    | `OCamlbuild -> "ocamlbuild -j 0 -use-ocamlfind -classic-display", "_build"
    | `Other (btool, bdir) -> btool, bdir
    in
    match Topkg.cmd with 
    | `Explain -> pr_explanation btool bdir pkg mvs
    | `Help -> pr_help ()
    | `Build -> warn_unused (); build btool bdir pkg mvs
end
