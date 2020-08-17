open Migrate_parsetree_versions

(** {1 State a rewriter can access} *)

type extra = ..

type config = {
  tool_name       : string;
  include_dirs    : string list;
  load_path       : string list;
  debug           : bool;
  for_package     : string option;
  (** Additional parameters that can be passed by a caller of
      [rewrite_{signature,structure}] to a specific register rewriter. *)
  extras          : extra list;
}

val make_config
  :  tool_name:string
  -> ?include_dirs:string list
  -> ?load_path:string list
  -> ?debug:bool
  -> ?for_package:string
  -> ?extras:extra list
  -> unit
  -> config

type cookies

val get_cookie
  : cookies
  -> string
  -> 'types ocaml_version -> 'types get_expression option

val set_cookie
  : cookies
  -> string
  -> 'types ocaml_version -> 'types get_expression
  -> unit

val set_global_cookie
  :  string
  -> 'types ocaml_version -> 'types get_expression
  -> unit

(** {1 Registering rewriters} *)

type 'types rewriter = config -> cookies -> 'types get_mapper

(** Register a ppx rewriter. [position] is a integer that indicates
    when the ppx rewriter should be applied. It is guaranteed that if
    two ppx rewriters [a] and [b] have different position numbers, then
    the one with the lowest number will be applied first. The rewriting
    order of ppx rewriters with the same position number is not
    specified. The default position is [0].

    Note that more different position numbers means more AST
    conversions and slower rewriting, so think twice before setting
    [position] to a non-zero number.
*)
val register
  :  name:string
  -> ?reset_args:(unit -> unit) -> ?args:(Arg.key * Arg.spec * Arg.doc) list
  -> ?position:int
  -> 'types ocaml_version -> 'types rewriter
  -> unit

(** Return the list of command line arguments registered by rewriters *)
val registered_args : unit -> (Arg.key * Arg.spec * Arg.doc) list

(** Call all the registered [reset_args] callbacks *)
val reset_args : unit -> unit

(** {1 Running registered rewriters} *)

val run_as_ast_mapper : ?exit_on_error:bool -> string list -> Ast_mapper.mapper

val run_as_ppx_rewriter :
  ?exit_on_error:bool -> ?argv:string array -> unit -> unit

val run_main : ?exit_on_error:bool -> ?argv:string array -> unit -> unit

(** {1 Manual mapping} *)

type some_signature =
  | Sig : (module Migrate_parsetree_versions.OCaml_version with
            type Ast.Parsetree.signature = 'concrete) * 'concrete -> some_signature

type some_structure =
  | Str : (module Migrate_parsetree_versions.OCaml_version with
            type Ast.Parsetree.structure = 'concrete) * 'concrete -> some_structure

val migrate_some_signature
  :  'version ocaml_version
  -> some_signature
  -> 'version get_signature

val migrate_some_structure
  :  'version ocaml_version
  -> some_structure
  -> 'version get_structure

val rewrite_signature
  :  config
  -> 'version ocaml_version
  -> 'version get_signature
  -> some_signature

val rewrite_structure
  :  config
  -> 'version ocaml_version
  -> 'version get_structure
  -> some_structure
