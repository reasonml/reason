(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                   Jérémie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* BEGIN of BLACK MAGIC *)
(*$ #use "src/cinaps_helpers" $*)

type _ witnesses = ..

type _ migration = ..
type _ migration += Undefined : _ migration

type 'a migration_info = {
  mutable next_version : 'a migration;
  mutable previous_version : 'a migration;
}

(** Abstract view of a version of an OCaml Ast *)
module type Ast = sig
  (*$ foreach_module (fun m types ->
      printf "module %s : sig\n" m;
      List.iter types ~f:(printf "type %s\n");
      printf "end\n"
    )
  *)
  module Parsetree : sig
    type structure
    type signature
    type toplevel_phrase
    type core_type
    type expression
    type pattern
    type case
    type type_declaration
    type type_extension
    type extension_constructor
  end
  module Outcometree : sig
    type out_value
    type out_type
    type out_class_type
    type out_module_type
    type out_sig_item
    type out_type_extension
    type out_phrase
  end
  module Ast_mapper : sig
    type mapper
  end
  (*$*)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
  val shallow_identity : Ast_mapper.mapper
  val map_signature : Ast_mapper.mapper -> Parsetree.signature -> Parsetree.signature
  val map_structure : Ast_mapper.mapper -> Parsetree.structure -> Parsetree.structure
  val make_top_mapper
    :  signature:(Parsetree.signature -> Parsetree.signature)
    -> structure:(Parsetree.structure -> Parsetree.structure)
    -> Ast_mapper.mapper
end

(* Shortcuts for talking about ast types outside of the module language *)

type 'a _types = 'a constraint 'a
  = <
    (*$ foreach_type (fun _ s -> printf "%-21s : _;\n" s) *)
    structure             : _;
    signature             : _;
    toplevel_phrase       : _;
    core_type             : _;
    expression            : _;
    pattern               : _;
    case                  : _;
    type_declaration      : _;
    type_extension        : _;
    extension_constructor : _;
    out_value             : _;
    out_type              : _;
    out_class_type        : _;
    out_module_type       : _;
    out_sig_item          : _;
    out_type_extension    : _;
    out_phrase            : _;
    mapper                : _;
    (*$*)
  >
;;

(*$ foreach_type (fun _ s ->
    printf "type 'a get_%s =\n" s;
    printf " 'x constraint 'a _types = < %s : 'x; .. >\n" s
  ) *)
type 'a get_structure =
  'x constraint 'a _types = < structure : 'x; .. >
type 'a get_signature =
  'x constraint 'a _types = < signature : 'x; .. >
type 'a get_toplevel_phrase =
  'x constraint 'a _types = < toplevel_phrase : 'x; .. >
type 'a get_core_type =
  'x constraint 'a _types = < core_type : 'x; .. >
type 'a get_expression =
  'x constraint 'a _types = < expression : 'x; .. >
type 'a get_pattern =
  'x constraint 'a _types = < pattern : 'x; .. >
type 'a get_case =
  'x constraint 'a _types = < case : 'x; .. >
type 'a get_type_declaration =
  'x constraint 'a _types = < type_declaration : 'x; .. >
type 'a get_type_extension =
  'x constraint 'a _types = < type_extension : 'x; .. >
type 'a get_extension_constructor =
  'x constraint 'a _types = < extension_constructor : 'x; .. >
type 'a get_out_value =
  'x constraint 'a _types = < out_value : 'x; .. >
type 'a get_out_type =
  'x constraint 'a _types = < out_type : 'x; .. >
type 'a get_out_class_type =
  'x constraint 'a _types = < out_class_type : 'x; .. >
type 'a get_out_module_type =
  'x constraint 'a _types = < out_module_type : 'x; .. >
type 'a get_out_sig_item =
  'x constraint 'a _types = < out_sig_item : 'x; .. >
type 'a get_out_type_extension =
  'x constraint 'a _types = < out_type_extension : 'x; .. >
type 'a get_out_phrase =
  'x constraint 'a _types = < out_phrase : 'x; .. >
type 'a get_mapper =
  'x constraint 'a _types = < mapper : 'x; .. >
(*$*)

module type OCaml_version = sig
  module Ast : Ast
  val version : int
  val string_version : string
  type types = <
    (*$ foreach_type (fun m s -> printf "%-21s : Ast.%s.%s;\n" s m s)*)
    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
    out_value             : Ast.Outcometree.out_value;
    out_type              : Ast.Outcometree.out_type;
    out_class_type        : Ast.Outcometree.out_class_type;
    out_module_type       : Ast.Outcometree.out_module_type;
    out_sig_item          : Ast.Outcometree.out_sig_item;
    out_type_extension    : Ast.Outcometree.out_type_extension;
    out_phrase            : Ast.Outcometree.out_phrase;
    mapper                : Ast.Ast_mapper.mapper;
    (*$*)
  > _types
  type _ witnesses += Version : types witnesses
  val migration_info : types migration_info
end

module Make_witness(Ast : Ast) =
struct
  type types = <
    (*$ foreach_type (fun m s -> printf "%-21s : Ast.%s.%s;\n" s m s)*)
    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
    out_value             : Ast.Outcometree.out_value;
    out_type              : Ast.Outcometree.out_type;
    out_class_type        : Ast.Outcometree.out_class_type;
    out_module_type       : Ast.Outcometree.out_module_type;
    out_sig_item          : Ast.Outcometree.out_sig_item;
    out_type_extension    : Ast.Outcometree.out_type_extension;
    out_phrase            : Ast.Outcometree.out_phrase;
    mapper                : Ast.Ast_mapper.mapper;
    (*$*)
  > _types
  type _ witnesses += Version : types witnesses
  let migration_info : types migration_info =
    { next_version = Undefined; previous_version = Undefined }
end

type 'types ocaml_version =
  (module OCaml_version
    (*$ let sep = with_then_and () in
      foreach_type (fun m s ->
          printf "%t type Ast.%s.%s = 'types get_%s\n" sep m s s) *)
    with type Ast.Parsetree.structure = 'types get_structure
     and type Ast.Parsetree.signature = 'types get_signature
     and type Ast.Parsetree.toplevel_phrase = 'types get_toplevel_phrase
     and type Ast.Parsetree.core_type = 'types get_core_type
     and type Ast.Parsetree.expression = 'types get_expression
     and type Ast.Parsetree.pattern = 'types get_pattern
     and type Ast.Parsetree.case = 'types get_case
     and type Ast.Parsetree.type_declaration = 'types get_type_declaration
     and type Ast.Parsetree.type_extension = 'types get_type_extension
     and type Ast.Parsetree.extension_constructor = 'types get_extension_constructor
     and type Ast.Outcometree.out_value = 'types get_out_value
     and type Ast.Outcometree.out_type = 'types get_out_type
     and type Ast.Outcometree.out_class_type = 'types get_out_class_type
     and type Ast.Outcometree.out_module_type = 'types get_out_module_type
     and type Ast.Outcometree.out_sig_item = 'types get_out_sig_item
     and type Ast.Outcometree.out_type_extension = 'types get_out_type_extension
     and type Ast.Outcometree.out_phrase = 'types get_out_phrase
     and type Ast.Ast_mapper.mapper = 'types get_mapper
     (*$*)
  )

type ('a, 'b) type_comparison =
  | Lt : ('a, 'b) type_comparison
  | Eq : ('a, 'a) type_comparison
  | Gt : ('a, 'b) type_comparison

let compare_ocaml_version
    (*$ foreach_type (fun _ s -> printf "(type %s1) (type %s2)\n" s s) *)
    (type structure1) (type structure2)
    (type signature1) (type signature2)
    (type toplevel_phrase1) (type toplevel_phrase2)
    (type core_type1) (type core_type2)
    (type expression1) (type expression2)
    (type pattern1) (type pattern2)
    (type case1) (type case2)
    (type type_declaration1) (type type_declaration2)
    (type type_extension1) (type type_extension2)
    (type extension_constructor1) (type extension_constructor2)
    (type out_value1) (type out_value2)
    (type out_type1) (type out_type2)
    (type out_class_type1) (type out_class_type2)
    (type out_module_type1) (type out_module_type2)
    (type out_sig_item1) (type out_sig_item2)
    (type out_type_extension1) (type out_type_extension2)
    (type out_phrase1) (type out_phrase2)
    (type mapper1) (type mapper2)
    (*$*)
    ((module A) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s1;\n" s s) *)
     structure             : structure1;
     signature             : signature1;
     toplevel_phrase       : toplevel_phrase1;
     core_type             : core_type1;
     expression            : expression1;
     pattern               : pattern1;
     case                  : case1;
     type_declaration      : type_declaration1;
     type_extension        : type_extension1;
     extension_constructor : extension_constructor1;
     out_value             : out_value1;
     out_type              : out_type1;
     out_class_type        : out_class_type1;
     out_module_type       : out_module_type1;
     out_sig_item          : out_sig_item1;
     out_type_extension    : out_type_extension1;
     out_phrase            : out_phrase1;
     mapper                : mapper1;
     (*$*)
     > ocaml_version)
    ((module B) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s2;\n" s s) *)
     structure             : structure2;
     signature             : signature2;
     toplevel_phrase       : toplevel_phrase2;
     core_type             : core_type2;
     expression            : expression2;
     pattern               : pattern2;
     case                  : case2;
     type_declaration      : type_declaration2;
     type_extension        : type_extension2;
     extension_constructor : extension_constructor2;
     out_value             : out_value2;
     out_type              : out_type2;
     out_class_type        : out_class_type2;
     out_module_type       : out_module_type2;
     out_sig_item          : out_sig_item2;
     out_type_extension    : out_type_extension2;
     out_phrase            : out_phrase2;
     mapper                : mapper2;
     (*$*)
     > ocaml_version)
  : (A.types, B.types) type_comparison
  =
  match A.Version with
  | B.Version -> Eq
  | _ when A.version < B.version -> Lt
  | _ when A.version > B.version -> Gt
  | _ -> assert false

type ('from, 'to_) migration_functions = {
  (*$ foreach_type (fun _ s ->
      printf "copy_%s: 'from get_%s -> 'to_ get_%s;\n" s s s) *)
  copy_structure: 'from get_structure -> 'to_ get_structure;
  copy_signature: 'from get_signature -> 'to_ get_signature;
  copy_toplevel_phrase: 'from get_toplevel_phrase -> 'to_ get_toplevel_phrase;
  copy_core_type: 'from get_core_type -> 'to_ get_core_type;
  copy_expression: 'from get_expression -> 'to_ get_expression;
  copy_pattern: 'from get_pattern -> 'to_ get_pattern;
  copy_case: 'from get_case -> 'to_ get_case;
  copy_type_declaration: 'from get_type_declaration -> 'to_ get_type_declaration;
  copy_type_extension: 'from get_type_extension -> 'to_ get_type_extension;
  copy_extension_constructor: 'from get_extension_constructor -> 'to_ get_extension_constructor;
  copy_out_value: 'from get_out_value -> 'to_ get_out_value;
  copy_out_type: 'from get_out_type -> 'to_ get_out_type;
  copy_out_class_type: 'from get_out_class_type -> 'to_ get_out_class_type;
  copy_out_module_type: 'from get_out_module_type -> 'to_ get_out_module_type;
  copy_out_sig_item: 'from get_out_sig_item -> 'to_ get_out_sig_item;
  copy_out_type_extension: 'from get_out_type_extension -> 'to_ get_out_type_extension;
  copy_out_phrase: 'from get_out_phrase -> 'to_ get_out_phrase;
  copy_mapper: 'from get_mapper -> 'to_ get_mapper;
  (*$*)
}

let id x = x
let migration_identity : ('a, 'a) migration_functions = {
  (*$ foreach_type (fun _ s -> printf "copy_%s = id;\n" s) *)
  copy_structure = id;
  copy_signature = id;
  copy_toplevel_phrase = id;
  copy_core_type = id;
  copy_expression = id;
  copy_pattern = id;
  copy_case = id;
  copy_type_declaration = id;
  copy_type_extension = id;
  copy_extension_constructor = id;
  copy_out_value = id;
  copy_out_type = id;
  copy_out_class_type = id;
  copy_out_module_type = id;
  copy_out_sig_item = id;
  copy_out_type_extension = id;
  copy_out_phrase = id;
  copy_mapper = id;
  (*$*)
}

let compose f g x = f (g x)
let migration_compose (ab : ('a, 'b) migration_functions) (bc : ('b, 'c) migration_functions) : ('a, 'c) migration_functions = {
  (*$ foreach_type (fun _ s ->
      printf "copy_%-21s = compose bc.copy_%-21s ab.copy_%s;\n" s s s) *)
  copy_structure             = compose bc.copy_structure             ab.copy_structure;
  copy_signature             = compose bc.copy_signature             ab.copy_signature;
  copy_toplevel_phrase       = compose bc.copy_toplevel_phrase       ab.copy_toplevel_phrase;
  copy_core_type             = compose bc.copy_core_type             ab.copy_core_type;
  copy_expression            = compose bc.copy_expression            ab.copy_expression;
  copy_pattern               = compose bc.copy_pattern               ab.copy_pattern;
  copy_case                  = compose bc.copy_case                  ab.copy_case;
  copy_type_declaration      = compose bc.copy_type_declaration      ab.copy_type_declaration;
  copy_type_extension        = compose bc.copy_type_extension        ab.copy_type_extension;
  copy_extension_constructor = compose bc.copy_extension_constructor ab.copy_extension_constructor;
  copy_out_value             = compose bc.copy_out_value             ab.copy_out_value;
  copy_out_type              = compose bc.copy_out_type              ab.copy_out_type;
  copy_out_class_type        = compose bc.copy_out_class_type        ab.copy_out_class_type;
  copy_out_module_type       = compose bc.copy_out_module_type       ab.copy_out_module_type;
  copy_out_sig_item          = compose bc.copy_out_sig_item          ab.copy_out_sig_item;
  copy_out_type_extension    = compose bc.copy_out_type_extension    ab.copy_out_type_extension;
  copy_out_phrase            = compose bc.copy_out_phrase            ab.copy_out_phrase;
  copy_mapper                = compose bc.copy_mapper                ab.copy_mapper;
  (*$*)
}

type _ migration += Migration : 'from ocaml_version * ('from, 'to_) migration_functions * 'to_ ocaml_version -> 'from migration

module type Migrate_module = sig
  module From : Ast
  module To : Ast
  (*$ foreach_type (fun m s ->
      printf "val copy_%-21s: From.%s.%s -> To.%s.%s\n" s m s m s) *)
  val copy_structure            : From.Parsetree.structure -> To.Parsetree.structure
  val copy_signature            : From.Parsetree.signature -> To.Parsetree.signature
  val copy_toplevel_phrase      : From.Parsetree.toplevel_phrase -> To.Parsetree.toplevel_phrase
  val copy_core_type            : From.Parsetree.core_type -> To.Parsetree.core_type
  val copy_expression           : From.Parsetree.expression -> To.Parsetree.expression
  val copy_pattern              : From.Parsetree.pattern -> To.Parsetree.pattern
  val copy_case                 : From.Parsetree.case -> To.Parsetree.case
  val copy_type_declaration     : From.Parsetree.type_declaration -> To.Parsetree.type_declaration
  val copy_type_extension       : From.Parsetree.type_extension -> To.Parsetree.type_extension
  val copy_extension_constructor: From.Parsetree.extension_constructor -> To.Parsetree.extension_constructor
  val copy_out_value            : From.Outcometree.out_value -> To.Outcometree.out_value
  val copy_out_type             : From.Outcometree.out_type -> To.Outcometree.out_type
  val copy_out_class_type       : From.Outcometree.out_class_type -> To.Outcometree.out_class_type
  val copy_out_module_type      : From.Outcometree.out_module_type -> To.Outcometree.out_module_type
  val copy_out_sig_item         : From.Outcometree.out_sig_item -> To.Outcometree.out_sig_item
  val copy_out_type_extension   : From.Outcometree.out_type_extension -> To.Outcometree.out_type_extension
  val copy_out_phrase           : From.Outcometree.out_phrase -> To.Outcometree.out_phrase
  val copy_mapper               : From.Ast_mapper.mapper -> To.Ast_mapper.mapper
  (*$*)
end

module Migration_functions
    (A : OCaml_version) (B : OCaml_version)
    (A_to_B : Migrate_module with module From = A.Ast and module To = B.Ast)
=
struct
  let migration_functions : (A.types, B.types) migration_functions =
    let open A_to_B in
    {
      (*$ foreach_type (fun _ s -> printf "copy_%s;\n" s) *)
      copy_structure;
      copy_signature;
      copy_toplevel_phrase;
      copy_core_type;
      copy_expression;
      copy_pattern;
      copy_case;
      copy_type_declaration;
      copy_type_extension;
      copy_extension_constructor;
      copy_out_value;
      copy_out_type;
      copy_out_class_type;
      copy_out_module_type;
      copy_out_sig_item;
      copy_out_type_extension;
      copy_out_phrase;
      copy_mapper;
      (*$*)
    }
end

module Register_migration (A : OCaml_version) (B : OCaml_version)
    (A_to_B : Migrate_module with module From = A.Ast and module To = B.Ast)
    (B_to_A : Migrate_module with module From = B.Ast and module To = A.Ast)
=
struct
  let () = (
    let is_undefined : type a. a migration -> bool = function
      | Undefined -> true
      | _ -> false
    in
    assert (A.version < B.version);
    assert (is_undefined A.migration_info.next_version);
    assert (is_undefined B.migration_info.previous_version);
    let module A_to_B_fun = Migration_functions(A)(B)(A_to_B) in
    let module B_to_A_fun = Migration_functions(B)(A)(B_to_A) in
    A.migration_info.next_version <-
      Migration ((module A), A_to_B_fun.migration_functions, (module B));
    B.migration_info.previous_version <-
      Migration ((module B), B_to_A_fun.migration_functions, (module A));
  )
end

type 'from immediate_migration =
  | No_migration : 'from immediate_migration
  | Immediate_migration
    :  ('from, 'to_) migration_functions * 'to_ ocaml_version
      -> 'from immediate_migration

let immediate_migration
    (*$ foreach_type (fun _ s -> printf "(type %s)\n" s) *)
    (type structure)
    (type signature)
    (type toplevel_phrase)
    (type core_type)
    (type expression)
    (type pattern)
    (type case)
    (type type_declaration)
    (type type_extension)
    (type extension_constructor)
    (type out_value)
    (type out_type)
    (type out_class_type)
    (type out_module_type)
    (type out_sig_item)
    (type out_type_extension)
    (type out_phrase)
    (type mapper)
    (*$*)
    ((module A) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s;\n" s s) *)
     structure             : structure;
     signature             : signature;
     toplevel_phrase       : toplevel_phrase;
     core_type             : core_type;
     expression            : expression;
     pattern               : pattern;
     case                  : case;
     type_declaration      : type_declaration;
     type_extension        : type_extension;
     extension_constructor : extension_constructor;
     out_value             : out_value;
     out_type              : out_type;
     out_class_type        : out_class_type;
     out_module_type       : out_module_type;
     out_sig_item          : out_sig_item;
     out_type_extension    : out_type_extension;
     out_phrase            : out_phrase;
     mapper                : mapper;
     (*$*)
     > ocaml_version)
    direction
  =
  let version = match direction with
    | `Next -> A.migration_info.next_version
    | `Previous -> A.migration_info.previous_version
  in
  match version with
  | Undefined -> No_migration
  | Migration (_, funs, to_) -> Immediate_migration (funs, to_)
  | _ -> assert false

let migrate
    (*$ foreach_type (fun _ s -> printf "(type %s1) (type %s2)\n" s s) *)
    (type structure1) (type structure2)
    (type signature1) (type signature2)
    (type toplevel_phrase1) (type toplevel_phrase2)
    (type core_type1) (type core_type2)
    (type expression1) (type expression2)
    (type pattern1) (type pattern2)
    (type case1) (type case2)
    (type type_declaration1) (type type_declaration2)
    (type type_extension1) (type type_extension2)
    (type extension_constructor1) (type extension_constructor2)
    (type out_value1) (type out_value2)
    (type out_type1) (type out_type2)
    (type out_class_type1) (type out_class_type2)
    (type out_module_type1) (type out_module_type2)
    (type out_sig_item1) (type out_sig_item2)
    (type out_type_extension1) (type out_type_extension2)
    (type out_phrase1) (type out_phrase2)
    (type mapper1) (type mapper2)
    (*$*)
    ((module A) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s1;\n" s s) *)
     structure             : structure1;
     signature             : signature1;
     toplevel_phrase       : toplevel_phrase1;
     core_type             : core_type1;
     expression            : expression1;
     pattern               : pattern1;
     case                  : case1;
     type_declaration      : type_declaration1;
     type_extension        : type_extension1;
     extension_constructor : extension_constructor1;
     out_value             : out_value1;
     out_type              : out_type1;
     out_class_type        : out_class_type1;
     out_module_type       : out_module_type1;
     out_sig_item          : out_sig_item1;
     out_type_extension    : out_type_extension1;
     out_phrase            : out_phrase1;
     mapper                : mapper1;
     (*$*)
     > ocaml_version)
    ((module B) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s2;\n" s s) *)
     structure             : structure2;
     signature             : signature2;
     toplevel_phrase       : toplevel_phrase2;
     core_type             : core_type2;
     expression            : expression2;
     pattern               : pattern2;
     case                  : case2;
     type_declaration      : type_declaration2;
     type_extension        : type_extension2;
     extension_constructor : extension_constructor2;
     out_value             : out_value2;
     out_type              : out_type2;
     out_class_type        : out_class_type2;
     out_module_type       : out_module_type2;
     out_sig_item          : out_sig_item2;
     out_type_extension    : out_type_extension2;
     out_phrase            : out_phrase2;
     mapper                : mapper2;
     (*$*)
     > ocaml_version)
  : (A.types, B.types) migration_functions
  =
  match A.Version with
  | B.Version -> migration_identity
  | _ ->
    let direction = if A.version < B.version then `Next else `Previous in
    let rec migrate (m : A.types immediate_migration) : (A.types, B.types) migration_functions =
      match m with
      | No_migration -> assert false
      | Immediate_migration (f, (module To)) ->
        match To.Version with
        | B.Version -> f
        | _ ->
          match immediate_migration (module To) direction with
          | No_migration -> assert false
          | Immediate_migration (g, to2) ->
            migrate (Immediate_migration (migration_compose f g, to2))
    in
    migrate (immediate_migration (module A) direction)

module Convert (A : OCaml_version) (B : OCaml_version) = struct
  let {
    (*$ foreach_type (fun _ s -> printf "copy_%s;\n" s) *)
    copy_structure;
    copy_signature;
    copy_toplevel_phrase;
    copy_core_type;
    copy_expression;
    copy_pattern;
    copy_case;
    copy_type_declaration;
    copy_type_extension;
    copy_extension_constructor;
    copy_out_value;
    copy_out_type;
    copy_out_class_type;
    copy_out_module_type;
    copy_out_sig_item;
    copy_out_type_extension;
    copy_out_phrase;
    copy_mapper;
    (*$*)
  } : (A.types, B.types) migration_functions =
    migrate (module A) (module B)
end

(*$ foreach_version (fun suffix version ->
    printf "module OCaml_%s = struct\n" suffix;
    printf "  module Ast = Ast_%s\n" suffix;
    printf "  include Make_witness(Ast_%s)\n" suffix;
    printf "  let version = %s\n" suffix;
    printf "  let string_version = %S\n" version;
    printf "end\n";
    printf "let ocaml_%s : OCaml_%s.types ocaml_version = (module OCaml_%s)\n"
      suffix suffix suffix;
  )
*)
module OCaml_402 = struct
  module Ast = Ast_402
  include Make_witness(Ast_402)
  let version = 402
  let string_version = "4.02"
end
let ocaml_402 : OCaml_402.types ocaml_version = (module OCaml_402)
module OCaml_403 = struct
  module Ast = Ast_403
  include Make_witness(Ast_403)
  let version = 403
  let string_version = "4.03"
end
let ocaml_403 : OCaml_403.types ocaml_version = (module OCaml_403)
module OCaml_404 = struct
  module Ast = Ast_404
  include Make_witness(Ast_404)
  let version = 404
  let string_version = "4.04"
end
let ocaml_404 : OCaml_404.types ocaml_version = (module OCaml_404)
module OCaml_405 = struct
  module Ast = Ast_405
  include Make_witness(Ast_405)
  let version = 405
  let string_version = "4.05"
end
let ocaml_405 : OCaml_405.types ocaml_version = (module OCaml_405)
module OCaml_406 = struct
  module Ast = Ast_406
  include Make_witness(Ast_406)
  let version = 406
  let string_version = "4.06"
end
let ocaml_406 : OCaml_406.types ocaml_version = (module OCaml_406)
module OCaml_407 = struct
  module Ast = Ast_407
  include Make_witness(Ast_407)
  let version = 407
  let string_version = "4.07"
end
let ocaml_407 : OCaml_407.types ocaml_version = (module OCaml_407)
module OCaml_408 = struct
  module Ast = Ast_408
  include Make_witness(Ast_408)
  let version = 408
  let string_version = "4.08"
end
let ocaml_408 : OCaml_408.types ocaml_version = (module OCaml_408)
module OCaml_409 = struct
  module Ast = Ast_409
  include Make_witness(Ast_409)
  let version = 409
  let string_version = "4.09"
end
let ocaml_409 : OCaml_409.types ocaml_version = (module OCaml_409)
module OCaml_410 = struct
  module Ast = Ast_410
  include Make_witness(Ast_410)
  let version = 410
  let string_version = "4.10"
end
let ocaml_410 : OCaml_410.types ocaml_version = (module OCaml_410)
module OCaml_411 = struct
  module Ast = Ast_411
  include Make_witness(Ast_411)
  let version = 411
  let string_version = "4.11"
end
let ocaml_411 : OCaml_411.types ocaml_version = (module OCaml_411)
(*$*)

let all_versions : (module OCaml_version) list = [
  (*$foreach_version (fun suffix _ ->
      printf "(module OCaml_%s : OCaml_version);\n" suffix)*)
  (module OCaml_402 : OCaml_version);
  (module OCaml_403 : OCaml_version);
  (module OCaml_404 : OCaml_version);
  (module OCaml_405 : OCaml_version);
  (module OCaml_406 : OCaml_version);
  (module OCaml_407 : OCaml_version);
  (module OCaml_408 : OCaml_version);
  (module OCaml_409 : OCaml_version);
  (module OCaml_410 : OCaml_version);
  (module OCaml_411 : OCaml_version);
  (*$*)
]

(*$foreach_version_pair (fun a b ->
    printf "include Register_migration(OCaml_%s)(OCaml_%s)\n" a b;
    printf "  (Migrate_parsetree_%s_%s)(Migrate_parsetree_%s_%s)\n" a b b a
  )
*)
include Register_migration(OCaml_402)(OCaml_403)
    (Migrate_parsetree_402_403)(Migrate_parsetree_403_402)
include Register_migration(OCaml_403)(OCaml_404)
    (Migrate_parsetree_403_404)(Migrate_parsetree_404_403)
include Register_migration(OCaml_404)(OCaml_405)
    (Migrate_parsetree_404_405)(Migrate_parsetree_405_404)
include Register_migration(OCaml_405)(OCaml_406)
    (Migrate_parsetree_405_406)(Migrate_parsetree_406_405)
include Register_migration(OCaml_406)(OCaml_407)
    (Migrate_parsetree_406_407)(Migrate_parsetree_407_406)
include Register_migration(OCaml_407)(OCaml_408)
    (Migrate_parsetree_407_408)(Migrate_parsetree_408_407)
include Register_migration(OCaml_408)(OCaml_409)
    (Migrate_parsetree_408_409)(Migrate_parsetree_409_408)
include Register_migration(OCaml_409)(OCaml_410)
    (Migrate_parsetree_409_410)(Migrate_parsetree_410_409)
include Register_migration(OCaml_410)(OCaml_411)
    (Migrate_parsetree_410_411)(Migrate_parsetree_411_410)
(*$*)

module OCaml_current = OCaml_OCAML_VERSION
let ocaml_current : OCaml_current.types ocaml_version = (module OCaml_current)

(* Make sure the preprocessing worked as expected *)
let _f (x : Parsetree.expression) : OCaml_current.Ast.Parsetree.expression = x
