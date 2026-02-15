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
  module Outcometree : sig
    type out_value
    type out_type
    type out_class_type
    type out_module_type
    type out_sig_item
    type out_type_extension
    type out_phrase
  end
  (*$*)
end

(* Shortcuts for talking about ast types outside of the module language *)

type 'a _types = 'a constraint 'a
  = <
    (*$ foreach_type (fun _ s -> printf "%-21s : _;\n" s) *)
    out_value             : _;
    out_type              : _;
    out_class_type        : _;
    out_module_type       : _;
    out_sig_item          : _;
    out_type_extension    : _;
    out_phrase            : _;
    (*$*)
  >
;;

(*$ foreach_type (fun _ s ->
    printf "type 'a get_%s =\n" s;
    printf " 'x constraint 'a _types = < %s : 'x; .. >\n" s
  ) *)
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
(*$*)

module type OCaml_version = sig
  module Ast : Ast
  val version : int
  val string_version : string
  type types = <
    (*$ foreach_type (fun m s -> printf "%-21s : Ast.%s.%s;\n" s m s)*)
    out_value             : Ast.Outcometree.out_value;
    out_type              : Ast.Outcometree.out_type;
    out_class_type        : Ast.Outcometree.out_class_type;
    out_module_type       : Ast.Outcometree.out_module_type;
    out_sig_item          : Ast.Outcometree.out_sig_item;
    out_type_extension    : Ast.Outcometree.out_type_extension;
    out_phrase            : Ast.Outcometree.out_phrase;
    (*$*)
  > _types
  type _ witnesses += Version : types witnesses
  val migration_info : types migration_info
end

module Make_witness(Ast : Ast) =
struct
  type types = <
    (*$ foreach_type (fun m s -> printf "%-21s : Ast.%s.%s;\n" s m s)*)
    out_value             : Ast.Outcometree.out_value;
    out_type              : Ast.Outcometree.out_type;
    out_class_type        : Ast.Outcometree.out_class_type;
    out_module_type       : Ast.Outcometree.out_module_type;
    out_sig_item          : Ast.Outcometree.out_sig_item;
    out_type_extension    : Ast.Outcometree.out_type_extension;
    out_phrase            : Ast.Outcometree.out_phrase;
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
    with type Ast.Outcometree.out_value = 'types get_out_value
     and type Ast.Outcometree.out_type = 'types get_out_type
     and type Ast.Outcometree.out_class_type = 'types get_out_class_type
     and type Ast.Outcometree.out_module_type = 'types get_out_module_type
     and type Ast.Outcometree.out_sig_item = 'types get_out_sig_item
     and type Ast.Outcometree.out_type_extension = 'types get_out_type_extension
     and type Ast.Outcometree.out_phrase = 'types get_out_phrase
     (*$*)
  )

type ('a, 'b) type_comparison =
  | Lt : ('a, 'b) type_comparison
  | Eq : ('a, 'a) type_comparison
  | Gt : ('a, 'b) type_comparison

let compare_ocaml_version
    (*$ foreach_type (fun _ s -> printf "(type %s1) (type %s2)\n" s s) *)
    (type out_value1) (type out_value2)
    (type out_type1) (type out_type2)
    (type out_class_type1) (type out_class_type2)
    (type out_module_type1) (type out_module_type2)
    (type out_sig_item1) (type out_sig_item2)
    (type out_type_extension1) (type out_type_extension2)
    (type out_phrase1) (type out_phrase2)
    (*$*)
    ((module A) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s1;\n" s s) *)
     out_value             : out_value1;
     out_type              : out_type1;
     out_class_type        : out_class_type1;
     out_module_type       : out_module_type1;
     out_sig_item          : out_sig_item1;
     out_type_extension    : out_type_extension1;
     out_phrase            : out_phrase1;
     (*$*)
     > ocaml_version)
    ((module B) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s2;\n" s s) *)
     out_value             : out_value2;
     out_type              : out_type2;
     out_class_type        : out_class_type2;
     out_module_type       : out_module_type2;
     out_sig_item          : out_sig_item2;
     out_type_extension    : out_type_extension2;
     out_phrase            : out_phrase2;
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
  copy_out_value: 'from get_out_value -> 'to_ get_out_value;
  copy_out_type: 'from get_out_type -> 'to_ get_out_type;
  copy_out_class_type: 'from get_out_class_type -> 'to_ get_out_class_type;
  copy_out_module_type: 'from get_out_module_type -> 'to_ get_out_module_type;
  copy_out_sig_item: 'from get_out_sig_item -> 'to_ get_out_sig_item;
  copy_out_type_extension: 'from get_out_type_extension -> 'to_ get_out_type_extension;
  copy_out_phrase: 'from get_out_phrase -> 'to_ get_out_phrase;
  (*$*)
}

let id x = x
let migration_identity : ('a, 'a) migration_functions = {
  (*$ foreach_type (fun _ s -> printf "copy_%s = id;\n" s) *)
  copy_out_value = id;
  copy_out_type = id;
  copy_out_class_type = id;
  copy_out_module_type = id;
  copy_out_sig_item = id;
  copy_out_type_extension = id;
  copy_out_phrase = id;
  (*$*)
}

let compose f g x = f (g x)
let migration_compose (ab : ('a, 'b) migration_functions) (bc : ('b, 'c) migration_functions) : ('a, 'c) migration_functions = {
  (*$ foreach_type (fun _ s ->
      printf "copy_%-21s = compose bc.copy_%-21s ab.copy_%s;\n" s s s) *)
  copy_out_value             = compose bc.copy_out_value             ab.copy_out_value;
  copy_out_type              = compose bc.copy_out_type              ab.copy_out_type;
  copy_out_class_type        = compose bc.copy_out_class_type        ab.copy_out_class_type;
  copy_out_module_type       = compose bc.copy_out_module_type       ab.copy_out_module_type;
  copy_out_sig_item          = compose bc.copy_out_sig_item          ab.copy_out_sig_item;
  copy_out_type_extension    = compose bc.copy_out_type_extension    ab.copy_out_type_extension;
  copy_out_phrase            = compose bc.copy_out_phrase            ab.copy_out_phrase;
  (*$*)
}

type _ migration += Migration : 'from ocaml_version * ('from, 'to_) migration_functions * 'to_ ocaml_version -> 'from migration

module type Migrate_module = sig
  module From : Ast
  module To : Ast
  (*$ foreach_type (fun m s ->
      printf "val copy_%-21s: From.%s.%s -> To.%s.%s\n" s m s m s) *)
  val copy_out_value            : From.Outcometree.out_value -> To.Outcometree.out_value
  val copy_out_type             : From.Outcometree.out_type -> To.Outcometree.out_type
  val copy_out_class_type       : From.Outcometree.out_class_type -> To.Outcometree.out_class_type
  val copy_out_module_type      : From.Outcometree.out_module_type -> To.Outcometree.out_module_type
  val copy_out_sig_item         : From.Outcometree.out_sig_item -> To.Outcometree.out_sig_item
  val copy_out_type_extension   : From.Outcometree.out_type_extension -> To.Outcometree.out_type_extension
  val copy_out_phrase           : From.Outcometree.out_phrase -> To.Outcometree.out_phrase
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
      copy_out_value;
      copy_out_type;
      copy_out_class_type;
      copy_out_module_type;
      copy_out_sig_item;
      copy_out_type_extension;
      copy_out_phrase;
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
    (type out_value)
    (type out_type)
    (type out_class_type)
    (type out_module_type)
    (type out_sig_item)
    (type out_type_extension)
    (type out_phrase)
    (*$*)
    ((module A) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s;\n" s s) *)
     out_value             : out_value;
     out_type              : out_type;
     out_class_type        : out_class_type;
     out_module_type       : out_module_type;
     out_sig_item          : out_sig_item;
     out_type_extension    : out_type_extension;
     out_phrase            : out_phrase;
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
    (type out_value1) (type out_value2)
    (type out_type1) (type out_type2)
    (type out_class_type1) (type out_class_type2)
    (type out_module_type1) (type out_module_type2)
    (type out_sig_item1) (type out_sig_item2)
    (type out_type_extension1) (type out_type_extension2)
    (type out_phrase1) (type out_phrase2)
    (*$*)
    ((module A) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s1;\n" s s) *)
     out_value             : out_value1;
     out_type              : out_type1;
     out_class_type        : out_class_type1;
     out_module_type       : out_module_type1;
     out_sig_item          : out_sig_item1;
     out_type_extension    : out_type_extension1;
     out_phrase            : out_phrase1;
     (*$*)
     > ocaml_version)
    ((module B) : <
     (*$ foreach_type (fun _ s -> printf "%-21s : %s2;\n" s s) *)
     out_value             : out_value2;
     out_type              : out_type2;
     out_class_type        : out_class_type2;
     out_module_type       : out_module_type2;
     out_sig_item          : out_sig_item2;
     out_type_extension    : out_type_extension2;
     out_phrase            : out_phrase2;
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
    copy_out_value;
    copy_out_type;
    copy_out_class_type;
    copy_out_module_type;
    copy_out_sig_item;
    copy_out_type_extension;
    copy_out_phrase;
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
module OCaml_412 = struct
  module Ast = Ast_412
  include Make_witness(Ast_412)
  let version = 412
  let string_version = "4.12"
end
let ocaml_412 : OCaml_412.types ocaml_version = (module OCaml_412)
module OCaml_413 = struct
  module Ast = Ast_413
  include Make_witness(Ast_413)
  let version = 413
  let string_version = "4.13"
end
let ocaml_413 : OCaml_413.types ocaml_version = (module OCaml_413)
module OCaml_414 = struct
  module Ast = Ast_414
  include Make_witness(Ast_414)
  let version = 414
  let string_version = "4.14"
end
let ocaml_414 : OCaml_414.types ocaml_version = (module OCaml_414)
module OCaml_500 = struct
  module Ast = Ast_500
  include Make_witness(Ast_500)
  let version = 500
  let string_version = "5.0"
end
let ocaml_500 : OCaml_500.types ocaml_version = (module OCaml_500)
module OCaml_51 = struct
  module Ast = Ast_51
  include Make_witness(Ast_51)
  let version = 510
  let string_version = "5.1"
end
let ocaml_51 : OCaml_51.types ocaml_version = (module OCaml_51)
module OCaml_52 = struct
  module Ast = Ast_52
  include Make_witness(Ast_52)
  let version = 520
  let string_version = "5.2"
end
let ocaml_52 : OCaml_52.types ocaml_version = (module OCaml_52)
module OCaml_53 = struct
  module Ast = Ast_53
  include Make_witness(Ast_53)
  let version = 530
  let string_version = "5.3"
end
let ocaml_53 : OCaml_53.types ocaml_version = (module OCaml_53)
module OCaml_54 = struct
  module Ast = Ast_54
  include Make_witness(Ast_54)
  let version = 540
  let string_version = "5.4"
end
let ocaml_54 : OCaml_54.types ocaml_version = (module OCaml_54)
(*$*)

let all_versions : (module OCaml_version) list = [
  (*$foreach_version (fun suffix _ ->
      printf "(module OCaml_%s : OCaml_version);\n" suffix)*)
  (module OCaml_408 : OCaml_version);
  (module OCaml_409 : OCaml_version);
  (module OCaml_410 : OCaml_version);
  (module OCaml_411 : OCaml_version);
  (module OCaml_412 : OCaml_version);
  (module OCaml_413 : OCaml_version);
  (module OCaml_414 : OCaml_version);
  (module OCaml_500 : OCaml_version);
  (module OCaml_51 : OCaml_version);
  (module OCaml_52 : OCaml_version);
  (module OCaml_53 : OCaml_version);
  (module OCaml_54 : OCaml_version);
  (*$*)
]

(*$foreach_version_pair (fun a b ->
    printf "include Register_migration(OCaml_%s)(OCaml_%s)\n" a b;
    printf "  (Migrate_parsetree_%s_%s)(Migrate_parsetree_%s_%s)\n" a b b a
  )
*)
include Register_migration(OCaml_408)(OCaml_409)
    (Migrate_parsetree_408_409)(Migrate_parsetree_409_408)
include Register_migration(OCaml_409)(OCaml_410)
    (Migrate_parsetree_409_410)(Migrate_parsetree_410_409)
include Register_migration(OCaml_410)(OCaml_411)
    (Migrate_parsetree_410_411)(Migrate_parsetree_411_410)
include Register_migration(OCaml_411)(OCaml_412)
    (Migrate_parsetree_411_412)(Migrate_parsetree_412_411)
include Register_migration(OCaml_412)(OCaml_413)
    (Migrate_parsetree_412_413)(Migrate_parsetree_413_412)
include Register_migration(OCaml_413)(OCaml_414)
    (Migrate_parsetree_413_414)(Migrate_parsetree_414_413)
include Register_migration(OCaml_414)(OCaml_500)
    (Migrate_parsetree_414_500)(Migrate_parsetree_500_414)
include Register_migration(OCaml_500)(OCaml_51)
    (Migrate_parsetree_500_51)(Migrate_parsetree_51_500)
include Register_migration(OCaml_51)(OCaml_52)
    (Migrate_parsetree_51_52)(Migrate_parsetree_52_51)
include Register_migration(OCaml_52)(OCaml_53)
    (Migrate_parsetree_52_53)(Migrate_parsetree_53_52)
include Register_migration(OCaml_53)(OCaml_54)
    (Migrate_parsetree_53_54)(Migrate_parsetree_54_53)
(*$*)

module OCaml_current = OCaml_OCAML_VERSION
let ocaml_current : OCaml_current.types ocaml_version = (module OCaml_current)

(* Make sure the preprocessing worked as expected *)
let _f (x : Outcometree.out_type) : OCaml_current.Ast.Outcometree.out_type = x
