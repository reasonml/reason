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

(*$ #use "src/cinaps_helpers" $*)

(* Shared definitions.
   Mostly errors about features missing in older versions. *)
module Def = Migrate_parsetree_def

(* Copy of OCaml parsetrees *)
(*$foreach_version (fun suffix _ ->
    printf "module Ast_%s = Ast_%s\n" suffix suffix
  )*)
module Ast_408 = Ast_408
module Ast_409 = Ast_409
module Ast_410 = Ast_410
module Ast_411 = Ast_411
module Ast_412 = Ast_412
module Ast_413 = Ast_413
module Ast_414 = Ast_414
module Ast_500 = Ast_500
module Ast_51 = Ast_51
module Ast_52 = Ast_52
module Ast_53 = Ast_54
module Ast_54 = Ast_54
(*$*)

(* Manual migration between versions *)
(*$foreach_version_pair (fun x y ->
    printf "module Migrate_%s_%s = Migrate_parsetree_%s_%s\n" x y x y;
    printf "module Migrate_%s_%s = Migrate_parsetree_%s_%s\n" y x y x;
  )*)
module Migrate_408_409 = Migrate_parsetree_408_409
module Migrate_409_408 = Migrate_parsetree_409_408
module Migrate_409_410 = Migrate_parsetree_409_410
module Migrate_410_409 = Migrate_parsetree_410_409
module Migrate_410_411 = Migrate_parsetree_410_411
module Migrate_411_410 = Migrate_parsetree_411_410
module Migrate_411_412 = Migrate_parsetree_411_412
module Migrate_412_411 = Migrate_parsetree_412_411
module Migrate_412_413 = Migrate_parsetree_412_413
module Migrate_413_412 = Migrate_parsetree_413_412
module Migrate_413_414 = Migrate_parsetree_413_414
module Migrate_414_413 = Migrate_parsetree_414_413
module Migrate_414_500 = Migrate_parsetree_414_500
module Migrate_500_414 = Migrate_parsetree_500_414
module Migrate_500_51 = Migrate_parsetree_500_51
module Migrate_51_500 = Migrate_parsetree_51_500
module Migrate_51_52 = Migrate_parsetree_51_52
module Migrate_52_51 = Migrate_parsetree_52_51
module Migrate_52_53 = Migrate_parsetree_52_53
module Migrate_53_52 = Migrate_parsetree_53_52
module Migrate_53_54 = Migrate_parsetree_53_54
module Migrate_54_53 = Migrate_parsetree_54_53
(*$*)

(* An abstraction of OCaml compiler versions *)
module Versions = Migrate_parsetree_versions

(* All versions are compatible with this signature *)
module type OCaml_version = Versions.OCaml_version

(*$foreach_version (fun suffix _ ->
    printf "module OCaml_%s = Versions.OCaml_%s\n" suffix suffix
  )*)
module OCaml_408 = Versions.OCaml_408
module OCaml_409 = Versions.OCaml_409
module OCaml_410 = Versions.OCaml_410
module OCaml_411 = Versions.OCaml_411
module OCaml_412 = Versions.OCaml_412
module OCaml_413 = Versions.OCaml_413
module OCaml_414 = Versions.OCaml_414
module OCaml_500 = Versions.OCaml_500
module OCaml_51 = Versions.OCaml_51
module OCaml_52 = Versions.OCaml_52
module OCaml_53 = Versions.OCaml_53
module OCaml_54 = Versions.OCaml_54
(*$*)
module OCaml_current = Versions.OCaml_current

(* A Functor taking two OCaml versions and producing a module of functions
   migrating from one to the other. *)
module Convert = Versions.Convert

(* Aliases for compiler-libs modules that might be shadowed *)
module Compiler_libs = struct
  module Location = Location
  module Longident = Longident

  module type Asttypes = module type of struct include Asttypes end
  module rec Asttypes : Asttypes = Asttypes

  module type Parsetree = module type of struct include Parsetree end
  module rec Parsetree : Parsetree = Parsetree

  module Docstrings = Docstrings
  module Ast_helper = Ast_helper
  module Ast_mapper = Ast_mapper
end
