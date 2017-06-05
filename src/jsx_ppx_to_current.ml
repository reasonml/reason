(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

(* isolated module for use by reactjs_jsx_ppx and reactjs_jsx_ppx_2, so that when bspacking, we don't have to drag in all the deps *)

open Migrate_parsetree

module To_current = Convert(OCaml_404)(OCaml_current)
