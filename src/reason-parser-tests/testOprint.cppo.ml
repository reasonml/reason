(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


(**
 * See `testOprint.js` for how this gets run.
 *
 * In order to test our outcome printer, we parse & typecheck the code provided on stdin.
 * That gives us a `Typedtree` (like an AST but with all the types included), which includes
 * the `signature` type of the module we just processed.
 * From there, `Printtyp` will helpfully convert the `signature` into something that our
 * outcome printer can handle.
 *
 * Outcome printers are mostly used with Repl's like utop or tools like Merlin, so there's
 * not a super easy path to "test it out", but this setup is hopefully not too complicated.
 *)

open Reason_omp

module Convert = Reason_omp.Convert (Reason_omp.OCaml_411) (Reason_omp.OCaml_current)
module ConvertBack = Reason_omp.Convert (Reason_omp.OCaml_current) (Reason_omp.OCaml_411)

let main () =
  let filename = "./TestTest.ml" in
  let modulename = "TestTest" in

  let lexbuf = Reason_toolchain.setup_lexbuf true filename in
  let impl = Reason_toolchain.RE.implementation in

#if OCAML_VERSION >= (4,9,0)
  Compmisc.init_path ();
#else
  Compmisc.init_path false;
#endif
  Env.set_unit_name modulename;

  let ast = impl lexbuf in
  let ast = Convert.copy_structure ast in
  let env = Compmisc.initial_env() in
#if OCAML_VERSION >= (4,13,0)
  let { Typedtree.structure = typedtree; _ } =
#else
  let (typedtree, _) =
#endif
    Typemod.type_implementation modulename modulename modulename env ast in
  let tree = Printtyp.tree_of_signature typedtree.Typedtree.str_type in
  let phrase = (Ast_411.Outcometree.Ophr_signature
    (List.map (fun item -> (ConvertBack.copy_out_sig_item item, None)) tree)
  ) in
  let fmt = Format.str_formatter in
  Reason_oprint.print_out_phrase fmt phrase;
  let result = Format.flush_str_formatter () in
  print_string result

let () = main ()
