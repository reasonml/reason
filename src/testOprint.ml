(* Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

module Convert = Migrate_parsetree.Convert (Migrate_parsetree.OCaml_404) (Migrate_parsetree.OCaml_current)
module ConvertBack = Migrate_parsetree.Convert (Migrate_parsetree.OCaml_current) (Migrate_parsetree.OCaml_404)

let main () =
  let filename = "./TestTest.ml" in
  let modulename = "TestTest" in

  let lexbuf = Reason_toolchain.setup_lexbuf true filename in
  let impl = Reason_toolchain.JS.canonical_implementation in

  Compmisc.init_path false;
  Env.set_unit_name modulename;

  let ast = impl lexbuf in
  let ast = Convert.copy_structure ast in
  let env = Compmisc.initial_env() in
  let (typedtree, thing) = Typemod.type_implementation modulename modulename modulename env ast in
  let tree = Printtyp.tree_of_signature typedtree.Typedtree.str_type in
  let phrase = (Ast_404.Outcometree.Ophr_signature
    (List.map (fun item -> (ConvertBack.copy_out_sig_item item, None)) tree)
  ) in
  let fmt = Format.str_formatter in
  Reason_oprint.print_out_phrase fmt phrase;
  let result = Format.flush_str_formatter () in
  print_string result

let () = main ()

