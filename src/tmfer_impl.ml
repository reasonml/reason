open Cmdliner

let tmfer _ =
  `Ok ()

let top_level_info =
  let doc = "Meta language utility" in
  let man = [`S "DESCRIPTION"; `P "tmfer is a parser and pretty-printer"] in
  let version = "Reason " ^ Package.version ^ " @ " ^ Package.git_short_version
  in
  Term.info "tmfer" ~version ~doc ~man

let file =
  let doc = "file to refmt" in
  let docv = "FILE" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv ~doc)

let tmfer_t =
  let open Term in
  const tmfer $ file

let () =
  match Term.eval ((Term.ret tmfer_t), top_level_info) with
  | `Error s -> exit 1
  | _ -> exit 0
