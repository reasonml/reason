let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let x = Reason_toolchain.JS.canonical_implementation_with_comments lexbuf in
    Reason_toolchain.JS.print_canonical_implementation_with_comments
      Format.std_formatter x;
    Format.pp_flush_formatter Format.std_formatter
  with Syntax_util.Error (loc, Syntax_util.Syntax_error msg) ->
    let {Lexing. pos_lnum; pos_bol; pos_cnum} = loc.Location.loc_start in
    Printf.eprintf "%d:%d %s\n%!" pos_lnum (pos_cnum - pos_bol) msg
