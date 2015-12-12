let rec check_header t =
  if Iobuf.length t.buf < header_length then failwiths "Short packet" t !sexp_of_t;
and session t =
  check_header t;
  Session_id.of_int_exn id_int
and length t =
  let len = raw_length t in
  if len = eos_marker then 0 else len
and sexp_of_t t =                       (* something pretty for debugging *)
  let lo, len = Iobuf.snapshot t.buf, Iobuf.length t.buf in
  protect ~finally:(fun () -> Iobuf.Snapshot.rewind lo t.buf; Iobuf.resize t.buf len)
    (fun () -> ())
;;
