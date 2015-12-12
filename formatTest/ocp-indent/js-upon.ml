let f x =
  stop
  (* We don't do this as a matter of style, but the indentation reveals a common
     mistake. *)
  >>> fun () -> don't_wait_for (close fd);
                bind fd

let f x =
  stop
  (* This is what was intended, which is indented correctly, although it's bad
     style on my part. *)
  >>> (fun () -> don't_wait_for (close fd));
  bind
