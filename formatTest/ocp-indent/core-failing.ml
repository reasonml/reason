exception IOError of
    int *
    exn

module type S = S
  with type ('a, 'b, 'c) map := ('a, 'b, 'c) t

let _ =
  let start_finaliser_thread () =
    ignore (Thread.create (fun () -> Fn.forever (fun () ->
        match read_finaliser_queue () with
        | None -> Thread.delay 1.0
        | Some f -> Exn.handle_uncaught ~exit:false f)) ())
  in
  ()

(* Reason: Commenting out invalid parse *)
(* module F *)
(*     (A:A) *)
(*     (B:B) *)
