let reset_cond =
  match states with
  | [ _ ] -> fun _ v _   -> e_id v
  |   _   -> fun s v clk -> false (* … *)
