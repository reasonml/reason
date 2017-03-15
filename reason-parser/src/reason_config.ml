(**
 * Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
 *)

let recoverable = ref false
let add_printers = ref false

let configure ~r ~ap = (
  recoverable := r;
  add_printers := ap
)
