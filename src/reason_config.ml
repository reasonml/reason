(**
 * Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
*)

let recoverable = ref false

let configure ~r = (
  recoverable := r
)
