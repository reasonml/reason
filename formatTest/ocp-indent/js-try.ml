(* nested "try" *)
try
  try x
  with e -> e
with e -> e (* indented too far *)
