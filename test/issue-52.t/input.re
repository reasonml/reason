let reasonDoubleBarNested = fun
  | X | Y _ _ _ | (Z _ _ | Q)  => true
  | _ => false;

