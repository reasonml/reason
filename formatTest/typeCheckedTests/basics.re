type reasonXyz =
  | X
  | Y of int int int
  | Z of int int
  | Q
  | R;

let reasonDoubleBar = fun
  | X | Y _ _ _ | Z _ _ | Q => true
  | _ => false;

let reasonDoubleBarNested = fun
  | X | Y _ _ _ | (Z _ _ | Q)  => true
  | _ => false;


/* Liberal use of the Any pattern being compatible with multiple
  arguments  */
let reasonDoubleBarAnyPatterns = fun
  | X | Y _ | Z _ | Q => true
  | _ => false;

let reasonDoubleBarNestedAnyPatterns = fun
  | X | Y _ | (Z _ | Q)  => true
  | _ => false;
