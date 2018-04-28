type range = {
  lnum_start: int;
  lnum_end: int
}

let makeRangeBetween loc1 loc2 = Location.{
  lnum_start = loc1.loc_end.pos_lnum + 1;
  lnum_end = loc2.loc_start.pos_lnum - 1;
}

let printRange r =
  print_string "range: {lnum_start: ";
  print_int r.lnum_start;
  print_string ", lnum_end: ";
  print_int r.lnum_end;
  print_string "};";
  print_newline ()
