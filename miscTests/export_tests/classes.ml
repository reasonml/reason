class%export istack = object
    val mutable v: int list = [0; 2]

    method pop: int option =
      match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None

    method push (hd: int): unit =
      v <- hd :: v
  end
