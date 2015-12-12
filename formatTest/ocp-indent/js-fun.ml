(* preferred list style *)
let z =
  f
    [ y
    ; foo ~f:(fun () ->
        arg)
    ]
;;
let z =
  f
    [ y
    ; foo ~f:(fun () ->
        arg
      )
    ]
;;

(* legacy list style *)
let _ =
  [ f (fun x ->
      x);
    f (fun x ->
      x);
    f (fun x ->
      x);
  ]
let _ =
  [ f (fun x ->
      x
    );
    f (fun x ->
      x
    );
    f (fun x ->
      x
    );
  ]
;;
let _ =
  [f (fun x ->
     x
   );
   f (fun x ->
     x
   );
   f (fun x ->
     x
   );
  ]
;;

let _ =
  x
  >>= fun x ->
  (try x with _ -> ())
  >>= fun x ->
  try x with _ -> ()
    >>= fun x ->
    x
;;

let () =
  expr
  >>| function
  | x -> 3
  | y -> 4
;;

let () =
  expr
  >>| fun z -> match z with
               | x -> 3
               | y -> 4
;;

let () =
  expr
  >>| fun z -> function
  | x -> 3
  | y -> 4
;;

let () =
  my_func () >>= function
  | A -> 0
  | B -> 0
;;

let () =
  my_func () >>= (function
    | A -> 0
    | B -> 0)
;;

let () =
  expr
  >>| function
  | x -> 3
  | y -> 4
;;

let () =
  expr
  >>| (function
    | x -> 3
    | y -> 4)
;;



let f =
  f >>= m (fun f ->
    fun x ->
      y);
  z
;;

let f =
  f
  |> m (fun f ->
    fun x ->
      y
  );
  z
;;
let f =
  f
  |> m (fun f ->
    fun x ->
      y);
  z
;;
