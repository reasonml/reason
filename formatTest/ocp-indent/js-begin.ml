let f = function
  | zoo -> begin
      foo;
      bar;
    end
;;
let g = function
  | zoo -> (
      foo;
      bar;
    )
;;
let () =
  begin match foo with
        | Bar -> snoo
  end
;;
