let f
  = fun x -> x
and g
  = fun x -> x
and h
  = fun x -> x

let rec f : 'a. 'a -> 'a
  = fun x -> g x
and g : 'a. 'a -> 'a
  = fun x -> h x
and h : 'a. 'a -> 'a
  = fun x -> f x
