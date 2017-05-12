type%export[@abstract] t = int
type%export[@abstract] t2 = int list
type%export mylist = int list
type%export[@as: t list] mylist2 = int list
type%export[@as: t2] mylist3 = int list

let%export[@as: (t * t)] m:(int * int) = (2,3)
and[@as: t] n: int = 5
