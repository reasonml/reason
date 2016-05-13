/* attributed to entire type not binding */
type t2 = (int, int);

/* Does not remain here */
type color =
  | Red of int  /* After red */
  | Black of int  /* After black */
  | Green of int;

/* After second green */
let blahCurriedX x =>
  fun
  /* After or pattern green */
  | Red 10
  | Black 20
  | Green 10 => 1
  | Red x => 0  /* After red */
  | Black x => 0  /* After black */
  | Green x => 0;

/* On next line after blahCurriedX def */