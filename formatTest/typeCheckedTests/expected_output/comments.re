/* **** comment */
/*** comment */
/** docstring */
/* comment */
/** docstring */
/*** comment */
/**** comment */
/***** comment */
/** */
/*** */
/**** */
/**/
/***/
/****/
/** (** comment *) */
/** (*** comment *) */
/* (** comment *) */
/* (*** comment *) */
/* *(*** comment *) */
/* comment **/
/* comment ***/
/* comment ****/
/* comment *****/
/**
 * Multiline
 */
/** Multiline
 *
 */
/**
 **
 */
let testingNotQuiteEndOfLineComments = [
  "Item 1",
  /* Comment For First Item */
  "Item 2",
  /* Comment For Second Item */
  "Item 3",
  /* Comment For Third Item */
  "Item 4"  /* Comment For Fourth Item - but no semi */
  /* Comment after last item in list. */
];

/* Comment after list bracket */
let testingEndOfLineComments = [
  "Item 1",  /* Comment For First Item */
  "Item 2",  /* Comment For Second Item */
  "Item 3",  /* Comment For Third Item */
  "Item 4"
  /* Comment For Fourth Item - but before semi */
  /* Comment after last item in list. */
];

/* Comment after list bracket */
/* This time no space between bracket and comment */
let testingEndOfLineComments = [];

/* Comment after list bracket */
type t = (int, int); /* End of line on t */

/* End of t2 line on int * int */
type t2 = (int, int);

/* End of t22 line on type t22 = */
type t22 = (int, int);

/* End of line on Y */
type variant =
  /* Comment above X */
  | X of int  /* End of line on X */
  /* Comment above Y */
  | Y of int;

/* Comment on entire type def for variant */
/* attached *above* x */
type x = {fieldOne: int}
/* Attached end of line after x */
/* attached *above* y */
and y = {fieldTwo: int};

/* Attached end of line after y */
let result =
  switch (X 3) {
  | X x =>
    /* Where does this comment go? */
    let tmp = x;
    x + tmp
  | Y x =>
    /* How about this one */
    let tmp = x;
    x + tmp
  };

let result =
  switch None {
  | Some {fieldOne: 20} =>
    /* Where does this comment go? */
    let tmp = 0;
    2 + tmp
  | Some {fieldOne: n} =>
    /* How about this one */
    let tmp = n;
    n + tmp
  | None => 20
  };

/* After green */
type color =
  | Red of int  /* After red */
  | Black of int  /* After black */
  | Green of int;

/* On next line after color type def */
/* After second green */
let blahCurriedX x =>
  fun
  /* After or pattern green */
  | Red 10
  | Black 20
  | Green 10 => 1
  | Red x => 0
  /* After red */
  | Black x => 0
  /* After black */
  | Green x => 0;

/* On next line after blahCurriedX def */