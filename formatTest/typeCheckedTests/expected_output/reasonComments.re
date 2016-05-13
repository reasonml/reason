/*-*/
3;

/*-*/
3;

3 /*-*/;

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
let testingEndOfLineComments = [
  "Item 1",
  /* Comment For First Item */
  "Item 2",
  /* Comment For Second Item */
  "Item 3",
  /* Comment For Third Item */
  "Item 4"
  /* Comment For Fourth Item - but before trailing comma */
  /* Comment after last item in list. */
]
/* Comment after rbracket */;

/* But if you place them after the comma at eol, they're preserved as such */
let testingEndOfLineComments = [
  "Item 1",  /* Comment For First Item */
  "Item 2",  /* Comment For Second Item */
  "Item 3",  /* Comment For Third Item */
  "Item 4"
  /* Comment For Fourth Item - but before trailing comma */
  /* Comment after last item in list. */
]
/* Comment after rbracket */;

/* The space between ; and comment shoudn't matter */
/* Comment after semi */
let testPlacementOfTrailingComment = [
  "Item 0"  /* */
  /* Comment after last item in list. */
];

/* The space between ; and comment shoudn't matter */
/* Comment after semi */
let testPlacementOfTrailingComment = [
  "Item 0"  /* */
  /* Comment after last item in list. */
];

/* Try again but without other things in the list */
/* Comment after semi */
let testPlacementOfTrailingComment = [
  "Item 0" /* */
];

/* The space between ; and comment shoudn't matter */
/* Comment after semi */
let testPlacementOfTrailingComment = [
  "Item 0"  /* */
  /* Comment after last item in list. */
];

let testingEndOfLineComments = []; /* Comment after entire let binding */

/* The following is not yet idempotent */
/* let myFunction */
/*     withFirstArg  /* First arg */ */
/*     andSecondArg  => { /* Second Arg */ */
/*   withFirstArg + andSecondArg /* before semi */ ; */
/* }; */
/* After Semi */
let myFunction
    /* First arg */
    withFirstArg
    /* Second Arg */
    andSecondArg => withFirstArg + andSecondArg;

type point = {
  x: string,  /* x field */
  y: string /* y field */
};

/* The way the last row comment is formatted is suboptimal becuase
 * record type definitions do not include enough location information */
type anotherpoint = {
  x: string,  /* x field */
  y: string /* y field */
}
/* comment as last row of record */;

type t = (int, int); /* End of line on t */

/* End of line on (int, int) */
type t2 = (int, int);

/* End of line on (int, int) */
type t3 = (int, int);

/* End of line on Y */
type variant =
  | X of (int, int)  /* End of line on X */
  | Y of (int, int);

/* Comment on entire type def for variant */
/* Before let */
/* After final semi in switch */
let res =
  /* Before switch */
  switch (X (2, 3)) {
  /* Above X line */
  | X _ => "result of X" /* End of arrow and X line */
  /* Above Y line */
  | Y _ => "result of Y" /* End of arrow and Y line */
  };

let res =
  switch (X (2, 3)) {
  /* End of X body line */
  |
      X (0, 0)  /* After X arrow */
      =>
     "result of X"
  /* End of X body line */
  | X (1, 0) =>
    /* Before X's arrow */
    "result of X"
  /* End of X body line */
  |
      X _  /* After X _ arrow */
      =>
     "result of X"
  /* Above Y line */
  | Y _ =>
    /* Comment above Y body */
    "result of Y"
  };

type variant2 =
  /* Comment above X */
  | X of (int, int)  /* End of line on X */
  /* Comment above Y */
  | Y of (int, int);

/* End of line on Y  */
type variant3 =
  /* Comment above X */
  | X of (int, int)  /* End of line on X */
  /* Comment above Y */
  | Y of (int, int);

/* attached *above* x */
type x = {fieldOne: int, fieldA: int}
/* Attached end of line after x */
/* attached *above* y */
and y = {fieldTwo: int}
/* Attached end of line after y */;

/* attached *above* x2 */
type x2 = {fieldOne: int, fieldA: int}
/* Attached end of line after x2 */
/* attached *above* y2 */
and y2 = {fieldTwo: int};

let result =
  switch None {
  |
      Some {fieldOne: 20, fieldA: a}  /* Where does this comment go? */
      =>

    let tmp = 0;
    2 + tmp
  | Some {fieldOne: n, fieldA: a} =>
    /* How about this one */
    let tmp = n;
    n + tmp
  | None => 20
  };

let res =
  /* Before switch */
  switch (X (2, 3)) {
  /* Above X line */
  | X _ => "result of X" /* End of arrow and X line */
  /* Above Y line */
  | Y _ => "result of Y" /* End of arrow and Y line */
  };

/*
 * Now these end of line comments *should* be retained.
 */
let result =
  switch None {
  | Some {
      fieldOne: 20,
      fieldA:
        /* end of line */
        a
      /* end of line */
    } =>
    let tmp = 0;
    2 + tmp
  | Some {
      fieldOne: n,
      fieldA:
        /* end of line */
        a
      /* end of line */
    } =>
    let tmp = n;
    n + tmp
  | None => 20
  };

/*
 * These end of line comments *should* be retained.
 * To get the simple expression eol comment to be retained, we just need to
 * implement label breaking eol behavior much like we did with sequences.
 * Otherwise, right now they are not idempotent.
 */
let res =
  switch (
    X
      /* Retain this */
      (2, 3)
  ) {
  /* Above X line */
  | X (
      _,  /* retain this */
      _ /* retain this */
    ) => "result of X"
  /* Above Y line */
  | Y _ => "result of Y" /* End of arrow and Y line */
  };

type optionalTuple =
  | OptTup of (
      option
        (
          int,  /* First int */
          int /* Second int */
        )
    );

type optionTuple =
  option
    (
      int,  /* First int */
      int /* Second int */
    );

type intPair = (
  int,  /* First int */
  int /* Second int */
);

type intPair2 = (
  /* First int */
  int,
  /* Second int */
  int
);

let result =
  /**/
  2 + 3;

/* This is not yet idempotent */
/* { */
/*   /**/ */
/*   (+) 2 3 */
/* }; */
let a = ();

for i in 0 to 10 {
  a
}
/* bla  */;

if true {
  ()
}
/* hello */;

/* After green end of line */
type color =
  | Red of int  /* After red end of line */
  | Black of int  /* After black end of line */
  | Green of int;

/* On next line after color type def */
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