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

/* On the entire type */
type variant =
  | X of (int, int)  /* End of line on X */
  | Y of (int, int);

let res =
  switch (X (2, 3)) {
  /* Above X line */
  | X _ => "result of X" /* End of X line */
  /* Above Y line */
  | Y _ => "result of Y" /* End of Y line */
  };

/* On entire type */
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

/* On entire type */
/* attached *above* x */
type x = {fieldOne: int}
/* Attached end of line after x */
/* attached *above* y */
and y = {fieldTwo: int}
/* Attached end of line after y */;

/* attached *above* x2 */
/* Attached to entire set of bindings */
type x2 = {fieldOne: int}
/* Attached end of line after x2 */
/* attached *above* y2 */
and y2 = {fieldTwo: int};

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
