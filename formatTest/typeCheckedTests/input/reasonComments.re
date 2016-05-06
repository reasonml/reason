3 /*-*/
;
3/*-*/
;
3/*-*/;
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
let testingEndOfLineComments =
  [
    "Item 1" /* Comment For First Item */,
    "Item 2" /* Comment For Second Item */,
    "Item 3" /* Comment For Third Item */,
    "Item 4" /* Comment For Fourth Item - but before trailing comma */,
    /* Comment after last item in list. */
  ] /* Comment after rbracket */;

/* But if you place them after the comma at eol, they're preserved as such */
let testingEndOfLineComments =
  [
    "Item 1", /* Comment For First Item */
    "Item 2", /* Comment For Second Item */
    "Item 3", /* Comment For Third Item */
    "Item 4" /* Comment For Fourth Item - but before trailing comma */,
    /* Comment after last item in list. */
  ] /* Comment after rbracket */ ;


/* The space between ; and comment shoudn't matter */
let testPlacementOfTrailingComment = [
  "Item 0" /* */
  /* Comment after last item in list. */
]; /* Comment after semi */

/* The space between ; and comment shoudn't matter */
let testPlacementOfTrailingComment = [
  "Item 0" /* */
  /* Comment after last item in list. */
];/* Comment after semi */

/* Try again but without other things in the list */
let testPlacementOfTrailingComment = [
  "Item 0" /* */
]; /* Comment after semi */

/* The space between ; and comment shoudn't matter */
let testPlacementOfTrailingComment = [
  "Item 0" /* */
  /* Comment after last item in list. */
];/* Comment after semi */

let testingEndOfLineComments = [];/* Comment after entire let binding */


/* The following is not yet idempotent */
/* let myFunction */
/*     withFirstArg  /* First arg */ */
/*     andSecondArg  => { /* Second Arg */ */
/*   withFirstArg + andSecondArg /* before semi */ ; */
/* }; */

let myFunction
    /* First arg */
    withFirstArg
    /* Second Arg */
    andSecondArg  => {
  withFirstArg + andSecondArg
}; /* After Semi */

type point = {
  x: string, /* x field */
  y: string, /* y field */
};

/* The way the last row comment is formatted is suboptimal becuase
 * record type definitions do not include enough location information */
type anotherpoint = {
  x: string, /* x field */
  y: string, /* y field */
  /* comment as last row of record */
};

type t = (int, int); /* End of line on t */
type t2 =
  (int, int) /* End of line on (int, int) */
  ;

type t3 =
  (int, int); /* End of line on (int, int) */


type variant =
  | X of (int, int) /* End of line on X */
  | Y of (int, int); /* On the entire type */

let res =
  switch (X (2, 3)) {
    /* Above X line */
    | X _ => "result of X"  /* End of X line */
    /* Above Y line */
    | Y _ => "result of Y"  /* End of Y line */
  };

type variant2 =
  /* Comment above X */
  | X of (int, int) /* End of line on X */
  /* Comment above Y */
  | Y of (int, int); /* On entire type */

type variant3 =
  /* Comment above X */
  | X of (int, int) /* End of line on X */
  /* Comment above Y */
  | Y of (int, int) /* End of line on Y  */
; /* On entire type */


type x = { /* attached *above* x */
  fieldOne : int
} /* Attached end of line after x */
and y = { /* attached *above* y */
  fieldTwo : int
} /* Attached end of line after y */
;

type x2 = { /* attached *above* x2 */
  fieldOne : int
} /* Attached end of line after x2 */
and y2 = { /* attached *above* y2 */
  fieldTwo : int
}; /* Attached to entire set of bindings */

type optionalTuple =
  | OptTup of (
      option (
        int, /* First int */
        int /* Second int */
      )
    );

type optionTuple =
  option (
    int, /* First int */
    int /* Second int */
  );

type intPair = (
  int, /* First int */
  int /* Second int */
);
type intPair2 = (
  /* First int */
  int,
  /* Second int */
  int
);

let result = {
  /**/
  (+) 2 3
};

/* This is not yet idempotent */
/* { */
/*   /**/ */
/*   (+) 2 3 */
/* }; */

let a = ();
for i in 0 to 10 {
  /* bla  */
  a
};

if true {
  /* hello */
  ()
};
