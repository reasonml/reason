/*
 * Multiline comment
 */

/*


 */

/*
 * Multiline comment with a // single line comment
 */
// Single line comment
let testPostComment = "";
// let commentedCode = "";

// Test inter-code comments
let testMultiline a => switch a {
  //  single line comment
  | `Thingy x => {
    print_string /* multiline comment should be fine */ "matched thingy x";
    let zz = 10; // post line single line comment
    zz;
  }
  | `Other x => {
    //  single line comment above
    print_string "matched other x";
    x;
  }
  //  single line comment below
};


/* short comment */
let x = [
    "test",
];


/* short comment */
let x = {
  // /* */
    let y = ""
};

// /* this is a valid nested comment*/ this is a valid comment


// valid /* this is a valid comment */

let z = 10;



// The following tests will test the conversion of /* */ to single line
// comments as well as the wrapping of interleaved comments within short sequences.

/*
 * Test wrapping every form of named arguments where various parts are
 * commented.
 */
let a = 10;
let b = 20;
/*A*/
let named
    /* a::a */
    a::a
    /* b::b */
    b::b =>
  /* a + b */
  a + b;

/*B*/
let namedAlias
    /* a::aa */
    a::aa
    /* b::bb */
    b::bb =>
  /* aa + bb */
  aa + bb;

/*C*/
let namedAnnot
    /* a::(a: option int) */
    a::(a: option int)
    /* b::(b: option int) */
    b::(b: option int) =>
  /* 20 */
  20;

/*D*/
let namedAliasAnnot
    /* a::(aa: option int) */
    a::(aa: option int)
    /* b::(bb: option int) */
    b::(bb: option int) =>
  /* 20 */
  20;

/*E*/
let optional
    /* a::a=? */
    a::a=?
    /* b::b=? */
    b::b=?
    /* () */
    () =>
  /* 10 */
  10;

/*F*/
let optionalAlias
    /* a::aa */
    a::aa=?
    /* ?b:bb */
    b::bb=?
    /* () */
    () =>
  /* 10 */
  10;

/*G*/
let optionalAnnot
    /* a::(a: option int)=? */
    a::(a: option int)=?
    /* ?b:(b: option int) */
    b::(b: option int)=?
    /* () */
    () =>
  /* 10 */
  10;

/*H*/
let optionalAliasAnnot
    /* a::(aa: option int)=? */
    a::(aa: option int)=?
    /* b::(bb: option int)=? */
    b::(bb: option int)=?
    /* () => */
    () =>
  /* 10 */
  10;
/*I: This one is really annoying? Where's the visual label?*/
let defOptional
    /* a::a=10 */
    a::a=10
    /* b::b=10 */
    b::b=10
    /* () => */
    () =>
  /* 10 */
  10;

/*J*/
let defOptionalAlias
    /* a::aa=10 */
    a::aa=10
    /* b::bb=10 */
    b::bb=10
    /* () => */
    () =>
  /* 10; */
  10;

/*K*/
let defOptionalAnnot
    /* a::(a:int)=10 */
    a::(a:int)=10
    /* b::(b:int)=10 */
    b::(b:int)=10
    /* () => */
    () =>
  /* 10; */
  10;


