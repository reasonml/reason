type obj = { a: int };
type x = option(obj);

let x = Some({
  a: 42
});

/*  
	Syntax `?.`
*/
let response1 = x?.a;


/*
	Translates to
*/

let response2 = 
switch x {
	| Some(x) => Some(x.a)
  | None => None
};




