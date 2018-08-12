type b = {
	c: string
};

type _a = {
	b: b
};

type a = option(_a);

let a = Some({
	b: {
  		c: "hi"
  	}
});

let codegen =  a?.b?.c;

let res = switch a {
 | None => None
 | Some(a) => {
  let b = Some(a.b);
 	switch b {
    	| None => None
      | Some(b) => Some(b.c)
    }
  }
};

