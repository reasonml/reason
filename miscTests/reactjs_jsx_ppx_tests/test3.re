[@@@bs.config {foo, jsx: 2}];
/* don't auto-format this file until https://github.com/facebook/reason/issues/904 is resolved */
<div />;

<div className="hello" />;

<div className="hello" width="10" />;

<div className="hello" width="10"> <li /> <Foo /> </div>;

<div className="hello" comp=(<Foo bar=1 />)> <li /> <Foo bar=2 /> </div>;

/* ============== */

<Foo />;

<Foo className="hello" />;

<Foo className="hello" width="10" />;

<Foo className="hello" width="10"> <li /> <Bar /> </Foo>;

<Foo className="hello" comp=(<Bar bar=1 />)> <li /> <Bar bar=2 /> </Foo>;

/* ============== */

<Foo key="someKey" className="hello" />;

<Foo key="someKey" ref=(some ref) className="hello" />;

<Foo.Bar key="someKey" ref=(some ref) className="hello"> <Bar /> </Foo.Bar>;
