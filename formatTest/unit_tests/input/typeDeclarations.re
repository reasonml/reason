/* === test wrapping for arrows === */
type foo = option((int => int));
type foo = option((int => (int => int)));
type foo = option(((int => int) => int));
type foo = option(((int, int) => int));

/* tuple */
type foo = option(((int => int), (int => int)));
type foo = option(((int => int), string));
type foo = option((string, (int => int), string));
type foo = option((string, (int => int)));

/* other preceeding/trailing */
type foo = option((int => int), (int => int));
type foo = option((int => int), string);
type foo = option(string, (int => int), string);
type foo = option(string, (int => int));

/* preceeding/trailing, more args */
type foo = option((int => string => int), (int => string => int));
type foo = option((int => string => int), string);
type foo = option(string, (int => string => int), string);
type foo = option(string, (int => string => int));

/* others */
type foo = option(string, option(int => int), string);
type foo = option(string, option(option(option(int) => int)), string);
/* === end test wrapping for arrows === */
