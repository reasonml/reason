/* dont touch these; no annotation */
type dom = {createElement: unit => unit};

let div = {createElement: fun () => ()};

div.createElement ();

module Gah = {
  let createElement () => ();
};

Gah.createElement ();

/* don't transform */
let asd = Bar.createElement foo::1 bar::2 ["a", "b"] [@jsxa] [@foo];

/* transform, keep [@foo] */
let asd = Bar.createElement foo::1 bar::2 ["a", "b"] [@JSX] [@foo];

/* nested modules */
Baz.Beee.createElement baz::2 ["a", "b"] [@JSX];

/* no prop */
Bar.createElement [foo] [@JSX];

/* empty children */
Bar.createElement foo::1 bar::2 [] [@JSX];

/* createElement nested in props */
Bar.createElement foo::(Baz.createElement baz::(Baaz.createElement [] [@JSX]) [] [@JSX]) [] [@JSX];

/* dom elements */
Div.createElement foo::1 bar::2 [] [@JSX];

/* createElement nested in children */
Bar.createElement
[
  Baz.Beee.createElement baz::2
    kek::(Foo.createElement [] [@JSX]) ["a", "b"] [@JSX],
  Bar.createElement [] [@JSX]
]
[@JSX];

(bar foo::1 children::((baz qux::2 [])[@JSX]) [])[@JSX ];
