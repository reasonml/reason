/* dont touch these; no annotation */
type dom = {createElement: unit => unit};

let div = {createElement: fun () => ()};

div.createElement ();

module Gah = {
  let createElement () => ();
};

Gah.createElement ();

/* don't transform */
let asd = Bar.createElement foo::1 bar::2 children::["a", "b"] () [@jsxa] [@foo];

/* transform, keep [@foo] */
let asd = Bar.createElement foo::1 bar::2 children::["a", "b"] () [@JSX] [@foo];

/* nested modules */
Baz.Beee.createElement baz::2 children::["a", "b"] () [@JSX];

/* no prop */
Bar.createElement () [@JSX];

/* empty children */
Bar.createElement foo::1 bar::2 children::[] () [@JSX];

/* createElement nested in props */
Bar.createElement foo::(Baz.createElement baz::(Baaz.createElement children::[] () [@JSX]) children::[] () [@JSX]) children::[] () [@JSX];

/* dom elements */
div foo::1 bar::2 children::[] () [@JSX];

div () [@JSX];

/* createElement nested in children */
Bar.createElement
  children::[
    Baz.Beee.createElement baz::2
      kek::(Foo.createElement children::[] () [@JSX]) children::["a", "b"] () [@JSX],
    Bar.createElement children::[] () [@JSX]
  ]
  ()
  [@JSX];

(bar foo::1 children::[(baz qux::2 children::[] ())[@JSX]] ())[@JSX ];
(bar_ foo::1 children::[(baz_ qux::2 children::[] ())[@JSX]] ())[@JSX ];
