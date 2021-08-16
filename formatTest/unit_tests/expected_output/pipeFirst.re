[@reason.version 3.7];
foo->f->g->h;

bar->f->g->h;

foo(g)->f(a, b)->g(c, d);

foo->f();

compilation
->Plugin.buildAssets
->Js.Json.stringify
->Node.Fs.writeFileAsUtf8Sync(_, path);

foo
->someLongIdentifier
->otherLongIdentifierWithArgs(a, b, c)
->longIdentWithVeryLongArgs(
    aaaaaaaaaaaaaaaaaaaaa,
    bbbbbbbbbbbbbbbb,
    ccccccccccccc,
  );

/* with comments */
compilation
/* first */
->Plugin.buildAssets /* first trail */
/* second */
->Js.Json.stringify /* second trail */
/* last */
->Node.Fs.writeFileAsUtf8Sync(_, path); /* last trail */

foo->bar->baz >>= monadicFunction |> bind;

compilation
->Plugin.buildAssets
->Js.Json.stringify
|> Cohttp_lwt_body.to_string
>|= (
  body =>
    Printf.sprintf(
      "okokok",
      uri,
      meth,
      headers,
      body,
    )
)
>>= (
  body =>
    Server.respond_string(~status, ~body, ())
);

x + y + foo->bar->baz;
x + y * foo->bar->baz;
x && y || foo->bar->baz;

x < foo->bar->baz;
foo !== bar->baz;
x |> y >>= foo->bar->baz;
let m = f => foo->bar->f;

obj##x->foo->bar;

event->target[0];

event->target[0];

event->target##value;

event->target##value;

event->target##value[0];

event->(target##value[0]);

event->target(foo);

event->(target(foo));

event->target(foo);

event->(target(foo));

foo->bar := baz;

foo->bar === baz;

event->target##value(foo);

event->target##(value(foo));

(foo^)->bar;

location##streets.foo[1];

(event->target^)##value;

event->target^ #= value;

foo->f(. a, b);
foo->f(. a, b)->g(. c, d);
foo->([@attr] f(. a, b))->([@attr2] f(. a, b));
foo->f(.);
foo->f(.)->g(.);
foo->([@attr] f(.))->([@attr] g(.));

("some-string" ++ "another")->more;
(-1)->foo;
- 1->foo;
!foo->bar;
(!foo)->bar;

a->(b##c);

a->b##c;

(
  switch (saveStatus) {
  | Pristine => ""
  | Saved => "Saved"
  | Saving => "Saving"
  | Unsaved => "Unsaved"
  }
)
->str;

<div>
  (
    switch (saveStatus) {
    | Pristine => ""
    | Saved => "Saved"
    | Saving => "Saving"
    | Unsaved => "Unsaved"
    }
  )
  ->str
</div>;

blocks->(blocks => {"blocks": blocks});
<div>
  blocks->(blocks => {"blocks": blocks})
</div>;

(state.title == "" ? "untitled" : state.title)
->str;

<title>
  (state.title == "" ? "untitled" : state.title)
  ->str
</title>;

ReasonReact.Router.watchUrl(url =>
  Route.urlToRoute(url)->ChangeView->(self.send)
);
ReasonReact.Router.watchUrl(url =>
  Route.urlToRoute(url)->ChangeView->self.send
);

window->Webapi.Dom.Window.open_(
  ~url,
  ~name="authWindow",
  ~features=params,
);

window->Webapi.Dom.Window.open_(
  ~url,
  ~name="authWindow",
  () => {
    let x = 1;
    let y = 2;
    x + y;
  },
);

reactClass->setNavigationOptions(
  NavigationOptions.t(
    ~title="Title",
    ~gesturesEnabled=false,
    (),
  ),
);

Foo.Bar.reactClass->setNavigationOptions(
  NavigationOptions.t(
    ~title="Title",
    ~gesturesEnabled=false,
    (),
  ),
);

foo##bar
->setNavigationOptions(
    NavigationOptions.t(
      ~title="Title",
      ~gesturesEnabled=false,
      (),
    ),
  );

<div>
  {items
   ->Belt.Array.map(ReasonReact.string)
   ->ReasonReact.array}
</div>;

a->(b->c);

<div> {T.t("value")->ReasonReact.string} </div>;

<div> {url->a(b, _)} </div>;
<div> {url->a(b, _)->a(b, _)} </div>;

foo->Option.map(fn(_, arg));
