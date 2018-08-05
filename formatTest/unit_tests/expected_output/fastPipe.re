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
