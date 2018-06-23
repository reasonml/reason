foo->f->g->h;

bar->f->g->h;

compilation
->Plugin.buildAssets
->Js.Json.stringify
->Node.Fs.writeFileAsUtf8Sync(_, path);

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
