foo |. f |. g |. h;

bar->f->g->h;

compilation
->Plugin.buildAssets
->Js.Json.stringify
->Node.Fs.writeFileAsUtf8Sync(_, path);
